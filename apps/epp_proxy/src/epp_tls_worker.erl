-module(epp_tls_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("epp_proxy.hrl").

%% gen_server callbacks
-export([handle_call/3, handle_cast/2, init/1,
	 start_link/1]).

-export([code_change/3]).

%% Initialize process
%% Assign an unique session id that will be passed on to http server as a cookie
init(Socket) ->
    lager:info("Created a worker process: [~p]", [self()]),
    SessionId = epp_util:session_id(self()),
    {ok, #state{socket = Socket, session_id = SessionId}}.

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%% First step after spinning off the process:
%% Perform an TLS handshake and gather data that we use a headers for
%% http request:
%% Common name,
%% Client certificate,
%% Client IP address,
%% If certificate is revoked, this will fail right away here.
%% mod_epp does exactly the same thing.
handle_cast(serve,
	    State = #state{socket = Socket,
			   session_id = _SessionId}) ->
    try
        % Check if we have a valid socket
        case ssl:peername(Socket) of
            {ok, {PeerIp, _PeerPort}} ->
                log_opened_connection(PeerIp),
                % Try to perform the handshake
                case ssl:handshake(Socket) of
                    {ok, SecureSocket} ->
                        NewState = state_from_socket(SecureSocket, State),
                        {noreply, NewState};
                    {error, notsup_on_transport_accept_socket} ->
                        lager:error("Socket not supported for TLS handshake. This may indicate the socket is not an SSL socket or was improperly initialized."),
                        {stop, normal, State};
                    {error, HandshakeError} ->
                        log_on_invalid_handshake(PeerIp, HandshakeError),
                        {stop, normal, State}
                end;
            {error, PeerError} ->
                lager:error("Invalid socket: cannot get peer information: ~p", [PeerError]),
                {stop, normal, State}
        end
    catch
        error:CatchError:Stacktrace ->
            lager:error("Exception during TLS handshake: ~p~nStacktrace: ~p", [CatchError, Stacktrace]),
            {stop, normal, State}
    end;
%% Step two: Using the state of the connection, get the hello route
%% from http server.  Send the response from HTTP server back to EPP
%% client.  When this succeeds, send "process_command" to self and
%% await further commands.
handle_cast(greeting,
	    State = #state{socket = Socket, session_id = SessionId,
			   headers = Headers}) ->
    Request = epp_http_client:request_builder(#{command =>
						    "hello",
						session_id => SessionId,
						raw_frame => "",
						headers => Headers,
						cl_trid => nomatch}),
    {_Status, Body} = epp_http_client:request(Request),
    frame_to_socket(Body, Socket),
    gen_server:cast(self(), process_command),
    {noreply,
     State#state{socket = Socket, session_id = SessionId}};
%% Step three to N:
%% Await input from client. Parse it, and perform an appropriate http request.
%% Commands go to commands, invalid XML goes to error.
%% Send the response from HTTP server back to EPP client.
%%
%% When the command from client was logout, close the connection and quit the
%% process.
%%
%% Otherwise send "process_command" again to self to repeat the process.
handle_cast(process_command,
	    State = #state{socket = Socket, session_id = SessionId,
			   headers = Headers}) ->
    RawFrame = frame_from_socket(Socket, State),
    case parse_frame(RawFrame) of
      #valid_frame{command = Command, cl_trid = ClTRID} ->
	  Request = epp_http_client:request_builder(#{command =>
							  Command,
						      session_id => SessionId,
						      raw_frame => RawFrame,
						      headers => Headers,
						      cl_trid => ClTRID});
      #invalid_frame{message = Message, code = Code,
		     cl_trid = ClTRID} ->
	  Command = "error",
	  Request = epp_http_client:request_builder(#{command =>
							  Command,
						      session_id => SessionId,
						      headers => Headers,
						      code => Code,
						      message => Message,
						      cl_trid => ClTRID})
    end,
    {_Status, Body} = epp_http_client:request(Request),
    frame_to_socket(Body, Socket),
    %% On logout, close the socket.
    %% Else, go back to the beginning of the loop.
    if Command =:= "logout" ->
	   case ssl:shutdown(Socket, read_write) of
	     ok -> {stop, normal, State};
	     {error, closed} -> {stop, normal, State}
	   end;
       true ->
	   gen_server:cast(self(), process_command),
	   {noreply,
	    State#state{socket = Socket, session_id = SessionId}}
    end.

handle_call(_E, _From, State) -> {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Wrap a message in EPP frame, and then send it to socket.
frame_to_socket(Message, Socket) ->
    Length = epp_util:frame_length_to_send(Message),
    ByteSize = <<Length:32/big>>,
    CompleteMessage = <<ByteSize/binary, Message/binary>>,
    write_line(Socket, CompleteMessage).

write_line(Socket, Line) -> ok = ssl:send(Socket, Line).

frame_from_socket(Socket, State) ->
    case ssl:recv(Socket, 0, ?DefaultTimeout) of
      {ok, Data} ->
	  EPPEnvelope = binary:part(Data, {0, 4}),
	  ReportedLength = binary:decode_unsigned(EPPEnvelope,
						  big),
	  read_until_exhausted(Socket, ReportedLength, Data);
      {error, closed} -> log_and_exit(State);
      {error, timeout} -> log_on_timeout(State)
    end.

%% When an EPP message is long, it will be received in smaller chunks.
%% For example, first 4 bytes equal 800 000, but we'd receive only 200 000
%% in the first chunk. In such a case, we should listen for more.
%%
%% Note that there is no case for messages the exceed the reported length of a
%% frame. Those cases are invalid from the perspective of EPP protocol and
%% should not be supported.
read_until_exhausted(Socket, ExpectedLength, Frame) ->
    if ExpectedLength =:= byte_size(Frame) ->
	   binary:part(Frame,
		       {byte_size(Frame), 4 - ExpectedLength});
       ExpectedLength > byte_size(Frame) ->
	   {ok, NextFrame} = ssl:recv(Socket, 0, ?DefaultTimeout),
	   NewFrame = <<Frame/binary, NextFrame/binary>>,
	   read_until_exhausted(Socket, ExpectedLength, NewFrame)
    end.

log_and_exit(State) ->
    lager:info("Client closed connection: [~p]~n", [State]),
    exit(normal).

log_on_timeout(State) ->
    lager:info("Client timed out: [~p]~n", [State]),
    exit(normal).

log_on_invalid_handshake(Ip, Error) ->
    ReadableIp = epp_util:readable_ip(Ip),
    lager:info("Failed SSL handshake. IP: ~s, Error: "
	       "[~p]~n",
	       [ReadableIp, Error]),
    exit(normal).

log_opened_connection(Ip) ->
    ReadableIp = epp_util:readable_ip(Ip),
    lager:info("New client connection. IP: ~s, Process: "
	       "~p.~n",
	       [ReadableIp, self()]).

%% Extract state info from socket. Fail if you must.
state_from_socket(Socket, State) ->
    {ok, {PeerIp, _PeerPort}} = ssl:peername(Socket),
    Headers = case ssl:peercert(Socket) of
        {ok, PeerCert} ->
            try
                {SSL_CLIENT_S_DN_CN, SSL_CLIENT_CERT} = epp_certs:headers_from_cert(PeerCert),
                [{"SSL-CLIENT-CERT", SSL_CLIENT_CERT},
                 {"SSL-CLIENT-S-DN-CN", SSL_CLIENT_S_DN_CN},
                 {"User-Agent", <<"EPP proxy">>},
                 {"X-Forwarded-for", epp_util:readable_ip(PeerIp)}]
            catch
                _:_ ->
                    lager:warning("Could not extract certificate information from client at IP: ~s", [epp_util:readable_ip(PeerIp)]),
                    [{"User-Agent", <<"EPP proxy">>},
                     {"X-Forwarded-for", epp_util:readable_ip(PeerIp)}]
            end;
        {error, _} ->
            lager:info("No client certificate provided from IP: ~s", [epp_util:readable_ip(PeerIp)]),
            [{"User-Agent", <<"EPP proxy">>},
             {"X-Forwarded-for", epp_util:readable_ip(PeerIp)}]
    end,
    NewState = State#state{socket = Socket,
			   headers = Headers},
    lager:info("Established connection with: [~p]~n",
	       [NewState]),
    NewState.

%% Get status, XML record, command and clTRID if defined.
%% Otherwise return an invalid frame with predefined error message and code.
parse_frame(Frame) ->
    ClTRID = epp_xml:find_cltrid(Frame),
    case epp_xml:parse(Frame) of
      {ok, XMLRecord} ->
	  Command = epp_xml:get_command(XMLRecord),
	  case epp_router:is_valid_command(Command) of
	    true ->
		#valid_frame{command = Command, cl_trid = ClTRID,
			     raw_frame = Frame};
	    false ->
		#invalid_frame{code = ?UnknownCommandErrorCode,
			       message = ?UnknownCommandErrorMessage,
			       cl_trid = ClTRID}
	  end;
      {error, _} ->
	  #invalid_frame{code = ?XMLErrorCode,
			 message = ?XMLErrorMessage, cl_trid = ClTRID}
    end.