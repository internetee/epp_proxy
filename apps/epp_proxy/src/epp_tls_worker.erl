-module(epp_tls_worker).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("epp_proxy.hrl").

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, start_link/1]).
-export([code_change/3]).

-export([request_from_map/1]).

-record(valid_frame, {command,
                      cl_trid,
                      raw_frame}).
-record(invalid_frame, {code,
                        cl_trid,
                        message}).
-record(state, {socket,
                session_id,
                headers }).

init(Socket) ->
    lager:info("Created a worker process: [~p]", [self()]),
    SessionId = epp_util:session_id(self()),
    {ok, #state{socket=Socket, session_id=SessionId}}.

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).


handle_cast(serve, State = #state{socket=Socket}) ->
    %% If certificate is revoked, this will fail right away here.
    %% mod_epp does exactly the same thing.
    {ok, SecureSocket} = ssl:handshake(Socket),
    NewState = state_from_socket(SecureSocket, State),
    {noreply, NewState};
handle_cast(greeting, State = #state{socket=Socket,
                                     session_id=SessionId,
                                     headers=Headers}) ->

    Request = epp_http_client:request_builder(#{command => "hello",
                                                session_id => SessionId,
                                                raw_frame => "",
                                                headers => Headers,
                                                cl_trid => nomatch}),

    {_Status, Body} = epp_http_client:request(Request),

    frame_to_socket(Body, Socket),
    gen_server:cast(self(), process_command),
    {noreply, State#state{socket=Socket, session_id=SessionId}};

%% Main loop of processing commands. Ends the connection when command is logout.
%%
handle_cast(process_command,
            State = #state{socket=Socket,session_id=SessionId,
                           headers=Headers}) ->
    RawFrame = frame_from_socket(Socket, State),

    case parse_frame(RawFrame) of
        #valid_frame{command=Command, cl_trid=ClTRID} ->
            Request = epp_http_client:request_builder(#{command => Command,
                                                        session_id => SessionId,
                                                        raw_frame => RawFrame,
                                                        headers => Headers,
                                                        cl_trid => ClTRID}),

            {_Status, Body} = epp_http_client:request(Request);
        #invalid_frame{message=Message, code=Code, cl_trid=ClTRID} ->
            Command = "error",
            Request = epp_http_client:request_builder(#{command => Command,
                                                        session_id => SessionId,
                                                        headers => Headers,
                                                        code => Code,
                                                        message => Message,
                                                        cl_trid => ClTRID}),
            {_Status, Body} = epp_http_client:error_request(Request)
        end,

    frame_to_socket(Body, Socket),

    %% On logout, close the socket.
    %% Else, go back to the beginning of the loop.
    if
        Command =:= "logout" ->
            ok = ssl:shutdown(Socket, read_write),
            {stop, normal, State};
        true ->
            gen_server:cast(self(), process_command),
            {noreply, State#state{socket=Socket, session_id=SessionId}}
    end.

handle_call(_E, _From, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Private functions
write_line(Socket, Line) ->
    ok = ssl:send(Socket, Line).

read_length(Socket) ->
    case ssl:recv(Socket, 4) of
        {ok, Data} ->
            Length = binary:decode_unsigned(Data, big),
            LengthToReceive = epp_util:frame_length_to_receive(Length),
            {ok, LengthToReceive};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

read_frame(Socket, FrameLength) ->
    case ssl:recv(Socket, FrameLength) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Map request and return values.
request_from_map(#{command := "error", session_id := SessionId,
                   code := Code, message := Message, headers:=Headers,
                   cl_trid := ClTRID}) ->
    URL = epp_router:route_request("error"),
    RequestMethod = epp_router:request_method("error"),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    QueryParams = query_params(Code, Message, ClTRID),
    Headers=Headers,
    Request = #epp_error_request{url=URL,
                           method=RequestMethod,
                           query_params=QueryParams,
                           cookies=[Cookie],
                           headers=Headers},
    lager:info("Error Request from map: [~p]~n", [Request]),
    Request;
request_from_map(#{command := Command, session_id := SessionId,
          raw_frame := RawFrame, headers:=Headers, cl_trid := ClTRID}) ->
    URL = epp_router:route_request(Command),
    RequestMethod = epp_router:request_method(Command),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    Body = request_body(Command, RawFrame, ClTRID),
    Headers=Headers,
    Request = #epp_request{url=URL,
                           method=RequestMethod,
                           body=Body,
                           cookies=[Cookie],
                           headers=Headers},
    lager:info("Request from map: [~p]~n", [Request]),
    Request;
request_from_map(#{command := Command, session_id := SessionId,
          raw_frame := RawFrame, common_name := CommonName,
          client_cert := ClientCert, peer_ip := PeerIp, cl_trid := ClTRID}) ->
    URL = epp_router:route_request(Command),
    RequestMethod = epp_router:request_method(Command),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    Body = request_body(Command, RawFrame, ClTRID),
    Headers = [{"SSL_CLIENT_CERT", ClientCert},
               {"SSL_CLIENT_S_DN_CN", CommonName},
               {"User-Agent", <<"EPP proxy">>},
               {"X-Forwarded-for", epp_util:readable_ip(PeerIp)}],
    Request = #epp_request{url=URL,
                           method=RequestMethod,
                           body=Body,
                           cookies=[Cookie],
                           headers=Headers},
    lager:info("Request from map: [~p]~n", [Request]),
    Request.

%% Return form data or an empty list.
request_body("hello", _, _) ->
    "";
request_body(_Command, RawFrame, nomatch) ->
    {multipart, [{<<"raw_frame">>, RawFrame}]};
request_body(_Command, RawFrame, ClTRID) ->
    {multipart, [{<<"raw_frame">>, RawFrame}, {<<"clTRID">>, ClTRID}]}.

query_params(Code, Message, nomatch) ->
    [{<<"code">>, Code}, {<<"msg">>, Message}];
query_params(Code, Message, ClTRID) ->
    [{<<"code">>, Code}, {<<"msg">>, Message}, {<<"clTRID">>, ClTRID}].

%% Wrap a message in EPP frame, and then send it to socket.
frame_to_socket(Message, Socket) ->
    Length = epp_util:frame_length_to_send(Message),
    ByteSize = << Length:32/big >>,
    write_line(Socket, ByteSize),
    write_line(Socket, Message).

%% First, listen for 4 bytes, then listen until the declared length.
%% Return the frame binary at the very end.
frame_from_socket(Socket, State) ->
    Length = case read_length(Socket) of
        {ok, Data} ->
            Data;
        {error, _Details} ->
            {stop, normal, State}
        end,

    Frame = case read_frame(Socket, Length) of
        {ok, FrameData} ->
            FrameData;
        {error, _FrameDetails} ->
            {stop, normal, State}
    end,
    Frame.

%% Extract state info from socket. Fail if you must.
state_from_socket(Socket, State) ->
    {ok, PeerCert} = ssl:peercert(Socket),
    {ok,  {PeerIp, _PeerPort}} = ssl:peername(Socket),
    {SSL_CLIENT_S_DN_CN, SSL_CLIENT_CERT} =
        epp_certs:headers_from_cert(PeerCert),
    Headers = [{"SSL_CLIENT_CERT", SSL_CLIENT_CERT},
               {"SSL_CLIENT_S_DN_CN", SSL_CLIENT_S_DN_CN},
               {"User-Agent", <<"EPP proxy">>},
               {"X-Forwarded-for", epp_util:readable_ip(PeerIp)}],
    NewState = State#state{socket=Socket, headers=Headers},
    lager:info("Established connection with: [~p]~n", [NewState]),
    NewState.

%% Get status, XML record, command and clTRID if defined
parse_frame(Frame) ->
    ClTRID = epp_xml:find_cltrid(Frame),
    case epp_xml:parse(Frame) of
        {ok, XMLRecord} ->
            Command = epp_xml:get_command(XMLRecord),
            #valid_frame{command=Command, cl_trid=ClTRID, raw_frame=Frame};
        {error, _} ->
            ErrorMessage = <<"Command syntax error.">>,
            #invalid_frame{code=2001, message=ErrorMessage, cl_trid=ClTRID}
        end.
