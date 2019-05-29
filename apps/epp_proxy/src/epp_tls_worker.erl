-module(epp_tls_worker).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("epp_proxy.hrl").

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, start_link/1]).
-export([code_change/3]).

-export([request/7]).

-record(state,{socket, session_id, common_name, client_cert, peer_ip}).

init(Socket) ->
    lager:info("Created a worker process"),
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
handle_cast(greeting, State = #state{socket=Socket, common_name=SSL_CLIENT_S_DN_CN,
                                     client_cert=SSL_CLIENT_CERT,
                                     session_id=SessionId,
                                     peer_ip=PeerIp}) ->
    Request = request("hello", SessionId, "", SSL_CLIENT_S_DN_CN,
                      SSL_CLIENT_CERT, PeerIp, nomatch),

    {_Status, _StatusCode, _Headers, ClientRef} =
        hackney:request(Request#epp_request.method, Request#epp_request.url,
                        Request#epp_request.headers, Request#epp_request.body,
                        [{cookie, Request#epp_request.cookies}, insecure]),

    {ok, Body} = hackney:body(ClientRef),

    frame_to_socket(Body, Socket),
    gen_server:cast(self(), process_command),
    {noreply, State#state{socket=Socket, session_id=SessionId}};
handle_cast(process_command, State = #state{socket=Socket,
                                            common_name=SSL_CLIENT_S_DN_CN,
                                            client_cert=SSL_CLIENT_CERT,
                                            session_id=SessionId,
                                            peer_ip=PeerIp}) ->
    Length = case read_length(Socket) of
        {ok, Data} ->
            Data;
        {error, _Details} ->
            {stop, normal, State}
        end,

    Frame = case read_frame(Socket, Length) of
        {ok, FrameData} ->
            io:format("~p~n", [FrameData]),
            FrameData;
        {error, _FrameDetails} ->
            {stop, normal, State}
    end,

    {ok, XMLRecord} = epp_xml:parse(Frame),
    ClTRID= epp_xml:find_cltrid(Frame),
    Command = epp_xml:get_command(XMLRecord),

    Request = request(Command, SessionId, Frame, SSL_CLIENT_S_DN_CN,
                      SSL_CLIENT_CERT, PeerIp, ClTRID),

    {_Status, _StatusCode, _Headers, ClientRef} =
        hackney:request(Request#epp_request.method, Request#epp_request.url,
                        Request#epp_request.headers, Request#epp_request.body,
                        [{cookie, Request#epp_request.cookies}, insecure]),

    {ok, Body} = hackney:body(ClientRef),

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

%% Private function
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
%% TODO: Make arguments into a map.
request(Command, SessionId, RawFrame, CommonName, ClientCert, PeerIp, ClTRID) ->
    URL = epp_router:route_request(Command),
    RequestMethod = epp_router:request_method(Command),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    case Command of
        "hello" ->
            Body = "";
        _ ->
            Body = {multipart, request_body(RawFrame, ClTRID)}
        end,
    Headers = [{"SSL_CLIENT_CERT", ClientCert},
               {"SSL_CLIENT_S_DN_CN", CommonName},
               {"User-Agent", <<"EPP proxy">>},
               {"X-Forwarded-for", epp_util:readable_ip(PeerIp)}],
    Request = #epp_request{url=URL, method=RequestMethod, body=Body, cookies=[Cookie],
                           headers=Headers},
    lager:info("Request to be sent: [~p]~n", [Request]),
    Request.

%% Return form data
request_body(RawFrame, nomatch) ->
    [{<<"raw_frame">>, RawFrame}];
request_body(RawFrame, ClTRID) ->
    [{<<"raw_frame">>, RawFrame}, {<<"clTRID">>, ClTRID}].

%% Wrap a message in EPP frame, and then send it to socket.
frame_to_socket(Message, Socket) ->
    Length = epp_util:frame_length_to_send(Message),
    ByteSize = << Length:32/big >>,
    write_line(Socket, ByteSize),
    write_line(Socket, Message).

%% Extract state info from socket. Fail if you must.
state_from_socket(Socket, State) ->
    {ok, PeerCert} = ssl:peercert(Socket),
    {ok,  {PeerIp, _PeerPort}} = ssl:peername(Socket),
    {SSL_CLIENT_S_DN_CN, SSL_CLIENT_CERT} =
        epp_certs:headers_from_cert(PeerCert),
    NewState = State#state{socket=Socket, common_name=SSL_CLIENT_S_DN_CN,
                           client_cert=SSL_CLIENT_CERT, peer_ip=PeerIp},
    lager:info("Established connection with: [~p]~n", [NewState]),
    NewState.
