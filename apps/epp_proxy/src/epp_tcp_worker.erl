-module(epp_tcp_worker).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, start_link/1]).
-export([code_change/3]).

-export([request/3]).

-record(state,{socket, length, session_id}).
-record(request,{method, url, body, cookies, headers}).

init(Socket) ->
    logger:info("Created a worker process"),
    SessionId = session_id(),
    {ok, #state{socket=Socket, session_id=SessionId}}.

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

handle_cast(serve, State = #state{socket=Socket}) ->
    {noreply, State#state{socket=Socket}};
handle_cast(greeting, State = #state{socket=Socket, session_id=SessionId}) ->
    Request = request("hello", SessionId, ""),
    logger:info("Request: ~p~n", [Request]),

    {_Status, _StatusCode, _Headers, ClientRef} =
        hackney:request(Request#request.method, Request#request.url,
                        Request#request.headers, Request#request.body,
                        [{cookie, Request#request.cookies}, insecure]),

    {ok, Body} = hackney:body(ClientRef),

    frame_to_socket(Body, Socket),
    gen_server:cast(self(), process_command),
    {noreply, State#state{socket=Socket, session_id=SessionId}};

handle_cast(process_command, State = #state{socket=Socket, session_id=SessionId}) ->
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
    Command = epp_xml:get_command(XMLRecord),

    Request = request(Command, SessionId, Frame),
    logger:info("Request: ~p~n", [Request]),

    {_Status, _StatusCode, _Headers, ClientRef} =
        hackney:request(Request#request.method, Request#request.url,
                        Request#request.headers, Request#request.body,
                        [{cookie, Request#request.cookies}, insecure]),

    {ok, Body} = hackney:body(ClientRef),

    frame_to_socket(Body, Socket),

    %% On logout, close the socket.
    %% Else, go back to the beginning of the loop.
    if
        Command =:= "logout" ->
            ok = gen_tcp:shutdown(Socket, read_write),
            {stop, normal, State};
        true ->
            gen_server:cast(self(), process_command),
            {noreply, State#state{socket=Socket, session_id=SessionId}}
    end.

handle_call(_E, _From, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Private function
write_line(Socket, Line) ->
    ok = gen_tcp:send(Socket, Line).

read_length(Socket) ->
    case gen_tcp:recv(Socket, 4) of
        {ok, Data} ->
            Length = binary:decode_unsigned(Data, big),
            LengthToReceive = epp_util:frame_length_to_receive(Length),
            {ok, LengthToReceive};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

read_frame(Socket, FrameLength) ->
    case gen_tcp:recv(Socket, FrameLength) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

session_id() ->
    UniqueMap = epp_util:create_map(self()),
    BinaryHash = epp_util:create_session_id(UniqueMap),
    BinaryHash.

%% Map request and return values
request(Command, SessionId, RawFrame) ->
    URL = epp_router:route_request(Command),
    RequestMethod = request_method(Command),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    case Command of
        "hello" ->
            Body = "";
        _ ->
            Body = {multipart, [{<<"raw_frame">>, RawFrame}]}
        end,
    Headers = [],
    #request{url=URL, method=RequestMethod, body=Body, cookies=[Cookie],
             headers=Headers}.

%% request method: GET for greeting, POST for everything else.
request_method("hello") ->
    get;
request_method(<<"hello">>) ->
    get;
request_method(_) ->
    post.

%% Wrap a message in EPP frame, and then send it to socket.
frame_to_socket(Message, Socket) ->
    Length = epp_util:frame_length_to_send(Message),
    ByteSize = << Length:32/big >>,
    write_line(Socket, ByteSize),
    write_line(Socket, Message).
