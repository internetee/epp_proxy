-module(epp_tcp_worker).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, start_link/1]).
-export([terminate/2, code_change/3]).

-record(state,{socket, length, session_id}).

init(Socket) ->
    logger:info("Created a test process"),
    {ok, #state{socket=Socket}}.

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

handle_cast(serve, State = #state{socket=Socket}) ->
    {noreply, State#state{socket=Socket}};
handle_cast(greeting, State = #state{socket=Socket}) ->
    SessionId = session_id(),

    {Status, StatusCode, Headers, ClientRef} =
        hackney:request(get, router:route_request("hello"), [], "",
                        [{cookie, [<<"session=">>, SessionId]}, insecure]),

    Body = <<"Some BODY">>,

    Length = byte_size(Body) + 4,
    ByteSize = << Length:32/big >>,
    io:format("State: ~p~n", [State]),
    write_line(Socket, ByteSize),
    write_line(Socket, Body),
    gen_server:cast(self(), read_length),
    {noreply, State#state{socket=Socket}};
handle_cast(read_length, State = #state{socket=Socket}) ->
    case read_length(Socket) of
        {ok, Data} ->
            NewState = State#state{length=Data},
            gen_server:cast(self(), read_frame),
            {noreply, NewState};
        {error, _Details} ->
            {stop, normal, State}
        end;
handle_cast(read_frame, State = #state{socket=Socket, length=Length}) ->
    case read_frame(Socket, Length) of
        {ok, Data} ->
            ByteLength = byte_size(Data) + 4,
            ByteSize = << ByteLength:32/big >>,
            write_line(Socket, ByteSize),
            write_line(Socket, Data),
            gen_server:cast(self(), read_length),
            {noreply, State};
        {error, _Details} ->
            {stop, normal, State}
    end;
handle_cast(Message, State) ->
    logger:info("Received message ~p.~n", [Message]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Private function
write_line(Socket, Line) ->
    gen_tcp:send(Socket, Line).

read_length(Socket) ->
    case gen_tcp:recv(Socket, 4) of
        {ok, Data} ->
            Length = binary:decode_unsigned(Data, big),
            io:format("Preparing to receive: ~p~n", [Length]),
            {ok, Length - 4};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

read_frame(Socket, FrameLength) ->
    case gen_tcp:recv(Socket, FrameLength) of
        {ok, Data} ->
            io:format("Frame: ~p~n", [Data]),
            {ok, Data};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

session_id() ->
    UniqueMap = epp_util:create_map(self()),
    BinaryHash = epp_util:create_session_id(UniqueMap),
    BinaryHash.
