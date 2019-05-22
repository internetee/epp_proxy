-module(epp_tcp).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-export([serve/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [],
    accept(Port),
    log_message(Port),
    {ok, {SupFlags, ChildSpecs}}.

accept(Port) ->
    Options = [binary,
               {packet, raw},
               {active, false},
               {reuseaddr, true}],

    {ok, ListenSocket} = gen_tcp:listen(Port, Options),
    loop_acceptor(ListenSocket).

loop_acceptor(ListenSocket) ->
    {ok, Client} = gen_tcp:accept(ListenSocket),
    Pid = spawn_link(?MODULE, serve, [Client]),
    logger:info("PID: ~p", [Pid]),
    ok = gen_tcp:controlling_process(Client, Pid),
    loop_acceptor(ListenSocket).

serve(Client) ->
    logger:info("Client name ~s", [Client]),
    send_greeting(Client),
    read_length(Client),
    exit(shutdown).

send_greeting(Client) ->
    Greeting = "Foo bar baz",
    Length = string:length(Greeting) + 4,
    ByteSize = << Length:32/big >>,
    write_line(ByteSize, Client),
    write_line(Greeting, Client).

write_line(Line, Client) ->
    gen_tcp:send(Client, Line).

read_length(Client) ->
    case gen_tcp:recv(Client, 4) of
        {ok, Data} -> logger:info("Received bytes: ~p", [Data]);
        {error, Error} -> logger:info("Received bytes: ~p", [Error])
        end.

log_message(Port) ->
    logger:info("TCP listening on Port: ~p", [Port]).
