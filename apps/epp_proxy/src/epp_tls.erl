-module(epp_tls).

-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/1, init/1, serve/1]).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
    process_flag(trap_exit, true),
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 60},
    ChildSpecs = [],
    log_message(Port),
    accept(Port),
    {ok, {SupFlags, ChildSpecs}}.

accept(Port) ->
    Options = [binary,
               {packet, raw},
               {active, false},
               {reuseaddr, true},
               {verify, verify_peer},
               {depth, 1},
               {cacertfile, "/Users/maciej/Development/internetee/docker-images/shared/ca/certs/ca.crt.pem"},
               {certfile, "/Users/maciej/Development/internetee/docker-images/shared/ca/certs/apache.crt"},
               {keyfile, "/Users/maciej/Development/internetee/docker-images/shared/ca/private/apache.key"}],

    {ok, ListenSocket} = ssl:listen(Port, Options),
    spawn_monitor(fun () -> loop_acceptor(ListenSocket) end).

loop_acceptor(ListenSocket) ->
    case ssl:transport_accept(ListenSocket) of
        {error, closed} -> exit(shutdown);
        {ok, Client} ->
            Pid = spawn_link(?MODULE, serve, [Client]),
            logger:info("PID: ~p", [Pid]),
            ok = ssl:controlling_process(Client, Pid)
    end,
    loop_acceptor(ListenSocket).

serve(Client) ->
    {ok, TSocket} = ssl:handshake(Client),
    {ok, ClientCert} = ssl:peercert(TSocket),
    logger:info("Client cert ~s", [ClientCert]),
    timer:sleep(100000),
    exit(shutdown).

log_message(Port) ->
    logger:info("SSL Listening on Port: ~p", [Port]).
