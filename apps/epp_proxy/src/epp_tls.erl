-module(epp_tls).

-behaviour(supervisor).
-export([start_link/1, init/1, serve/1]).

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
               {reuseaddr, true},
               {verify, verify_peer},
               {depth, 1},
               {cacertfile, "/Users/maciej/Development/internetee/docker-images/shared/ca/certs/ca.crt.pem"},
               {certfile, "/Users/maciej/Development/internetee/docker-images/shared/ca/certs/apache.crt"},
               {keyfile, "/Users/maciej/Development/internetee/docker-images/shared/ca/private/apache.key"}],

    {ok, ListenSocket} = ssl:listen(Port, Options),
    loop_acceptor(ListenSocket).

loop_acceptor(ListenSocket) ->
    {ok, Client} = ssl:transport_accept(ListenSocket),
    Pid = spawn_link(?MODULE, serve, [Client]),
    logger:info("PID: ~p", [Pid]),
    ok = ssl:controlling_process(Client, Pid),
    loop_acceptor(ListenSocket).

serve(Client) ->
    {ok, TSocket} = ssl:handshake(Client),
    {ok, ClientCert} = ssl:peername(TSocket),
    logger:info("Client name ~s", [ClientCert]),
    timer:sleep(1000),
    exit(shutdown).

log_message(Port) ->
    logger:info("SSL Listening on Port: ~p", [Port]).
