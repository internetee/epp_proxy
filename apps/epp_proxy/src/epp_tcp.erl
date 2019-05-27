-module(epp_tcp).

-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/1, init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
    process_flag(trap_exit, true),
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 60},
    ChildSpecs = [#{id => epp_tcp_acceptor,
                    start => {epp_tcp_acceptor, start_link, [Port]}}],
    log_message(Port),
    {ok, {SupFlags, ChildSpecs}}.

log_message(Port) ->
    logger:info("TCP listening on Port: ~p", [Port]).
