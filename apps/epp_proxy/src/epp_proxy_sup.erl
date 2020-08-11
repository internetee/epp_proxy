%%%-------------------------------------------------------------------
%% @doc epp_proxy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(epp_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DevMode,
	application:get_env(epp_proxy, dev_mode)).

-define(TCPPort,
	case application:get_env(epp_proxy, tcp_port) of
	  undefined -> undefined;
	  {ok, Value} -> Value
	end).

-define(TLSPort,
	case application:get_env(epp_proxy, tls_port) of
	  undefined -> undefined;
	  {ok, Val} -> Val
	end).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 3,
		 period => 60},
    TCPAcceptor = #{id => epp_tcp_acceptor, type => worker,
		    modules => [epp_tcp_acceptor],
		    start => {epp_tcp_acceptor, start_link, [?TCPPort]}},
    TLSAcceptor = #{id => epp_tls_acceptor, type => worker,
		    modules => [epp_tls_acceptor],
		    start => {epp_tls_acceptor, start_link, [?TLSPort]}},
    PoolSupervisor = #{id => epp_pool_supervisor,
		       type => supervisor, modules => [epp_pool_supervisor],
		       start => {epp_pool_supervisor, start_link, []}},
    MemoryMonitor = #{id => memory_monitor, type => worker,
		      modules => [memory_monitor],
		      start => {memory_monitor, start_link, []}},
		TLSMonitor = #{id => epp_tls_monitor, type => worker,
			modules => [epp_tls_monitor],
			start => {epp_tls_monitor, start_link, []}},
    SharedSpecs = [TLSAcceptor, PoolSupervisor,
		   MemoryMonitor, TLSMonitor],
    ChildrenSpec = case ?DevMode of
		     {ok, true} -> [TCPAcceptor | SharedSpecs];
		     _ -> SharedSpecs
		   end,
    {ok, {SupFlags, ChildrenSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================

