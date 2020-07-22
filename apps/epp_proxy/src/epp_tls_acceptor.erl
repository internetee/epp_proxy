-module(epp_tls_acceptor).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(POOL_SUPERVISOR, epp_pool_supervisor).

-define(THIRTY_MINUTES_IN_MS, 30 * 30 * 1000).

-define(WORKER, epp_tls_worker).

%% gen_server callbacks
-export([handle_call/3, handle_cast/2, init/1,
	 start_link/1, terminate/2, handle_info/1, handle_info/2]).

-export([crl_file/0, crl_file/1]).

-record(state, {socket, port, options, timer}).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port,
			  []).

init(Port) ->
    DefaultOptions = [binary, {packet, raw},
		      {active, false}, {reuseaddr, true},
		      {verify, verify_peer}, {depth, 1},
		      {cacertfile, ca_cert_file()}, {certfile, cert_file()},
		      {keyfile, key_file()}],
    Options = handle_crl_check_options(DefaultOptions),
    TimerReference = erlang:send_after(?THIRTY_MINUTES_IN_MS, self(), reload_clr_file),
    {ok, ListenSocket} = ssl:listen(Port, Options),
    gen_server:cast(self(), accept),
    {ok,
     #state{socket = ListenSocket, port = Port,
	    options = Options, timer = TimerReference}}.

%% Acceptor has only one state that goes in a loop:
%% 1. Listen for a connection from anyone.
%% 2. Ask supervisor to return a worker.
%% 3. Pass the connection to the worker and make it a controlling process for
%%    the socket.
%% 4. Go back to listening.
handle_cast(accept,
	    State = #state{socket = ListenSocket, port = Port,
			   options = Options}) ->
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),
    {ok, NewOwner} = create_worker(AcceptSocket),
    ok = ssl:controlling_process(AcceptSocket, NewOwner),
    gen_server:cast(NewOwner, serve),
    gen_server:cast(NewOwner, greeting),
    gen_server:cast(self(), accept),
    {noreply,
     State#state{socket = ListenSocket, port = Port,
		 options = Options}}.

handle_info(reload_crl_file) ->
  case crl_file() of
    undefined -> {noreply};
    {ok, File} ->
      ssl_crl_cache:insert({file, File}),
      {noreply}
  end.

handle_info(reload_crl_file, State = #state{socket = ListenSocket, port = Port,
  options = _Options, timer = TimerReference}) ->
  _ = erlang:cancel_timer(TimerReference, [{async, true}, {info, false}]),
  TRef = erlang:send_after(?THIRTY_MINUTES_IN_MS, self(), reload_clr_file),
  DefaultOptions = [binary, {packet, raw},
    {active, false}, {reuseaddr, true},
    {verify, verify_peer}, {depth, 1},
    {cacertfile, ca_cert_file()}, {certfile, cert_file()},
    {keyfile, key_file()}],
  NewOptions = handle_crl_check_options(DefaultOptions),
  ok = ssl:close(ListenSocket),
  {ok, NewSocket} = ssl:listen(Port, NewOptions),
  gen_server:cast(self(), accept),
  {noreply, State#state{socket = NewSocket, port = Port,
    options = NewOptions, timer = TRef}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
    Timer = State#state.timer,
    _ = erlang:cancel_timer(Timer, [{async, true}, {info, false}]),
    ok.

handle_call(_E, _From, State) -> {noreply, State}.

%% Create a worker process. These are short lived and should not be restarted,
%% but for the purpose of order we should put them in a supervision tree.
create_worker(Socket) ->
    ChildSpec = #{id => rand:uniform(), type => worker,
		  modules => [?WORKER], restart => temporary,
		  start => {?WORKER, start_link, [Socket]}},
    supervisor:start_child(?POOL_SUPERVISOR, ChildSpec).

%% Private functions for returning paths to files. It costs almost nothing
%% to query them from ETS.
ca_cert_file() ->
    case application:get_env(epp_proxy, cacertfile_path) of
      undefined -> undefined;
      {ok, CaCertFile} -> epp_util:path_for_file(CaCertFile)
    end.

cert_file() ->
    case application:get_env(epp_proxy, certfile_path) of
      undefined -> undefined;
      {ok, CertFile} -> epp_util:path_for_file(CertFile)
    end.

key_file() ->
    case application:get_env(epp_proxy, keyfile_path) of
      undefined -> undefined;
      {ok, KeyFile} -> epp_util:path_for_file(KeyFile)
    end.

crl_file() ->
    case application:get_env(epp_proxy, crlfile_path) of
      undefined -> undefined;
      {ok, CrlFile} -> epp_util:path_for_file(CrlFile)
    end.

crl_file(path) ->
    epp_util:path_for_file(path).


%% In some environments, we do not perform a CRL check. Therefore, we need
%% different options proplist.
handle_crl_check_options(Options) ->
    case application:get_env(epp_proxy, crlfile_path) of
      undefined -> Options;
      {ok, CrlFile} ->
	  NewOptions = [{crl_check, peer},
			{crl_cache, {ssl_crl_hash_dir, {internal, [{dir, epp_util:path_for_file(CrlFile)}]}}}
			| Options],
	  NewOptions
    end.
