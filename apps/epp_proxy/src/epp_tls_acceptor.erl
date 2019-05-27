-module(epp_tls_acceptor).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(POOL_SUPERVISOR, epp_pool_supervisor).
-define(WORKER, epp_tls_worker).

-define(CaCertFile,
        case application:get_env(epp_proxy, cacertfile_path) of
            undefined -> undefined;
            {ok, Value} -> Value
            end).

-define(CertFile,
        case application:get_env(epp_proxy, certfile_path) of
            undefined -> undefined;
            {ok, Value} -> Value
            end).
-define(KeyFile,
        case application:get_env(epp_proxy, keyfile_path) of
            undefined -> undefined;
            {ok, Value} -> Value
            end).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, start_link/1]).

-record(state, {socket, port, options}).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []).

init(Port) ->
    Options = [binary,
               {packet, raw},
               {active, false},
               {reuseaddr, true},
               {verify, verify_peer},
               {depth, 1},
               {cacertfile, ?CaCertFile},
               {certfile, ?CertFile},
               {keyfile, ?KeyFile}],

    {ok, ListenSocket} = ssl:listen(Port, Options),
    gen_server:cast(self(), accept),

    {ok, #state{socket=ListenSocket, port=Port, options=Options}}.

handle_cast(accept, State = #state{socket=ListenSocket, port=Port, options=Options}) ->
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),
    {ok, NewOwner} = create_worker(AcceptSocket),
    ok = ssl:controlling_process(AcceptSocket, NewOwner),
    gen_server:cast(NewOwner, serve),
    gen_server:cast(NewOwner, greeting),
    gen_server:cast(self(), accept),
    {noreply, State#state{socket=ListenSocket, port=Port, options=Options}}.

handle_call(_E, _From, State) -> {noreply, State}.

create_worker(Socket) ->
    ChildSpec = #{id => rand:uniform(),
                   type => worker,
                   modules => [?WORKER],
                   restart => temporary,
                   start => {?WORKER, start_link, [Socket]}},
    supervisor:start_child(?POOL_SUPERVISOR, ChildSpec).
