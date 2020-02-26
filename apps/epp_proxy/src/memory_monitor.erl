%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created: 20 Feb 2020
%%%-------------------------------------------------------------------
-module(memory_monitor).

-behaviour(gen_server).

-define(TEN_MINUTES_IN_MS, 10 * 30 * 1000).

-define(THIRTY_MINUTES_IN_MS, 30 * 30 * 1000).

-define(ONE_HOUR_IN_MS, 60 * 60 * 1000).

-export([init/1, start_link/0]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2]).

-export([log_memory/0]).

-record(state, {timer_ref  :: timer:tref()}).

-type state() :: #state{}.

-spec start_link() -> ignore | {error, _} | {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec init([]) -> {ok, state()}.

init([]) ->
    {ok, TimerReference} =
	timer:send_interval(?THIRTY_MINUTES_IN_MS, log_usage),
    erlang:send(self(), log_usage),
    {ok, #state{timer_ref = TimerReference}}.

%%%-------------------------------------------------------------------
%%% GenServer callbacks
%%%-------------------------------------------------------------------
-spec handle_call(_, _, State) -> {stop,
				   not_implemented, State}.

handle_call(_M, _F, State) ->
    {stop, not_implemented, State}.

-spec handle_cast(_, State) -> {stop, not_implemented,
				State}.

handle_cast(_M, State) ->
    {stop, not_implemented, State}.

-spec handle_info(log_usage, _) -> {noreply, _}.

handle_info(log_usage, State) ->
    ok = log_memory(), {noreply, State}.

-spec terminate(_, state()) -> ok.

terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.timer_ref), ok.

-spec code_change(_, _, _) -> {ok, _}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
log_memory() ->
    Mem = erlang:memory(),
    Values = lists:map(fun ({Name, Value}) ->
			       List = io_lib:format("~s: ~s",
						    [Name,
						     human_filesize(Value)]),
			       binary:list_to_bin(List)
		       end,
		       Mem),
    Values,
    lager:info("EPP proxy memory usage ~s.",
	       [lists:join(", ", Values)]).

human_filesize(Size) ->
    human_filesize(Size,
		   ["B", "KB", "MB", "GB", "TB", "PB"]).

human_filesize(S, [_ | [_ | _] = L]) when S >= 1024 ->
    human_filesize(S / 1024, L);
human_filesize(S, [M | _]) ->
    List = io_lib:format("~.2f ~s", [float(S), M]),
    binary:list_to_bin(List).
