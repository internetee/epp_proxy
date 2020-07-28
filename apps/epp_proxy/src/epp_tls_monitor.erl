%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% Monitor module for reloading epp_tls_acceptor on runtime
%%% Used to renew CRLs once in 30 minutes
%%% @end
%%% Created: 20 Feb 2020
%%%-------------------------------------------------------------------
-module(epp_tls_monitor).

-behaviour(gen_server).

-define(THIRTY_MINUTES_IN_MS, 30 * 60 * 1000).

-export([init/1, start_link/0]).

-export([code_change/3, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2]).

-export([reload_acceptor/0]).

-record(state, {timer_ref  :: timer:tref()}).

-type state() :: #state{}.

-spec start_link() -> ignore | {error, _} | {ok, pid()}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [],
    []).

-spec init([]) -> {ok, state()}.

init([]) ->
  TimerReference = erlang:send_after(?THIRTY_MINUTES_IN_MS, self(), reload_acceptor),
  erlang:send(self(), reload_acceptor),
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

-spec handle_info(reload_acceptor, _) -> {noreply, _}.

handle_info(reload_acceptor, State = #state{timer_ref = TimerReference}) ->
  _ = erlang:cancel_timer(TimerReference, [{async, true}, {info, false}]),
  TRef = erlang:send_after(?THIRTY_MINUTES_IN_MS, self(), reload_clr_file),
  ok = reload_acceptor(),
  {noreply, State#state{timer_ref = TRef}}.

-spec terminate(_, state()) -> ok.

terminate(_Reason, State) ->
  _ = erlang:cancel_timer(State#state.timer_ref, [{async, true}, {info, false}]),
  ok.

-spec code_change(_, _, _) -> {ok, _}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
reload_acceptor() ->
  supervisor:terminate_child(epp_proxy_sup, epp_tls_acceptor),
  supervisor:restart_child(epp_proxy_sup, epp_tls_acceptor),
  ok.
