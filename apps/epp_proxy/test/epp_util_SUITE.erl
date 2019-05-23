-module(epp_util_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([run_eunit/1]).

all() -> [run_eunit].

%% Run Unit tests.
%% Todo: these should be property tests, not unit tests.
run_eunit(_Config) ->
    ok = eunit:test(epp_util_tests).
