-module(epp_xml_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([run_eunit/1]).

all() -> [run_eunit].

%% Run Unit tests
run_eunit(_Config) ->
    ok = eunit:test(epp_xml_tests).
