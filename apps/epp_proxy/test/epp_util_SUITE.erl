-module(epp_util_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([session_id_test_case/1, create_map_test_case/1, create_session_id_test_case/1,
         frame_length_test_case/1, frame_length_to_receive_test_case/1,
         frame_length_to_send_test_case/1, readable_ip_test_case/1]).

all() -> [session_id_test_case, create_map_test_case, create_session_id_test_case,
         frame_length_test_case, frame_length_to_receive_test_case,
         frame_length_to_send_test_case, readable_ip_test_case].

%% Todo: these should be property tests, not unit tests.
session_id_test_case(_Config) ->
    Pid = spawn(fun () -> ok end),
    SessionId = epp_util:session_id(Pid),
    true = is_list(SessionId),
    true = length(SessionId) > 0,
    ok.

create_map_test_case(_Config) ->
    Pid = spawn(fun () -> ok end),
    Map = epp_util:create_map(Pid),
    Pid = maps:get("pid", Map),
    true = is_list(maps:get("timestamp", Map)),
    true = is_float(maps:get("random", Map)),
    ok.

create_session_id_test_case(_Config) ->
    Pid = list_to_pid("<0.130.0>"),
    Random = 0.7131518292439796,
    Time = "2019-05-23T14:47:52+03:00",
    Map = #{"pid" => Pid, "random" => Random, "timestamp" => Time},
    Hash = epp_util:create_session_id(Map),
    true = is_list(Hash),
    "88F49C2B1BDD6F2355BF4424A67C928DA7C5616B30F7C5B35F17747348AF61EF8723ED6BE6012D879CB3D6A7EC7A187660A56910ED896AE67DE533C212D" = Hash,
    ok.

frame_length_test_case(_Config) ->
    2 = epp_util:frame_length("aa"),
    2 = epp_util:frame_length(<<"aa">>),
    2 = epp_util:frame_length(<<"OÜ">>),
    ok.

frame_length_to_receive_test_case(_Config) ->
    2 = epp_util:frame_length_to_receive(6),
    0 = epp_util:frame_length_to_receive(4),
    {'EXIT', {function_clause, _}} =
        (catch epp_util:frame_length_to_receive(-22)),
    ok.

frame_length_to_send_test_case(_Config) ->
    18 = epp_util:frame_length_to_send("<epp><command>"),
    4 = epp_util:frame_length_to_send(""),
    ok.

readable_ip_test_case(_Config) ->
    <<"127.0.0.1">> = epp_util:readable_ip({127,0,0,1}),
    {'EXIT', {function_clause, _}}
        = (catch epp_util:readable_ip({127,0,0,1,0})),
    ok.
