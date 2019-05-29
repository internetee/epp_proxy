-module(epp_util_tests).

-include_lib("eunit/include/eunit.hrl").

session_id_test() ->
    Pid = spawn(fun () -> ok end),
    SessionId = epp_util:session_id(Pid),
    ?assert(is_list(SessionId)),
    ?assert(length(SessionId) > 0).

create_map_test() ->
    Pid = spawn(fun () -> ok end),
    Map = epp_util:create_map(Pid),
    ?assertEqual(Pid, maps:get("pid", Map)),
    ?assert(is_list(maps:get("timestamp", Map))),
    ?assert(is_float(maps:get("random", Map))).

create_session_id_test() ->
    Pid = list_to_pid("<0.130.0>"),
    Random = 0.7131518292439796,
    Time = "2019-05-23T14:47:52+03:00",
    Map = #{"pid" => Pid, "random" => Random, "timestamp" => Time},
    Hash = epp_util:create_session_id(Map),
    ?assert(is_list(Hash)),
    ?assertEqual("88F49C2B1BDD6F2355BF4424A67C928DA7C5616B30F7C5B35F17747348AF61EF8723ED6BE6012D879CB3D6A7EC7A187660A56910ED896AE67DE533C212D", Hash).

frame_length_test() ->
    ?assertEqual(2, epp_util:frame_length("aa")),
    ?assertEqual(2, epp_util:frame_length(<<"aa">>)),
    ?assertEqual(2, epp_util:frame_length(<<"OÜ">>)).

frame_length_to_receive_test() ->
    ?assertEqual(2, epp_util:frame_length_to_receive(6)),
    ?assertEqual(0, epp_util:frame_length_to_receive(4)),
    ?assertError(function_clause, epp_util:frame_length_to_receive(-22)).

frame_length_to_send_test() ->
    ?assertEqual(18, epp_util:frame_length_to_send("<epp><command>")),
    ?assertEqual(4, epp_util:frame_length_to_send("")).

readable_ip_test() ->
    ?assertEqual(<<127,46,0,46,0,46,1>>, epp_util:readable_ip({127,0,0,1})),
    ?assertError(function_clause, epp_util:readable_ip({127,0,0,1,0})).
