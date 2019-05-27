-module(epp_util_tests).

-include_lib("eunit/include/eunit.hrl").

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
