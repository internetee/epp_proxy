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
    ?assert(is_binary(Hash)),
    ?assertEqual(<<136,244,9,194,177,11,221,111,35,85,191,68,36,166,124,146,
                   141,167,197,97,107,48,247,197,179,95,23,7,71,52,138,246,30,
                   248,114,62,214,190,96,18,216,121,203,61,106,126,199,161,135,
                   102,0,165,105,16,237,137,106,230,125,229,51,194,1,45>>, Hash).
