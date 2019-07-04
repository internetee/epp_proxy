-module(epp_http_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("epp_proxy.hrl").

hello_request_builder_test() ->
    RequestMap = #{command => "hello", session_id => "Random",
                   cl_trid => "EE-123456789", raw_frame => "",
                   headers => [{"User-Agent", <<"EPP proxy">>}]},
    Request = epp_http_client:request_builder(RequestMap),
    ExpectedTuple = {epp_request,get,"https://registry.test/epp/session/hello",
                     [],
                     [<<"session=Random; Version=1">>],
                     [{"User-Agent",<<"EPP proxy">>}]},
    ?assert(is_record(Request, epp_request)),
    ?assertEqual(ExpectedTuple, Request).

error_request_builder_test() ->
    RequestMap = #{command => "error", session_id => "Random",
                   cl_trid => "EE-123456789", code => <<"2001">>,
                   message => <<"Expected better XML">>,
                   headers => [{"User-Agent", <<"EPP proxy">>}]},
    Request = epp_http_client:request_builder(RequestMap),
    ExpectedTuple = {epp_error_request,get,"https://registry.test/epp/error",
                     [{<<"code">>,<<"2001">>},
                      {<<"msg">>,<<"Expected better XML">>},
                      {<<"clTRID">>,"EE-123456789"}],
                     [<<"session=Random; Version=1">>],
                     [{"User-Agent",<<"EPP proxy">>}]},
    ?assert(is_record(Request, epp_error_request)),
    ?assertEqual(ExpectedTuple, Request).

command_request_builder_test() ->
    RequestMap = #{command => "create", session_id => "Random",
                   cl_trid => "EE-123456789", raw_frame => "Some XML here",
                   headers => [{"User-Agent", <<"EPP proxy">>}]},
    Request = epp_http_client:request_builder(RequestMap),
    ExpectedTuple = {epp_request,post,
                     "https://registry.test/epp/command/create",
                     {multipart,
                      [{<<"raw_frame">>,"Some XML here"},
                       {<<"clTRID">>,"EE-123456789"}]},
                     [<<"session=Random; Version=1">>],
                     [{"User-Agent",<<"EPP proxy">>}]},
    ?assert(is_record(Request, epp_request)),
    ?assertEqual(ExpectedTuple, Request).
