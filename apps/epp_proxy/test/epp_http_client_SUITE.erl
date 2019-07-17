-module(epp_http_client_SUITE).

-include("epp_proxy.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([hello_request_builder_test_case/1,
         error_request_builder_test_case/1,
         command_request_builder_test_case/1,
         registry_unreachable_test_case/1]).

all() ->
    [hello_request_builder_test_case,
     error_request_builder_test_case,
     command_request_builder_test_case,
     registry_unreachable_test_case].

init_per_suite(Config) ->
    application:ensure_all_started(hackney),
    Config.

end_per_suite(Config) ->
    application:stop(hackney),
    Config.

hello_request_builder_test_case(_Config) ->
    RequestMap = #{command => "hello", session_id => "Random",
                   cl_trid => "EE-123456789", raw_frame => "",
                   headers => [{"User-Agent", <<"EPP proxy">>}]},
    Request = epp_http_client:request_builder(RequestMap),
    ExpectedTuple = {epp_request,get,"http://localhost:9292/session/hello",
                     [],
                     [<<"session=Random; Version=1">>],
                     [{"User-Agent",<<"EPP proxy">>}], "hello"},
    true = is_record(Request, epp_request),
    ExpectedTuple =  Request.

error_request_builder_test_case(_Config) ->
    RequestMap = #{command => "error", session_id => "Random",
                   cl_trid => "EE-123456789", code => <<"2001">>,
                   message => <<"Expected better XML">>,
                   headers => [{"User-Agent", <<"EPP proxy">>}]},
    Request = epp_http_client:request_builder(RequestMap),
    ExpectedTuple = {epp_request,get,"http://localhost:9292/error",
                     [{<<"code">>,<<"2001">>},
                      {<<"msg">>,<<"Expected better XML">>},
                      {<<"clTRID">>,"EE-123456789"}],
                     [<<"session=Random; Version=1">>],
                     [{"User-Agent",<<"EPP proxy">>}], "error"},
    true = is_record(Request, epp_request),
    ExpectedTuple = Request,
    ok.

command_request_builder_test_case(_Config) ->
    RequestMap = #{command => "create", session_id => "Random",
                   cl_trid => "EE-123456789", raw_frame => "Some XML here",
                   headers => [{"User-Agent", <<"EPP proxy">>}]},
    Request = epp_http_client:request_builder(RequestMap),
    ExpectedTuple = {epp_request,post,
                     "http://localhost:9292/command/create",
                     {multipart,
                      [{<<"raw_frame">>,"Some XML here"},
		       {<<"frame">>,"Some XML here"},
                       {<<"clTRID">>,"EE-123456789"}]},
                     [<<"session=Random; Version=1">>],
                     [{"User-Agent",<<"EPP proxy">>}], "create"},
    true = is_record(Request, epp_request),
    ExpectedTuple = Request,
    ok.

registry_unreachable_test_case(_Config) ->
    Request = {epp_request,
               post,
               "http://localhost:9999/someurl",
               {multipart,
                [{<<"raw_frame">>,"Some XML here"},
		 {<<"frame">>,"Some XML here"},
                 {<<"clTRID">>,"EE-123456789"}]},
               [<<"session=Random; Version=1">>],
               [{"User-Agent",<<"EPP proxy">>}], "create"},
    {2400, _CannedResponse} = epp_http_client:request(Request),
    ok.
