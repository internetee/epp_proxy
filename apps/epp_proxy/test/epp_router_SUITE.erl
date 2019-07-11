-module(epp_router_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([request_method_test_case/1, hello_url_test_case/1, login_url_test_case/1,
         logout_url_test_case/1, check_url_test_case/1, info_url_test_case/1, poll_url_test_case/1,
         create_url_test_case/1, delete_url_test_case/1, renew_url_test_case/1,
         update_url_test_case/1, transfer_url_test_case/1, error_url_test_case/1]).

all() -> [request_method_test_case, hello_url_test_case, login_url_test_case,
          logout_url_test_case, check_url_test_case, info_url_test_case, poll_url_test_case,
          create_url_test_case, delete_url_test_case, renew_url_test_case,
          update_url_test_case, transfer_url_test_case, error_url_test_case].

%% Run Unit tests
request_method_test_case(_Config) ->
    get = epp_router:request_method("hello"),
    get = epp_router:request_method(<<"hello">>),
    get = epp_router:request_method("error"),
    get = epp_router:request_method(<<"error">>),
    post = epp_router:request_method("create"),
    post = epp_router:request_method(123),
    ok.

%% TODO: Make less verbose and repetitive
hello_url_test_case(_Config) ->
    "http://localhost:9292/epp/session/hello" = epp_router:route_request("hello"),
    "http://localhost:9292/epp/session/hello" = epp_router:route_request(<<"hello">>),
    ok.

login_url_test_case(_Config) ->
    "http://localhost:9292/epp/session/login" = epp_router:route_request("login"),
    "http://localhost:9292/epp/session/login" = epp_router:route_request(<<"login">>),
    ok.

logout_url_test_case(_Config) ->
    "http://localhost:9292/epp/session/logout" = epp_router:route_request("logout"),
    "http://localhost:9292/epp/session/logout" = epp_router:route_request(<<"logout">>),
    ok.

check_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/check" = epp_router:route_request("check"),
    "http://localhost:9292/epp/command/check" = epp_router:route_request(<<"check">>),
    ok.

info_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/info" = epp_router:route_request("info"),
    "http://localhost:9292/epp/command/info" = epp_router:route_request(<<"info">>),
    ok.

poll_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/poll" = epp_router:route_request("poll"),
    "http://localhost:9292/epp/command/poll" = epp_router:route_request(<<"poll">>),
    ok.

create_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/create" = epp_router:route_request("create"),
    "http://localhost:9292/epp/command/create" = epp_router:route_request(<<"create">>),
    ok.

delete_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/delete" = epp_router:route_request("delete"),
    "http://localhost:9292/epp/command/delete" = epp_router:route_request(<<"delete">>),
    ok.

renew_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/renew" = epp_router:route_request("renew"),
    "http://localhost:9292/epp/command/renew" = epp_router:route_request(<<"renew">>),
    ok.

update_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/update" = epp_router:route_request("update"),
    "http://localhost:9292/epp/command/update" = epp_router:route_request(<<"update">>),
    ok.

transfer_url_test_case(_Config) ->
    "http://localhost:9292/epp/command/transfer" = epp_router:route_request("transfer"),
    "http://localhost:9292/epp/command/transfer" = epp_router:route_request(<<"transfer">>),
    ok.

error_url_test_case(_Config) ->
    "http://localhost:9292/epp/error" = epp_router:route_request("error"),
    "http://localhost:9292/epp/error" = epp_router:route_request(<<"error">>),
    ok.
