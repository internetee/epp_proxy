-module(epp_router_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([request_method_case/1, hello_url_case/1, login_url_case/1,
         logout_url_case/1, check_url_case/1, info_url_case/1, poll_url_case/1,
         create_url_case/1, delete_url_case/1, renew_url_case/1,
         update_url_case/1, transfer_url_case/1, error_url_case/1]).

all() -> [request_method_case, hello_url_case, login_url_case,
          logout_url_case, check_url_case, info_url_case, poll_url_case,
          create_url_case, delete_url_case, renew_url_case,
          update_url_case, transfer_url_case, error_url_case].

%% Run Unit tests
request_method_case(_Config) ->
    get = epp_router:request_method("hello"),
    get = epp_router:request_method(<<"hello">>),
    get = epp_router:request_method("error"),
    get = epp_router:request_method(<<"error">>),
    post = epp_router:request_method("create"),
    post = epp_router:request_method(123),
    ok.

%% TODO: Make less verbose and repetitive
hello_url_case(_Config) ->
    "https://localhost:9292/epp/session/hello" = epp_router:route_request("hello"),
    "https://localhost:9292/epp/session/hello" = epp_router:route_request(<<"hello">>),
    ok.

login_url_case(_Config) ->
    "https://localhost:9292/epp/session/login" = epp_router:route_request("login"),
    "https://localhost:9292/epp/session/login" = epp_router:route_request(<<"login">>),
    ok.

logout_url_case(_Config) ->
    "https://localhost:9292/epp/session/logout" = epp_router:route_request("logout"),
    "https://localhost:9292/epp/session/logout" = epp_router:route_request(<<"logout">>),
    ok.

check_url_case(_Config) ->
    "https://localhost:9292/epp/command/check" = epp_router:route_request("check"),
    "https://localhost:9292/epp/command/check" = epp_router:route_request(<<"check">>),
    ok.

info_url_case(_Config) ->
    "https://localhost:9292/epp/command/info" = epp_router:route_request("info"),
    "https://localhost:9292/epp/command/info" = epp_router:route_request(<<"info">>),
    ok.

poll_url_case(_Config) ->
    "https://localhost:9292/epp/command/poll" = epp_router:route_request("poll"),
    "https://localhost:9292/epp/command/poll" = epp_router:route_request(<<"poll">>),
    ok.

create_url_case(_Config) ->
    "https://localhost:9292/epp/command/create" = epp_router:route_request("create"),
    "https://localhost:9292/epp/command/create" = epp_router:route_request(<<"create">>),
    ok.

delete_url_case(_Config) ->
    "https://localhost:9292/epp/command/delete" = epp_router:route_request("delete"),
    "https://localhost:9292/epp/command/delete" = epp_router:route_request(<<"delete">>),
    ok.

renew_url_case(_Config) ->
    "https://localhost:9292/epp/command/renew" = epp_router:route_request("renew"),
    "https://localhost:9292/epp/command/renew" = epp_router:route_request(<<"renew">>),
    ok.

update_url_case(_Config) ->
    "https://localhost:9292/epp/command/update" = epp_router:route_request("update"),
    "https://localhost:9292/epp/command/update" = epp_router:route_request(<<"update">>),
    ok.

transfer_url_case(_Config) ->
    "https://localhost:9292/epp/command/transfer" = epp_router:route_request("transfer"),
    "https://localhost:9292/epp/command/transfer" = epp_router:route_request(<<"transfer">>),
    ok.

error_url_case(_Config) ->
    "https://localhost:9292/epp/error" = epp_router:route_request("error"),
    "https://localhost:9292/epp/error" = epp_router:route_request(<<"error">>),
    ok.
