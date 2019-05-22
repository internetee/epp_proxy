-module(router_tests).

-include_lib("eunit/include/eunit.hrl").

is_valid_epp_command_test() ->
    Commands = ["hello", "login", "logout", "check", "info", "poll",
               "create", "delete", "renew", "update", "transfer"],
    lists:foreach(fun (N) ->
                          ?assert(router:is_valid_epp_command(N))
                  end,
                  Commands).

%% TODO: Make less verbose and repetitive
hello_url_test() ->
    ?assertEqual("https://registry.test/epp/session/hello", router:route_request("hello")),
    ?assertEqual("https://registry.test/epp/session/hello", router:route_request(<<"hello">>)).

login_url_test() ->
    ?assertEqual("https://registry.test/epp/session/login", router:route_request("login")),
    ?assertEqual("https://registry.test/epp/session/login", router:route_request(<<"login">>)).

logout_url_test() ->
    ?assertEqual("https://registry.test/epp/session/logout", router:route_request("logout")),
    ?assertEqual("https://registry.test/epp/session/logout", router:route_request(<<"logout">>)).

check_url_test() ->
    ?assertEqual("https://registry.test/epp/command/check", router:route_request("check")),
    ?assertEqual("https://registry.test/epp/command/check", router:route_request(<<"check">>)).

info_url_test() ->
    ?assertEqual("https://registry.test/epp/command/info", router:route_request("info")),
    ?assertEqual("https://registry.test/epp/command/info", router:route_request(<<"info">>)).

poll_url_test() ->
    ?assertEqual("https://registry.test/epp/command/poll", router:route_request("poll")),
    ?assertEqual("https://registry.test/epp/command/poll", router:route_request(<<"poll">>)).

create_url_test() ->
    ?assertEqual("https://registry.test/epp/command/create", router:route_request("create")),
    ?assertEqual("https://registry.test/epp/command/create", router:route_request(<<"create">>)).

delete_url_test() ->
    ?assertEqual("https://registry.test/epp/command/delete", router:route_request("delete")),
    ?assertEqual("https://registry.test/epp/command/delete", router:route_request(<<"delete">>)).

renew_url_test() ->
    ?assertEqual("https://registry.test/epp/command/renew", router:route_request("renew")),
    ?assertEqual("https://registry.test/epp/command/renew", router:route_request(<<"renew">>)).

update_url_test() ->
    ?assertEqual("https://registry.test/epp/command/update", router:route_request("update")),
    ?assertEqual("https://registry.test/epp/command/update", router:route_request(<<"update">>)).

transfer_url_test() ->
    ?assertEqual("https://registry.test/epp/command/transfer", router:route_request("transfer")),
    ?assertEqual("https://registry.test/epp/command/transfer", router:route_request(<<"transfer">>)).
