-module(epp_router).

-export([request_method/1, route_request/1]).

-define(validCommands,
        ["hello", "login", "logout", "check", "info", "poll",
         "create", "delete", "renew", "update", "transfer"]).

%% 47 is the character code
-define(forwardSlash, 47).

%% request method: GET for greeting, POST for everything else.
request_method("hello") -> get;
request_method(<<"hello">>) -> get;
request_method("error") -> get;
request_method(<<"error">>) -> get;
request_method(_) -> post.

%% Base router
route_request(Command) when is_binary(Command) ->
    List = binary_to_list(Command), url_map(List);
route_request(Command) when is_list(Command) ->
    url_map(Command).

%% Actually route to places
url_map(Command) when is_list(Command) ->
    case Command of
        %% Session commands
        "hello" ->
            unicode:characters_to_list([base_session_url(),
                                        Command]);
        "login" ->
            unicode:characters_to_list([base_session_url(),
                                        Command]);
        "logout" ->
            unicode:characters_to_list([base_session_url(),
                                        Command]);
        %% Poll commands
        "check" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
        "info" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
        "poll" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
        %% Transform commands
        "create" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
        "delete" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
        "renew" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
        "update" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
                                                % Transfer is both poll and query
        "transfer" ->
            unicode:characters_to_list([base_command_url(),
                                        Command]);
                                                % Error route
        "error" ->
            base_error_url()        % Anything else should fail.
    end.

%% This allows the person who configures proxy to not care about trailing
%% slashes in HTTP.
appendable_route(Route) ->
    case lists:last(Route) of
        ?forwardSlash -> Route;
        _ -> unicode:characters_to_list([Route, "/"])
    end.

%% This allows the person who configures proxy to not care about trailing
%% slashes in HTTP.
parametrizable_route(Route) ->
    case lists:last(Route) of
        ?forwardSlash -> lists:droplast(Route);
        _ -> Route
    end.

%% Every time a request is made, this will go to ETS to check what's the route,
%% But that is fast enough to not be noticed by anyone.
base_session_url() ->
    case application:get_env(epp_proxy, epp_session_url) of
        undefined -> "https://registry.test/epp/session/";
        {ok, Value} -> appendable_route(Value)
    end.

base_command_url() ->
    case application:get_env(epp_proxy, epp_command_url) of
        undefined -> "https://registry.test/epp/command/";
        {ok, Value} -> appendable_route(Value)
    end.

base_error_url() ->
    case application:get_env(epp_proxy, epp_error_url) of
        undefined -> "https://registry.test/epp/error";
        {ok, Value} -> parametrizable_route(Value)
    end.
