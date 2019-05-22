-module(router).

-export([route_request/1, is_valid_epp_command/1]).

-define(validCommands, ["hello", "login", "logout", "check", "info", "poll",
                        "create", "delete", "renew", "update", "transfer"]).

%% By default, return test values. This might cause issues in production.
-define(baseSessionUrl,
        case application:get_env(epp_proxy, epp_session_url) of
            undefined -> "https://registry.test/epp/session/";
            {ok, Value} -> Value
            end).

-define(baseCommandUrl,
        case application:get_env(epp_proxy, epp_command_url) of
            undefined -> "https://registry.test/epp/command/";
            {ok, Value} -> Value
            end).

%% Save yourself some checking beforehand.
is_valid_epp_command(Command) ->
    lists:member(Command, ?validCommands).

%% Base router
route_request(Command) when is_binary(Command) ->
    List = binary_to_list(Command),
    url_map(List);
route_request(Command) when is_list(Command) -> url_map(Command).

%% Actually route to places
url_map(Command) when is_list(Command) ->
    case Command of
        %% Session commands
        "hello"   -> string:concat(base_session_url(), Command);
        "login"   -> string:concat(base_session_url(), Command);
        "logout"  -> string:concat(base_session_url(), Command);
        %% Poll commands
        "check"   -> string:concat(base_command_url(), Command);
        "info"    -> string:concat(base_command_url(), Command);
        "poll"    -> string:concat(base_command_url(), Command);
        %% Transform commands
        "create"  -> string:concat(base_command_url(), Command);
        "delete"  -> string:concat(base_command_url(), Command);
        "renew"   -> string:concat(base_command_url(), Command);
        "update"  -> string:concat(base_command_url(), Command);
        % Transfer is both poll and query
        "transfer" -> string:concat(base_command_url(), Command)
        % Anything else should fail.
     end.

%% Just return the macros as defined
base_session_url() ->
    ?baseSessionUrl.

base_command_url() ->
    ?baseCommandUrl.
