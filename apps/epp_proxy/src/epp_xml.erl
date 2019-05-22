-module(epp_xml).

-export([find_cltrid/1, get_command/1, parse/1]).

-include_lib("xmerl/include/xmerl.hrl").

%% Get command from an xmlElement. Otherwise return undefined.
get_command(Record) when is_record(Record, xmlElement) ->
    case xmerl_xpath:string("name(/epp/command/*[1])", Record) of
        {xmlObj, string, []} -> undefined;
        {xmlObj, string, Command} -> Command
        end;
get_command(_) -> undefined.

%% xml_erl expects a list of characters, so let's give it to it.
%% Otherwise return an error tuple.
parse(Text) when is_list(Text) -> parse_list(Text);
parse(Text) when is_binary(Text) ->
    List = binary_to_list(Text),
    parse_list(List);
parse(_) -> {error, {fatal, {expected_binary_or_list}}}.

%% Parse a record that came from the wire and return a xmlElement record.
parse_list(List) when is_list(List) ->
    try xmerl_scan:string(List, [{quiet, 'true'}]) of
        {Record, []} when is_record(Record, xmlElement) -> {ok, Record}
    catch
        exit:X -> {error, X}
    end.

%% The idea is that even when XML command is invalid,
%% we should recover cltrid from it, hence the regex use.
-spec find_cltrid(any()) -> binary() | nomatch.
find_cltrid(Text) when is_list(Text) -> run_regex(Text);
find_cltrid(Text) when is_binary(Text) -> run_regex(Text);
find_cltrid(_) -> nomatch.

run_regex(Text) ->
    {ok, MP} = re:compile("<clTRID>(?<cltrid>.+)<\/clTRID>"),
    case re:run(Text, MP, [{capture, ["cltrid"], binary}]) of
        {match, [Binary]} -> Binary;
        nomatch -> nomatch
        end.
