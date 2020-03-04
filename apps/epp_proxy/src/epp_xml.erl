-module(epp_xml).

-export([find_cltrid/1, get_command/1, parse/1]).

%% We are only interested in start element of a kind. The list produced
%% by this will need reversing after it is complete.
%% This parsing is naive, expects command/hello element to come right
%% after epp, but this should be everything we need for the purpose.
-define(PARSER_FUN,
	fun (Event, Acc) ->
		case Event of
		  {startElement, _, Name, _, _} -> [Name | Acc];
		  _ -> Acc
		end
	end).

%% Get command a list of elements found by erlsom.
%% Otherwise return undefined.
get_command(["epp", "command", Command | _Rest]) ->
    Command;
get_command(["epp", "hello" | _Rest]) -> "hello";
get_command(_) -> undefined.

%% xml_erl expects a list of characters, so let's give it to it.
%% Otherwise return an error tuple.
parse(Text) when is_list(Text) -> parse_list(Text);
parse(Text) when is_binary(Text) ->
    List = binary_to_list(Text), parse_list(List);
parse(_) -> {error, {fatal, {expected_binary_or_list}}}.

parse_list(List) when is_list(List) ->
    try erlsom:parse_sax(List, [], ?PARSER_FUN) of
      {ok, Result, _} ->
	  ProperResult = lists:reverse(Result), {ok, ProperResult}
    catch
      {error, Error} -> {error, Error};
      error:Error -> {error, Error};
      Error -> {error, Error}
    end.

%% The idea is that even when XML command is invalid,
%% we should recover cltrid from it, hence the regex use.
-spec find_cltrid(any()) -> binary() | nomatch.

find_cltrid(Text) when is_list(Text) -> run_regex(Text);
find_cltrid(Text) when is_binary(Text) ->
    run_regex(Text);
find_cltrid(_) -> nomatch.

run_regex(Text) ->
    {ok, MP} = re:compile("<clTRID>(?<cltrid>.+)</clTRID>"),
    case re:run(Text, MP, [{capture, ["cltrid"], binary}])
	of
      {match, [Binary]} -> Binary;
      nomatch -> nomatch
    end.
