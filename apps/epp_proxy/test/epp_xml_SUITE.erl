-module(epp_xml_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(sampleCommandList,
        "<epp>
           <command>
            <login>
            <clID>test</clID>
            <pw>test</pw>
            </login>
            <clTRID>sample1trid</clTRID>
            </command>
            </epp>").

-define(validXMLNotEPPList,
        "<user>
           <name>test</name>
            <email>test@test.com</email>
            </user>").

-export([all/0]).
-export([parse_not_a_list_or_binary_test_case/1,
         parse_sample_valid_xml_binary_test_case/1,
         parse_sample_valid_xml_list_test_case/1,
         parse_sample_invalid_xml_binary_test_case/1,
         parse_sample_invalid_xml_list_test_case/1,
         find_cltrid_binary_test_case/1,
         find_cltrid_empty_binary_test_case/1,
         find_cltrid_empty_list_test_case/1,
         find_cltrid_list_test_case/1,
         find_cltrid_atom_test_case/1,
         get_command_success_test_case/1,
         get_command_xml_not_epp_failure_test_case/1,
         get_command_failure_test_case/1]).

all() -> [parse_not_a_list_or_binary_test_case,
          parse_sample_valid_xml_binary_test_case,
          parse_sample_valid_xml_list_test_case,
          parse_sample_invalid_xml_binary_test_case,
          parse_sample_invalid_xml_list_test_case,
          find_cltrid_binary_test_case,
          find_cltrid_empty_binary_test_case,
          find_cltrid_empty_list_test_case,
          find_cltrid_list_test_case,
          find_cltrid_atom_test_case,
          get_command_success_test_case,
          get_command_xml_not_epp_failure_test_case,
          get_command_failure_test_case].

parse_not_a_list_or_binary_test_case(_Config) ->
    Input = 1234,
    ExpectedOutput = {error, {fatal, {expected_binary_or_list}}},
    ExpectedOutput = epp_xml:parse(Input),
    ok.

parse_sample_valid_xml_list_test_case(_Config) ->
    Input = ?sampleCommandList,
    {ok, Result} = epp_xml:parse(Input),
    ?assertEqual(["epp", "command", "login", "clID", "pw", "clTRID"],
                 Result),
    ok.

parse_sample_valid_xml_binary_test_case(_Config) ->
    Input = list_to_binary(?sampleCommandList),
    {ok, Result} = epp_xml:parse(Input),
    ?assertEqual(["epp", "command", "login", "clID", "pw", "clTRID"],
                 Result),
    ok.

parse_sample_invalid_xml_list_test_case(_Config) ->
    Input = "Some text",
    ExpectedResult = {error, "Malformed: Illegal character in prolog"},
    ?assertEqual(ExpectedResult, epp_xml:parse(Input)),
    ok.

parse_sample_invalid_xml_binary_test_case(_Config) ->
    Input = <<"</epp>\n">>,
    ExpectedResult = {error, {badmatch, []}},
    ?assertEqual(ExpectedResult, epp_xml:parse(Input)),
    ok.

%% find_cltrid
find_cltrid_empty_list_test_case(_Config) ->
    Input = "",
    nomatch = epp_xml:find_cltrid(Input),
    ok.

find_cltrid_list_test_case(_Config) ->
    Input = list_to_binary(?sampleCommandList),
    <<"sample1trid">> = epp_xml:find_cltrid(Input),
    ok.

find_cltrid_empty_binary_test_case(_Config) ->
    Input = <<"">>,
    nomatch = epp_xml:find_cltrid(Input),
    ok.

find_cltrid_binary_test_case(_Config) ->
    Input = ?sampleCommandList,
    <<"sample1trid">> = epp_xml:find_cltrid(Input),
    ok.

find_cltrid_atom_test_case(_Config) ->
    Input = atom,
    nomatch = epp_xml:find_cltrid(Input),
    ok.

%% get_command
get_command_success_test_case(_Config) ->
    %% We require an existing xlmElement record to pass around.
    {ok, XMLElement} = epp_xml:parse(?sampleCommandList),
    Command = epp_xml:get_command(XMLElement),
    "login" = Command,
    ok.

get_command_xml_not_epp_failure_test_case(_Config) ->
    {ok, XMLElement} = epp_xml:parse(?validXMLNotEPPList),
    Command = epp_xml:get_command(XMLElement),
    undefined = Command,
    ok.

get_command_failure_test_case(_Config) ->
    %% Can pass any garbage, should get back undefined.,
    Command = epp_xml:get_command("Some random string"),
    undefined = Command,
    ok.
