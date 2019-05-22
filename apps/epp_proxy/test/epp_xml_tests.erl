-module(epp_xml_tests).

-include_lib("eunit/include/eunit.hrl").
%% This is required for parse tests.
-include_lib("xmerl/include/xmerl.hrl").

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

%% parse
parse_not_a_list_or_binary_test() ->
    Input = 1234,
    ExpectedOutput = {error, {fatal, {expected_binary_or_list}}},
    ?assertEqual(ExpectedOutput, epp_xml:parse(Input)).

parse_sample_valid_xml_list_test() ->
    Input = ?sampleCommandList,
    {ok, Record} = epp_xml:parse(Input),
    ?assert(is_record(Record, xmlElement)).

parse_sample_valid_xml_binary_test() ->
    Input = list_to_binary(?sampleCommandList),
    {ok, Record} = epp_xml:parse(Input),
    ?assert(is_record(Record, xmlElement)).

parse_sample_invalid_xml_list_test() ->
    Input = "Some text",
    {error, Error} = epp_xml:parse(Input).

parse_sample_invalid_xml_binary_test() ->
    Input = list_to_binary("Some text"),
    {error, Error} = epp_xml:parse(Input).

%% find_cltrid
find_cltrid_empty_list_test() ->
    Input = "",
    ?assertEqual(nomatch, epp_xml:find_cltrid(Input)).

find_cltrid_list_test() ->
    Input = list_to_binary(?sampleCommandList),
    ?assertEqual(<<"sample1trid">>, epp_xml:find_cltrid(Input)).

find_cltrid_empty_binary_test() ->
    Input = <<"">>,
    ?assertEqual(nomatch, epp_xml:find_cltrid(Input)).

find_cltrid_binary_test() ->
    Input = ?sampleCommandList,
    ?assertEqual(<<"sample1trid">>, epp_xml:find_cltrid(Input)).


%% get_command
