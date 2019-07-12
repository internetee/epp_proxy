-module(tls_client_SUITE).

-include("epp_proxy.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([frame_size_test_case/1,
         greetings_test_case/1]).

all() ->
    [frame_size_test_case,
     greetings_test_case].


init_per_suite(Config) ->
    application:ensure_all_started(epp_proxy),
    application:ensure_all_started(hackney),
    CWD = code:priv_dir(epp_proxy),
    Options = [binary,
               {certfile, filename:join(CWD, "test_ca/certs/webclient.crt.pem")},
	       {keyfile, filename:join(CWD, "test_ca/private/webclient.key.pem")},
               {active, false}],
    [{ssl_options, Options} | Config].

end_per_suite(Config) ->
    application:stop(epp_proxy),
    application:stop(hackney),
    Config.

%% Test Cases
frame_size_test_case(Config) ->
    Options = proplists:get_value(ssl_options, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    {ok, Data} = ssl:recv(Socket, 0, 1200),
    true = (byte_size(Data) =:= length_of_data(Data)),
    ok.

greetings_test_case(Config) ->
    Options = proplists:get_value(ssl_options, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    Data = receive_data(Socket),
    match_data(Data, "<greeting>"),
    ok.

%% Helper functions:
length_of_data(Data) ->
    EPPEnvelope = binary:part(Data, {0, 4}),
    ReportedLength = binary:decode_unsigned(EPPEnvelope, big),
    ReportedLength.

send_data(Message, Socket) ->
    Length = epp_util:frame_length_to_send(Message),
    ByteSize = <<Length:32/big>>,
    CompleteMessage = <<ByteSize/binary, Message/binary>>,
    ok = ssl:send(Socket, CompleteMessage).

receive_data(Socket) ->
    {ok, Data} = ssl:recv(Socket, 0, 1200),
    EppEnvelope = binary:part(Data, {0, 4}),
    ReportedLength = binary:decode_unsigned(EppEnvelope, big),
    binary:part(Data, {byte_size(Data), 4 - ReportedLength}).

match_data(Data, Pattern) ->
    {ok, MatchPattern} = re:compile(Pattern),
    {match, _Captured} = re:run(Data, Pattern).
