-module(tls_client_SUITE).

-include("epp_proxy.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([frame_size_test_case/1,
         greetings_test_case/1,
         session_test_case/1,
         valid_command_test_case/1,
         invalid_command_test_case/1,
         error_test_case/1]).

all() ->
    [frame_size_test_case,
     greetings_test_case,
     session_test_case,
     valid_command_test_case,
     invalid_command_test_case,
     error_test_case].

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

session_test_case(Config) ->
    Options = proplists:get_value(ssl_options, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    _Data = receive_data(Socket),
    ok = send_data(login_command(), Socket),
    LoginResponse = receive_data(Socket),
    match_data(LoginResponse, "Command completed successfully"),
    match_data(LoginResponse, "ccReg-5886259930"),
    LogoutCommand =
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
          "<epp xmlns=\"https://epp.tld.ee/schema/epp-ee-1.0.xsd\">\n"
          "<command>\n"
          "<logout>\n"
          "<clID>test_bestnames</clID>\n"
          "<pw>testtest</pw>\n"
          "<options>\n"
          "<version>1.0</version>\n"
          "<lang>en</lang>\n"
          "</options>\n"
          "<svcs>\n"
          "<objURI>https://epp.tld.ee/schema/domain-eis-1.0.xsd</objURI>\n"
          "<objURI>https://epp.tld.ee/schema/contact-ee-1.1.xsd</objURI>\n"
          "<objURI>urn:ietf:params:xml:ns:host-1.0</objURI>\n"
          "<objURI>urn:ietf:params:xml:ns:keyrelay-1.0</objURI>\n"
          "</svcs>\n"
          "</logout>\n"
          "</command>\n"
          "</epp>\n">>,
    ok = send_data(LogoutCommand, Socket),
    LogoutResponse = receive_data(Socket),
    match_data(LogoutResponse,
               "Command completed successfully; ending session"),
    %% After receiving logout, connection should be closed.
    {error, closed} = receive_data(Socket),
    ok.

valid_command_test_case(Config) ->
    Options = proplists:get_value(ssl_options, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    _Data = receive_data(Socket),
    ok = send_data(login_command(), Socket),
    _LoginResponse = receive_data(Socket),
    PollCommand =
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
          "<epp xmlns=\"https://epp.tld.ee/schema/epp-ee-1.0.xsd\">\n"
          "<command>\n"
          "<poll op=\"req\"/>\n"
          "<clTRID>foo bar baz</clTRID>\n"
          "</command>\n"
          "</epp>\n">>,
    ok = send_data(PollCommand, Socket),
    PollResponse = receive_data(Socket),
    match_data(PollResponse,
               "Command completed successfully; no messages"),
    ok.

%% Sending an invalid command frame should close the connection.
%% It also crashes the process.
invalid_command_test_case(Config) ->
    Options = proplists:get_value(ssl_options, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    _Data = receive_data(Socket),
    ok = send_data(login_command(), Socket),
    _LoginResponse = receive_data(Socket),
    InvalidCommand =
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
          "<epp xmlns=\"https://epp.tld.ee/schema/epp-ee-1.0.xsd\">\n"
          "<command>\n"
          "<fooo op=\"req\"/>\n"
          "<clTRID>foo bar baz</clTRID>\n"
          "</command>\n"
          "</epp>\n">>,
    ok = send_data(InvalidCommand, Socket),
    {error, closed} = receive_data(Socket),
    ok.

error_test_case(Config) ->
    Options = proplists:get_value(ssl_options, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    _Data = receive_data(Socket),
    ok = send_data(login_command(), Socket),
    _LoginResponse = receive_data(Socket),
    InvalidXml =
        <<"</epp>\n">>,
    ok = send_data(InvalidXml, Socket),
    ErrorResponse = receive_data(Socket),
    match_data(ErrorResponse,
               "Command syntax error."),
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
    case ssl:recv(Socket, 0, 1200) of
        {error, Reason} -> {error, Reason};
        {ok, Data } ->
            EppEnvelope = binary:part(Data, {0, 4}),
            ReportedLength = binary:decode_unsigned(EppEnvelope, big),
            binary:part(Data, {byte_size(Data), 4 - ReportedLength})
        end.

match_data(Data, Pattern) ->
    {ok, MatchPattern} = re:compile(Pattern),
    {match, _Captured} = re:run(Data, MatchPattern).

login_command() ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
      "<epp xmlns=\"https://epp.tld.ee/schema/epp-ee-1.0.xsd\">\n"
      "<command>\n"
      "<login>\n"
      "<clID>test_bestnames</clID>\n"
      "<pw>testtest</pw>\n"
      "<options>\n"
      "<version>1.0</version>\n"
      "<lang>en</lang>\n"
      "</options>\n"
      "<svcs>\n"
      "<objURI>https://epp.tld.ee/schema/domain-eis-1.0.xsd</objURI>\n"
      "<objURI>https://epp.tld.ee/schema/contact-ee-1.1.xsd</objURI>\n"
      "<objURI>urn:ietf:params:xml:ns:host-1.0</objURI>\n"
      "<objURI>urn:ietf:params:xml:ns:keyrelay-1.0</objURI>\n"
      "</svcs>\n"
      "</login>\n"
      "</command>\n"
      "</epp>\n">>.
