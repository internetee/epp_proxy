-module(tls_client_SUITE).

-include("epp_proxy.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([frame_size_test_case/1]).

all() ->
    [frame_size_test_case].


init_per_suite(Config) ->
    application:ensure_all_started(epp_proxy),
    application:ensure_all_started(hackney),
    [{my_key, <<"my value">>} | Config].

%% Test Cases
frame_size_test_case(_Config) ->
    CWD = code:priv_dir(epp_proxy),
    Options = [binary,
               {certfile, filename:join(CWD, "test_ca/certs/webclient.crt.pem")},
	       {keyfile, filename:join(CWD, "test_ca/private/webclient.key.pem")},
               {active, false}],
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    {ok, Data} = ssl:recv(Socket, 0, 1200),
    true = (byte_size(Data) =:= length_of_data(Data)),
    ok.

end_per_suite(Config) ->
    application:stop(epp_proxy),
    application:stop(hackney),
    Config.

%% Helper functions:
length_of_data(Data) ->
    EPPEnvelope = binary:part(Data, {0, 4}),
    ReportedLength = binary:decode_unsigned(EPPEnvelope, big),
    ReportedLength.
