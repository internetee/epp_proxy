-module(tls_client_optional_cert_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         connect_without_client_cert_test/1, connect_with_client_cert_test/1]).

all() -> [connect_without_client_cert_test, connect_with_client_cert_test].

init_per_suite(Config) ->
    application:set_env(epp_proxy, require_client_certs, false),
    application:ensure_all_started(epp_proxy),
    application:ensure_all_started(hackney),
    CWD = code:priv_dir(epp_proxy),
    WithCert = [binary,
                {certfile, filename:join(CWD, "test_ca/certs/client.crt.pem")},
                {keyfile, filename:join(CWD, "test_ca/private/client.key.pem")},
                {active, false}],
    [{with_cert, WithCert} | Config].

end_per_suite(Config) ->
    application:unset_env(epp_proxy, require_client_certs),
    application:stop(epp_proxy),
    application:stop(hackney),
    Config.

connect_without_client_cert_test(_Config) ->
    {ok, Socket} = ssl:connect("localhost", 1443, [binary, {active, false}], 2000),
    {ok, _Data} = ssl:recv(Socket, 0, 1200),
    ok.

connect_with_client_cert_test(Config) ->
    Options = proplists:get_value(with_cert, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    {ok, _Data} = ssl:recv(Socket, 0, 1200),
    ok.