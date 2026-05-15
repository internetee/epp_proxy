-module(tls_client_optional_cert_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         connect_without_client_cert_test/1, connect_with_client_cert_test/1]).

all() -> [connect_without_client_cert_test, connect_with_client_cert_test].

init_per_suite(Config) ->
    application:ensure_all_started(hackney),
    application:ensure_all_started(epp_proxy),
    application:set_env(epp_proxy, require_client_certs, false),
    ok = restart_tls_acceptor(),
    CWD = code:priv_dir(epp_proxy),
    WithCert = client_ssl_options(
                 filename:join(CWD, "test_ca/certs/client.crt.pem"),
                 filename:join(CWD, "test_ca/private/client.key.pem")
                ),
    [{with_cert, WithCert} | Config].

end_per_suite(Config) ->
    application:set_env(epp_proxy, require_client_certs, true),
    ok = restart_tls_acceptor(),
    Config.

connect_without_client_cert_test(_Config) ->
    Options = [binary, {verify, verify_none}, {active, false}],
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    {ok, _Data} = ssl:recv(Socket, 0, 1200),
    ok.

connect_with_client_cert_test(Config) ->
    Options = proplists:get_value(with_cert, Config),
    {ok, Socket} = ssl:connect("localhost", 1443, Options, 2000),
    {ok, _Data} = ssl:recv(Socket, 0, 1200),
    ok.

client_ssl_options(CertFile, KeyFile) ->
    [binary,
     {verify, verify_none},
     {certfile, CertFile},
     {keyfile, KeyFile},
     {active, false}].

%% ssl:listen options are fixed at acceptor init; restart after set_env so
%% fail_if_no_peer_cert picks up the new require_client_certs value.
restart_tls_acceptor() ->
    Pid = whereis(epp_tls_acceptor),
    true = is_pid(Pid),
    Ref = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 ->
            ct:fail("epp_tls_acceptor did not shut down")
    end,
    wait_for_tls_acceptor(_Attempts = 50),
    ok.

wait_for_tls_acceptor(0) ->
    ct:fail("epp_tls_acceptor did not restart");
wait_for_tls_acceptor(N) ->
    case whereis(epp_tls_acceptor) of
        undefined ->
            timer:sleep(100),
            wait_for_tls_acceptor(N - 1);
        _Pid ->
            ok
    end.
