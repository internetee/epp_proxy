%% These records are used by both epp_tcp_worker and epp_tls_worker.
-record(epp_request,
        {method,       % get
         url,          % "https://example.com/some-url"
         payload,      % {[{<<"msg">>, <<"Some">>}, {<<"code">>, <<"2001">>}]}
         cookies,      % [<<"session=SomeSession; Version=1">>]
         headers,      % [{"User-Agent", <<"EPP proxy">>}, {"Other", <<"Header">>}]
         epp_verb      % Epp verb that is targeted, plus 'error'
        }).

-type epp_request() :: #epp_request{}.
