%% These records are used by both epp_tcp_worker and epp_tls_worker.
-record(epp_request,
        {method,   % get | post (atom)
         url,      % "https://example.com/some-url"
         body,     % "" | {multipart [{{<<"raw_frame">>, "Some body"}}]}
         cookies,  % [<<"session=SomeSession; Version=1">>]
         headers   % [{"User-Agent", <<"EPP proxy">>}, {"Other", <<"Header">>}]
        }).

-record(epp_error_request,
       {method,       % get
        url,          % "https://example.com/some-url"
        query_params, % {[{<<"msg">>, <<"Some">>}, {<<"code">>, <<"2001">>}]}
        cookies,      % [<<"session=SomeSession; Version=1">>]
        headers        % [{"User-Agent", <<"EPP proxy">>}, {"Other", <<"Header">>}]
       }).

-type epp_request() :: #epp_request{}.
-type epp_error_request() :: #epp_error_request{}.
