%% This record is used by both epp_tcp_worker and epp_tls_worker.
-record(epp_request,
        {method,   % get | post (atom)
         url,      % "https://example.com/some-url"
         body,     % "" | {multipart [{{<<"raw_frame">>, "Some body"}}]}
         cookies,  % "" | {multipart [{{<<"raw_frame">>, "Some body"}}]}
         headers   % [{"User-Agent", <<"EPP proxy">>}, {"Other", <<"Header">>}]
        }).
