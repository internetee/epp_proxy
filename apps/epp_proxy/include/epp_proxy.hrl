%% These records are used by both epp_tcp_worker and epp_tls_worker.
-record(epp_request,
        {method,       % get
         url,          % "https://example.com/some-url"
         payload,      % {[{<<"msg">>, <<"Some">>}, {<<"code">>, <<"2001">>}]}
         cookies,      % [<<"session=SomeSession; Version=1">>]
         headers,      % [{"User-Agent", <<"EPP proxy">>}, {"Other", <<"Header">>}]
         epp_verb      % Epp verb that is targeted, plus 'error'
        }).

-record(valid_frame, {command, cl_trid, raw_frame}).

-record(invalid_frame, {code, cl_trid, message}).

-record(state, {socket, session_id, headers}).

-type epp_request() :: #epp_request{}.

-define(XMLErrorCode, <<"2001">>).

-define(XMLErrorMessage, <<"Command syntax error.">>).

-define(UnknownCommandErrorCode, <<"2000">>).

-define(UnknownCommandErrorMessage, <<"Unknown command.">>).

-define(DefaultTimeout, 120000).
