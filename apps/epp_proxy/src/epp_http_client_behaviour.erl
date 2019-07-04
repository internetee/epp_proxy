-module(epp_http_client_behaviour).

-include("epp_proxy.hrl").

-type http_response() :: {integer(), binary()}.

%% Abstract module for http client behaviour. It should call EPP HTTP server
%% and return a response back to the caller.
-callback request(epp_request()) -> http_response().

-callback
error_request(epp_error_request()) -> http_response().

-callback request_builder(map()) -> epp_request() |
                                    epp_error_request().
