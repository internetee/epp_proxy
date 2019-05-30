-module(epp_http_client).

-include("epp_proxy.hrl").

-behaviour(epp_http_client_behaviour).

-export([request/1, error_request/1]).

%% Callback API
request(#epp_request{} = Request) ->
    HackneyArgs = handle_args(Request),
    {Status, _StatusCode, _Headers, ClientRef} =
        apply(hackney, request, HackneyArgs),
    {ok, Body} = hackney:body(ClientRef),
    {Status, Body}.

error_request(#epp_error_request{} = Request) ->
    HackneyArgs = handle_error_args(Request),
    {Status, _StatusCode, _Headers, ClientRef} =
        apply(hackney, request, HackneyArgs),
    {ok, Body} = hackney:body(ClientRef),
    {Status, Body}.

%% Private API
-spec handle_args(epp_request()) -> list().
handle_args(#epp_request{method=get, url=URL, headers=Headers, body="",
                         cookies=Cookies}) ->
    [get, URL, Headers, "", [{cookie, Cookies}, insecure]];
handle_args(#epp_request{method=post, url=URL, headers=Headers, body=Body,
                         cookies=Cookies}) ->
    [post, URL, Headers, Body, [{cookie, Cookies}, insecure]].

-spec handle_error_args(epp_error_request()) -> list().
handle_error_args(#epp_error_request{method=get, url=URL, headers=Headers,
                                     query_params=Params, cookies=Cookies}) ->
    QueryString = hackney_url:qs(Params),
    CompleteURL = [URL, <<"?">>, QueryString],
    [get, CompleteURL, Headers, "", [{cookie, Cookies}, insecure]].
