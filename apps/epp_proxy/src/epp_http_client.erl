-module(epp_http_client).

-include("epp_proxy.hrl").

-behaviour(epp_http_client_behaviour).

-export([request/1, error_request/1, request_builder/1]).

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

request_builder(Map) ->
    request_from_map(Map).

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


%% Map request and return values.
request_from_map(#{command := "error", session_id := SessionId,
                   code := Code, message := Message, headers:=Headers,
                   cl_trid := ClTRID}) ->
    URL = epp_router:route_request("error"),
    RequestMethod = epp_router:request_method("error"),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    QueryParams = query_params(Code, Message, ClTRID),
    Headers=Headers,
    Request = #epp_error_request{url=URL,
                           method=RequestMethod,
                           query_params=QueryParams,
                           cookies=[Cookie],
                           headers=Headers},
    lager:info("Error Request from map: [~p]~n", [Request]),
    Request;
request_from_map(#{command := Command, session_id := SessionId,
          raw_frame := RawFrame, headers:=Headers, cl_trid := ClTRID}) ->
    URL = epp_router:route_request(Command),
    RequestMethod = epp_router:request_method(Command),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    Body = request_body(Command, RawFrame, ClTRID),
    Headers=Headers,
    Request = #epp_request{url=URL,
                           method=RequestMethod,
                           body=Body,
                           cookies=[Cookie],
                           headers=Headers},
    lager:info("Request from map: [~p]~n", [Request]),
    Request;
request_from_map(#{command := Command, session_id := SessionId,
          raw_frame := RawFrame, common_name := CommonName,
          client_cert := ClientCert, peer_ip := PeerIp, cl_trid := ClTRID}) ->
    URL = epp_router:route_request(Command),
    RequestMethod = epp_router:request_method(Command),
    Cookie = hackney_cookie:setcookie("session", SessionId, []),
    Body = request_body(Command, RawFrame, ClTRID),
    Headers = [{"SSL_CLIENT_CERT", ClientCert},
               {"SSL_CLIENT_S_DN_CN", CommonName},
               {"User-Agent", <<"EPP proxy">>},
               {"X-Forwarded-for", epp_util:readable_ip(PeerIp)}],
    Request = #epp_request{url=URL,
                           method=RequestMethod,
                           body=Body,
                           cookies=[Cookie],
                           headers=Headers},
    lager:info("Request from map: [~p]~n", [Request]),
    Request.

%% Return form data or an empty list.
request_body("hello", _, _) ->
    "";
request_body(_Command, RawFrame, nomatch) ->
    {multipart, [{<<"raw_frame">>, RawFrame}]};
request_body(_Command, RawFrame, ClTRID) ->
    {multipart, [{<<"raw_frame">>, RawFrame}, {<<"clTRID">>, ClTRID}]}.

query_params(Code, Message, nomatch) ->
    [{<<"code">>, Code}, {<<"msg">>, Message}];
query_params(Code, Message, ClTRID) ->
    [{<<"code">>, Code}, {<<"msg">>, Message}, {<<"clTRID">>, ClTRID}].
