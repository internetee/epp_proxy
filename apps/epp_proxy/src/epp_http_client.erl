-module(epp_http_client).

-include("epp_proxy.hrl").

-behaviour(epp_http_client_behaviour).

-export([request/1, request_builder/1]).

-define(errorCommand, "error").

-define(helloCommand, "hello").

%% Callback API
request(#epp_request{} = Request) ->
    [Method, URL, Headers, Payload, Options] =
	handle_args(Request),
    case hackney:request(Method, URL, Headers, Payload,
			 Options)
	of
      {error, Error} -> log_and_return_canned(Error, Request);
      {Status, _StatusCode, _Headers, ClientRef} ->
	  {ok, Body} = hackney:body(ClientRef), {Status, Body}
    end.

request_builder(Map) -> request_from_map(Map).

%% Private API
-spec handle_args(epp_request()) -> list().

%% For hello command, we ignore the payload, and send an empty body over the wire.
handle_args(#epp_request{method = get, url = URL,
			 headers = Headers, cookies = Cookies,
			 epp_verb = ?helloCommand}) ->
    [get, URL, Headers, "", hackney_options(Cookies)];
%% For error command, we convert the message and code into query parameters,
%% and append them to the original URL.
handle_args(#epp_request{method = get, url = URL,
			 payload = Payload, headers = Headers,
			 cookies = Cookies, epp_verb = ?errorCommand}) ->
    QueryString = hackney_url:qs(Payload),
    CompleteURL = [URL, <<"?">>, QueryString],
    [get, CompleteURL, Headers, "",
     hackney_options(Cookies)];
%% For valid commands, we set the multipart body earlier, now we just pass it on.
handle_args(#epp_request{method = post, url = URL,
			 payload = Payload, headers = Headers,
			 cookies = Cookies}) ->
    [post, URL, Headers, Payload, hackney_options(Cookies)].

%% Map request and return values.
request_from_map(#{command := ?errorCommand,
		   session_id := SessionId, code := Code,
		   message := Message, headers := Headers,
		   cl_trid := ClTRID}) ->
    URL = epp_router:route_request(?errorCommand),
    RequestMethod =
	epp_router:request_method(?errorCommand),
    Cookie = hackney_cookie:setcookie("session", SessionId,
				      []),
    QueryParams = query_params(Code, Message, ClTRID),
    Headers = Headers,
    Request = #epp_request{url = URL,
			   method = RequestMethod, payload = QueryParams,
			   cookies = [Cookie], headers = Headers,
			   epp_verb = ?errorCommand},
    lager:info("Error Request from map: [~p]~n", [Request]),
    Request;
request_from_map(#{command := Command,
		   session_id := SessionId, raw_frame := RawFrame,
		   headers := Headers, cl_trid := ClTRID}) ->
    URL = epp_router:route_request(Command),
    RequestMethod = epp_router:request_method(Command),
    Cookie = hackney_cookie:setcookie("session", SessionId,
				      []),
    Body = request_body(Command, RawFrame, ClTRID),
    Headers = Headers,
    Request = #epp_request{url = URL,
			   method = RequestMethod, payload = Body,
			   cookies = [Cookie], headers = Headers,
			   epp_verb = Command},
    lager:info("Request from map: [~p]~n", [Request]),
    Request.

%% Get hackney options
hackney_options(Cookies) ->
    case application:get_env(epp_proxy, insecure) of
      false -> [{cookie, Cookies}, insecure];
      _ -> [{cookie, Cookies}, {connect_timeout, 120000}, {recv_timeout, 120000}]
    end.

%% Return form data or an empty list.
request_body(?helloCommand, _, _) -> "";
request_body(_Command, RawFrame, nomatch) ->
    {multipart,
     [{<<"raw_frame">>, RawFrame}, {<<"frame">>, RawFrame}]};
request_body(_Command, RawFrame, ClTRID) ->
    {multipart,
     [{<<"raw_frame">>, RawFrame}, {<<"frame">>, RawFrame},
      {<<"clTRID">>, ClTRID}]}.

%% Return a list of properties that each represent a query part in a query string.
%% [{"user", "eis"}]} becomes later https://example.com?user=eis
query_params(Code, Message, nomatch) ->
    [{<<"code">>, Code}, {<<"msg">>, Message}];
query_params(Code, Message, ClTRID) ->
    [{<<"code">>, Code}, {<<"msg">>, Message},
     {<<"clTRID">>, ClTRID}].

%% Log critical information about a request that failed, and then
%% return a canned response with internal error status.
log_and_return_canned(Error, Request) ->
    lager:alert("Registry cannot be reached!"),
    lager:alert("Error contacting registry: [~p, ~p]~n",
		[Error, Request]),
    {2400, canned_response()}.

%% In case registry is not accessible, return this response.
%% In the future, this should be configurable.
canned_response() ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<"
      "epp xmlns=\"https://epp.tld.ee/schema/epp-ee-"
      "1.0.xsd\" xmlns:xsi=\"http://www.w3.org/2001/"
      "XMLSchema-instance\" xsi:schemaLocation=\"lib"
      "/schemas/epp-ee-1.0.xsd\">\n  <response>\n "
      "   <result code=\"2400\">\n      <msg "
      "lang=\"en\">Internal server error.</msg>\n "
      "   </result>\n  </response>\n</epp>">>.
