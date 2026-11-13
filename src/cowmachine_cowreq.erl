%% @author William Fank Thomé
%% @copyright 2026 William Fank Thomé
%% @doc The request API for the raw request (the "cowreq") on any server
%% (`cowmachine_server').
%%
%% Cowmachine hands middlewares a request map shaped like Cowboy's, whichever
%% server serves it. The functions here take the same arguments and return
%% the same values as the `cowboy_req' functions of the same name. Use them
%% instead of `cowboy_req' in middlewares and wherever code works on the raw
%% request, and that code runs on Cowboy and on any other server.
%% `cowmachine_req' is the API for a controller's `Context', which wraps the
%% cowreq.
%%
%% Functions that read or set fields of the map delegate to `cowboy_req' and
%% work the same for every server. Those that touch the connection (sending a
%% response, reading the body) are done by the server serving the request.
%%
%% It has `cowboy_req''s functions except `push/3,4' (HTTP/2 server push)
%% and `cast/2', which sends Cowboy's stream handlers any message:
%% `set_options/2' does what cowmachine casts, as
%% `cowboy_req:cast({set_options, Opts}, Req)' did.
%%
%% ```
%% execute(Req0, Env) ->
%%     case cowmachine_cowreq:header(<<"x-api-key">>, Req0) of
%%         undefined ->
%%             {stop, cowmachine_cowreq:reply(401, Req0)};
%%         _Key ->
%%             {ok, Req0, Env}
%%     end.
%% '''
%% @end

%% Copyright 2026 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(cowmachine_cowreq).

%% Reading the request
-export([
    method/1,
    version/1,
    scheme/1,
    host/1,
    host_info/1,
    port/1,
    path/1,
    path_info/1,
    qs/1,
    uri/1,
    uri/2,
    peer/1,
    sock/1,
    cert/1,
    header/2,
    header/3,
    headers/1,
    parse_header/2,
    parse_header/3,
    parse_qs/1,
    match_qs/2,
    filter_cookies/2,
    parse_cookies/1,
    match_cookies/2,
    binding/2,
    binding/3,
    bindings/1
]).

%% Reading the body
-export([
    has_body/1,
    body_length/1,
    read_body/1,
    read_body/2,
    read_urlencoded_body/1,
    read_urlencoded_body/2,
    read_and_match_urlencoded_body/2,
    read_and_match_urlencoded_body/3,
    read_part/1,
    read_part/2,
    read_part_body/1,
    read_part_body/2
]).

%% Responding
-export([
    set_resp_cookie/3,
    set_resp_cookie/4,
    resp_header/2,
    resp_header/3,
    resp_headers/1,
    set_resp_header/3,
    set_resp_headers/2,
    has_resp_header/2,
    delete_resp_header/2,
    set_resp_body/2,
    has_resp_body/1,
    inform/2,
    inform/3,
    reply/2,
    reply/3,
    reply/4,
    stream_reply/2,
    stream_reply/3,
    stream_body/3,
    stream_events/3,
    stream_trailers/2
]).

%% The connection
-export([
    set_options/2
]).

-export_type([req/0, request_opts/0]).

-type req() :: cowboy_req:req().
%% The raw request, a map shaped like Cowboy's.

-type request_opts() :: #{
    idle_timeout => timeout(),
    metrics_user_data => map()
}.
%% The options `set_options/2' changes while a request is served.

%%
%% Reading the request
%%

%% @doc The request method, such as `<<"GET">>'.
-spec method(req()) -> binary().
method(Req) -> cowboy_req:method(Req).

%% @doc The HTTP version, such as `` 'HTTP/1.1' ''.
-spec version(req()) -> cowboy:http_version().
version(Req) -> cowboy_req:version(Req).

%% @doc The scheme, `<<"http">>' or `<<"https">>'.
-spec scheme(req()) -> binary().
scheme(Req) -> cowboy_req:scheme(Req).

%% @doc The host the request was sent to, without the port.
-spec host(req()) -> binary().
host(Req) -> cowboy_req:host(Req).

%% @doc The host's tokens a router matched with `...', or `undefined'.
-spec host_info(req()) -> cowboy_router:tokens() | undefined.
host_info(Req) -> cowboy_req:host_info(Req).

%% @doc The port the request was sent to.
-spec port(req()) -> inet:port_number().
port(Req) -> cowboy_req:port(Req).

%% @doc The request path.
-spec path(req()) -> binary().
path(Req) -> cowboy_req:path(Req).

%% @doc The path's tokens a router matched with `...', or `undefined'.
-spec path_info(req()) -> cowboy_router:tokens() | undefined.
path_info(Req) -> cowboy_req:path_info(Req).

%% @doc The raw query string.
-spec qs(req()) -> binary().
qs(Req) -> cowboy_req:qs(Req).

%% @doc The request URI, as `cowboy_req:uri/1'.
-spec uri(req()) -> iodata().
uri(Req) -> cowboy_req:uri(Req).

%% @doc The request URI with parts replaced or left out, as `cowboy_req:uri/2'.
-spec uri(req(), map()) -> iodata().
uri(Req, Opts) -> cowboy_req:uri(Req, Opts).

%% @doc The client's address and port.
-spec peer(req()) -> {inet:ip_address(), inet:port_number()}.
peer(Req) -> cowboy_req:peer(Req).

%% @doc The local address and port the connection was accepted on.
-spec sock(req()) -> {inet:ip_address(), inet:port_number()}.
sock(Req) -> cowboy_req:sock(Req).

%% @doc The client's TLS certificate (DER), or `undefined'.
-spec cert(req()) -> binary() | undefined.
cert(Req) -> cowboy_req:cert(Req).

%% @doc A request header's value, or `undefined'. `Name' is lowercase.
-spec header(binary(), req()) -> binary() | undefined.
header(Name, Req) -> cowboy_req:header(Name, Req).

%% @doc A request header's value, or `Default'. `Name' is lowercase.
-spec header(binary(), req(), Default) -> binary() | Default.
header(Name, Req, Default) -> cowboy_req:header(Name, Req, Default).

%% @doc The request headers, as a map with lowercase names.
-spec headers(req()) -> cowboy:http_headers().
headers(Req) -> cowboy_req:headers(Req).

%% @doc A request header parsed, as `cowboy_req:parse_header/2'.
-spec parse_header(binary(), req()) -> any().
parse_header(Name, Req) -> cowboy_req:parse_header(Name, Req).

%% @doc A request header parsed, or `Default', as `cowboy_req:parse_header/3'.
-spec parse_header(binary(), req(), any()) -> any().
parse_header(Name, Req, Default) -> cowboy_req:parse_header(Name, Req, Default).

%% @doc The query string as a list of `{Key, Value}', `Value' `true' for a
%% flag.
-spec parse_qs(req()) -> [{binary(), binary() | true}].
parse_qs(Req) -> cowboy_req:parse_qs(Req).

%% @doc Query string values matched against constraints, as
%% `cowboy_req:match_qs/2'.
-spec match_qs(cowboy:fields(), req()) -> map().
match_qs(Fields, Req) -> cowboy_req:match_qs(Fields, Req).

%% @doc The request with only the cookies named in `Names' kept, as
%% `cowboy_req:filter_cookies/2'.
-spec filter_cookies([atom() | binary()], Req) -> Req when Req :: req().
filter_cookies(Names, Req) -> cowboy_req:filter_cookies(Names, Req).

%% @doc The request cookies as a list of `{Name, Value}'.
-spec parse_cookies(req()) -> [{binary(), binary()}].
parse_cookies(Req) -> cowboy_req:parse_cookies(Req).

%% @doc Cookies matched against constraints, as `cowboy_req:match_cookies/2'.
-spec match_cookies(cowboy:fields(), req()) -> map().
match_cookies(Fields, Req) -> cowboy_req:match_cookies(Fields, Req).

%% @doc A route binding's value, or `undefined'.
-spec binding(atom(), req()) -> any() | undefined.
binding(Name, Req) -> cowboy_req:binding(Name, Req).

%% @doc A route binding's value, or `Default'.
-spec binding(atom(), req(), Default) -> any() | Default.
binding(Name, Req, Default) -> cowboy_req:binding(Name, Req, Default).

%% @doc The route bindings.
-spec bindings(req()) -> cowboy_router:bindings().
bindings(Req) -> cowboy_req:bindings(Req).

%%
%% Reading the body
%%

%% @doc Whether the request has a body.
-spec has_body(req()) -> boolean().
has_body(Req) -> cowboy_req:has_body(Req).

%% @doc The body's length, or `undefined' when it isn't known up front.
-spec body_length(req()) -> non_neg_integer() | undefined.
body_length(Req) -> cowboy_req:body_length(Req).

%% @doc Read the request body, as `cowboy_req:read_body/1'.
-spec read_body(Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req :: req().
read_body(Req) -> read_body(Req, #{}).

%% @doc Read the request body, as `cowboy_req:read_body/2'.
-spec read_body(Req, cowboy_req:read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
    when Req :: req().
read_body(Req, Opts) -> (cowmachine_server:of_req(Req)):read_body(Req, Opts).

%% @doc Read and decode an urlencoded body, as
%% `cowboy_req:read_urlencoded_body/1'.
-spec read_urlencoded_body(Req) -> {ok, [{binary(), binary() | true}], Req} when Req :: req().
read_urlencoded_body(Req) ->
    read_urlencoded_body(Req, #{ length => 64000, period => 5000 }).

%% @doc Read and decode an urlencoded body, as
%% `cowboy_req:read_urlencoded_body/2'.
-spec read_urlencoded_body(Req, cowboy_req:read_body_opts()) ->
    {ok, [{binary(), binary() | true}], Req} when Req :: req().
read_urlencoded_body(Req0, Opts) ->
    case read_body(Req0, Opts) of
        {ok, Body, Req} ->
            try
                {ok, cow_qs:parse_qs(Body), Req}
            catch _:_:Stacktrace ->
                erlang:raise(exit, {request_error, urlencoded_body,
                                    'Malformed body; application/x-www-form-urlencoded expected.'},
                             Stacktrace)
            end;
        {more, Body, _Req} ->
            case byte_size(Body) < maps:get(length, Opts, 64000) of
                true ->
                    exit({request_error, timeout,
                          'The request body was not received within the configured time.'});
                false ->
                    exit({request_error, payload_too_large,
                          'The request body is larger than allowed by configuration.'})
            end
    end.

%% @doc Read an urlencoded body and match it against constraints, as
%% `cowboy_req:read_and_match_urlencoded_body/2'.
-spec read_and_match_urlencoded_body(cowboy:fields(), Req) -> {ok, map(), Req} when Req :: req().
read_and_match_urlencoded_body(Fields, Req) ->
    read_and_match_urlencoded_body(Fields, Req, #{ length => 64000, period => 5000 }).

%% @doc Read an urlencoded body and match it against constraints, as
%% `cowboy_req:read_and_match_urlencoded_body/3'.
-spec read_and_match_urlencoded_body(cowboy:fields(), Req, cowboy_req:read_body_opts()) ->
    {ok, map(), Req} when Req :: req().
read_and_match_urlencoded_body(Fields, Req0, Opts) ->
    {ok, Qs, Req} = read_urlencoded_body(Req0, Opts),
    %% Cowboy's own matching, run on the decoded body re-encoded as a query
    %% string, with its error named as Cowboy names it for a body.
    try cowboy_req:match_qs(Fields, Req#{ qs => cow_qs:qs(Qs) }) of
        Map -> {ok, Map, Req}
    catch
        exit:{request_error, {match_qs, Errors}, _} ->
            exit({request_error, {read_and_match_urlencoded_body, Errors},
                  'Urlencoded request body validation constraints failed for the reasons provided.'})
    end.

%% @doc Read the next multipart part's headers, as `cowboy_req:read_part/1'.
-spec read_part(Req) -> {ok, cowboy:http_headers(), Req} | {done, Req} when Req :: req().
read_part(Req) -> read_part(Req, #{ length => 64000, period => 5000 }).

%% @doc Read the next multipart part's headers, as `cowboy_req:read_part/2'.
-spec read_part(Req, cowboy_req:read_body_opts()) -> {ok, cowboy:http_headers(), Req} | {done, Req}
    when Req :: req().
read_part(Req, Opts) -> (cowmachine_server:of_req(Req)):read_part(Req, Opts).

%% @doc Read the current multipart part's body, as
%% `cowboy_req:read_part_body/1'.
-spec read_part_body(Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req :: req().
read_part_body(Req) -> read_part_body(Req, #{}).

%% @doc Read the current multipart part's body, as
%% `cowboy_req:read_part_body/2'.
-spec read_part_body(Req, cowboy_req:read_body_opts()) ->
    {ok, binary(), Req} | {more, binary(), Req} when Req :: req().
read_part_body(Req, Opts) -> (cowmachine_server:of_req(Req)):read_part_body(Req, Opts).

%%
%% Responding
%%

%% @doc Set a cookie on the response.
-spec set_resp_cookie(binary(), iodata(), Req) -> Req when Req :: req().
set_resp_cookie(Name, Value, Req) -> cowboy_req:set_resp_cookie(Name, Value, Req).

%% @doc Set a cookie on the response, with its attributes
%% (`cow_cookie:cookie_opts()').
-spec set_resp_cookie(binary(), iodata(), Req, cow_cookie:cookie_opts()) -> Req when Req :: req().
set_resp_cookie(Name, Value, Req, Opts) -> cowboy_req:set_resp_cookie(Name, Value, Req, Opts).

%% @doc A response header set on the request, or `undefined'.
-spec resp_header(binary(), req()) -> binary() | undefined.
resp_header(Name, Req) -> cowboy_req:resp_header(Name, Req).

%% @doc A response header set on the request, or `Default'.
-spec resp_header(binary(), req(), Default) -> binary() | Default.
resp_header(Name, Req, Default) -> cowboy_req:resp_header(Name, Req, Default).

%% @doc The response headers set on the request.
-spec resp_headers(req()) -> cowboy:http_headers().
resp_headers(Req) -> cowboy_req:resp_headers(Req).

%% @doc Set a response header. `Name' is lowercase.
-spec set_resp_header(binary(), iodata(), Req) -> Req when Req :: req().
set_resp_header(Name, Value, Req) -> cowboy_req:set_resp_header(Name, Value, Req).

%% @doc Set response headers from a map of lowercase names.
-spec set_resp_headers(cowboy:http_headers(), Req) -> Req when Req :: req().
set_resp_headers(Headers, Req) -> cowboy_req:set_resp_headers(Headers, Req).

%% @doc Whether a response header is set.
-spec has_resp_header(binary(), req()) -> boolean().
has_resp_header(Name, Req) -> cowboy_req:has_resp_header(Name, Req).

%% @doc Remove a response header.
-spec delete_resp_header(binary(), Req) -> Req when Req :: req().
delete_resp_header(Name, Req) -> cowboy_req:delete_resp_header(Name, Req).

%% @doc Set the response body, sent by `reply/2,3'.
-spec set_resp_body(cowboy_req:resp_body(), Req) -> Req when Req :: req().
set_resp_body(Body, Req) -> cowboy_req:set_resp_body(Body, Req).

%% @doc Whether a response body is set.
-spec has_resp_body(req()) -> boolean().
has_resp_body(Req) -> cowboy_req:has_resp_body(Req).

%% @doc Send an informational (1xx) response.
-spec inform(cowboy:http_status(), req()) -> ok.
inform(Status, Req) -> inform(Status, #{}, Req).

%% @doc Send an informational (1xx) response with headers.
-spec inform(cowboy:http_status(), cowboy:http_headers(), req()) -> ok.
inform(Status, Headers, Req) -> (cowmachine_server:of_req(Req)):inform(Status, Headers, Req).

%% @doc Send the response, with the headers and body set on the request.
-spec reply(cowboy:http_status(), Req) -> Req when Req :: req().
reply(Status, Req) -> reply(Status, #{}, Req).

%% @doc Send the response, with `Headers' added and the body set on the
%% request.
-spec reply(cowboy:http_status(), cowboy:http_headers(), Req) -> Req when Req :: req().
reply(Status, Headers, #{ resp_body := Body } = Req) ->
    reply(Status, Headers, Body, Req);
reply(Status, Headers, Req) ->
    reply(Status, Headers, <<>>, Req).

%% @doc Send the response, with `Headers' added to those set on the request.
-spec reply(cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body(), Req) -> Req
    when Req :: req().
reply(Status, Headers, Body, Req) ->
    (cowmachine_server:of_req(Req)):reply(Status, Headers, Body, Req).

%% @doc Start a streamed response, with the headers set on the request.
-spec stream_reply(cowboy:http_status(), Req) -> Req when Req :: req().
stream_reply(Status, Req) -> stream_reply(Status, #{}, Req).

%% @doc Start a streamed response, with `Headers' added to those set on the
%% request.
-spec stream_reply(cowboy:http_status(), cowboy:http_headers(), Req) -> Req when Req :: req().
stream_reply(Status, Headers, Req) ->
    (cowmachine_server:of_req(Req)):stream_reply(Status, Headers, Req).

%% @doc Send a piece of a streamed response's body.
-spec stream_body(iodata(), fin | nofin, req()) -> ok.
stream_body(Data, IsFin, Req) -> (cowmachine_server:of_req(Req)):stream_body(Data, IsFin, Req).

%% @doc Send server-sent events on a streamed response, as
%% `cowboy_req:stream_events/3'.
-spec stream_events(cow_sse:event() | [cow_sse:event()], fin | nofin, req()) -> ok.
stream_events(Event, IsFin, Req) when is_map(Event) ->
    stream_events([Event], IsFin, Req);
stream_events(Events, IsFin, Req) ->
    stream_body(cow_sse:events(Events), IsFin, Req).

%% @doc End a streamed response with trailers.
-spec stream_trailers(cowboy:http_headers(), req()) -> ok.
stream_trailers(Trailers, Req) -> (cowmachine_server:of_req(Req)):stream_trailers(Trailers, Req).

%%
%% The connection
%%

%% @doc Change options of the connection serving a request: `idle_timeout',
%% and `metrics_user_data', merged into the `user_data' of the request's
%% metrics. A server ignores those it has no counterpart for. On Cowboy it is
%% `cowboy_req:cast({set_options, Opts}, Req)'.
-spec set_options(request_opts(), req()) -> ok.
set_options(Opts, Req) -> (cowmachine_server:of_req(Req)):set_options(Opts, Req).
