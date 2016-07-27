%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%%
%% @doc Request functions for cowmachine

%% Copyright 2016 Marc Worrell
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

-module(cowmachine_req).
-author("Marc Worrell <marc@worrell.nl").

-export([
    init_req/2,
    set_req/2,
    req/1
    ]).

-export([
    site/1,
    method/1,
    version/1,
    base_uri/1,
    scheme/1,
    host/1,
    port/1,
    is_ssl/1,
    peer/1,

    raw_path/1,
    qs/1,
    req_qs/1,

    path_info/1,

    set_disp_path/2,
    disp_path/1,

    get_req_header/2,
    set_resp_content_type/2,
    resp_content_type/1,

    set_range_ok/2,
    is_range_ok/1,

    set_resp_header/3,
    set_resp_headers/2,
    get_resp_header/2,
    get_resp_headers/1,
    remove_resp_header/2,

    set_resp_cookie/4,
    get_resp_cookies/1,

    get_cookie_value/2,
    req_cookie/1,

    set_response_code/2,
    response_code/1,

    set_resp_chosen_charset/2,
    resp_chosen_charset/1,

    set_resp_transfer_encoding/2,
    resp_transfer_encoding/1,

    set_resp_content_encoding/2,
    resp_content_encoding/1,

    set_resp_body/2,
    resp_body/1,
    has_resp_body/1,

    do_redirect/2,
    resp_redirect/1,

    req_body/1,
    req_body/2,

    stream_req_body/2,

    set_metadata/3,
    get_metadata/2
    ]).

%% @doc The request context stores everything needed for the request handling.
%%      Inside Zotonic all functions work with a site specific context, this
%%      context has optionally a request. That is why the context is wrapped around
%%      the cowboy request.
%%
%%      The cowmachine context must be the cowboy request or a tuple.
%%      If it is a tuple then it is assumed to be a record then the cowboy request
%%      is at position 2.
-type context() :: cowboy_req:req() | tuple().

-export_type([context/0]).


-type resp_body() :: iolist()
                   | {device, pos_integer(), file:io_device()}
                   | {device, file:io_device()}
                   | {file, pos_integer(), filename:filename()}
                   | {file, filename:filename()}
                   | {stream, function()}
                   | {stream, pos_integer(), function()}
                   | {writer, function()}.

-export_type([resp_body/0]).

%% @doc Set some intial metadata in the cowboy req
-spec init_req(cowboy_req:req(), cowboy_middleware:env()) -> cowboy_req:req().
init_req(Req, Env) ->
    Req#{
        cowmachine_site => maps:get(site, Env, undefined),

        cowmachine_resp_code => 500,
        cowmachine_resp_redirect => false,
        cowmachine_resp_cookies => [],
        cowmachine_resp_content_encoding => <<"identity">>,
        cowmachine_resp_transfer_encoding => undefined,
        cowmachine_resp_content_type => <<"binary/octet-stream">>,
        cowmachine_resp_chosen_charset => undefined,
        cowmachine_resp_body => undefined,

        cowmachine_disp_path => undefined,
        cowmachine_range_ok => true,

        cowmachine_peer => peer_req(Req),
        cowmachine_cookies => cowboy_req:parse_cookies(Req)
    }.


%% @doc Optionally wrap the cowboy request in the context.
-spec set_req(cowboy_req:req(), context()) -> context().
set_req(Req, Context) when is_tuple(Context) ->
    erlang:setelement(Context, 2, Req);
set_req(Req, OldReq) when is_map(OldReq); OldReq =:= undefined ->
    Req.

%% @doc Fetch the cowboy request from the context.
req(Context) when is_tuple(Context) ->
    erlang:element(2, Context);
req(Req) when is_map(Req) ->
    Req.


%% @doc Return the cowmachine site.
-spec site(context()) -> atom().
site(Context) ->
    maps:get(cowmachine_site, req(Context)).

%% @doc Return the request Method
-spec method(context()) -> binary().
method(Context) ->
    cowboy_req:method(req(Context)).

%% @doc Return the http version as a tuple {minor,major}.
-spec version(context()) -> {Major::integer(), Minor::integer()}.
version(Context) ->
    case cowboy_req:version(req(Context)) of
        'HTTP/1.0' -> {1,0};
        'HTTP/1.1' -> {1,1}
    end.

%% @doc Return the base uri of the request.
-spec base_uri(context()) -> binary().
base_uri(Context) ->
    Req = req(Context),
    Scheme = scheme(Context),
    Uri = [
        z_convert:to_binary(Scheme),
        <<"://">>,
        cowboy_req:host(Req),
        case cowboy_req:port(Req) of
            80 when Scheme =:= http -> [];
            433 when Scheme =:= https -> [];
            Port -> [ $:, integer_to_binary(Port) ]
        end,
        $/
    ],
    iolist_to_binary(Uri). 

%% @doc Return the scheme used (https or http)
-spec scheme(context()) -> http|https.
scheme(Context) ->
    Transport = cowboy_req:transport(req(Context)),
    case Transport:secure() of
        true -> https;
        false -> http
    end.

%% @doc Return the http host
-spec host(context()) -> binary().
host(Context) ->
    cowboy_req:host(req(Context)).

%% @doc Return the http port
-spec port(context()) -> integer().
port(Context) ->
    cowboy_req:port(req(Context)).

%% @doc Check if the connection is secure (SSL)
-spec is_ssl(context()) -> boolean().
is_ssl(Context) ->
    Transport = cowboy_req:transport(req(Context)),
    Transport:secure().

%% @doc Return the peer of this request, take x-forwarded-for into account if the peer
%%      is an ip4 LAN address
-spec peer(context()) -> binary().
peer(Context) ->
    maps:get(cowmachine_peer, req(Context)).

peer_req(Req) ->
    z_convert:to_binary(peer_from_peername(cowboy_req:peer(Req), Req)).

peer_from_peername({Addr={10, _, _, _}, _Port}, Req) ->  
    x_peername(inet_parse:ntoa(Addr), Req);
peer_from_peername({Addr={172, Second, _, _}, _Port}, Req) when (Second > 15) andalso (Second < 32) ->
    x_peername(inet_parse:ntoa(Addr), Req);
peer_from_peername({Addr={192, 168, _, _}, _Port}, Req) ->
    x_peername(inet_parse:ntoa(Addr), Req);
peer_from_peername({{127, 0, 0, 1}, _Port}, Req) ->
    x_peername(<<"127.0.0.1">>, Req);
peer_from_peername({Addr, _Port}, _Req) ->
    inet_parse:ntoa(Addr);
peer_from_peername(_Error, Req) ->
    x_peername(<<"-">>, Req).

x_peername(Default, Req) ->
    case cowboy_req:get_header(<<"x-forwarded-for">>, Req) of
        undefined -> Default;
        Hosts -> z_string:trim(lists:last(binary:split(Hosts, <<",">>, [global])))
    end.


%% @doc Return the undecoded request path as-is
-spec raw_path(context()) -> binary().
raw_path(Context) ->
    cowboy_req:path(req(Context)).

%% @doc Return the undecoded query string, <<>> when no query string.
-spec qs(context()) -> binary().
qs(Context) ->
    cowboy_req:qs(req(Context)).

%% @doc Return the undecoded query string, <<>> when no query string.
-spec req_qs(context()) -> list({binary(), binary()}).
req_qs(Context) ->
    Qs = qs(Context),
    cowmachine_util:parse_qs(Qs).

%% @doc Fetch all bindings from the dispatcher.
-spec path_info(context()) -> list({atom(), term()}).
path_info(Context) ->
    maps:get(bindings, req(Context)).

%% @doc Fetch a request header, the header must be a lowercase binary.
-spec get_req_header(binary(), context()) -> binary() | undefined.
get_req_header(H, Context) ->
    cowboy_req:header(H, req(Context)).

%% @doc Set the content type of the response
-spec set_resp_content_type(binary(), context()) -> context().
set_resp_content_type(CT, Context) ->
    Req = req(Context),
    set_req(Req#{cowmachine_resp_content_type => CT}, Context).

%% @doc Fetch the content type of the response
-spec resp_content_type(context()) -> binary().
resp_content_type(Context) ->
    maps:get(cowmachine_resp_content_type, req(Context)).


%% @doc Set the 'is_range_ok' flag.
-spec set_range_ok(boolean(), context()) -> context().
set_range_ok(IsRangeOk, Context) ->
    Req = req(Context),
    set_req(Req#{cowmachine_range_ok => IsRangeOk}, Context).

%% @doc Fetch the 'is_range_ok' flag.
-spec is_range_ok(context()) -> boolean().
is_range_ok(Context) ->
    maps:get(cowmachine_range_ok, req(Context)).

%% @doc Add a response header, replacing an existing header with the same name.
%% The header must be a lowercased binary. If the value is a list of binaries then
%% they are joined with a comma as separator.
-spec set_resp_header(binary(), binary()|list(binary())|string(), context()) -> context().
set_resp_header(Header, [V|Rs], Context) when is_binary(V) ->
    V1 = iolist_to_binary([V, [ [", ", R] || R <- Rs] ]),
    set_resp_header(Header, V1, Context);
set_resp_header(Header, Value, Context) ->
    Req = cowboy_req:set_resp_header(Header, z_convert:to_binary(Value), req(Context)),
    set_req(Req, Context).

%% @doc Set multiple response headers.
-spec set_resp_headers([{binary(), binary()}], context()) -> context().
set_resp_headers([], Context) ->
    Context;
set_resp_headers([{Header, Value}|Hs], Context) ->
    Context1 = set_resp_header(Header, Value, Context),
    set_resp_headers(Hs, Context1).

%% @doc Fetch the response header, undefined if not set.
-spec get_resp_header(binary(), context()) -> binary() | undefined.
get_resp_header(Header, Context) ->
    Hs = maps:get(resp_headers, req(Context), #{}),
    maps:get(Header, Hs, undefined).

%% @doc Fetch all response headers.
-spec get_resp_header(context()) -> map().
get_resp_headers(Context) ->
    maps:get(resp_headers, req(Context), #{}).

%% @doc Remove the response header from the list for response headers.
-spec remove_resp_header(binary(), context()) -> context().
remove_resp_header(Header, Context) ->
    Req = cowboy_req:delete_resp_header(Header, req(Context)),
    set_req(Req, Context).

%% @doc Add a cookie to the response cookies
-spec set_resp_cookie(binary(), binary(), list(), context()) -> context().
set_resp_cookie(Key, Value, Options, Context) ->
    Req = req(Context),
    Hdr = cow_cookies:set_cookie(Key, Value, Options),
    Cookies = maps:get(cowmachine_resp_cookies, Req),
    set_req(Req#{cowmachine_resp_cookies => [Hdr | Cookies]}, Context).

%% @doc Fetch all response cookies.
-spec get_resp_cookies(context()) -> [ binary() ].
get_resp_cookies(Context) ->
    maps:get(cowmachine_resp_cookies, req(Context)).

%% @doc Fetch the value of a cookie.
-spec get_cookie_value(binary(), context()) -> binary() | undefined.
get_cookie_value(Name, Context) ->
    Cookies = maps:get(cowmachine_cookies, req(Context)),
    proplists:get_value(Name, Cookies).

%% @doc Fetch all cookies.
-spec req_cookie(context()) -> list().
req_cookie(Context) ->
    maps:get(cowmachine_cookies, req(Context)).

%% @doc Set the preliminary HTTP response code for the request. This can be changed.
-spec set_response_code(integer(), context()) -> context().
set_response_code(Code, Context) when is_integer(Code) ->
    Req = req(Context),
    set_req(Req#{cowmachine_resp_code => Code}, Context).

%% @doc Fetch the preliminary HTTP response code for the request. This can be changed.
-spec response_code(context()) -> integer().
response_code(Context) ->
    maps:get(cowmachine_resp_code, req(Context)).

%% @doc Set the chosen charset.
-spec set_resp_chosen_charset(binary()|undefined, context()) -> context().
set_resp_chosen_charset(CharSet, Context) when is_binary(CharSet); CharSet =:= undefined ->
    Req = req(Context),
    set_req(Req#{cowmachine_resp_chosen_charset => CharSet}, Context).

%% @doc Get the chosen charset.
-spec resp_chosen_charset(context()) -> binary() | undefined.
resp_chosen_charset(Context) ->
    maps:get(cowmachine_resp_chosen_charset, req(Context)).

%% @doc Set the transfer encoding
-spec set_resp_transfer_encoding(binary(), context()) -> context().
set_resp_transfer_encoding(Enc, Context) when is_binary(Enc) ->
    Req = req(Context),
    set_req(Req#{cowmachine_resp_transfer_encoding => Enc}, Context).

%% @doc Get the transfer encoding.
-spec resp_transfer_encoding(context()) -> binary().
resp_transfer_encoding(Context) ->
    maps:get(cowmachine_resp_transfer_encoding, req(Context)).

%% @doc Set the content encoding
-spec set_resp_content_encoding(binary(), context()) -> context().
set_resp_content_encoding(Enc, Context) when is_binary(Enc) ->
    Req = req(Context),
    set_req(Req#{cowmachine_resp_content_encoding => Enc}, Context).

%% @doc Get the content encoding.
-spec resp_content_encoding(context()) -> binary().
resp_content_encoding(Context) ->
    maps:get(cowmachine_resp_content_encoding, req(Context)).

%% @doc Set the 'redirect' flag, used during POST processing to check if a 303 should be returned.
-spec do_redirect(boolean(), context()) -> context().
do_redirect(IsRedirect, Context) when is_boolean(IsRedirect) ->
    Req = req(Context),
    set_req(Req#{cowmachine_resp_redirect => IsRedirect}, Context).

%% @doc Return the 'redirect' flag, used during POST processing to check if a 303 should be returned.
-spec resp_redirect(context()) -> boolean().
resp_redirect(Context) ->
    maps:get(cowmachine_resp_redirect, req(Context)).

%% @doc Set the dispatch path of the request.
-spec set_disp_path(binary(), context()) -> context().
set_disp_path(Path, Context) ->
    Req = req(Context),
    set_req(Req#{cowmachine_disp_path => Path}, Context).

%% @doc Return the dispatch path of the request.
-spec disp_path(context()) -> binary() | undefined.
disp_path(Context) ->
    maps:get(cowmachine_disp_path, req(Context)).

%% @doc Set the response body, this must be converted to a response body that Cowboy can handle.
-spec set_resp_body(resp_body(), context()) -> context().
set_resp_body(RespBody, Context) ->
    Req = req(Context),
    set_req(Req#{cowmachine_resp_body => RespBody}, Context).

%% @doc Return the response body, this must be converted to a response body that Cowboy can handle.
-spec resp_body(context()) -> resp_body().
resp_body(Context) ->
    maps:get(cowmachine_resp_body, req(Context)).

%% @doc Check if a response body has been set.
-spec has_resp_body(context()) -> context().
has_resp_body(Context) ->
    case maps:get(cowmachine_resp_body, req(Context)) of
        undefined -> false;
        [] -> false;
        <<>> -> false;
        _ -> true
    end.

%% @doc Fetch the request body as a single binary. 
%% Per default we don't receive more than ~128K bytes.
-spec req_body(context()) -> {binary()|undefined, context()}.
req_body(Context) ->
    req_body(128*1024, Context).

-spec req_body(pos_integer(), context()) -> {binary()|undefined, context()}.
req_body(MaxLength, Context) when MaxLength > 0 ->
    Req = req(Context),
    Opts = [
        {length, MaxLength},
        {read_timeout, 10000}
    ],
    case cowboy_req:read_body(Req, Opts) of
        {ok, Body, Req2} ->
            {Body, set_req(Req2, Context)};
        {more, _Body, Req2} ->
            lager:warning("[~p] Dropped request body, as it is larger than ~p bytes.",
                          [site(Context), MaxLength]),
            {undefined, set_req(Req2, Context)}
    end.

-spec stream_req_body(integer(), context()) -> {ok|more, binary(), context()}.
stream_req_body(ChunkSize, Context) ->
    Opts = [
        % {length, 1024*1024*1024},
        {length, ChunkSize},
        {read_timeout, 10000}
    ],
    {Next, Chunk, Req1} = cowboy_req:read_body(req(Context), Opts),
    {Next, Chunk, set_req(Req1, Context)}.


-spec set_metadata(atom(), term(), context()) -> context().
set_metadata(Key, Value, Context) ->
    Req = req(Context),
    Req1 = Req#{ {cowmachine, Key} => Value },
    set_req(Req1, Context).

-spec get_metadata(atom(), context()) -> term() | undefined.
get_metadata('chosen-charset', Context) ->
    resp_chosen_charset(Context);
get_metadata('content-encoding', Context) ->
    resp_content_encoding(Context);
get_metadata('transfer-encoding', Context) ->
    resp_transfer_encoding(Context);
get_metadata('content-type', Context) ->
    resp_content_type(Context);
get_metadata(Key, Context) ->
    maps:get({cowmachine, Key}, req(Context), undefined).
