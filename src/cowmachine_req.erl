%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2019 Marc Worrell
%%
%% @doc Request functions for cowmachine

%% Copyright 2016-2019 Marc Worrell
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

-include("cowmachine_log.hrl").

-export([
    init_env/2
    ]).

-export([
    init_context/3,
    set_req/2,
    req/1,
    set_env/2,
    env/1
    ]).

-export([
    controller/1,
    controller_options/1,

    site/1,
    method/1,
    version/1,
    base_uri/1,
    scheme/1,
    host/1,
    port/1,
    is_ssl/1,
    is_proxy/1,
    peer/1,
    peer_ip/1,

    raw_path/1,
    path/1,
    qs/1,
    req_qs/1,

    path_info/1,

    set_disp_path/2,
    disp_path/1,

    get_req_header/2,
    get_req_headers/1,
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
    encode_content/2,

    set_resp_body/2,
    resp_body/1,
    has_resp_body/1,

    set_resp_redirect/2,
    resp_redirect/1,

    has_req_body/1,
    req_body/1,
    req_body/2,

    stream_req_body/2,

    set_metadata/3,
    get_metadata/2
    ]).


-type context_map() :: #{
    cowreq := cowboy_req:req(),
    cowenv := cowboy_middleware:env(),
    any() => any()
}.

%% The request context stores everything needed for the request handling.
%% Inside Zotonic all functions work with a site specific context, this
%% context has optionally a request. That is why the context is wrapped around
%% the cowboy request.
%%
%% The cowmachine context must be the cowreq record, a tuple or a map.
%% If it is a tuple then it is assumed to be a record then the cowreq
%% is at position 2 and the cowenv at position 3.
-type context() :: context_map() | tuple().

%% Used to stop a request with a specific HTTP status code
-type halt() :: {error, term()} | {halt, 200..599}.

%% Response body, can be data, a file, device or streaming functions.
-type resp_body() :: iodata()
                   | {device, Size::non_neg_integer(), file:io_device()}
                   | {device, file:io_device()}
                   | {file, Size::non_neg_integer(), file:filename_all()}
                   | {file, file:filename_all()}
                   | {stream, {streamdata(), streamfun()}}
                   | {stream, Size::non_neg_integer(), {streamdata(), streamfun()}}
                   | {stream, streamfun()}
                   | {stream, Size::non_neg_integer(), streamfun()}
                   | {writer, writerfun()}
                   | undefined.

%% Streaming function, repeatedly called to fetch the next chunk
-type streamfun() :: fun( ( parts(), context() ) -> {streamdata(), streamfun_next()} )
                   | fun( ( context() ) -> {streamdata(), streamfun_next()} )
                   | fun( () -> {streamdata(), streamfun_next()} )
                   | done.

-type streamfun_next() :: fun( ( context() ) -> {streamdata(), streamfun_next()} )
                        | fun( () -> {streamdata(), streamfun_next()} )
                        | done.

-type streamdata() :: iodata()
                    | {file, non_neg_integer(), file:filename_all()}
                    | {file, file:filename_all()}.

%% Writer function, calls output function till finished
-type writerfun() :: fun( (outputfun(), context()) -> context() ).
-type outputfun() :: fun( (iodata(), IsFinal::boolean(), context()) -> context() ).

%% Media types for accepted and provided content types
-type media_type() :: binary()
                    | {binary(), binary(), list( {binary(), binary()} )}
                    | {binary(), binary()}
                    | {binary(), list( {binary(), binary()} )}.

-type parts() :: all
               | {ranges(), Size :: non_neg_integer(), Boundary :: binary(), ContentType :: binary()}.
-type ranges() :: [ {Offset :: non_neg_integer(), Length :: non_neg_integer()} ].

-export_type([
    context/0,
    context_map/0,
    halt/0,
    resp_body/0,
    streamfun/0,
    streamfun_next/0,
    streamdata/0,
    media_type/0,
    parts/0,
    ranges/0
]).

%% @doc Set some intial metadata in the cowboy req
-spec init_env(cowboy_req:req(), cowboy_middleware:env()) -> cowboy_middleware:env().
init_env(Req, Env) ->
    Bindings = maps:get(bindings, Req, #{}),
    Env1 = lists:foldl(
                fun(K, Acc) -> maps:remove(K, Acc) end,
                Env,
                [ site, context ]),
    EnvProxy = ensure_proxy_args(Req, Env1),
    EnvProxy#{
        cowmachine_site => maps:get(site, Env, undefined),

        cowmachine_controller => maps:get(cowmachine_controller, Env),
        cowmachine_controller_options => maps:get(cowmachine_controller_options, Env, []),

        cowmachine_resp_code => 500,
        cowmachine_resp_redirect => false,
        cowmachine_resp_content_encoding => <<"identity">>,
        cowmachine_resp_transfer_encoding => undefined,
        cowmachine_resp_content_type => <<"binary/octet-stream">>,
        cowmachine_resp_chosen_charset => undefined,
        cowmachine_resp_body => undefined,

        cowmachine_disp_path => maps:get('*', Bindings, undefined),
        cowmachine_range_ok => true,

        cowmachine_cookies => cowboy_req:parse_cookies(Req)
    }.

ensure_proxy_args(Req, Env) ->
    case maps:get(cowmachine_proxy, Env, undefined) of
        undefined -> cowmachine_proxy:update_env(Req, Env);
        IsProxy when is_boolean(IsProxy) -> Env
    end.

%% @doc Initialize the context with the Req and Env
-spec init_context( cowboy_req:req(), cowboy_middleware:env(), undefined | map() | tuple()) -> context().
init_context( Req, Env, undefined ) ->
    init_context(Req, Env, #{});
init_context( Req, Env, M ) when is_map(M) ->
    M#{ cowreq => Req, cowenv => Env };
init_context( Req, Env, Tuple) when is_tuple(Tuple) ->
    T1 = setelement(2, Tuple, Req),
    setelement(3, T1, Env).

%% @doc Update the cowboy request in the context.
-spec set_req(cowboy_req:req(), context()) -> context().
set_req(Req, Context) when is_tuple(Context) ->
    erlang:setelement(2, Context, Req);
set_req(Req, #{ cowreq := _ } = Map) ->
    Map#{ cowreq => Req }.

%% @doc Fetch the cowboy request from the context.
-spec req(context()) -> cowboy_req:req().
req(Context) when is_tuple(Context) ->
    erlang:element(2, Context);
req(#{ cowreq := Req }) ->
    Req.

%% @doc Update the cowboy middleware env in the context.
-spec set_env(cowboy_middleware:env(), context()) -> context().
set_env(Env, Context) when is_tuple(Context) ->
    erlang:setelement(3, Context, Env);
set_env(Env, #{ cowenv := _ } = Map) ->
    Map#{ cowenv => Env }.

%% @doc Fetch the cowboy middleware env from the context.
-spec env(context()) -> cowboy_middleware:env().
env(Context) when is_tuple(Context) ->
    erlang:element(3, Context);
env(#{ cowenv := Env }) ->
    Env.


%% @doc Return the current cowmachine controller
-spec controller(context()) -> module().
controller(Context) ->
    maps:get(cowmachine_controller, env(Context)).

%% @doc Return the current cowmachine controller options
-spec controller_options(context()) -> list().
controller_options(Context) ->
    maps:get(cowmachine_controller_options, env(Context), []).

%% @doc Return the cowmachine site.
-spec site(context()) -> atom().
site(Context) ->
    maps:get(cowmachine_site, env(Context)).

%% @doc Return the request Method
-spec method(context()) -> binary().
method(Context) ->
    cowboy_req:method(req(Context)).

%% @doc Return the http version as a tuple {minor,major}.
-spec version(context()) -> {Major::integer(), Minor::integer()}.
version(Context) ->
    case cowboy_req:version(req(Context)) of
        'HTTP/1.0' -> {1,0};
        'HTTP/1.1' -> {1,1};
        'HTTP/2'   -> {2,0}
    end.

%% @doc Return the base uri of the request.
-spec base_uri(context()) -> binary().
base_uri(Context) ->
    Scheme = scheme(Context),
    Uri = [
        z_convert:to_binary(Scheme),
        <<"://">>,
        host(Context),
        case port(Context) of
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
    case maps:get(cowmachine_forwarded_proto, env(Context)) of
        <<"http">> -> http;
        <<"https">> -> https
    end.

%% @doc Return the http host
-spec host(context()) -> binary().
host(Context) ->
    maps:get(cowmachine_forwarded_host, env(Context)).

%% @doc Return the http port
-spec port(context()) -> integer().
port(Context) ->
    maps:get(cowmachine_forwarded_port, env(Context)).

%% @doc Check if the connection is secure (SSL)
-spec is_ssl(context()) -> boolean().
is_ssl(Context) ->
    https =:= scheme(Context).

%% @doc Check if the request is forwarded by a proxy
-spec is_proxy(context()) -> boolean().
is_proxy(Context) ->
    maps:get(cowmachine_proxy, env(Context)).

%% @doc Return the peer of this request, take x-forwarded-for into account if the peer
%%      is an ip4 LAN address
-spec peer(context()) -> binary().
peer(Context) ->
    maps:get(cowmachine_remote, env(Context)).

%% @doc Return the peer of this request, take x-forwarded-for into account if the peer
%%      is an ip4 LAN address
-spec peer_ip(context()) -> tuple().
peer_ip(Context) ->
    maps:get(cowmachine_remote_ip, env(Context)).


%% @doc Return the undecoded request path as-is, including the query string
-spec raw_path(context()) -> binary().
raw_path(Context) ->
    Path = cowboy_req:path(req(Context)),
    case qs(Context) of
        <<>> -> Path;
        Qs -> <<Path/binary, $?, Qs/binary>>
    end.

%% @doc Return the undecoded request path as-is
-spec path(context()) -> binary().
path(Context) ->
    cowboy_req:path(req(Context)).

%% @doc Return the undecoded query string, &lt;&lt;>> when no query string.
-spec qs(context()) -> binary().
qs(Context) ->
    cowboy_req:qs(req(Context)).

%% @doc Return the decoded query string, [] when no query string.
-spec req_qs(context()) -> list({binary(), binary()}).
req_qs(Context) ->
    Qs = qs(Context),
    cowmachine_util:parse_qs(Qs).

%% @doc Fetch all bindings from the dispatcher.
-spec path_info(context()) -> cowboy_router:bindings().
path_info(Context) ->
    maps:get(bindings, req(Context), #{}).

%% @doc Fetch a request header, the header must be a lowercase binary.
-spec get_req_header(binary(), context()) -> binary() | undefined.
get_req_header(H, Context) when is_binary(H) ->
    cowboy_req:header(H, req(Context)).

%% @doc Fetch all request headers.
-spec get_req_headers(context()) -> #{ binary() => binary() }.
get_req_headers(Context) ->
    cowboy_req:headers(req(Context)).

%% @doc Set the content type of the response
-spec set_resp_content_type(cow_http_hd:media_type() | binary(), context()) -> context().
set_resp_content_type(CT, Context) when is_binary(CT) ->
    set_resp_content_type(cow_http_hd:parse_content_type(CT), Context);
set_resp_content_type(CT, Context) when is_tuple(CT) ->
    Env = env(Context),
    set_env(Env#{ cowmachine_resp_content_type => CT }, Context).

%% @doc Fetch the content type of the response
-spec resp_content_type(context()) -> cow_http_hd:media_type().
resp_content_type(Context) ->
    maps:get(cowmachine_resp_content_type, env(Context)).


%% @doc Set the 'is_range_ok' flag.
-spec set_range_ok(boolean(), context()) -> context().
set_range_ok(IsRangeOk, Context) ->
    Env = env(Context),
    set_env(Env#{cowmachine_range_ok => IsRangeOk}, Context).

%% @doc Fetch the 'is_range_ok' flag.
-spec is_range_ok(context()) -> boolean().
is_range_ok(Context) ->
    maps:get(cowmachine_range_ok, env(Context)).

%% @doc Add a response header, replacing an existing header with the same name.
%% The header must be a lowercased binary. If the value is a list of binaries then
%% they are joined with a comma as separator.
-spec set_resp_header(binary(), binary()|list(binary())|string(), context()) -> context().
set_resp_header(Header, [V|Rs], Context) when is_binary(Header), is_binary(V) ->
    V1 = iolist_to_binary([V, [ [", ", R] || R <- Rs] ]),
    set_resp_header(Header, V1, Context);
set_resp_header(Header, Value, Context)
    when Header =:= <<"location">>;
         Header =:= <<"content-location">> ->
    case cowmachine_util:valid_location(Value) of
        {true, Loc} ->
            set_resp_header_1(Header, Loc, Context);
        false ->
            throw({http_invalid_location, Header, Value})
    end;
set_resp_header(Header, Value, Context) when is_binary(Header) ->
    set_resp_header_1(Header, Value, Context).

set_resp_header_1(Header, Value, Context) ->
    V = z_convert:to_binary(Value),
    case cowmachine_util:is_valid_header(Header) andalso cowmachine_util:is_valid_header_value(V) of
        true ->
            Req = cowboy_req:set_resp_header(Header, z_convert:to_binary(Value), req(Context)),
            set_req(Req, Context);
        false ->
            throw({http_invalid_header, Header, V})
    end.

%% @doc Set multiple response headers.
-spec set_resp_headers([{binary(), binary()}], context()) -> context().
set_resp_headers([], Context) ->
    Context;
set_resp_headers([{Header, Value}|Hs], Context) ->
    Context1 = set_resp_header(Header, Value, Context),
    set_resp_headers(Hs, Context1).

%% @doc Fetch the response header, undefined if not set.
-spec get_resp_header(binary(), context()) -> binary() | undefined.
get_resp_header(Header, Context) when is_binary(Header) ->
    Hs = maps:get(resp_headers, req(Context), #{}),
    maps:get(Header, Hs, undefined).

%% @doc Fetch all response headers.
-spec get_resp_headers(context()) -> map().
get_resp_headers(Context) ->
    maps:get(resp_headers, req(Context), #{}).

%% @doc Remove the response header from the list for response headers.
-spec remove_resp_header(binary(), context()) -> context().
remove_resp_header(Header, Context) when is_binary(Header) ->
    Req = cowboy_req:delete_resp_header(Header, req(Context)),
    set_req(Req, Context).

%% @doc Add a cookie to the response cookies
-spec set_resp_cookie(binary(), binary(), list(), context()) -> context().
set_resp_cookie(Key, Value, Options, Context) when is_binary(Key), is_binary(Value) ->
    Options1 = [ {K,V} || {K,V} <- Options, V =/= undefined ],
    Req = cowboy_req:set_resp_cookie(Key, Value, req(Context), maps:from_list(Options1)),
    set_req(Req, Context).

%% @doc Fetch all response cookies.
-spec get_resp_cookies(context()) -> [ {binary(),binary()} ].
get_resp_cookies(Context) ->
    maps:to_list(maps:get(resp_cookies, req(Context))).

%% @doc Fetch the value of a cookie.
-spec get_cookie_value(binary(), context()) -> binary() | undefined.
get_cookie_value(Name, Context) when is_binary(Name) ->
    Cookies = maps:get(cowmachine_cookies, env(Context)),
    proplists:get_value(Name, Cookies).

%% @doc Fetch all cookies.
-spec req_cookie(context()) -> list().
req_cookie(Context) ->
    maps:get(cowmachine_cookies, env(Context)).

%% @doc Set the preliminary HTTP response code for the request. This can be changed.
-spec set_response_code(integer(), context()) -> context().
set_response_code(Code, Context) when is_integer(Code) ->
    Env = env(Context),
    set_env(Env#{cowmachine_resp_code => Code}, Context).

%% @doc Fetch the preliminary HTTP response code for the request. This can be changed.
-spec response_code(context()) -> integer().
response_code(Context) ->
    maps:get(cowmachine_resp_code, env(Context)).

%% @doc Set the chosen charset.
-spec set_resp_chosen_charset(binary()|undefined, context()) -> context().
set_resp_chosen_charset(CharSet, Context) when is_binary(CharSet); CharSet =:= undefined ->
    Env = env(Context),
    set_env(Env#{cowmachine_resp_chosen_charset => CharSet}, Context).

%% @doc Get the chosen charset.
-spec resp_chosen_charset(context()) -> binary() | undefined.
resp_chosen_charset(Context) ->
    maps:get(cowmachine_resp_chosen_charset, env(Context)).

%% @doc Set the transfer encoding
-spec set_resp_transfer_encoding({binary(), function()}, context()) -> context().
set_resp_transfer_encoding(Enc, Context) ->
    Env = env(Context),
    set_env(Env#{cowmachine_resp_transfer_encoding => Enc}, Context).

%% @doc Get the transfer encoding.
-spec resp_transfer_encoding(context()) -> {binary(), function()} | undefined.
resp_transfer_encoding(Context) ->
    maps:get(cowmachine_resp_transfer_encoding, env(Context)).

%% @doc Set the content encoding
-spec set_resp_content_encoding(binary(), context()) -> context().
set_resp_content_encoding(Enc, Context) when is_binary(Enc) ->
    Env = env(Context),
    set_env(Env#{cowmachine_resp_content_encoding => Enc}, Context).

%% @doc Get the content encoding.
-spec resp_content_encoding(context()) -> binary().
resp_content_encoding(Context) ->
    maps:get(cowmachine_resp_content_encoding, env(Context)).

%% @doc Encode the content according to the selected content encoding
-spec encode_content(iodata(), context()) -> iolist().
encode_content(Content, Context) ->
    encode_content_1(resp_content_encoding(Context), Content).

encode_content_1(<<"gzip">>, Content) -> zlib:gzip(Content);
encode_content_1(<<"identity">>, Content) -> Content.


%% @doc Set the 'redirect' flag, used during POST processing to check if a 303 should be returned.
-spec set_resp_redirect(boolean() | binary(), context()) -> context().
set_resp_redirect(Location, Context) when is_binary(Location) ->
    Env = env(Context),
    Context1 = set_resp_header(<<"location">>, Location, Context),
    set_env(Env#{ cowmachine_resp_redirect => true }, Context1);
set_resp_redirect(IsRedirect, Context) when is_boolean(IsRedirect) ->
    Env = env(Context),
    set_env(Env#{ cowmachine_resp_redirect => IsRedirect }, Context).

%% @doc Return the 'redirect' flag, used during POST processing to check if a 303 should be returned.
-spec resp_redirect(context()) -> boolean().
resp_redirect(Context) ->
    maps:get(cowmachine_resp_redirect, env(Context)).

%% @doc Set the dispatch path of the request.
-spec set_disp_path(binary(), context()) -> context().
set_disp_path(Path, Context) ->
    Env = env(Context),
    set_env(Env#{cowmachine_disp_path => Path}, Context).

%% @doc Return the dispatch path of the request.
-spec disp_path(context()) -> binary() | undefined.
disp_path(Context) ->
    maps:get(cowmachine_disp_path, env(Context)).

%% @doc Set the response body, this must be converted to a response body that Cowboy can handle.
-spec set_resp_body(resp_body(), context()) -> context().
set_resp_body(RespBody, Context) ->
    Env = env(Context),
    set_env(Env#{cowmachine_resp_body => RespBody}, Context).

%% @doc Return the response body, this must be converted to a response body that Cowboy can handle.
-spec resp_body(context()) -> resp_body().
resp_body(Context) ->
    maps:get(cowmachine_resp_body, env(Context)).

%% @doc Check if a response body has been set.
-spec has_resp_body(context()) -> boolean().
has_resp_body(Context) ->
    case maps:get(cowmachine_resp_body, env(Context)) of
        undefined -> false;
        [] -> false;
        <<>> -> false;
        _ -> true
    end.


%% @doc Check if the request has a body
-spec has_req_body(context()) -> boolean().
has_req_body(Context) ->
    cowboy_req:has_body(req(Context)).

%% @doc Fetch the request body as a single binary.
%% Per default we don't receive more than ~128K bytes.
-spec req_body(context()) -> {binary()|undefined, context()}.
req_body(Context) ->
    req_body(128*1024, Context).

-spec req_body(non_neg_integer(), context()) -> {binary()|undefined, context()}.
req_body(MaxLength, Context) when MaxLength > 0 ->
    Req = req(Context),
    Opts = #{
        length => MaxLength,
        timeout => 10000
    },
    case cowboy_req:read_body(Req, Opts) of
        {ok, Body, Req2} ->
            {Body, set_req(Req2, Context)};
        {more, _Body, Req2} ->
            cowmachine:log(#{ level => warning,
                              at => ?AT,
                              text => lists:flatten(
                                        io_lib:format("Dropped request body, as it is larger than ~p bytes.",
                                                      [MaxLength]))
                              }, Req2),
            {undefined, set_req(Req2, Context)}
    end.

-spec stream_req_body(non_neg_integer(), context()) -> {ok|more, binary(), context()}.
stream_req_body(ChunkSize, Context) ->
    Opts = #{
        % length => 1024*1024*1024,
        length => ChunkSize,
        timeout => 10000
    },
    {Next, Chunk, Req1} = cowboy_req:read_body(req(Context), Opts),
    {Next, Chunk, set_req(Req1, Context)}.


-spec set_metadata(atom(), term(), context()) -> context().
set_metadata(Key, Value, Context) ->
    Env = env(Context),
    Env1 = Env#{ {cowmachine, Key} => Value },
    set_env(Env1, Context).

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
    maps:get({cowmachine, Key}, env(Context), undefined).
