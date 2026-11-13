%% @author William Fank Thomé
%% @copyright 2026 William Fank Thomé
%% @doc The HTTP server cowmachine runs on.
%%
%% Cowmachine runs on Cowboy by default (`cowmachine_server_cowboy'). Another
%% server plugs in through this behaviour: set the `server' key of the
%% `cowmachine' application's environment to the module that implements it.
%%
%% ```
%% {cowmachine, [
%%     {server, my_server}
%% ]}
%% '''
%%
%% A server module does three things:
%%
%% <ul>
%% <li>It starts and stops listeners from the options `start_listener/2'
%%     takes, described below. The shared keys mean the same for every
%%     server, and `options' holds the server's own.</li>
%% <li>It runs the listener's middlewares on each request, with `execute/3'
%%     when the server has no middlewares of its own, handing them a request
%%     map shaped like Cowboy's, described below.</li>
%% <li>It does what touches the connection: sending responses, reading the
%%     body, changing the connection's options and upgrading to a WebSocket.
%%     The functions of this module call these callbacks for the server in the
%%     request's `cowmachine_server' key, and Cowboy's when the key is
%%     absent.</li>
%% </ul>
%%
%% == The listener options ==
%%
%% <ul>
%% <li>`port': the port to bind, `0' for any free one (see `port/1').</li>
%% <li>`ip': the address to bind, every IPv4 interface when absent. An IPv6
%%     address binds IPv6, and `ipv6_v6only' then says whether IPv4 clients
%%     are served too (the operating system decides when absent).</li>
%% <li>`tls': the `ssl' server options, for an HTTPS listener. Without it the
%%     listener serves plain HTTP.</li>
%% <li>`backlog', `max_connections' and `num_acceptors': the listen backlog,
%%     the most open connections and the number of accepting processes.</li>
%% <li>`request_timeout': milliseconds to wait for a request.</li>
%% <li>`middlewares': the `cowboy_middleware' modules each request runs
%%     through, in order, and `env' the environment they start with.</li>
%% <li>`metrics_callback': called once per request with the map Cowboy's
%%     `cowboy_metrics_h' builds, at least its `reason', `req', `req_start',
%%     `req_end', `req_body_length', `req_body_end', `resp_start',
%%     `resp_status', `resp_body_length' and `user_data' keys.
%%     `metrics_req_filter' and `metrics_resp_headers_filter' trim the
%%     request and the response headers it carries.</li>
%% <li>`options': the server's own options.</li>
%% </ul>
%%
%% == The request map ==
%%
%% Cowmachine reads and sets the request map with `cowboy_req''s functions
%% that do not touch the connection, so a server's map carries the keys and
%% value types of Cowboy's:
%%
%% <ul>
%% <li>`cowmachine_server': the server module</li>
%% <li>`method' (`<<"GET">>'), `version' (an atom, `` 'HTTP/1.1' ''),
%%     `scheme' (`<<"http">>' or `<<"https">>'), `host', `port', `path' and
%%     `qs' (the raw query string)</li>
%% <li>`headers': a map of lowercase names to values, repeated headers joined
%%     with `, ' (`; ' for `cookie')</li>
%% <li>`peer' (`{IP, Port}'), and `sock' and `cert' when the server has
%%     them</li>
%% <li>`has_body' (a boolean) and `body_length' (an integer, or `undefined'
%%     when unknown)</li>
%% </ul>
%%
%% `cowboy_req''s setters add the response keys: `resp_headers' (a map),
%% `resp_cookies' (a map of names to complete `set-cookie' values) and
%% `resp_body'.
%%
%% == The request callbacks ==
%%
%% They take and return what the `cowboy_req' functions of the same name do:
%%
%% <ul>
%% <li>`reply/4' sends `resp_headers' and every `resp_cookies' value as
%%     `set-cookie' along with the call's headers (which win). It returns the
%%     request without the `resp_*' keys and with `has_sent_resp' set. A
%%     `204' or `304' response, or a reply to a `HEAD' request, has no
%%     body.</li>
%% <li>`read_body/2' takes Cowboy's options (`length', `period', `timeout')
%%     and exits as Cowboy does when the body can't be read.</li>
%% <li>`set_options/2' takes the options cowmachine changes while it serves a
%%     request: `idle_timeout' (`infinity' while it streams a response) and
%%     `metrics_user_data' (merged into the `user_data' of the request's
%%     metrics). A server ignores those it has no counterpart for.</li>
%% <li>`websocket_upgrade/5' takes what `cowboy_websocket:upgrade/5' does,
%%     and runs `Handler''s `cowboy_websocket' callbacks the way Cowboy
%%     does.</li>
%% </ul>
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

-module(cowmachine_server).

-export([
    module/0,
    of_req/1,

    start_listener/2,
    stop_listener/1,
    port/1,

    execute/3,

    reply/2,
    reply/4,
    stream_reply/3,
    stream_body/3,
    read_body/2,
    set_options/2,
    websocket_upgrade/5
]).

-export_type([
    listener_ref/0,
    listener_opts/0,
    request_opts/0
]).

-type listener_ref() :: ranch:ref().
%% The name of a listener.

-type listener_opts() :: #{
    port := inet:port_number(),
    ip => inet:ip_address(),
    ipv6_v6only => boolean(),
    tls => [ssl:tls_server_option()],
    backlog => pos_integer(),
    max_connections => pos_integer() | infinity,
    num_acceptors => pos_integer(),
    request_timeout => timeout(),
    middlewares := [module()],
    env => cowboy_middleware:env(),
    metrics_callback => fun((map()) -> any()),
    metrics_req_filter => fun((cowboy_req:req()) -> map()),
    metrics_resp_headers_filter => fun((cowboy:http_headers()) -> cowboy:http_headers()),
    options => map()
}.
%% A listener's options, described in the module documentation.

-type request_opts() :: #{
    idle_timeout => timeout(),
    metrics_user_data => map()
}.
%% The options `set_options/2' changes while a request is served.

%% Listeners

-callback start_listener(Ref :: listener_ref(), Opts :: listener_opts()) ->
    {ok, pid()} | {error, term()}.

-callback stop_listener(Ref :: listener_ref()) -> ok | {error, not_found}.

-callback port(Ref :: listener_ref()) -> inet:port_number().

%% Requests

-callback reply(Status :: cowboy:http_status(), Headers :: cowboy:http_headers(),
                Body :: cowboy_req:resp_body(), Req) -> Req when Req :: cowboy_req:req().

-callback stream_reply(Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Req) ->
    Req when Req :: cowboy_req:req().

-callback stream_body(Data :: iodata(), IsFin :: fin | nofin, Req :: cowboy_req:req()) -> ok.

-callback read_body(Req, Opts :: cowboy_req:read_body_opts()) ->
    {ok, binary(), Req} | {more, binary(), Req} when Req :: cowboy_req:req().

-callback set_options(Opts :: request_opts(), Req :: cowboy_req:req()) -> ok.

-callback websocket_upgrade(Req, Env, Handler :: module(), State :: term(),
                            Opts :: cowboy_websocket:opts()) ->
    {ok, Req, Env} when Req :: cowboy_req:req(), Env :: cowboy_middleware:env().


%% @doc The server module the `cowmachine' application's environment names,
%% `cowmachine_server_cowboy' by default.
-spec module() -> module().
module() ->
    application:get_env(cowmachine, server, cowmachine_server_cowboy).

%% @doc The server module serving a request.
-spec of_req(cowboy_req:req()) -> module().
of_req(#{ cowmachine_server := Server }) -> Server;
of_req(_Req) -> cowmachine_server_cowboy.

%% @doc Start a listener named `Ref' on the configured server (`module/0').
-spec start_listener(listener_ref(), listener_opts()) -> {ok, pid()} | {error, term()}.
start_listener(Ref, Opts) ->
    (module()):start_listener(Ref, Opts).

%% @doc Stop the listener named `Ref' on the configured server (`module/0').
-spec stop_listener(listener_ref()) -> ok | {error, not_found}.
stop_listener(Ref) ->
    (module()):stop_listener(Ref).

%% @doc The port the listener named `Ref' is bound to, on the configured
%% server (`module/0'). For a listener started on port `0' it is the one the
%% operating system picked.
-spec port(listener_ref()) -> inet:port_number().
port(Ref) ->
    (module()):port(Ref).

%% @doc Run a request through `Middlewares', each a `cowboy_middleware', in
%% order, until one stops or the last returns. A server without middlewares of
%% its own runs a listener's `middlewares' with it.
-spec execute(Req, Env, Middlewares) -> {ok, Req, Env} | {stop, Req}
    when Req :: cowboy_req:req(),
         Env :: cowboy_middleware:env(),
         Middlewares :: [module()].
execute(Req, Env, []) ->
    {ok, Req, Env};
execute(Req, Env, [Middleware | Rest]) ->
    case Middleware:execute(Req, Env) of
        {ok, Req1, Env1} -> execute(Req1, Env1, Rest);
        {stop, Req1} -> {stop, Req1}
    end.

%% @doc Send a response with the request's `resp_body', as
%% `cowboy_req:reply/2' does.
-spec reply(cowboy:http_status(), Req) -> Req when Req :: cowboy_req:req().
reply(Status, #{ resp_body := Body } = Req) ->
    reply(Status, #{}, Body, Req);
reply(Status, Req) ->
    reply(Status, #{}, <<>>, Req).

%% @doc Send a response, as `cowboy_req:reply/4' does.
-spec reply(cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body(), Req) -> Req
    when Req :: cowboy_req:req().
reply(Status, Headers, Body, Req) ->
    (of_req(Req)):reply(Status, Headers, Body, Req).

%% @doc Start a streamed response, as `cowboy_req:stream_reply/3' does.
-spec stream_reply(cowboy:http_status(), cowboy:http_headers(), Req) -> Req
    when Req :: cowboy_req:req().
stream_reply(Status, Headers, Req) ->
    (of_req(Req)):stream_reply(Status, Headers, Req).

%% @doc Send part of a streamed response, as `cowboy_req:stream_body/3' does.
-spec stream_body(iodata(), fin | nofin, cowboy_req:req()) -> ok.
stream_body(Data, IsFin, Req) ->
    (of_req(Req)):stream_body(Data, IsFin, Req).

%% @doc Read the request body, as `cowboy_req:read_body/2' does.
-spec read_body(Req, cowboy_req:read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
    when Req :: cowboy_req:req().
read_body(Req, Opts) ->
    (of_req(Req)):read_body(Req, Opts).

%% @doc Change options of the connection serving a request.
-spec set_options(request_opts(), cowboy_req:req()) -> ok.
set_options(Opts, Req) ->
    (of_req(Req)):set_options(Opts, Req).

%% @doc Upgrade the request to a WebSocket running `Handler''s
%% `cowboy_websocket' callbacks, as `cowboy_websocket:upgrade/5' does.
-spec websocket_upgrade(Req, Env, module(), term(), cowboy_websocket:opts()) -> {ok, Req, Env}
    when Req :: cowboy_req:req(), Env :: cowboy_middleware:env().
websocket_upgrade(Req, Env, Handler, State, Opts) ->
    (of_req(Req)):websocket_upgrade(Req, Env, Handler, State, Opts).
