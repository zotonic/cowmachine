%% @author William Fank Thomé
%% @copyright 2026 William Fank Thomé
%% @doc Cowmachine's default server: Cowboy (`cowmachine_server').
%%
%% A listener runs its `middlewares' as Cowboy middlewares, and the request
%% callbacks are Cowboy's own `cowboy_req' functions. The listener's `options'
%% are Cowboy's protocol options, which win over those built from the shared
%% keys: set `stream_handlers' there, for example. With a `metrics_callback',
%% `cowboy_metrics_h' runs before `cowboy_stream_h'.
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

-module(cowmachine_server_cowboy).

-behaviour(cowmachine_server).

-export([
    start_listener/2,
    stop_listener/1,
    port/1,
    info/1,

    reply/4,
    inform/3,
    stream_reply/3,
    stream_body/3,
    stream_trailers/2,
    read_body/2,
    read_part/2,
    read_part_body/2,
    set_options/2,
    websocket_upgrade/5
]).

%% Listeners

%% @private
-spec start_listener(cowmachine_server:listener_ref(), cowmachine_server:listener_opts()) ->
    {ok, pid()} | {error, term()}.
start_listener(Ref, #{ tls := TlsOpts } = Opts) ->
    cowboy:start_tls(Ref, transport_opts(TlsOpts, Opts), protocol_opts(Opts));
start_listener(Ref, Opts) ->
    cowboy:start_clear(Ref, transport_opts([], Opts), protocol_opts(Opts)).

%% @private
-spec stop_listener(cowmachine_server:listener_ref()) -> ok | {error, not_found}.
stop_listener(Ref) ->
    cowboy:stop_listener(Ref).

%% @private
-spec port(cowmachine_server:listener_ref()) -> inet:port_number().
port(Ref) ->
    ranch:get_port(Ref).

%% @private
-spec info(cowmachine_server:listener_ref()) ->
    {ok, cowmachine_server:listener_info()} | {error, not_found}.
info(Ref) ->
    try ranch:info(Ref) of
        Info -> {ok, listener_info(Info)}
    catch
        %% Ranch raises badarg for a listener it doesn't know.
        error:badarg -> {error, not_found}
    end.

%% Ranch 1 reports a proplist and Ranch 2 a map, with the same keys. Dialyzer
%% sees the Ranch cowmachine builds with, so it would flag the clause for the
%% other one, which an application can lock (Zotonic runs Ranch 1).
-dialyzer({no_match, listener_info/1}).
listener_info(Info) when is_list(Info) ->
    listener_info(maps:from_list(Info));
listener_info(Info) ->
    maps:with([status, ip, port, active_connections, all_connections, max_connections], Info).

%% Ranch's options: the listener's socket options, and its connection limits.
transport_opts(TlsOpts, #{ port := Port } = Opts) ->
    SocketOpts = maps:fold(fun socket_opt/3, [{port, Port} | TlsOpts], Opts),
    maps:merge(
        #{ socket_opts => SocketOpts },
        maps:with([max_connections, num_acceptors], Opts)).

%% The socket option a listener option sets, if any.
socket_opt(ip, IP, Acc) -> [{ip, IP} | Acc];
socket_opt(ipv6_v6only, V6Only, Acc) -> [{ipv6_v6only, V6Only} | Acc];
socket_opt(backlog, Backlog, Acc) -> [{backlog, Backlog} | Acc];
socket_opt(_Key, _Value, Acc) -> Acc.

%% Cowboy's options: the middlewares and their environment, the request
%% timeout and the metrics, under the listener's own `options'.
protocol_opts(#{ middlewares := Middlewares } = Opts) ->
    Shared = maps:with(
        [request_timeout, metrics_callback, metrics_req_filter, metrics_resp_headers_filter],
        Opts),
    Base = Shared#{
        middlewares => Middlewares,
        env => maps:get(env, Opts, #{})
    },
    WithStreamHandlers = case Opts of
        #{ metrics_callback := _ } -> Base#{ stream_handlers => [cowboy_metrics_h, cowboy_stream_h] };
        #{} -> Base
    end,
    maps:merge(WithStreamHandlers, maps:get(options, Opts, #{})).

%% Requests

%% @private
-spec reply(cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body(), Req) -> Req
    when Req :: cowboy_req:req().
reply(Status, Headers, Body, Req) ->
    cowboy_req:reply(Status, Headers, Body, Req).

%% @private
-spec inform(cowboy:http_status(), cowboy:http_headers(), cowboy_req:req()) -> ok.
inform(Status, Headers, Req) ->
    cowboy_req:inform(Status, Headers, Req).

%% @private
-spec stream_reply(cowboy:http_status(), cowboy:http_headers(), Req) -> Req
    when Req :: cowboy_req:req().
stream_reply(Status, Headers, Req) ->
    cowboy_req:stream_reply(Status, Headers, Req).

%% @private
-spec stream_body(iodata(), fin | nofin, cowboy_req:req()) -> ok.
stream_body(Data, IsFin, Req) ->
    cowboy_req:stream_body(Data, IsFin, Req).

%% @private
-spec stream_trailers(cowboy:http_headers(), cowboy_req:req()) -> ok.
stream_trailers(Trailers, Req) ->
    cowboy_req:stream_trailers(Trailers, Req).

%% @private
-spec read_body(Req, cowboy_req:read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
    when Req :: cowboy_req:req().
read_body(Req, Opts) ->
    cowboy_req:read_body(Req, Opts).

%% @private
-spec read_part(Req, cowboy_req:read_body_opts()) -> {ok, cowboy:http_headers(), Req} | {done, Req}
    when Req :: cowboy_req:req().
read_part(Req, Opts) ->
    cowboy_req:read_part(Req, Opts).

%% @private
-spec read_part_body(Req, cowboy_req:read_body_opts()) ->
    {ok, binary(), Req} | {more, binary(), Req} when Req :: cowboy_req:req().
read_part_body(Req, Opts) ->
    cowboy_req:read_part_body(Req, Opts).

%% @private
-spec set_options(cowmachine_cowreq:request_opts(), cowboy_req:req()) -> ok.
set_options(Opts, Req) ->
    cowboy_req:cast({set_options, Opts}, Req).

%% @private
-spec websocket_upgrade(Req, Env, module(), term(), cowboy_websocket:opts()) -> {ok, Req, Env}
    when Req :: cowboy_req:req(), Env :: cowboy_middleware:env().
websocket_upgrade(Req, Env, Handler, State, Opts) ->
    cowboy_websocket:upgrade(Req, Env, Handler, State, Opts).
