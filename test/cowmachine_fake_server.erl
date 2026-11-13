%% @hidden
%% A cowmachine_server for cowmachine_server_tests: each request callback
%% sends the request's `owner' the call it got, so the tests see what
%% cowmachine_server hands the server.

-module(cowmachine_fake_server).

-behaviour(cowmachine_server).

-export([
    start_listener/2,
    stop_listener/1,
    port/1,
    reply/4,
    stream_reply/3,
    stream_body/3,
    read_body/2,
    set_options/2,
    websocket_upgrade/5
]).

start_listener(Ref, _Opts) -> {error, {not_a_listener, Ref}}.

stop_listener(_Ref) -> {error, not_found}.

port(_Ref) -> 1234.

reply(Status, Headers, Body, Req) ->
    record({reply, Status, Headers, iolist_to_binary(Body)}, Req),
    Req#{ has_sent_resp => true }.

stream_reply(Status, Headers, Req) ->
    record({stream_reply, Status, Headers}, Req),
    Req#{ has_sent_resp => headers }.

stream_body(Data, IsFin, Req) ->
    record({stream_body, iolist_to_binary(Data), IsFin}, Req),
    ok.

read_body(Req, Opts) ->
    record({read_body, Opts}, Req),
    {ok, <<"body">>, Req}.

set_options(Opts, Req) ->
    record({set_options, Opts}, Req),
    ok.

websocket_upgrade(Req, Env, Handler, State, Opts) ->
    record({websocket_upgrade, Handler, State, Opts}, Req),
    {ok, Req, Env}.

record(Call, #{ owner := Owner }) ->
    Owner ! {?MODULE, Call}.
