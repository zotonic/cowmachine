%% @hidden
%% A cowmachine_server for cowmachine_server_tests and cowmachine_cowreq_tests:
%% each request callback sends the request's `owner' the call it got, and the
%% reads return what the request's `fake_results' map holds for them, so the
%% tests see what cowmachine hands the server.

-module(cowmachine_fake_server).

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

start_listener(Ref, _Opts) -> {error, {not_a_listener, Ref}}.

stop_listener(_Ref) -> {error, not_found}.

port(_Ref) -> 1234.

info(_Ref) -> {error, not_found}.

reply(Status, Headers, Body, Req) ->
    record({reply, Status, Headers, iolist_to_binary(Body)}, Req),
    Req#{ has_sent_resp => true }.

inform(Status, Headers, Req) ->
    record({inform, Status, Headers}, Req),
    ok.

stream_reply(Status, Headers, Req) ->
    record({stream_reply, Status, Headers}, Req),
    Req#{ has_sent_resp => headers }.

stream_body(Data, IsFin, Req) ->
    record({stream_body, iolist_to_binary(Data), IsFin}, Req),
    ok.

stream_trailers(Trailers, Req) ->
    record({stream_trailers, Trailers}, Req),
    ok.

read_body(Req, Opts) ->
    record({read_body, Opts}, Req),
    result(read_body, Req).

read_part(Req, Opts) ->
    record({read_part, Opts}, Req),
    result(read_part, Req).

read_part_body(Req, Opts) ->
    record({read_part_body, Opts}, Req),
    result(read_part_body, Req).

set_options(Opts, Req) ->
    record({set_options, Opts}, Req),
    ok.

websocket_upgrade(Req, Env, Handler, State, Opts) ->
    record({websocket_upgrade, Handler, State, Opts}, Req),
    {ok, Req, Env}.

record(Call, #{ owner := Owner }) ->
    Owner ! {?MODULE, Call}.

result(Name, #{ fake_results := Results } = Req) ->
    case maps:get(Name, Results) of
        {Tag, Data} -> {Tag, Data, Req};
        done -> {done, Req}
    end.
