%% @hidden

-module(cowmachine_server_tests).

-include_lib("eunit/include/eunit.hrl").

-export([execute/2]).

%%
%% Dispatch to the request's server
%%

of_req_test() ->
    ?assertEqual(cowmachine_fake_server, cowmachine_server:of_req(req())),
    ?assertEqual(cowmachine_server_cowboy, cowmachine_server:of_req(#{})).

module_test() ->
    ?assertEqual(cowmachine_server_cowboy, cowmachine_server:module()),
    application:set_env(cowmachine, server, cowmachine_fake_server),
    try
        ?assertEqual(cowmachine_fake_server, cowmachine_server:module()),
        ?assertEqual({error, {not_a_listener, fake}},
                     cowmachine_server:start_listener(fake, #{ port => 0, middlewares => [] })),
        ?assertEqual({error, not_found}, cowmachine_server:stop_listener(fake)),
        ?assertEqual(1234, cowmachine_server:port(fake))
    after
        application:unset_env(cowmachine, server)
    end.

reply_test() ->
    Req = req(),
    ?assertMatch(#{ has_sent_resp := true },
                 cowmachine_server:reply(200, #{ <<"a">> => <<"b">> }, [<<"x">>, "y"], Req)),
    ?assertEqual({reply, 200, #{ <<"a">> => <<"b">> }, <<"xy">>}, call()).

reply_with_resp_body_test() ->
    %% As cowboy_req:reply/2: the body set on the request.
    _ = cowmachine_server:reply(404, (req())#{ resp_body => <<"gone">> }),
    ?assertEqual({reply, 404, #{}, <<"gone">>}, call()),
    _ = cowmachine_server:reply(204, req()),
    ?assertEqual({reply, 204, #{}, <<>>}, call()).

stream_test() ->
    Req = cowmachine_server:stream_reply(200, #{}, req()),
    ?assertEqual({stream_reply, 200, #{}}, call()),
    ok = cowmachine_server:stream_body(<<"part">>, nofin, Req),
    ?assertEqual({stream_body, <<"part">>, nofin}, call()).

read_body_test() ->
    Req = req(),
    ?assertEqual({ok, <<"body">>, Req}, cowmachine_server:read_body(Req, #{ length => 10 })),
    ?assertEqual({read_body, #{ length => 10 }}, call()).

set_options_test() ->
    ok = cowmachine_server:set_options(#{ idle_timeout => infinity }, req()),
    ?assertEqual({set_options, #{ idle_timeout => infinity }}, call()).

websocket_upgrade_test() ->
    Req = req(),
    ?assertEqual({ok, Req, #{ e => 1 }},
                 cowmachine_server:websocket_upgrade(Req, #{ e => 1 }, handler, state, #{})),
    ?assertEqual({websocket_upgrade, handler, state, #{}}, call()).

%%
%% Middlewares
%%

execute_test() ->
    %% Each middleware sees the request and environment the one before
    %% returned, and a stop ends the chain.
    Req = #{ trail => [] },
    ?assertEqual({ok, Req, #{}}, cowmachine_server:execute(Req, #{}, [])),
    ?assertEqual({ok, #{ trail => [b, a] }, #{ steps => [] }},
                 cowmachine_server:execute(Req, #{ steps => [a, b] }, [?MODULE, ?MODULE])),
    ?assertEqual({stop, #{ trail => [stop] }},
                 cowmachine_server:execute(Req, #{ steps => [stop, a] }, [?MODULE, ?MODULE])).

execute(#{ trail := Trail } = Req, #{ steps := [stop | _] }) ->
    {stop, Req#{ trail => [stop | Trail] }};
execute(#{ trail := Trail } = Req, #{ steps := [Step | Steps] } = Env) ->
    {ok, Req#{ trail => [Step | Trail] }, Env#{ steps => Steps }}.

%%
%% Helpers
%%

req() ->
    #{ cowmachine_server => cowmachine_fake_server, owner => self() }.

call() ->
    receive
        {cowmachine_fake_server, Call} -> Call
    after 1000 ->
        error(no_call)
    end.
