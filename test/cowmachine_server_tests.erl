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
