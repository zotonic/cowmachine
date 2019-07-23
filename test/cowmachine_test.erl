-include_lib("eunit/include/eunit.hrl").

-module(cowmachine_test).

-export([
    execute/2,
    process/4
]).

cowmachine_start_test() ->
    ok = application:ensure_started(ranch),
    TestPid = self(), 
    spawn_link(fun() ->
                  {ok, _} = cowboy:start_clear(
                              cowmachine_test_listener,
                              [
                               inet,
                               {port, 1234}
                              ],
                              cowboy_options()),
                  TestPid ! started


          end),
    %% Wait for the server to start
    receive started -> ok end,

    bad  = httpc:request("http://127.0.0.1:1234/"),

    ok.

%%
%% Cowmachine middleware test
%%
       
%% Use this module as middleware, and controller
cowboy_options() ->
    #{
        middlewares => [ ?MODULE, cowmachine ]
    }.

% Place this module in the request as controller for all requests.
execute(Req, Env) ->
    {ok, Req, Env#{ controller => ?MODULE } }.

process(<<"GET">>, _ContentType, _Accepted, Context) ->
    io:fwrite(standard_error, "process: ~p ~n", [Context]),
    {<<"Hello World">>, Context}.


