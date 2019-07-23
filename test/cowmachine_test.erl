-include_lib("eunit/include/eunit.hrl").

-module(cowmachine_test).

-export([
    execute/2,
    process/4
]).

cowmachine_start_test() ->
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(cowboy),

    ok = application:ensure_started(compiler),
    ok = application:ensure_started(syntax_tools),
    ok = application:ensure_started(goldrush),
    ok = application:ensure_started(lager),

    TestPid = self(), 
    spawn_link(fun() ->
                  {ok, _} = cowboy:start_clear(
                              cowmachine_test_listener,
                              [ inet, {port, 1234} ],
                              cowboy_options()),
                  TestPid ! started
          end),
    %% Wait for the server to start
    receive started -> ok end,

    %% Do a request to the test server, and check the response
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "Hello World"}} = httpc:request("http://127.0.0.1:1234/"),

    ok.

%%
%% Helpers
%%
       
%% Use this module as middleware, and controller
cowboy_options() ->
    #{ middlewares => [ ?MODULE, cowmachine ] }.

%% Middleware export,  places this module in the request as controller.
execute(Req, Env) ->
    {ok, Req, Env#{ controller => ?MODULE } }.

%% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
    {<<"Hello World">>, Context}.


