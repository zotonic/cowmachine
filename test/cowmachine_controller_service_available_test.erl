-include_lib("eunit/include/eunit.hrl").

-module(cowmachine_controller_service_available_test).

-export([
    execute/2,
    process/4,
	service_available/1
]).

% Run test module
% $ rebar3 eunit -v -m cowmachine_controller_service_available_test

cowmachine_start_test() ->
	{ok, _} = application:ensure_all_started(cowmachine),
    
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
	?assertMatch(
		{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "Hello World"}},
		httpc:request("http://127.0.0.1:1234/")
	),
	?assertEqual(ok, cowboy:stop_listener(cowmachine_test_listener)).

%%
%% Helpers
%%
       
%% Use this module as middleware, and controller
cowboy_options() ->
    #{ middlewares => [ ?MODULE, cowmachine ] }.

%% Middleware export,  places this module in the request as controller.
execute(Req, Env) ->
    {ok, Req, Env#{ cowmachine_controller => ?MODULE } }.

%% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
    {<<"Hello World">>, Context}.

%% Controller export
service_available(Context) -> {true, Context}.
