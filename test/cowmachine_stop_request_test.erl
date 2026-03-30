%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_stop_request_test).

-include_lib("eunit/include/eunit.hrl").

-export([
    execute/2,
    process/4
]).

% Run test module
% $ rebar3 eunit -v -m cowmachine_stop_request_test

%% @doc Test that a controller throwing {stop_request, ResponseCode, Reason}
%% returns ResponseCode in the response (not 500).
%% This exercises the catch clause:
%%   throw:{stop_request, ResponseCode, _Reason} when is_integer(ResponseCode)
stop_request_3tuple_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),

    TestPid = self(),
    spawn_link(fun() ->
                  {ok, _} = cowboy:start_clear(
                              cowmachine_stop_request_test_listener,
                              [ inet, {port, 0} ],
                              cowboy_options()),
                  TestPid ! started
          end),
    %% Wait for the server to start
    receive
        started -> ok
    after 5000 ->
        error(server_start_timeout)
    end,

    Port = ranch:get_port(cowmachine_stop_request_test_listener),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/",

    %% The controller throws {stop_request, 503, test_reason}.
    %% Without the catch clause for 3-tuples with non-4xx/non-500 codes,
    %% this would be caught by the generic throw handler and return 500.
    %% With the clause, the response should be 503.
    ?assertMatch(
        {ok, {{"HTTP/1.1", 503, _}, _Headers, _Body}},
        httpc:request(Url)
    ),
    ?assertEqual(ok, cowboy:stop_listener(cowmachine_stop_request_test_listener)).

%%
%% Helpers
%%

%% Use this module as middleware, and controller
cowboy_options() ->
    #{ middlewares => [ ?MODULE, cowmachine ] }.

%% Middleware export, places this module in the request as controller.
execute(Req, Env) ->
    {ok, Req, Env#{ cowmachine_controller => ?MODULE } }.

%% Controller export - throws {stop_request, 503, test_reason}
process(<<"GET">>, _ContentType, _Accepted, _Context) ->
    throw({stop_request, 503, test_reason}).
