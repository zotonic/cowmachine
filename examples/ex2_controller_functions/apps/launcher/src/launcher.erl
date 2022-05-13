-module(launcher).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    io:fwrite("Start web server~n"),
	{ok, _} = application:ensure_all_started(controller_functions),
	run(),
	erlang:halt(0).


%%====================================================================
%% Internal functions
%%====================================================================

run() ->
	Input = io:read("To stop the web server, type \"exit.\" and press \"Enter\" >>"),
	
	Term = element(2,Input),
	case Term of
		exit ->
			ok;
		_ ->
			run()
	end.
	