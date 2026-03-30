-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	{ok, Pid} = cowboy:start_clear(
		cowmachine_listener,
		[{port, 1234}],
		#{middlewares => [controller, cowmachine]}),
	{ok, Pid}.

stop(_State) ->
    ok.