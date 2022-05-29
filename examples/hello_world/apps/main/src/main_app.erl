-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	Name = cowmachine_listener,
	TransportOpts = [ inet, {port, 1234} ],
	ProtocolOpts = cowboy_options(),
	{ok, _} = cowboy:start_clear(
						Name, 
						TransportOpts,
						ProtocolOpts).
	
stop(_State) ->
    ok.

%% internal functions

%% Use this module as middleware, and controller
cowboy_options() ->
    #{ middlewares => [ controller, cowmachine ] }.
