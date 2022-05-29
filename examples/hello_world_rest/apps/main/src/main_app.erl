-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	
	Name = cowmachine_listener,
	TransportOpts = [ inet, {port, 1234} ],
	
	ProtocolOpts = fun() ->
		Dispatch = fun() -> 
			Host = '_',
			Path = "/",
			Handler = mainpage, 
			Opts = [],
			RoutePath = {Path, Handler, Opts},
			Route = {Host, [RoutePath]},
			cowboy_router:compile([Route])
		end(),	
		cowboy_options(Dispatch)
	end(),
	
	ProtocolOpts = cowboy_options(Dispatch),
	{ok, Pid} = cowboy:start_clear(
						Name, 
						TransportOpts,
						ProtocolOpts),
	{ok, Pid}.
	

stop(_State) ->
    ok.

%% internal functions

%% Use this module as middleware, and controller
cowboy_options(_Dispatch) ->

	TypeOfCallingCowmachine = rand:uniform(2),
	%TypeOfCallingCowmachine = 1,
	case TypeOfCallingCowmachine of
		1 ->
			#{ 
				middlewares => [
					% ... add your dispatcher middlware
					mycowboy_middleware
				]	
			};
		2 ->
			#{ 
				%env => #{dispatch => Dispatch},
				middlewares => [ 
					% ... add your dispatcher middlware
					controller,
					cowmachine 
				] 
			}
	end.