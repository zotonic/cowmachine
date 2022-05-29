-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	
	% The listener name is used to refer to this listener in future calls
	Name = cowmachine_listener,
	
	TransportOpts = [inet, {port, 1234}],
	
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
	
	{ok, ListenerPid} = cowboy:start_clear(Name,
								TransportOpts,
								ProtocolOpts
								),
	%io:format("~nListenerPid=~p~n", [ListenerPid]),
	{ok, ListenerPid}.
	
stop(_State) ->
    ok.

%% internal functions

%% Use this module as middleware, and controller
cowboy_options(Dispatch) ->

	TypeOfCallingCowmachine = rand:uniform(2),
	% TypeOfCallingCowmachine = 1+1,
	case TypeOfCallingCowmachine of
		1 ->
			#{ 
				env => #{dispatch => Dispatch}
			};
		2 ->
			#{ 
				middlewares => [ 
					% ... add your dispatcher middlware
					controller,
					cowmachine 
				] 
			}
	end.