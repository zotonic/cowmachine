-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	
	% The listener name is used to refer to this listener in future calls
	Name = http,
	
	TransportOpts = [inet, {port, 1234}],
	
	ProtocolOpts = fun() ->
		Dispatch = fun() -> 
			Host = '_',
			
			IndexHtml = fun() -> 
				{ok, CurrentDir} = file:get_cwd(),
				PrivDir = filename:join([CurrentDir, "priv"]),
				filename:join([PrivDir, "index.html"])
			end(),
			%io:format("Path =~p~n",[IndexHtml]),
			RoutePath1 = fun()->
				Path = "/",
				Handler = cowboy_static, 
				InitialState = {file, IndexHtml},
				RoutePath = {Path, Handler, InitialState},
				RoutePath
			end(),
			
			RoutePath2 = fun()->
				Path = "/websocket",
				Handler = ws_h, 
				Opts = [],
				RoutePath = {Path, Handler, Opts},
				RoutePath
			end(),
			
			RoutePath3 = fun()->
				Path = "/assets/[...]",
				Handler = cowboy_static, 
				InitialState = {dir, "static/assets"},
				RoutePath = {Path, Handler, InitialState},
				RoutePath
			end(),
			
			Route = {Host, [
			RoutePath1,
			RoutePath2,
			RoutePath3
			]},
			cowboy_router:compile([Route])
		
		end(),	
		cowboy_options(Dispatch)
	end(),
	%io:format("Priv dir =~p~n",[code:priv_dir(websocket)]),
	{ok, ListenerPid} = cowboy:start_clear(Name,
								TransportOpts,
								ProtocolOpts
								),
	{ok, ListenerPid}.
	
	
stop(_State) ->
    ok.

%% internal functions

%% Use this module as middleware, and controller
cowboy_options(Dispatch) ->

	TypeOfCallingCowmachine = rand:uniform(2),
	%TypeOfCallingCowmachine = 1+1,
	case TypeOfCallingCowmachine of
		1 ->
			#{ 
				env => #{dispatch => Dispatch}
			};
		2 ->
			#{ 
				env => #{dispatch => Dispatch},
				middlewares => [ 
					% ... add your dispatcher middlware
					controller,
					cowmachine 
				] 
			}
	end.