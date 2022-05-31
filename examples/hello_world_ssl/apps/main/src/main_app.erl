-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	
	% The listener name is used to refer to this listener in future calls
	Name = https,
	
	TransportOpts = fun() ->
		SSL_Dir = fun() -> 
			{ok, CurrentDir} = file:get_cwd(),
			PrivDir = filename:join([CurrentDir, "priv"]),
			filename:join([PrivDir, "ssl"])		
		end(),	
		
		CA_Certfile = {cacertfile, filename:join([SSL_Dir, "cowboy-ca.crt"])},
		Certfile = {certfile, filename:join([SSL_Dir, "server.crt"])},
		Keyfile = {keyfile, filename:join([SSL_Dir, "server.key"])},	
		[{port, 8443}, CA_Certfile, Certfile, Keyfile]
	end(),
	
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
	
	
	%io:format("~nTransportOpts=~p~n", [TransportOpts]),
	%io:format("~nProtocolOpts=~p~n", [ProtocolOpts]),
	{ok, ListenerPid} = cowboy:start_tls(Name,
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
	%TypeOfCallingCowmachine = 2,
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