-module(main).
-behaviour(gen_server).
-export([start_link/0]). % convenience call for startup
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]). % gen_server callbacks
-define(SERVER, ?MODULE). % macro just defines this module as server

%%% convenience function for startup
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init([]) ->
        ServerPid = startServer(),
		{ok, [ServerPid]}.

handle_call(_Request, _From, State) ->
        {noreplay, State}.

handle_cast(_Msg, State) ->
        {noreplay, State}.

handle_info(_Info, State) ->
        {noreplay, State}.

terminate(_Reason, _State) ->
        
	ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%% Internal functions

startServer() ->
    % The listener name is used to refer to this listener in future calls
Name = http,
        
TransportOpts = [inet, {port, 1234}],

ProtocolOpts = fun() ->
	Dispatch = fun() -> 
		Host = '_',
		
		RoutePath1 = fun()->
			Path = "/",
			Handler = cowboy_static,
			InitialState = {priv_file, main, "index.html"}, 
			RoutePath = {Path, Handler, InitialState},
			RoutePath
		end(),
				
		RoutePath2 = fun()->
			Path = "/assets/[...]",
			Handler = cowboy_static, 
			InitialState = {priv_dir, main, "static/assets"},    
			RoutePath = {Path, Handler, InitialState},
			RoutePath
		end(),
		
		RoutePath3 = fun()->
			Path = "/favicon.ico",
			Handler = cowboy_static, 
			InitialState = {priv_file, main, "favicon.ico"},
			RoutePath = {Path, Handler, InitialState},
			RoutePath
		end(),

		RoutePath4 = fun()->
			Path = "/upload",
			Handler = mainpage, 
			Opts = [],
			RoutePath = {Path, Handler, Opts},
			RoutePath
		end(),
			

		Route = {Host, [
		RoutePath1,
		RoutePath2,
		RoutePath3,
		RoutePath4
		]},
		cowboy_router:compile([Route])
	
	end(),	
	cowboy_options(Dispatch)
end(),
{ok, ListenerPid} = cowboy:start_clear(Name,
							TransportOpts,
							ProtocolOpts
							),
ListenerPid.

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