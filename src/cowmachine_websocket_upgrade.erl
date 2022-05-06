-module(cowmachine_websocket_upgrade).

-export([
    upgrade/2
    ]).

%% @doc Upgrade the request to a websocket request

-spec upgrade(Handler, Context) -> Result when
	Handler :: atom(), 
	Context :: cowmachine_req:context(),
	Result :: {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(Handler, Context) ->
    Req = cowmachine_req:req(Context),
    Env = cowmachine_req:env(Context),
    Opts = #{
        idle_timeout => infinity,
        compress => true
    },

    % Ensure the handler module is loaded. Cowboy uses erlang:function_exported/3 to
    % check if optional callbacks are available. When the handler module is not loaded
    % yet it will not call the optional callbacks for the first request. This ensures
    % the module will be loaded.
    {module, Handler} = code:ensure_loaded(Handler),

    cowboy_websocket:upgrade(Req, Env, Handler, Context, Opts).
