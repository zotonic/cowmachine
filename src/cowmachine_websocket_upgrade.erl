-module(cowmachine_websocket_upgrade).

-export([
    upgrade/2
    ]).

%% @doc Upgrade the request to a websocket request
-spec upgrade(atom(), cowmachine_req:context()) -> {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(Handler, Context) ->
    Req = cowmachine_req:req(Context),
    Env = cowmachine_req:env(Context),
    Opts = #{
        idle_timeout => infinity
    },
    {module, Handler} = code:ensure_loaded(Handler),
    cowboy_websocket:upgrade(Req, Env, Handler, Context, Opts).
