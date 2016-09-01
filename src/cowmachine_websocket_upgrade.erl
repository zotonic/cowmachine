-module(cowmachine_websocket_upgrade).

-export([
    upgrade/2
    ]).

%% @doc Upgrade the request to a websocket request
-spec upgrade(atom(), cowmachine_req:context()) -> {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(Handler, Context) ->
    Req = cowmachine_req:req(Context),
    Env = #{},
    cowboy_websocket:upgrade(Req, Env, Handler, Context, infinity, run).
