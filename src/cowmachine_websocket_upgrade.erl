-module(cowmachine_websocket_upgrade).

-export([
    upgrade/2
    ]).

%% @doc Upgrade the request to a websocket request
%% @todo Cleanup the context req, remove all unneeded data (maybe the whole req?)
-spec upgrade(atom(), cowmachine_req:context()) -> {ok, cowboy_req:req(), cowboy_middleware:env()}.
upgrade(Handler, Context) ->
    Req = cowmachine_req:req(Context),
    Env = #{},
    cowmachine_websocket:upgrade(Req, Env, Handler, Context, infinity, run).
