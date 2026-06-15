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
    Opts = ws_opts(Context),

    % Ensure the handler module is loaded. Cowboy uses erlang:function_exported/3 to
    % check if optional callbacks are available. When the handler module is not loaded
    % yet it will not call the optional callbacks for the first request. This ensures
    % the module will be loaded.
    {module, Handler} = code:ensure_loaded(Handler),

    cowboy_websocket:upgrade(Req, Env, Handler, Context, Opts).

% Reasonably safe websocket config.
ws_opts(Context) ->
    case cowmachine_req:get_metadata(ws_opts, Context) of
        M when is_map(M) ->
            maps:merge(ws_defaults(), M);
        _ ->
            ws_defaults()
    end.

ws_defaults() ->
    #{
      idle_timeout => 120000, % For slow clients while also preventing slow client DoS
      max_frame_size => 524288, % 512kb
      compress => true
     }.

