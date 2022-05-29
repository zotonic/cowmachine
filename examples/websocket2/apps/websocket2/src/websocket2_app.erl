%%%-------------------------------------------------------------------
%% @doc websocket2 public API
%% @end
%%%-------------------------------------------------------------------

-module(websocket2_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    websocket2_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
