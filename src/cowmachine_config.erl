%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman 
%%
%% @doc Retrieve cowmachine configuration options.
%% @end

-module(cowmachine_config).

-export([
    ws_opts/0,
    env/2
]).

env(Key, Default) ->
    case application:get_env(cowmachine, Key) of
        {ok, V} -> V;
        _ -> Default
    end.

% Reasonably safe websocket config.
ws_opts() ->
    #{
      idle_timeout => env(ws_idle_timeout, 120000), % For slow clients while also preventing slow client DoS
      max_frame_size => env(ws_max_frame_size, 524288), % 512kb
      compress => env(ws_compress, true)
     }.

