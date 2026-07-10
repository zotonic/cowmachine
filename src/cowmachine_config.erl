%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman 
%%
%% @doc Retrieve cowmachine configuration options.
%% @end

-module(cowmachine_config).

-export([
    env/2
]).

env(Key, Default) ->
    case application:get_env(cowmachine, Key) of
        {ok, V} -> V;
        _ -> Default
    end.

