%% @doc Macro to make logging easier.

-define(AT, (lists:flatten(io_lib:format("~p:~p:~B", [?MODULE, ?FUNCTION_NAME, ?LINE])))).
