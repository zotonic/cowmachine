%% @doc Data for cowmachine's decision core

-record(cmstate, {
    % Cowboy state
    env :: cowboy_middleware:env(),
    controller :: atom(),

    % Memo cache for controller calls
    cache = #{} :: map(),
    options = #{} :: map()
}).

-define(DBG(Msg), error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [?MODULE, ?LINE, Msg])).
