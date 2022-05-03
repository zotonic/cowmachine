-record(cmstate, {
    % Cowboy state
    controller :: atom(),
    is_process_called = false :: boolean(),

    % Memo cache for controller calls
    cache = #{} :: map(),
    options = #{} :: map()
}).

-type cmstate() :: #cmstate{
	% Cowboy state
    controller :: atom(),
    is_process_called :: boolean(),

    % Memo cache for controller calls
    cache :: map(),
    options :: map()
}. %% Data for cowmachine's decision core



-define(DBG(Msg), error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [?MODULE, ?LINE, Msg])).
