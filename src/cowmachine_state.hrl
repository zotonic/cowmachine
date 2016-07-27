%% @doc Data for cowmachine's decision core

-record(cmstate, {
    % Cowboy state
    env :: cowboy_middleware:env(),

    controller :: atom(),
    controller_options :: list(),

    % Memo cache for resource calls
    cache = #{} :: map()

    % socket=undefined,
    % metadata=dict:new() :: wm_dict(),
    % range=undefined,
    % peer=undefined :: inet:ip_address(),
    % bodyfetch=undefined,
    % log_data=undefined,

    % Reqdata
    % method  :: wrq:method(),
    % scheme  :: wrq:scheme(),
    % version :: wrq:version(),
    % disp_path,
    % path     :: string(),
    % raw_path :: string(),
    % path_info :: wm_dict(),
    % path_tokens :: [string()],
    % app_root  :: string(),
    % response_code :: pos_integer(),
    % max_recv_body :: pos_integer(),
    % resp_code = 500 :: integer(),
    % resp_redirect = false :: boolean(),
    % resp_headers = [] :: list(),
    % resp_content_encoding = <<"identity">> :: binary(),
    % resp_transfer_encoding = undefined :: undefined | {binary(),function()},
    % resp_content_type = <<"binary/octet-stream">> :: binary(),
    % resp_chosen_charset = undefined :: binary() | undefined,
    % resp_body = undefined  :: any(),
    % req_cookie    :: string(),
    % req_qs        :: string(),
    % req_headers   :: wm_gb_tree(), %% mochiheaders
    % req_body,
    % is_range_ok = false :: boolean(),
    % host_tokens  :: [string()],
    % port  :: inet:port_number(),
}).

-define(DBG(Msg), error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [?MODULE, ?LINE, Msg])).
