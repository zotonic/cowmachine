-module(controller2).

-export([
    execute/2,
    process/4
]).

%% Cowmachine API

%% Middleware export,  places this module in the request as controller.
execute(Req, Env) ->
    {ok, Req, Env#{ cowmachine_controller => ?MODULE } }.

% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
	List = ["Hello\r\n", "World\r\n", "Chunked!\r\n"],
	put("List", List),
	StreamContext = cowmachine_req:set_resp_body(
	{stream,
			{get_item(),	
				fun Next() ->
					timer:sleep(1000),
					CurrentItem = get_item(),
					case CurrentItem of
						{done, Item} ->
							{
								Item,
								done
							};
						_ -> 		
							{
								CurrentItem,
								Next
							}	
					end
				end				
			}
	}, Context),
	{{halt, 200}, StreamContext}.

%% internal functions	

get_item() ->
	List = get("List"),
	CurrentItem = case length(List) of
		1 -> 
			Item = hd(List),
			erase("List"),
			{done,Item};
		_ -> 
			[Item |Tail] = List,
			put("List", Tail),
			Item
	end,
	CurrentItem.