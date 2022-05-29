-module(controller).

-export([
    execute/2,
    process/4,
	content_types_provided/1
]).

%% Cowmachine API

%% Middleware export, place this module in the request as controller.
execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
	
	Path = cowmachine_req:path(Context),
	ResultContext = case Path of
		<<"/">> ->
			IndexHtml = file(code:priv_dir(main), "index.html"),
			cowmachine_req:set_resp_body({file, IndexHtml}, Context);
		<<"/favicon.ico">> ->
			Favicon = file("priv", "favicon.ico"),
			FaviconContentType = cowmachine_util:normalize_content_type(<<"image/x-icon">>),
			NewContent = cowmachine_req:set_resp_content_type(FaviconContentType,Context),	
			NewContentRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,<<"image/x-icon">>,NewContent),
			cowmachine_req:set_resp_body({file, Favicon}, NewContentRespHeader);	
		<<"/assets/", FileName/binary>>	->
			FileNameList = binary_to_list(FileName),
			File = file(filename:join([code:priv_dir(main), "static/assets"]), FileNameList),
			ContentTypeController = set_content_types_provider(FileNameList, Context),
			cowmachine_req:set_resp_body({file, File}, ContentTypeController);
		<<"/eventsource">> ->
			EventStream = cowmachine_util:normalize_content_type(<<"text/event-stream">>),
			NewContent = cowmachine_req:set_resp_content_type(EventStream,Context),	
			NewContentRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,<<"text/event-stream">>,NewContent),
			StreamContext = cowmachine_req:set_resp_body(
		{stream,
			{list_to_binary(""),	
			fun Next() ->
				timer:sleep(500),
				Event = #{
					id => id(),
					data => getEmoji()
				},
				Req = cowmachine_req:req(NewContentRespHeader),
				Req1 = Req#{has_sent_resp => headers},
				FinNoFin = nofin,
				ok = cowboy_req:stream_events(Event, FinNoFin, Req1),
				
				{
					list_to_binary(""),
					Next
				}	
			end
			}
			
		},
		NewContentRespHeader),
			% io:format("StreamContext=~p~n",[StreamContext]),
			StreamContext		
	end,
	{true, ResultContext}.

content_types_provided(Context) -> 
	Html = cowmachine_util:normalize_content_type(<<"text/html">>),
	Css = cowmachine_util:normalize_content_type(<<"text/css">>),
	JS = cowmachine_util:normalize_content_type(<<"text/javascript">>),
	Favicon = cowmachine_util:normalize_content_type(<<"image/x-icon">>),
	EventStream = cowmachine_util:normalize_content_type(<<"text/event-stream">>),
	
	{[Html, Css, JS, Favicon, EventStream], Context}.

%% internal functions	

file(DirName, FileName) ->
	{ok, CurrentDir} = file:get_cwd(),
	ParentDir = filename:join([CurrentDir, DirName]),
	filename:join([ParentDir, FileName]).
	
set_content_types_provider(FileName,Context) ->
	Extension = filename:extension(FileName),
	case Extension of
		".css" ->
		  NewContent = cowmachine_req:set_resp_content_type(<<"text/css">>,Context),
		  NewContentRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,<<"text/css">>,NewContent),
		  NewContentRespHeader;
		".js" ->
			NewContent = cowmachine_req:set_resp_content_type(<<"application/javascript">>,Context),
		    NewContentRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,<<"application/javascript">>,NewContent),
			NewContentRespHeader;
		_ -> 
			NewContent = cowmachine_req:set_resp_content_type(<<"text/plain">>,Context),
		    NewContent
	end.

id() ->
	integer_to_list(erlang:unique_integer([positive, monotonic]), 16).

getEmoji() ->
	HtmlEntityList = emoji:all(),
	HtmlEntityListSize = length(HtmlEntityList),
	HtmlEntityId = rand:uniform(HtmlEntityListSize),
	HtmlEntity = lists:nth(HtmlEntityId, HtmlEntityList),
	HtmlEntity.