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
		<<"/websocket">> ->
			{ok, Req, Env} = cowmachine_websocket_upgrade:upgrade(ws_h, Context),
			ReqContext =cowmachine_req:set_req(Req, Context),
			EnvContext = cowmachine_req:set_env(Env, ReqContext),
			EnvContext
	end,
	{true, ResultContext}.

content_types_provided(Context) -> 
	Html = cowmachine_util:normalize_content_type(<<"text/html">>),
	Css = cowmachine_util:normalize_content_type(<<"text/css">>),
	JS = cowmachine_util:normalize_content_type(<<"text/javascript">>),
	Favicon = cowmachine_util:normalize_content_type(<<"image/x-icon">>),
	{[Html, Css, JS, Favicon], Context}.
	
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

