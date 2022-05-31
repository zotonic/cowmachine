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
			NewValue = integer_to_binary(rand:uniform(1000000)),
			Key = <<"server">>, 
			Value = NewValue,
			Options = [{path, <<"/">>}, {samesite, 'None'}, {secure, true}],
			Resp_cookieContext = cowmachine_req:set_resp_cookie(Key, Value, Options, Context),
			
			ClientCookie = cowmachine_req:get_cookie_value(<<"client">>, Resp_cookieContext),
			ServerCookie = cowmachine_req:get_cookie_value(<<"server">>, Resp_cookieContext),
			{ok, Body} = toppage_dtl:render([
				{client, ClientCookie},
				{server, ServerCookie}
			]),
			cowmachine_req:set_resp_body(Body, Resp_cookieContext);
		<<"/favicon.ico">> ->
			Favicon = file("priv", "favicon.ico"),
			FaviconContentType = cowmachine_util:normalize_content_type(<<"image/x-icon">>),
			NewContent = cowmachine_req:set_resp_content_type(FaviconContentType,Context),	
			NewContentRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,<<"image/x-icon">>,NewContent),
			cowmachine_req:set_resp_body({file, Favicon}, NewContentRespHeader)
	end,
	{true, ResultContext}.

content_types_provided(Context) -> 
	Html = cowmachine_util:normalize_content_type(<<"text/html">>),
	Favicon = cowmachine_util:normalize_content_type(<<"image/x-icon">>),
	{[Html, Favicon], Context}.
	
%% internal functions	

file(DirName, FileName) ->
	{ok, CurrentDir} = file:get_cwd(),
	ParentDir = filename:join([CurrentDir, DirName]),
	filename:join([ParentDir, FileName]).
	