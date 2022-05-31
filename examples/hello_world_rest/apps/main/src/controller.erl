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
process(<<"GET">>, _ContentType, Accepted, Context) ->
	%io:format("HTML. ContentType =~p~n, Accepted = ~p~n",[_ContentType,_Accepted]),
	
	RespBody = case Accepted of
		{<<"text">>,<<"plain">>,[]} -> 
			text();
		{<<"text">>,<<"html">>,[]} ->
			html();
		{<<"application">>,<<"json">>,[]} ->
			json()
	end,		
	{RespBody, Context}.

content_types_provided(Context) -> 
	Html = cowmachine_util:normalize_content_type(<<"text/html">>),
	Text = cowmachine_util:normalize_content_type(<<"text/plain">>),
	JSON = cowmachine_util:normalize_content_type(<<"application/json">>),
	{[Html, Text, JSON], Context}.	


%% internal functions

text() ->
	Body = <<"REST Hello World as text!">>,
	Body.

html() ->
	Body = <<"<html>
	<head>
		<meta charset=\"utf-8\">
		<title>REST Hello World!</title>
	</head>
	<body>
		<p>REST Hello World as HTML!</p>
	</body>
</html>">>,
	Body.

json() ->
	Body = <<"{\"rest\": \"Hello World!\"}">>,
	Body.

