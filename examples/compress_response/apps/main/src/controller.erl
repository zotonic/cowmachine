-module(controller).

-export([
    execute/2,
    process/4,
	content_types_provided/1,
	content_encodings_provided/1	
]).

%% Cowmachine API

%% Middleware export, place this module in the request as controller.
execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

content_types_provided(Context) -> 
	Html = cowmachine_util:normalize_content_type(<<"text/html">>),
	Text = cowmachine_util:normalize_content_type(<<"text/plain">>),
	{[Html, Text], Context}.	
	
content_encodings_provided(Context) -> 
	{[<<"identity">>, <<"gzip">>], Context}.

% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
	% io:format("Context = ~p~n",[Context]),
	BigBody =
<<"A cowboy is an animal herder who tends cattle on ranches in North America,
traditionally on horseback, and often performs a multitude of other ranch-
related tasks. The historic American cowboy of the late 19th century arose
from the vaquero traditions of northern Mexico and became a figure of special
significance and legend. A subtype, called a wrangler, specifically tends the
horses used to work cattle. In addition to ranch work, some cowboys work for
or participate in rodeos. Cowgirls, first defined as such in the late 19th
century, had a less-well documented historical role, but in the modern world
have established the ability to work at virtually identical tasks and obtained
considerable respect for their achievements. There are also cattle handlers
in many other parts of the world, particularly South America and Australia,
who perform work similar to the cowboy in their respective nations.\n">>,
    
	BigBodyContent = do_encoding(BigBody, Context),
	NewContent = cowmachine_req:set_resp_content_type(<<"text/plain">>,BigBodyContent),
	NewContentRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,<<"text/plain">>,NewContent),
	
	{true, NewContentRespHeader}.

%%% Internal functions

-spec do_encoding(RespBody, Context) -> Result when
	RespBody :: cowmachine_req:resp_body(), 
	Context :: cowmachine_req:context(),
	Result :: cowmachine_req:context().		
do_encoding(RespBody, Context) ->
	Resp_headers = cowmachine_req:get_resp_headers(Context),
	RespContext = case maps:is_key(<<"content-encoding">>, Resp_headers) of
		false -> 
			cowmachine_req:set_resp_body(RespBody,Context);
		true ->
			ContentEncoding = maps:get(<<"content-encoding">>, Resp_headers),
			case ContentEncoding of
				<<"gzip">> ->
					GzipBigBody = zlib:gzip(RespBody),
					cowmachine_req:set_resp_body(GzipBigBody,Context);
				_ -> throw({badmatch, ContentEncoding})
			end	
	end,
	RespContext	.