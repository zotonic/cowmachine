-module(controller).

-export([
    execute/2,
	resource_exists/1,
	content_types_provided/1
]).

execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

% Controller export

resource_exists(Context) ->
	Path = cowmachine_req:path(Context),
	io:format("Path=~p~n",[Path]),
	
	ResultContext = case Path of
		<<"/">> ->
			IndexHtml = file(code:priv_dir(main), "examples.html"),
			cowmachine_req:set_resp_body({file, IndexHtml}, Context);
		<<"/assets/", FileName/binary>>	->
			FileNameList = binary_to_list(FileName),
			File = file(filename:join([code:priv_dir(main), "static/assets"]), FileNameList),
			cowmachine_req:set_resp_body({file, File}, Context);	
		_ ->
			[_|FileName] = binary_to_list(Path),
			File = file(code:priv_dir(main), FileName),
			% io:format("File=~p~n",[File]),
			cowmachine_req:set_resp_body({file, File}, Context)	
	end,
	{true, ResultContext}.

content_types_provided(Context) ->
	{ContentType, NewContext} =  setContentType(Context),
	{[ContentType], NewContext}.	

%% internal functions

file(DirName, FileName) ->
	{ok, CurrentDir} = file:get_cwd(),
	ParentDir = filename:join([CurrentDir, DirName]),
	filename:join([ParentDir, FileName]).

-spec setContentType(Context)-> Result when
	Context :: cowmachine_req:context(), 
	Result :: {ContentType, Context},
	ContentType :: cowmachine_req:media_type().
setContentType(Context)->
	PathBinary = tunePath(Context),
	MediaType = cow_mimetypes:all(PathBinary),
	ContentType = cowmachine_util:format_content_type(MediaType),
	% io:format("PathBinary = ~p, MediaType = ~p, ContentType =~p~n", [PathBinary, MediaType, ContentType]),
	NewContent = cowmachine_req:set_resp_content_type(MediaType,Context),	
	NewContentRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,ContentType,NewContent),
	{MediaType, NewContentRespHeader}.

tunePath(Context) ->
	PathBinary = cowmachine_req:path(Context),
	case PathBinary of
		<<"/">> -> <<"index.html">>;
		_ -> PathBinary
	end.