-module(controller).

-export([
    execute/2,
	process/4,
	resource_exists/1,
	content_types_provided/1,
	content_types_accepted/1,
	allowed_methods/1,
	post_is_create/1,
	create_path/1
]).

execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

process(<<"GET">>, _ContentType, _Accepted, Context) ->
	{true, Context};
process(<<"POST">>, _ContentType, _Accepted, Context) ->
	case cowmachine_req:resp_content_type(Context) of
		{<<"multipart">>,<<"form-data">>,[]} ->
			{List, ReqDoneContext} = collect_multipart(Context, []),
			HtmlContentType = <<"text/html">>,
			HtmlMediaType = cowmachine_util:normalize_content_type(HtmlContentType),
			Context1 = cowmachine_req:set_resp_content_type(HtmlMediaType, ReqDoneContext),	
			Context2 = cowmachine_req:set_resp_header(<<"content-type">>,HtmlContentType, Context1),
			Context3 = cowmachine_req:set_resp_chosen_charset(<<"utf-8">>,Context2),
			FileNameListHtml = builFileNameListHtml(List),
			% io:format("FileNameListHtml=~p~n",[FileNameListHtml]),
			
			RespBody = "<!DOCTYPE html><html><head><title>Uploaded Files</title><head><body><button onclick=\"history.back()\">Go Back</button><h2>Uploaded file list:</h2><ul>" 
				++ FileNameListHtml 
				++ "</ul></body></html>",
			{RespBody, Context3};
		_ ->
			{true, Context}
	end.	
	
% % Controller export

allowed_methods(Context) -> 
	{[ 
		<<"GET">>, <<"HEAD">>, <<"POST">>
	],	Context}.


resource_exists(Context) ->
	Path = cowmachine_req:path(Context),
	% io:format("resource_exists. Path=~p~n",[Path]),
	case Path of
		<<"/">>	->
			FileName = "index.html",
			File = file(code:priv_dir(main), FileName),
			IndexContext = cowmachine_req:set_resp_body({file, File}, Context),
			{true, IndexContext};
		<<"/assets/", FileName/binary>>	->
			FileNameList = binary_to_list(FileName),
			File = file(filename:join([code:priv_dir(main), "static/assets"]), FileNameList),
			{true, cowmachine_req:set_resp_body({file, File}, Context)};
		_ ->
			case filename:extension(Path) of
				<<>> -> 
					% io:format("resource_exists. Context=~p~n",[Context]),
					{true, Context};
				_ ->
					[_|FileName] = binary_to_list(Path),
					File = file(code:priv_dir(main), FileName),
					{true, cowmachine_req:set_resp_body({file, File}, Context)}
			end	
	end.

content_types_accepted(Context) ->
	% io:format("~p~n",[content_types_accepted]),
	ContentType = cowmachine_req:resp_content_type(Context),
	% io:format("content_types_accepted. ~p~n",[ContentType]),
	{[ContentType], Context}.		

content_types_provided(Context) ->
	% io:format("~p~n",[content_types_provided]),
	
	{ContentType, NewContext} =  setContentType(Context),
	% io:format("content_types_provided. after. ContentType=~p~n",[ContentType]),
	% io:format("content_types_provided. after. resp content type. ContentType=~p~n",[cowmachine_req:resp_content_type(NewContext)]),
	
	{[ContentType], NewContext}.	

post_is_create(Context) -> 
	% io:format("post_is_create. ~p~n",[post_is_create]),
	% POST has PUT behaviour
	{false, Context}.

create_path(Context) ->
	Path = cowmachine_req:path(Context),
	% io:format("create_path. Path=~p~n",[Path]),
	{Path, Context}.


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
	% io:format("setContentType. MediaType=~p~n",[MediaType]),
	{CheckedMediaType, CheckedContentType} = checkMediaType(MediaType, Context),
	% io:format("CheckedMediaType=~p~n",[CheckedMediaType]),
	ContentType = cowmachine_util:format_content_type(CheckedMediaType),
	% io:format("setContentType. ContentType=~p~n",[ContentType]),
	NewContext = cowmachine_req:set_resp_content_type(ContentType, CheckedContentType),	
	NewContextRespHeader = cowmachine_req:set_resp_header(<<"content-type">>,ContentType,NewContext),
 	{ContentType, NewContextRespHeader}.

checkMediaType(ContentType,Context) ->
	HeaderContentType = cowmachine_req:get_req_header(<<"content-type">>,Context),
	case HeaderContentType of
		<<"multipart/form-data;", _Rest/binary>> ->
			MultiPartContentType = <<"multipart/form-data">>,
			NormalizedContentType = cowmachine_util:normalize_content_type(MultiPartContentType),
			{NormalizedContentType, Context};
		_ -> {ContentType, Context}
	end.

tunePath(Context) ->
	PathBinary = cowmachine_req:path(Context),
	case PathBinary of
		<<"/">> -> <<"index.html">>;
		_ -> PathBinary
	end.

collect_multipart(Context, Acc) ->
	% io:format("~p~n",["collect_multipart"]),
	Req0 = cowmachine_req:req(Context),
	case cowboy_req:read_part(Req0) of
		{ok, Headers, Req1} ->
			{FileNameResult, BodyResult, ReqResult} = case cow_multipart:form_data(Headers) of
				{data, FileName} ->
					{ok, Body, Req} = cowboy_req:read_part_body(Req1),
					{FileName, Body, Req};
				{file, _FieldName, FileName, _ContentType} ->
					{FileBody, Req} = stream_file(Req1,[]),
					% io:format("FileName=~p, FileBody=~p~n",[FileName, FileBody]),
					{FileName, FileBody, Req}
				end,
				NewContext = cowmachine_req:set_req(ReqResult,Context),
				collect_multipart(NewContext, Acc ++ [{FileNameResult, BodyResult}]);
		{done, ReqDone} ->
			ReqDoneContext = cowmachine_req:set_req(ReqDone, Context),
			{Acc, ReqDoneContext}
	end.

stream_file(Req0, Acc) ->
	case cowboy_req:read_part_body(Req0) of
		{ok, LastBodyChunk, Req} ->
			FileBody = list_to_binary(Acc ++ [LastBodyChunk]), 
			{FileBody, Req};
		{more, BodyChunk, Req} ->
			stream_file(Req, Acc ++ [BodyChunk])
	end.

builFileNameListHtml(List) ->
	StringList = ["<li>" ++ tuneFileName(FileName) ++ "</li>" || {FileName, _FileBody} <- List],
	String = lists:flatten(StringList),
	String.

-spec tuneFileName(FileName) -> Result when
	FileName :: binary(),
	Result :: list().
tuneFileName(FileName) ->
	String = binary_to_list(FileName),
	NewString = filterCharacter($", String, ""),
	NewString.

filterCharacter(_Character, [], Acc) -> Acc;
filterCharacter(Character, [Character|Rest], Acc) ->
	filterCharacter(Character, Rest, Acc);
filterCharacter(Character, [Char|Rest], Acc) -> 
	filterCharacter(Character, Rest, Acc ++ [Char]).

