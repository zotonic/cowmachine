-module(controller).

-export([
    execute/2,
    process/4,
	service_available/1,
	resource_exists/1,
	is_authorized/1,
	forbidden/1,
	allow_missing_post/1,
	malformed_request/1,
	uri_too_long/1,
	known_content_type/1,
	valid_content_headers/1,
	valid_entity_length/1,
	options/1,
	known_methods/1,
	allowed_methods/1,
	validate_content_checksum/1,
	delete_resource/1,
	post_is_create/1,
	create_path/1,
	content_types_provided/1,
	content_types_accepted/1,
	charsets_provided/1,
	content_encodings_provided/1,
	transfer_encodings_provided/1,
	variances/1,
	is_conflict/1,
	multiple_choices/1,
	previously_existed/1,
	moved_permanently/1,
	moved_temporarily/1,
	last_modified/1,
	generate_etag/1,
	finish_request/1
]).

%% Cowmachine API

%% Middleware export,  places this module in the request as controller.
execute(Req, Env) ->
    {ok, Req, Env#{ cowmachine_controller => ?MODULE } }.

% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
	{<<"Get. Hello World">>, Context};
process(<<"POST">>, _ContentType, _Accepted, Context) ->
	{<<"POST. Hello World">>, Context}.
	%{Context, Context}.
	

service_available(Context) -> 
	{true, Context}.
	%{false, Context}.
	
resource_exists(Context) -> 
	{true, Context}.
	%{false, Context}.
	
is_authorized(Context) -> 
	{true, Context}.
	%{false, Context}.

forbidden(Context) -> 
	%{true, Context}.
	{false, Context}.
	
allow_missing_post(Context) -> 
	%{true, Context}.
	{false, Context}.
	
malformed_request(Context) -> 
	%{true, Context}.
	{false, Context}.
	
uri_too_long(Context) -> 
	%{true, Context}.
	{false, Context}.

known_content_type(Context) -> 
	{true, Context}.
	%{false, Context}.

valid_content_headers(Context) -> 
	{true, Context}.
	%{false, Context}.
	
valid_entity_length(Context) -> 
	{true, Context}.
	%{false, Context}.
	
options(Context) -> 
	{[
		{<<"host">>, <<"localhost:1234">>},
		{<<"accept-encoding">>, <<"gzip, deflate, br">>}
	], Context}.
	
known_methods(Context) -> 
	{[ 
		<<"GET">>, <<"HEAD">>, <<"POST">>, 
		<<"PUT">>, <<"PATCH">>, <<"DELETE">>,
		<<"TRACE">>, <<"CONNECT">>, <<"OPTIONS">> 
	 ],	Context}.
	 
allowed_methods(Context) -> 
	{[ 
		<<"GET">>, <<"HEAD">>, <<"POST">>
	 ],	Context}.

validate_content_checksum(Context) -> 
	{not_validated,	Context}.
	
delete_resource(Context) -> 
	%{true, Context}.
	{false, Context}.
	
post_is_create(Context) -> 
	%{true, Context}.
	{false, Context}.
	
create_path(Context) -> 
	{<<"/">>, Context}.
	
content_types_provided(Context) -> 
	%% Default Content Type here is  <<"binary/octet-stream">>
	%DefaultRespContentType = cowmachine_req:resp_content_type(Context),
	%io:format("Resp content type = ~p~n",[DefaultRespContentType]),
	%{[DefaultRespContentType], Context}.
	
	%% Produce 406 Not Acceptable while GET
	%{[], Context}.

	TextPlainContentTypeContext = cowmachine_req:set_resp_content_type(<<"text/plain">>, Context),
	CurrentRespContentType = cowmachine_req:resp_content_type(TextPlainContentTypeContext),
	%io:format("Resp content type = ~p~n",[CurrentRespContentType]),
	{[CurrentRespContentType], TextPlainContentTypeContext}.
	
content_types_accepted(Context) -> 
	{[], Context}.

charsets_provided(Context) -> 
	{[cowmachine_req:resp_chosen_charset(Context)], Context}.
	
	
content_encodings_provided(Context) -> 
	%{[<<"identity">>, <<"gzip">>], Context}.
	%{[<<"gzip">>, <<"identity">>], Context}.
	{[<<"identity">>], Context}.
	%{[<<"gzip">>], Context}.
	
transfer_encodings_provided(Context) -> 
	{[
	{<<"identity">>, fun(X) -> X end},
		{<<"gzip">>, fun(X) -> 
				io:format("X = ~p~n",[X]),
				zlib:gzip(X) 
			end}], Context}.
		
variances(Context) -> 
	{[], Context}.
	
is_conflict(Context) -> 
	{false, Context}.
	
multiple_choices(Context) -> 
	{false, Context}.
	
previously_existed(Context) -> 
	{false, Context}.

moved_permanently(Context) -> 
	{false, Context}.
	
moved_temporarily(Context) -> 
	{false, Context}.
	
last_modified(Context) -> 
	{calendar:now_to_local_time(erlang:timestamp()), Context}.
	
generate_etag(Context) -> 
	{undefined, Context}.
	
finish_request(Context) -> 
	{undefined, Context}.