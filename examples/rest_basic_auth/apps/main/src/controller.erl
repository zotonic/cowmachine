-module(controller).

-export([
    execute/2,
    process/4,
	content_types_provided/1,
    is_authorized/1
]).

%% Cowmachine API

%% Middleware export, place this module in the request as controller.
execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
    User = cowmachine_req:get_resp_header(<<"user">>, Context),
    case User of 
        undefined ->
            {"Your are not authorised", Context};
        _ -> 
            ResultString = "Hello, " ++ binary_to_list(User) ++ "!\n",
            {ResultString, Context}            
    end.

is_authorized(Context) ->
    Header = cowmachine_req:get_req_header(<<"authorization">>, Context),
   
    case Header of
        <<"Basic ", User/binary>> ->
            DecodedAuthInfo = base64:decode_to_string(User),
             [Login, _Password] = string:tokens(DecodedAuthInfo, ":"),
            Context1 = cowmachine_req:set_resp_header(<<"user">>, Login, Context),
            {true, Context1};
		undefined ->
            {<<"Basic realm=\"cowmachine\"">>, Context}
    end.

content_types_provided(Context) ->
    Text = cowmachine_util:normalize_content_type(<<"text/plain">>),
    NewContext = cowmachine_req:set_resp_content_type(Text, Context),	
	{[Text], NewContext}.
 