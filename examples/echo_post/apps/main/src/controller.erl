-module(controller).

-export([
    execute/2,
    process/4,
	allowed_methods/1,
    content_types_provided/1	
]).

%% Cowmachine API

%% Middleware export, place this module in the request as controller.
execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

% Controller export
process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
	HasBody = cowmachine_req:has_req_body(Context),
	maybe_echo(HasBody, Context);
process(_, _, _, Context) ->
	%% Method not allowed.
	{{halt, 405}, Context}.

allowed_methods(Context) -> 
	{[ 
		<<"GET">>, <<"HEAD">>, <<"POST">>
	 ],	Context}.

content_types_provided(Context) -> 
	Text = cowmachine_util:normalize_content_type(<<"text/plain">>),
	{[Text], Context}.	

	
%% internal functions	
	
maybe_echo(true, Context) ->
	ReqBody = cowmachine_req:req_body(Context),
	ReqData = element(1,ReqBody),
	ParseResult = cowmachine_util:parse_qs(ReqData),
	FindResult = lists:keyfind(<<"echo">>, 1, ParseResult),
	case FindResult of
		{_, Value} ->
			{Value, Context};
		false ->
			RespBodyContext = cowmachine_req:set_resp_body("Missing echo parameter.",Context),
			
			{{halt, 400}, RespBodyContext}
			
	end;

maybe_echo(false, Context) ->
	RespBodyContext = cowmachine_req:set_resp_body("Missing body!",Context),
	{{halt, 400}, RespBodyContext}.
