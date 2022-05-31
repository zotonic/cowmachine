-module(controller).

-export([
    execute/2,
    process/4
	
]).

%% Cowmachine API

%% Middleware export, place this module in the request as controller.
execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

% Controller export
process(<<"GET">>, _ContentType, _Accepted, Context) ->
	
	ReqQs = cowmachine_req:req_qs(Context),
	%io:format("ReqQs = ~p~n",[ReqQs]),
	FindResult = lists:keyfind(<<"echo">>, 1, ReqQs),

	case FindResult of
		{_, Value} ->
			{Value, Context};
		false ->
			Context400 = cowmachine_req:set_response_code(400, Context),
			{<<"Missing echo parameter.">>, Context400}
	end;
process(_, _ContentType, _Accepted, Context) ->
	%% Method not allowed.
	Context405 = cowmachine_req:set_response_code(405, Context),
	{true, Context405}.