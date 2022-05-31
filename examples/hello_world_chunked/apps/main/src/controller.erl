-module(controller).

-export([
    execute/2,
    process/4
]).

%% Cowmachine API

%% Middleware export,  places this module in the request as controller.
execute(Req, Env) ->
    {ok, Req, Env#{ cowmachine_controller => ?MODULE } }.

% Controller export
process(<<"GET">>, _AcceptedCT, _ProvidedCT, Context) ->
    Req0 = cowmachine_req:req(Context),
    Req = cowboy_req:stream_reply(200, Req0),
	cowboy_req:stream_body("Hello\r\n", nofin, Req),
	timer:sleep(1000),
	cowboy_req:stream_body("World\r\n", nofin, Req),
	timer:sleep(1000),
	cowboy_req:stream_body("Chunked!\r\n", fin, Req),
	{{halt, 200}, Context}.
