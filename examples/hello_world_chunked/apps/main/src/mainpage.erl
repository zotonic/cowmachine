-module(mainpage).

-export([init/2]).

init(Req0, Opts) ->
	Req = cowboy_req:stream_reply(200, Req0),
	cowboy_req:stream_body("Hello\r\n", nofin, Req),
	timer:sleep(1000),
	cowboy_req:stream_body("World\r\n", nofin, Req),
	timer:sleep(1000),
	cowboy_req:stream_body("Chunked!\r\n", fin, Req),
	{ok, Req, Opts}.