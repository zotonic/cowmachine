-module(mainpage).

-export([init/2]).


init(Req0, Opts) ->
	
	Status = 200,
	Headers = #{
		<<"content-type">> => <<"text/plain">>
	},
	Body = <<"Hello world!">>,
	
	%io:format("~nBody=~p~n", [Body]),
	
	% Send the response
	Req = cowboy_req:reply(Status, Headers, Body, Req0),
	{ok, Req, Opts}.