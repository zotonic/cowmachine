-module(mainpage).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, Opts) ->
	{ok, Headers, Req2} = cowboy_req:read_part(Req),
	io:format("Headers =~p~n",
		[Headers]),
	{ok, Data, Req3} = cowboy_req:read_part_body(Req2),
	{file, <<"inputfile">>, Filename, ContentType}
		= cow_multipart:form_data(Headers),
	io:format("Received file ~p of content-type ~p as follow:~n~p~n~n",
		[Filename, ContentType, Data]),
	% Echo = io_lib:format("Received file ~p of content-type ~p as follow:~n~p~n~n",
	% 	[Filename, ContentType, Data]),
	% cowboy_req:reply(200, #{
	% 		<<"content-type">> => <<"text/plain; charset=utf-8">>
	% 	}, Echo, Req3),
	cowboy_req:reply(200, #{
		<<"content-type">> => ContentType
	}, Data, Req3),	
	{ok, Req3, Opts}.