-module(mainpage).
-behaviour(cowboy_handler).
-export([init/2]).
-export([info/3]).

init(Req0, Opts) ->
	Req = cowboy_req:stream_reply(200, #{
		<<"content-type">> => <<"text/event-stream">>
	}, Req0),
	HtmlEntity = getEmoji(),
	erlang:send_after(500, self(), {message, HtmlEntity}),

	{cowboy_loop, Req, Opts}.

info({message, Msg}, Req, State) ->
	NewState = case State of
		[] -> [0];
		List -> [hd(List)+1]
	end,
	Event = #{
		id => id(),
		data => Msg
	},
	cowboy_req:stream_events(Event, nofin, Req),
	
	HtmlEntity = getEmoji(),
	erlang:send_after(500, self(), {message, HtmlEntity}),
	% io:format("Id=~p, State = ~B~n",[maps:get(id,Event), hd(NewState)]),
	{ok, Req, NewState}.

id() ->
	integer_to_list(erlang:unique_integer([positive, monotonic]), 16).

getEmoji() ->
	HtmlEntityList = emoji:all(),
	HtmlEntityListSize = length(HtmlEntityList),
	HtmlEntityId = rand:uniform(HtmlEntityListSize),
	HtmlEntity = lists:nth(HtmlEntityId, HtmlEntityList),
	HtmlEntity.