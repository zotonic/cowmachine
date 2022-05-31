-module(ws_h).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	% Starts a timer. When the timer expires, the message {timeout, TimerRef, Msg} 
	% is sent to the process identified by Dest.
	erlang:start_timer(2000, self(), <<"Hello!">>),
	{[], State}.

websocket_handle({text, Msg}, State) ->
	{[{text, << "That's what you said! ", Msg/binary >>}], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(2000, self(), <<"How are you doing?">>),
	{[{text, Msg}], State};
websocket_info(_Info, State) ->
	{[], State}.