%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%%
%% @doc Cowmachine: webmachine middleware for Cowboy/Zotonic

%% Copyright 2016 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(cowmachine).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(cowboy_middleware).

-export([
    execute/2,
    request/5
]).

-include("cowmachine_state.hrl").

%% @doc Cowboy middleware, route the new request. Continue with the cowmachine,
%%      requests a redirect or return a 400 on an unknown host.
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, #{controller=Controller, controller_options=ControllerOpts} = Env) ->
    Context = maps:get(context, Env, Req),
    request(Controller, ControllerOpts, Req, Env, Context1).


%% @doc Handle a request, executes the cowmachine http states. Can be used by middleware
%% functions to add some additional initialization of controllers or context.
-spec request(atom(), list(), Req, Env, term()) -> {ok, Req, Env} | {stop, Req}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
request(Controller, ControllerOpts, Req, Env, Context) ->
    State = #cmstate{
        env=Env,
        controller=Controller,
        controller_options=ControllerOpts,
        cache = #{}
    },
    Site = maps:get(site, Env, undefined),
    try
        Context1 = cowmachine_req:set_req(cowmachine_req:init_req(Req, Env), Context),
        case cowmachine_decision_core:handle_request(State, Context1) of
            {_Finish, _StateResult, ContextResult} ->
                cowmachine_response:send_response(ContextResult, Env);
            {upgrade, UpgradeFun, _StateResult, ContextResult} ->
                Controller:UpgradeFun(ContextResult)
        end
    catch
        throw:{stop_request, 500, Reason} ->
            lager:error("[~p] stop_request ~p (reason ~p)", [Site, 500, Reason]),
            handle_stop_request(500, Site, Reason, Req, State, Context);
        throw:{stop_request, ResponseCode, Reason} when is_integer(ResponseCode), ResponseCode >= 400, ResponseCode < 500 ->
            handle_stop_request(ResponseCode, Site, Reason, Req, State, Context);
        throw:{stop_request, 500} ->
            StackTrace = erlang:get_stacktrace(),
            lager:error("[~p] stop_request ~p (stacktrace ~p)", [Site, 500, StackTrace]),
            handle_stop_request(500, Site, undefined, Req, State, Context);
        throw:{stop_request, ResponseCode} when is_integer(ResponseCode), ResponseCode >= 400, ResponseCode < 500 ->
            handle_stop_request(ResponseCode, Site, undefined, Req, State, Context);
        throw:{stop_request, ResponseCode} when is_integer(ResponseCode) ->
            {stop, {ResponseCode, Req}};
        throw:Error ->
            Reason = {error, {throw, Error, erlang:get_stacktrace()}},
            handle_stop_request(500, Site, Reason, Req, State, Context);
        Type:Error ->
            Stacktrace = erlang:get_stacktrace(),
            lager:warning("[~p] Error ~p:~p in ~p", [Site, Type, Error, Stacktrace]),
            {stop, {500, Req}}
    end.

handle_stop_request(ResponseCode, Site, Reason, Req, State, Context) ->
    Bindings = [
        {error_reason, Reason},
        {http_status_code, ResponseCode}
    ],
    State1 = State#cmstate{
        controller = controller_http_error,
        controller_options = []
    },
    Req1 = Req#{bindings => Bindings},
    Context1 = cowmachine_req:set_req(cowmachine_req:init_req(Req1), Context),
    try
        {_Finish, _StateResult, ContextResult} = cowmachine_decision_core:handle_request(State1, Context1),
        cowmachine_response:send_response(ContextResult, State#cmstate.env)
    catch
        throw:{stop_request, Code, Reason} ->
            lager:warning("[~p] Error ~p (reason ~p)", [Site, Code, Reason]),
            {stop, {Code, Req}};
        Type:Error ->
            Stacktrace = erlang:get_stacktrace(),
            lager:warning("[~p] Error ~p:~p in ~p", [Site, Type, Error, Stacktrace]),
            {stop, {500, Req}}
    end.

