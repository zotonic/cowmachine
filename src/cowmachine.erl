%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2022 Marc Worrell
%%
%% @doc Cowmachine: webmachine middleware for Cowboy/Zotonic

%% Copyright 2016-2022 Marc Worrell
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
    request/2
]).

%% Internal logging interface
-export([log/1, log/2]).

-include_lib("kernel/include/logger.hrl").
-include("cowmachine_state.hrl").
-include("cowmachine_log.hrl").

%% @doc Cowboy middleware, route the new request. Continue with the cowmachine,
%%      requests a redirect or return a 400 on an unknown host.
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req :: cowboy_req:req(),
         Env :: cowboy_middleware:env().
execute(Req, #{ cowmachine_controller := _Controller } = Env) ->
    ContextEnv = maps:get(cowmachine_context, Env, undefined),
    Context = cowmachine_req:init_context(Req, Env, ContextEnv),
    request(Context, #{}).


%% @doc Handle a request, executes the cowmachine http states. Can be used by middleware
%% functions to add some additional initialization of controllers or context.
-spec request(Context, Options::map()) -> {ok, Req, Env} | {stop, Req}
    when Context :: cowmachine_req:context(),
         Req :: cowboy_req:req(),
         Env :: cowboy_middleware:env().
request(Context, Options) ->
    Req = cowmachine_req:req(Context),
    Env = cowmachine_req:env(Context),
    Controller = maps:get(cowmachine_controller, Env),
    case request_1(Controller, Req, Env, Options, Context) of
        {upgrade, UpgradeFun, _StateResult, ContextResult} ->
            Controller:UpgradeFun(ContextResult);
        Other ->
            Other
    end.

request_1(Controller, Req, Env, Options, Context) ->
    State = #cmstate{
        controller = Controller,
        cache = #{},
        options = Options
    },
    Site = maps:get(site, Env, undefined),
    try
        EnvInit = cowmachine_req:init_env(Req, Env),
        Context1 = cowmachine_req:set_env(EnvInit, Context),
        case cowmachine_decision_core:handle_request(State, Context1) of
            {_Finish, _StateResult, ContextResult} ->
                ContextResult1 = case maps:get(on_handled, Options, undefined) of
                    undefined -> ContextResult;
                    Fun when is_function(Fun) -> Fun(ContextResult)
                end,
                cowmachine_response:send_response(ContextResult1);
            {upgrade, UpgradeFun, _StateResult, ContextResult} ->
                {upgrade, UpgradeFun, _StateResult, ContextResult}
        end
    catch
        throw:{stop_request, 500, {Reason, Stacktrace}} when is_list(Stacktrace) ->
            log(#{ at => ?AT, level => error, code => 500, text => "Stop request",
                   reason => Reason, stack => Stacktrace}, Req),
            handle_stop_request(500, Site, undefined, Req, Env, State, Context);
        throw:{stop_request, 500, Reason} ->
            log(#{ at => ?AT, level => error, code => 500, text => "Stop request", reason => Reason}, Req),
            handle_stop_request(500, Site, {throw, Reason}, Req, Env, State, Context);
        throw:{stop_request, ResponseCode, Reason} when is_integer(ResponseCode), ResponseCode >= 400, ResponseCode < 500 ->
            handle_stop_request(ResponseCode, Site, {throw, Reason}, Req, Env, State, Context);
        throw:{stop_request, 500}:Stacktrace ->
            log(#{ at => ?AT, level => error, code => 500, text => "Stop request",
                   stack => Stacktrace}, Req),
            handle_stop_request(500, Site, undefined, Req, Env, State, Context);
        throw:{stop_request, ResponseCode} when is_integer(ResponseCode), ResponseCode >= 400, ResponseCode < 500 ->
            handle_stop_request(ResponseCode, Site, undefined, Req, Env, State, Context);
        throw:{stop_request, ResponseCode} when is_integer(ResponseCode) ->
            {stop, cowboy_req:reply(ResponseCode, Req)};
        throw:invalid_percent_encoding ->
            log(#{ at => ?AT, level => error, code => 400, text => "Illegal percent encoding" }, Req),
            {stop, cowboy_req:reply(400, Req)};
        throw:invalid_qs_name ->
            log(#{ at => ?AT, level => error, code => 400, text => "Illegal query argument name" }, Req),
            {stop, cowboy_req:reply(400, Req)};
        throw:Reason:Stacktrace ->
            log(#{ at => ?AT, level => error, code => 500, text => "Unexpected throw",
                   class => throw, reason => Reason,
                   stack => Stacktrace}, Req),
            handle_stop_request(500, Site, {throw, {Reason, Stacktrace}}, Req, Env, State, Context);
        Class:Reason:Stacktrace ->
            log(#{ at => ?AT, level => error, code => 500, text => "Unexpected exception",
                   class => Class, reason => Reason,
                   stack => Stacktrace}, Req),
            handle_stop_request(500, Site, {throw, {Reason, Stacktrace}}, Req, Env, State, Context)
    end.

% @todo add the error controller as an application env, if not defined then just terminate with the corresponding error code.
handle_stop_request(ResponseCode, _Site, Reason, Req, Env, State, Context) ->
    State1 = State#cmstate{
        controller = controller_http_error
    },
    EnvInit = cowmachine_req:init_env(Req, Env),
    Context1 = cowmachine_req:set_env(EnvInit, Context),
    Context2 = cowmachine_req:set_metadata(controller_module_error, State#cmstate.controller, Context1),
    Context3 = cowmachine_req:set_metadata(http_status_code, ResponseCode, Context2),
    Context4 = cowmachine_req:set_metadata(error_reason, Reason, Context3),
    try
        {_Finish, _StateResult, ContextResult} = cowmachine_decision_core:handle_request(State1, Context4),
        ContextRespCode = cowmachine_req:set_response_code(ResponseCode, ContextResult),
        cowmachine_response:send_response(ContextRespCode)
    catch
        throw:{stop_request, Code, CReason} ->
            log(#{ at => ?AT, level => warning,
                   text => "Stop request",
                   code => Code,
                   reason => CReason }, Req),
            {stop, cowboy_req:reply(Code, Req)};
        Class:CReason:Stacktrace->
            log(#{ at => ?AT, level => warning,
                   text => "Unexpected exception",
                   code => 500,
                   class => Class,
                   reason => CReason,
                   stack => Stacktrace
                 }, Req),
            {stop, cowboy_req:reply(500, Req)}
    end.


%%
%% Logging
%%

log(#{ level := Level } = Report) ->
    log_report(Level, Report#{
        in => cowmachine,
        node => node()
    }).

log(#{ level := Level } = Report, Req) when is_map(Req) ->
    Report1 = lists:foldl(fun({Key, Fun}, Acc) ->
                                  case Fun(Req) of
                                      undefined -> Acc;
                                      {ok, Val} -> Acc#{ Key => Val }
                                  end
                          end, Report, [{src, fun src/1},
                                        {dst, fun dst/1},
                                        {path, fun path/1}]),
    log_report(Level, Report1#{
        in => cowmachine,
        node => node()
    }).

log_report(debug, Report) when is_map(Report) ->
    ?LOG_DEBUG(Report);
log_report(info, Report) when is_map(Report) ->
    ?LOG_INFO(Report);
log_report(notice, Report) when is_map(Report) ->
    ?LOG_NOTICE(Report);
log_report(warning, Report) when is_map(Report) ->
    ?LOG_WARNING(Report);
log_report(error, Report) when is_map(Report) ->
    ?LOG_ERROR(Report).

src(#{ peer := {IP, Port} }) -> {ok, ip_info(IP, Port)};
src(_) -> undefined.

dst(#{ sock := {IP, Port} } ) -> {ok, ip_info(IP, Port)};
dst(#{ port := Port }) -> {ok, #{ port => Port }};
dst(_) -> undefined.

path(#{ path := Path }) -> {ok, Path};
path(_) -> undefined.

ip_info(IP, Port) ->
    IPType = case tuple_size(IP) of 4 -> ip4; 8 -> ip6 end,
    #{IPType => inet_parse:ntoa(IP), port => Port}.
