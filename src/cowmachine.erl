%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2022 Marc Worrell
%%
%% @doc Cowmachine: webmachine middleware for Cowboy/Zotonic
%% @end

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

%% @private
-export_type([cmstate/0]).

%% @doc Cowboy middleware, route the new request. Continue with the cowmachine,
%%      requests a redirect or return a 400 on an unknown host.
-spec execute(Req, Env) -> Result
    when Req :: cowboy_req:req(),
         Env :: cowboy_middleware:env(),
		 Result :: {ok, Req, Env} | {stop, Req}.
execute(Req, #{ cowmachine_controller := _Controller } = Env) ->
    ContextEnv = maps:get(cowmachine_context, Env, undefined),
    Context = cowmachine_req:init_context(Req, Env, ContextEnv),
    request(Context, #{}).


%% @doc Handle a request, executes the cowmachine http states. Can be used by middleware
%% functions to add some additional initialization of controllers or context.

-spec request(Context, Options) -> Result
    when Context :: cowmachine_req:context(),
         Options :: map(),
		 Req :: cowboy_req:req(),
         Env :: cowboy_middleware:env(),
		 Result :: {ok, Req, Env} | {stop, Req}.
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

-spec request_1(Controller, Req, Env, Options, Context) -> Result when
	Controller :: atom(),
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env(),
	Options :: map(),
	Context :: cowmachine_req:context(),
	Result :: {ok, Req, Env} | {stop, Req} | {upgrade, UpgradeFun, State, Context},
	UpgradeFun :: atom(),
	State :: cmstate().
request_1(Controller, Req, Env, Options, Context) ->
    State = #cmstate{
        controller = Controller,
        cache = #{},
        options = Options
    },
    try
        handle(State, Req, Env, Options, Context)
    catch
        % Process control exits must never be turned into an error page.
        % This mirrors what cowboy_stream_h:request_process/3 does.
        exit:Reason:Stack when Reason =:= normal; Reason =:= shutdown ->
            erlang:raise(exit, Reason, Stack);
        exit:Reason={shutdown, _}:Stack ->
            erlang:raise(exit, Reason, Stack);
        Class:Reason:Stack ->
            stop_request(Class, Reason, Stack, State, Req, Env, Context)
    end.

%% @doc Run the request through the decision core and send the response.
-spec handle(State, Req, Env, Options, Context) -> Result when
    State :: cmstate(),
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env(),
    Options :: map(),
    Context :: cowmachine_req:context(),
    Result :: {ok, Req, Env} | {stop, Req} | {upgrade, UpgradeFun, State, Context},
    UpgradeFun :: atom().
handle(State, Req, Env, Options, Context) ->
    EnvInit = cowmachine_req:init_env(Req, Env),
    Context1 = cowmachine_req:set_env(EnvInit, Context),
    Context2 = case maps:get(on_request, Options, undefined) of
        undefined -> Context1;
        FunReq when is_function(FunReq) -> FunReq(Context1)
    end,
    case cowmachine_decision_core:handle_request(State, Context2) of
        {upgrade, UpgradeFun, StateResult, ContextResult} ->
            {upgrade, UpgradeFun, StateResult, ContextResult};
        {_Finish, _StateResult, ContextResult} ->
            ContextResult1 = case maps:get(on_handled, Options, undefined) of
                undefined -> ContextResult;
                FunHandled when is_function(FunHandled) -> FunHandled(ContextResult)
            end,
            cowmachine_response:send_response(ContextResult1)
    end.

%% @doc Map a failure in the request process to a response. Failures that
%% map to 4xx and 500 are handled by the error controller, other codes
%% are sent as a bare response.
-spec stop_request(Class, Reason, Stack, State, Req, Env, Context) -> Result when
    Class :: error | exit | throw,
    Reason :: term(),
    Stack :: list(),
    State :: cmstate(),
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env(),
    Context :: cowmachine_req:context(),
    Result :: {ok, Req, Env} | {stop, Req}.
stop_request(Class, Reason, Stack, State, Req, Env, Context) ->
    {Code, Level, Text} = classify(Class, Reason),
    log_failure(Level, Code, Text, Class, Reason, Stack, Req),
    case use_error_controller(Code) of
        true ->
            ErrorReason = error_reason(Class, Reason, Stack),
            handle_stop_request(Code, ErrorReason, Req, Env, State, Context);
        false ->
            {stop, cowboy_req:reply(Code, Req)}
    end.

%% @doc Classify a failure as a response code, a log level (`undefined' for
%% no logging) and a log text. Cowboy signals request errors using `exit', see
%% `cowboy_req:parse_header/3' and `cowboy_req:read_body/2'.
-spec classify(Class, Reason) -> {Code, Level, Text} when
    Class :: error | exit | throw,
    Reason :: term(),
    Code :: integer(),
    Level :: undefined | warning | error,
    Text :: binary().
classify(throw, {stop_request, Code}) when is_integer(Code) ->
    {Code, stop_level(Code), <<"Stop request">>};
classify(throw, {stop_request, Code, _Reason}) when is_integer(Code) ->
    {Code, stop_level(Code), <<"Stop request">>};
classify(throw, invalid_percent_encoding) ->
    {400, warning, <<"Illegal percent encoding">>};
classify(throw, invalid_qs_name) ->
    {400, warning, <<"Illegal query argument name">>};
classify(throw, too_many_qs_names) ->
    {400, warning, <<"Too many query arguments">>};
classify(exit, {request_error, timeout, _}) ->
    {408, warning, <<"Request timeout">>};
classify(exit, {request_error, payload_too_large, _}) ->
    {413, warning, <<"Request body too large">>};
classify(exit, {request_error, _, _}) ->
    {400, warning, <<"Bad request">>};
classify(exit, timeout) ->
    % Raised by cowboy_req:read_body/2 if the body is not received in time.
    {408, warning, <<"Request timeout">>};
classify(throw, _) ->
    {500, error, <<"Unexpected throw">>};
classify(_Class, _Reason) ->
    {500, error, <<"Unexpected exception">>}.

stop_level(500) -> error;
stop_level(_) -> undefined.

use_error_controller(500) -> true;
use_error_controller(Code) -> Code >= 400 andalso Code < 500.

%% @doc The reason passed to the error controller. Explicit stop requests keep
%% the old format. Other failures are tagged with their real class.
error_reason(throw, {stop_request, _Code}, _Stack) ->
    undefined;
error_reason(throw, {stop_request, _Code, Reason}, _Stack) ->
    {throw, Reason};
error_reason(Class, Reason, Stack) ->
    {Class, {Reason, Stack}}.

log_failure(undefined, _Code, _Text, _Class, _Reason, _Stack, _Req) ->
    ok;
log_failure(Level, Code, Text, Class, Reason, Stack, Req) ->
    Report = #{ at => ?AT, level => Level, code => Code, text => Text,
                class => Class, reason => Reason },
    log(case Level of
            error -> Report#{ stack => Stack };
            _ -> Report
        end, Req).


% @todo add the error controller as an application env, if not defined then just terminate with the corresponding error code.

-spec handle_stop_request(ResponseCode, Reason, Req, Env, State, Context) -> Result when
	ResponseCode :: integer(),
	Reason :: any(),
	Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env(),
	State :: cmstate(),
	Context :: cowmachine_req:context(),
	Result :: {ok, Req, Env} | {stop, Req}.
handle_stop_request(ResponseCode, Reason, Req, Env, State, Context) ->
    try
        State1 = State#cmstate{
            controller = controller_http_error
        },
        EnvInit = cowmachine_req:init_error_env(Req, Env),
        Context1 = cowmachine_req:set_env(EnvInit, Context),
        Context2 = cowmachine_req:set_metadata(controller_module_error, State#cmstate.controller, Context1),
        Context3 = cowmachine_req:set_metadata(http_status_code, ResponseCode, Context2),
        Context4 = cowmachine_req:set_metadata(error_reason, Reason, Context3),
        {_Finish, _StateResult, ContextResult} = cowmachine_decision_core:handle_request(State1, Context4),
        ContextRespCode = cowmachine_req:set_response_code(ResponseCode, ContextResult),
        cowmachine_response:send_response(ContextRespCode)
    catch
        exit:CReason:CStack when CReason =:= normal; CReason =:= shutdown ->
            erlang:raise(exit, CReason, CStack);
        exit:CReason={shutdown, _}:CStack ->
            erlang:raise(exit, CReason, CStack);
        CClass:CReason:CStack ->
            % The error controller failed, fall back to a bare response. Use
            % the code of the original failure, unless the error controller
            % explicitly asked for another code.
            {_, _, Text} = classify(CClass, CReason),
            FallbackCode = fallback_code(CClass, CReason, ResponseCode),
            log(#{ at => ?AT, level => warning,
                   text => Text,
                   code => FallbackCode,
                   class => CClass,
                   reason => CReason,
                   stack => CStack
                 }, Req),
            {stop, cowboy_req:reply(FallbackCode, Req)}
    end.

fallback_code(throw, {stop_request, Code}, _Default) when is_integer(Code) -> Code;
fallback_code(throw, {stop_request, Code, _}, _Default) when is_integer(Code) -> Code;
fallback_code(_Class, _Reason, Default) -> Default.


%%
%% Logging
%%
-spec log(Report) -> Result when
	Report :: map(),
	Result :: any().
log(#{ level := Level } = Report) ->
    log_report(Level, Report#{
        in => cowmachine,
        node => node()
    }).

-spec log(Report, Req) -> Result when
	Report :: map(),
	Req :: map(),
	Result :: any().
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

-spec log_report(LogLevel, Report) -> Result when
	LogLevel :: debug | info | notice | warning | error,
	Report :: map(),
	Result :: any().
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

-spec src(IpInfo) -> Result when
	IpInfo :: #{ peer := {IP, Port} } | any(),
	Port :: integer(),
	IP :: tuple(),
	Result :: {ok, map()} | undefined.
src(#{ peer := {IP, Port} }) -> {ok, ip_info(IP, Port)};
src(_) -> undefined.

-spec dst(DstInfo) -> Result when
	DstInfo :: #{ sock := {IP, Port} } | #{ port := Port } | any(),
	IP :: tuple(), 
	Port :: integer(),
	Result :: {ok, #{IPType => string(), port => Port}} | {ok, #{ port => Port }} | undefined,
	IPType :: ip4 | ip6.
dst(#{ sock := {IP, Port} } ) -> {ok, ip_info(IP, Port)};
dst(#{ port := Port }) -> {ok, #{ port => Port }};
dst(_) -> undefined.

-spec path(PathInfo) -> Result when
	PathInfo :: #{ path := Path } | any(),
	Path :: any(),
	Result :: {ok, Path} | undefined.
path(#{ path := Path }) -> {ok, Path};
path(_) -> undefined.

-spec ip_info(IP, Port) -> Result when
	IP :: tuple(), 
	Port :: integer(),
	IPType :: ip4 | ip6,
	Result :: #{IPType => string(), port => Port}.
ip_info(IP, Port) ->
    IPType = case tuple_size(IP) of 4 -> ip4; 8 -> ip6 end,
    #{IPType => inet_parse:ntoa(IP), port => Port}.
