%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2007-2009 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc HTTP decision core for cowmachine

-module(cowmachine_decision_core).

-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Bryan Fink <bryan@basho.com>').
-author('Marc Worrell <marc@worrell.nl>').

-export([handle_request/2]).

-include("cowmachine_state.hrl").

handle_request(#cmstate{controller=Controller} = CmState, Context) ->
    try
        code:ensure_loaded(Controller),
        d(v3b13, CmState, Context)
    catch
        error:Error ->
            Stacktrace = erlang:get_stacktrace(),
            throw({stop_request, 500, {Error, Stacktrace}})
    end.

%% @doc Call the controller
-spec controller_call(atom(), #cmstate{}, term()) -> {term(), #cmstate{}, term()}.
controller_call(Callback, #cmstate{cache=Cache} = State, Context) ->
    case is_cacheable(Callback) of
        true ->
            case maps:find(Callback, Cache) of
                error -> 
                    {T, Context1} = cowmachine_controller:do(Callback, State, Context),
                    State1 = State#cmstate{cache = Cache#{Callback => T}},
                    {T, State1, Context1};
                {ok, Cached} -> 
                    {Cached, State, Context}
            end;
        false ->
            {T, Context1} = cowmachine_controller:do(Callback, State, Context),
            {T, State, Context1}
    end.

is_cacheable(charsets_provided) -> true;
is_cacheable(content_types_provided) -> true;
is_cacheable(content_encodings_provided) -> true;
is_cacheable(last_modified) -> true;
is_cacheable(generate_etag) -> true;
is_cacheable(_) -> false.


controller_call_process(ContentType, State, Context) ->
    {T, Context1} = cowmachine_controller:do_process(ContentType, State, Context),
    {T, State, Context1}.


d(DecisionID, State, Context) ->
    % webmachine_controller:log_d(Rs, DecisionID),
    decision(DecisionID, State, Context).

respond(Code, State, Context) ->
    {RsCode, RdCode} = case Code of
        200 ->
            case cowmachine_req:get_req_header(<<"te">>, Context) of
                undefined -> {State, Context};
                TEHdr -> choose_transfer_encoding(TEHdr, State, Context)
            end;
        Code when Code =:= 403; Code =:= 404; Code =:= 410 ->
            controller_call(finish_request, State, Context),
            throw({stop_request, Code});
        304 ->
            RdNoCT = cowmachine_req:remove_resp_header(<<"content-type">>, Context),
            {Etag, StateEt, RdEt0} = controller_call(generate_etag, State, RdNoCT),
            RdEt = case Etag of
                undefined -> RdEt0;
                ETag -> cowmachine_req:set_resp_header(<<"etag">>, cow_uri:urlencode(ETag), RdEt0)
            end,
            {Expires, StateExp, ExpCtx0} = controller_call(expires, StateEt, RdEt),
            ExpCtx = case Expires of
                undefined ->
                    ExpCtx0;
                Exp ->
                    cowmachine_req:set_resp_header(
                                    <<"expires">>,
                                    z_convert:to_binary(httpd_util:rfc1123_date(calendar:universal_time_to_local_time(Exp))),
                                    ExpCtx0)
            end,
            {StateExp, ExpCtx};
        _ -> 
            {State, Context}
    end,
    RdRespCode = cowmachine_req:set_response_code(Code, RdCode),
    controller_call(finish_request, RsCode, RdRespCode).

respond(Code, Headers, State, Context) ->
    ContextHs = cowmachine_req:set_resp_headers(Headers, Context),
    respond(Code, State, ContextHs).

error_response(Code, Reason, State, Context) ->
    controller_call(finish_request, State, Context),
    throw({stop_request, Code, Reason}).

error_response(Reason, State, Context) ->
    error_response(500, Reason, State, Context).

decision_test({Test, Rs, Rd}, TestVal, TrueFlow, FalseFlow) ->
    decision_test(Test, TestVal, TrueFlow, FalseFlow, Rs, Rd).


decision_test({error, Reason}, _Test, _TrueFlow, _FalseFlow, State, Context) ->
    error_response(Reason, State, Context);
decision_test({error, Reason0, Reason1}, _Test, _TrueFlow, _FalseFlow, State, Context) ->
    error_response({Reason0, Reason1}, State, Context);
decision_test({halt, Code}, _Test, _TrueFlow, _FalseFlow, State, Context) ->
    respond(Code, State, Context);
decision_test(Test, Test, TrueFlow, _FalseFlow, State, Context) ->
    decision_flow(TrueFlow, Test, State, Context);
decision_test(Test, _Test, _TrueFlow, FalseFlow, State, Context) ->
    decision_flow(FalseFlow, Test, State, Context).

decision_test_fn({Test, State, Context}, TestFn, TrueFlow, FalseFlow) ->
    decision_test_fn(Test, TestFn, TrueFlow, FalseFlow, State, Context).

decision_test_fn({error, Reason}, _TestFn, _TrueFlow, _FalseFlow, State, Context) ->
    error_response(Reason, State, Context);
decision_test_fn({error, R0, R1}, _TestFn, _TrueFlow, _FalseFlow, State, Context) ->
    error_response({R0, R1}, State, Context);
decision_test_fn({halt, Code}, _TestFn, _TrueFlow, _FalseFlow, State, Context) ->
    respond(Code, State, Context);
decision_test_fn(Test,TestFn,TrueFlow,FalseFlow, State, Context) ->
    case TestFn(Test) of
        true -> decision_flow(TrueFlow, Test, State, Context);
        false -> decision_flow(FalseFlow, Test, State, Context)
    end.
    
decision_flow(X, _TestResult, State, Context) when is_atom(X) -> 
    d(X, State, Context);
decision_flow(X, _TestResult, State, Context) when is_integer(X), X < 500 ->
    respond(X, State, Context);
decision_flow(X, TestResult, State, Context) when is_integer(X), X >= 500 ->
    error_response(X, TestResult, State, Context);
decision_flow({ErrCode, Reason}, _TestResult, State, Context) when is_integer(ErrCode) ->
    error_response(ErrCode, Reason, State, Context).


%% "Service Available"
decision(v3b13, State, Context) -> 
    decision_test(controller_call(service_available, State, Context), true, v3b12, 503);

%% "Known method?"
decision(v3b12, State, Context) ->
    {Methods, S1, C1} = controller_call(known_methods, State, Context),
    decision_test(lists:member(cowmachine_req:method(C1), Methods), true, v3b11, 501, S1, C1);

%% "URI too long?"
decision(v3b11, State, Context) ->
    decision_test(controller_call(uri_too_long, State, Context), true, 414, v3b10);

%% "Method allowed?"
decision(v3b10, State, Context) ->
    {Methods, S1, C1} = controller_call(allowed_methods, State, Context),
    case lists:member(cowmachine_req:method(C1), Methods) of
        true ->
            d(v3b9, S1, C1);
        false ->
            CtxAllow = cowmachine_req:set_resp_header(<<"allow">>, [z_convert:to_binary(M) || M <- Methods], C1),
            respond(405, S1, CtxAllow)
    end;

%% "Content-MD5 present?"
decision(v3b9, State, Context) ->
    ContentMD5 = cowmachine_req:get_req_header(<<"content-md5">>, Context),
    decision_test(ContentMD5, undefined, v3b9b, v3b9a, State, Context);

%% "Content-MD5 valid?"
decision(v3b9a, State, Context) ->
    {Md5Valid, S1, C1} = controller_call(validate_content_checksum, State, Context),
    case Md5Valid of
        {error, Reason} ->
            error_response(Reason, S1, C1);
        {halt, Code} ->
            respond(Code, S1, C1);
        not_validated ->
            Checksum = base64:decode(cowmachine_req:get_req_header(<<"content-md5">>, C1)),
            {Body, C2} = cowmachine_req:req_body(C1),
            BodyHash = crypto:hash(md5, Body),
            case BodyHash =:= Checksum of
                true -> d(v3b9b, S1, C2);
                _ ->
                    respond(400, S1, C2)
            end;
        false ->
            respond(400, S1, C1);
        _ ->
            d(v3b9b, S1, C1)
    end;

%% "Malformed?"
decision(v3b9b, State, Context) ->
    decision_test(controller_call(malformed_request, State, Context), true, 400, v3b8);

%% "Authorized?"
decision(v3b8, #cmstate{ options = Options } = State, Context) ->
    Context1 = case maps:get(on_welformed, Options, undefined) of
                    undefined -> Context;
                    Fun when is_function(Fun) -> Fun(Context)
               end,
    {IsAuthorized, S1, C1} = controller_call(is_authorized, State, Context1),
    case IsAuthorized of
        true -> 
            d(v3b7, S1, C1);
        {error, Reason} ->
            error_response(Reason, S1, C1);
        {halt, Code}  ->
            respond(Code, S1, C1);
        AuthHead ->
            CtxAuth = cowmachine_req:set_resp_header(<<"www-authenticate">>, AuthHead, C1),
            respond(401, S1, CtxAuth)
    end;

%% "Forbidden?"
decision(v3b7, State, Context) ->
    decision_test(controller_call(forbidden, State, Context), true, 403, v3b6_upgrade);

%% "Upgrade?"
decision(v3b6_upgrade, State, Context) ->
    case cowmachine_req:get_req_header(<<"upgrade">>, Context) of
        undefined ->
            decision(v3b6, State, Context);
        UpgradeHdr ->
            case cowmachine_req:get_req_header(<<"connection">>, Context) of
                undefined ->
                    decision(v3b6, State, Context);
                Connection ->
                    case contains_token(<<"upgrade">>, Connection) of
                        true ->
                            {Choosen, S1, C1} = choose_upgrade(UpgradeHdr, State, Context),
                            case Choosen of
                                none ->
                                    decision(v3b6, S1, C1);
                                {_Protocol, UpgradeFunc} ->
                                    {upgrade, UpgradeFunc, S1, C1}
                            end;
                        false ->
                            decision(v3b6, State, Context)
                    end
            end
    end;

%% "Okay Content-* Headers?"
decision(v3b6, State, Context) ->
    decision_test(controller_call(valid_content_headers, State, Context), true, v3b5, 501);

%% "Known Content-Type?"
decision(v3b5, State, Context) ->
    decision_test(controller_call(known_content_type, State, Context), true, v3b4, 415);

%% "Req Entity Too Large?"
decision(v3b4, State, Context) ->
    decision_test(controller_call(valid_entity_length, State, Context), true, v3b3, 413);

%% "OPTIONS?"
decision(v3b3, State, Context) ->
    case cowmachine_req:method(Context) of
        <<"OPTIONS">> ->
            {Hdrs, S1, C1} = controller_call(options, State, Context),
            respond(200, Hdrs, S1, C1);
        _ ->
            d(v3c3, State, Context)
    end;

%% Accept exists?
decision(v3c3, State, Context) ->
    case cowmachine_req:get_req_header(<<"accept">>, Context) of
        undefined ->
            % No accept header, select the first content-type provided
            {ContentTypes, S1, C1} = controller_call(content_types_provided, State, Context),
            MType = hd(ContentTypes),
            C2 = cowmachine_req:set_resp_content_type(MType, C1),
            d(v3d4, S1, C2);
        _ ->
            d(v3c4, State, Context)
    end;

%% Acceptable media type available? (check against Accept header)
decision(v3c4, State, Context) ->
    {ContentTypesProvided, S1, C1} = controller_call(content_types_provided, State, Context),
    AcceptHdr = cowmachine_req:get_req_header(<<"accept">>, C1),
    case cowmachine_util:choose_media_type_provided(ContentTypesProvided, AcceptHdr) of
        none ->
            respond(406, S1, C1);
        MType ->
            C2 = cowmachine_req:set_resp_content_type(MType, C1),
            d(v3d4, S1, C2)
    end;

%% Accept-Language exists?
decision(v3d4, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"accept-language">>, Context), undefined, v3e5, v3d5, State, Context);

%% Acceptable Language available? %% WMACH-46 (do this as proper conneg)
decision(v3d5, State, Context) ->
    decision_test(controller_call(language_available, State, Context), true, v3e5, 406);

%% Accept-Charset exists?
decision(v3e5, State, Context) ->
    case cowmachine_req:get_req_header(<<"accept-charset">>, Context) of
        undefined -> decision_test(choose_charset(<<"*">>, State, Context), none, 406, v3f6);
        _ -> d(v3e6, State, Context)
    end;

%% Acceptable Charset available?
decision(v3e6, State, Context) ->
    decision_test(
        choose_charset(cowmachine_req:get_req_header(<<"accept-charset">>, Context), State, Context),
        none, 406, v3f6);

%% Accept-Encoding exists?
% (also, set content-type header here, now that charset is chosen)
decision(v3f6, State, Context) ->
    CType = cowmachine_req:resp_content_type(Context),
    CSet = case cowmachine_req:resp_chosen_charset(Context) of
               undefined -> CType;
               CS -> <<CType/binary, "; charset=", CS/binary>>
           end,
    C1 = cowmachine_req:set_resp_header(<<"content-type">>, CSet, Context),
    case cowmachine_req:get_req_header(<<"accept-encoding">>, C1) of
        undefined ->
            decision_test(
                    choose_content_encoding(<<"identity;q=1.0,*;q=0.5">>, State, C1),
                    none, 406, v3g7);
        _ -> d(v3f7, State, C1)
    end;

%% Acceptable encoding available?
decision(v3f7, State, Context) ->
    decision_test(
            choose_content_encoding(cowmachine_req:get_req_header(<<"accept-encoding">>, Context), State, Context),
            none, 406, v3g7);

%% "Resource exists?"
decision(v3g7, State, Context) ->
    % this is the first place after all conneg, so set Vary here
    {Variances, S1, C1} = variances(State, Context),
    VarCtx = case Variances of
        [] -> C1;
        _ -> cowmachine_req:set_resp_header(<<"vary">>, [Variances], C1)
    end,
    decision_test(controller_call(resource_exists, S1, VarCtx), true, v3g8, v3h7);

%% "If-Match exists?"
decision(v3g8, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"if-match">>, Context), undefined, v3h10, v3g9, State, Context);

%% "If-Match: * exists"
decision(v3g9, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"if-match">>, Context), <<"*">>, v3h10, v3g11, State, Context);

%% "ETag in If-Match"
decision(v3g11, State, Context) ->
    ETags = cowmachine_util:split_quoted_strings(cowmachine_req:get_req_header(<<"if-match">>, Context)),
    decision_test_fn(controller_call(generate_etag, State, Context),
                     fun(ETag) -> lists:member(ETag, ETags) end,
                     v3h10, 412);

%% "If-Match: * exists"
decision(v3h7, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"if-match">>, Context), "*", 412, v3i7, State, Context);

%% "If-unmodified-since exists?"
decision(v3h10, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"if-unmodified-since">>, Context), undefined, v3i12, v3h11, State, Context);

%% "I-UM-S is valid date?"
decision(v3h11, State, Context) ->
    IUMSDate = cowmachine_req:get_req_header(<<"if-unmodified-since">>, Context),
    decision_test(cowmachine_util:convert_request_date(IUMSDate), bad_date, v3i12, v3h12, State, Context);

%% "Last-Modified > I-UM-S?"
decision(v3h12, State, Context) ->
    ReqDate = cowmachine_req:get_req_header(<<"if-unmodified-since">>, Context),
    ReqErlDate = cowmachine_util:convert_request_date(ReqDate),
    {ResErlDate, S1, C1} = controller_call(last_modified, State, Context),
    decision_test(ResErlDate > ReqErlDate, true, 412, v3i12, S1, C1);

%% "Moved permanently? (apply PUT to different URI)"
decision(v3i4, State, Context) ->
    {MovedPermanently, S1, C1} = controller_call(moved_permanently, State, Context),
    case MovedPermanently of
        {true, MovedURI} ->
            LocCtx = cowmachine_req:set_resp_header(<<"location">>, MovedURI, C1),
            respond(301, S1, LocCtx);
        false ->
            d(v3p3, S1, C1);
        {error, Reason} ->
            error_response(Reason, S1, C1);
        {halt, Code} ->
            respond(Code, S1, C1)
    end;

%% PUT?
decision(v3i7, State, Context) ->
    decision_test(cowmachine_req:method(Context), <<"PUT">>, v3i4, v3k7, State, Context);

%% "If-none-match exists?"
decision(v3i12, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"if-none-match">>, Context), undefined, v3l13, v3i13, State, Context);

%% "If-None-Match: * exists?"
decision(v3i13, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"if-none-match">>, Context), <<"*">>, v3j18, v3k13, State, Context);

%% GET or HEAD?
decision(v3j18, State, Context) ->
    decision_test(lists:member(cowmachine_req:method(Context),[<<"GET">>, <<"HEAD">>]), true, 304, 412, State, Context);

%% "Moved permanently?"
decision(v3k5, State, Context) ->
    {MovedPermanently, S1, C1} = controller_call(moved_permanently, State, Context),
    case MovedPermanently of
        {true, MovedURI} ->
            LocCtx = cowmachine_req:set_resp_header(<<"location">>, MovedURI, C1),
            respond(301, S1, LocCtx);
        false ->
            d(v3l5, S1, C1);
        {error, Reason} ->
            error_response(Reason, S1, C1);
        {halt, Code} ->
            respond(Code, S1, C1)
    end;

%% "Previously existed?"
decision(v3k7, State, Context) ->
    decision_test(controller_call(previously_existed, State, Context), true, v3k5, v3l7);

%% "Etag in if-none-match?"
decision(v3k13, State, Context) ->
    ETags = cowmachine_util:split_quoted_strings(cowmachine_req:get_req_header(<<"if-none-match">>, Context)),
    decision_test_fn(controller_call(generate_etag, State, Context),
                     %% Membership test is a little counter-intuitive here; if the
                     %% provided ETag is a member, we follow the error case out
                     %% via v3j18.
                     fun(ETag) -> lists:member(ETag, ETags) end,
                     v3j18, v3l13);

%% "Moved temporarily?"
decision(v3l5, State, Context) ->
    {MovedTemporarily, S1, C1} = controller_call(moved_temporarily, State, Context),
    case MovedTemporarily of
    {true, MovedURI} ->
        LocCtx = cowmachine_req:set_resp_header(<<"location">>, MovedURI, C1),
        respond(307, S1, LocCtx);
    false ->
        d(v3m5, S1, C1);
    {error, Reason} ->
        error_response(Reason, S1, C1);
    {halt, Code} ->
        respond(Code, S1, C1)
    end;

%% "POST?"
decision(v3l7, State, Context) ->
    decision_test(cowmachine_req:method(Context), <<"POST">>, v3m7, 404, State, Context);

%% "IMS exists?"
decision(v3l13, State, Context) ->
    decision_test(cowmachine_req:get_req_header(<<"if-modified-since">>, Context), undefined, v3m16, v3l14, State, Context);

%% "IMS is valid date?"
decision(v3l14, State, Context) -> 
    IMSDate = cowmachine_req:get_req_header(<<"if-modified-since">>, Context),
    decision_test(cowmachine_util:convert_request_date(IMSDate), bad_date, v3m16, v3l15, State, Context);

%% "IMS > Now?"
decision(v3l15, State, Context) ->
    NowDateTime = calendar:universal_time(),
    ReqDate = cowmachine_req:get_req_header(<<"if-modified-since">>, Context),
    ReqErlDate = cowmachine_util:convert_request_date(ReqDate),
    decision_test(ReqErlDate > NowDateTime, true, v3m16, v3l17, State, Context);

%% "Last-Modified > IMS?"
decision(v3l17, State, Context) ->
    ReqDate = cowmachine_req:get_req_header(<<"if-modified-since">>, Context),    
    ReqErlDate = cowmachine_util:convert_request_date(ReqDate),
    {ResErlDate, S1, C1} = controller_call(last_modified, State, Context),
    decision_test(ResErlDate =:= undefined orelse ResErlDate > ReqErlDate,
                  true, v3m16, 304, S1, C1);

%% "POST?"
decision(v3m5, State, Context) ->
    decision_test(cowmachine_req:method(Context), <<"POST">>, v3n5, 410, State, Context);

%% "Server allows POST to missing resource?"
decision(v3m7, State, Context) ->
    decision_test(controller_call(allow_missing_post, State, Context), true, v3n11, 404);

%% "DELETE?"
decision(v3m16, State, Context) ->
    decision_test(cowmachine_req:method(Context), <<"DELETE">>, v3m20, v3n16, State, Context);

%% DELETE enacted immediately?
%% Also where DELETE is forced.
decision(v3m20, State, Context) ->
    decision_test(controller_call(delete_resource, State, Context), true, v3m20b, 500);
decision(v3m20b, State, Context) ->
    decision_test(controller_call(delete_completed, State, Context), true, v3o20, 202);

%% "Server allows POST to missing resource?"
decision(v3n5, State, Context) ->
    decision_test(controller_call(allow_missing_post, State, Context), true, v3n11, 410);

%% "Redirect?"
decision(v3n11, State, Context) ->
    {PostIsCreate, S1, C1} = controller_call(post_is_create, State, Context),
    {Stage1, SStage1, CtxStage1} = case PostIsCreate of
        true ->
            {CreatePath, S2, C2} = controller_call(create_path, S1, C1),
            case CreatePath of
                NewPath when is_binary(NewPath) ->
                    {BaseUri0, S3, C3} = controller_call(base_uri, S2, C2),
                    NewPath1 = case NewPath of
                                    <<$/, Path/binary>> -> Path;
                                    _ -> NewPath
                               end,
                    PathCtx = cowmachine_req:set_disp_path(<<$/, NewPath1/binary>>, C3),
                    LocCtx = case cowmachine_req:get_resp_header(<<"location">>, PathCtx) of
                        undefined ->
                            BaseUri = case BaseUri0 of
                                            undefined -> cowmachine_req:base_uri(C3);
                                            _ -> BaseUri0
                                        end,
                            Loc = case binary:last(BaseUri) of
                                    $/ -> <<BaseUri/binary, NewPath1/binary>>;
                                    _ -> <<BaseUri/binary, $/, NewPath1/binary>>
                                  end,
                            cowmachine_req:set_resp_header(<<"location">>, Loc, PathCtx);
                        _ ->
                            PathCtx
                    end,
                    {Res, S4, C4} = accept_helper(S3, LocCtx),
                    case Res of
                        {halt, Code} -> respond(Code, S4, C4);
                        {error, _,_} -> error_response(Res, S4, C4);
                        {error, _} -> error_response(Res, S4, C4);
                        _ -> {stage1_ok, S4, C4}
                    end;
                undefined ->
                    error_response("post_is_create w/o create_path", S2, C2);
                _ ->
                    error_response("create_path not a string", S2, C2)
            end;
        _ ->
            {ProcessPost, S2, C2} = accept_helper(S1, C1),
            case ProcessPost of
                true -> {stage1_ok, S2, C2};
                {halt, Code} -> respond(Code, S2, C2);
                Err -> error_response(Err, S2, C2)
            end
    end,
    case Stage1 of
        stage1_ok ->
            case cowmachine_req:resp_redirect(CtxStage1) of
                true ->
                    case cowmachine_req:get_resp_header(<<"location">>, CtxStage1) of
                        undefined ->
                            respond(500, "Response had set_resp_redirect but no Location header", SStage1, CtxStage1);
                        _ ->
                            respond(303, SStage1, CtxStage1)
                    end;
                false ->
                    d(v3p11, SStage1, CtxStage1)
            end;
        _ ->
            {nop, SStage1, CtxStage1}
    end;

%% "POST?"
decision(v3n16, State, Context) ->
    decision_test(cowmachine_req:method(Context), <<"POST">>, v3n11, v3o16, State, Context);

%% Conflict? -- Only with PUT
decision(v3o14, State, Context) ->
    {IsConflict, S1, C1} = controller_call(is_conflict, State, Context),
    case IsConflict of
        true -> respond(409, S1, C1);
        _ ->
            {Res, SHelp, CHelp} = accept_helper(S1, C1),
            case Res of
                {halt, Code} -> respond(Code, SHelp, CHelp);
                {error, _, _} -> error_response(Res, SHelp, CHelp);
                {error, _} -> error_response(Res, SHelp, CHelp);
                _ -> d(v3p11, SHelp, CHelp)
            end
    end;

%% "PUT?"
decision(v3o16, State, Context) ->
    decision_test(cowmachine_req:method(Context), <<"PUT">>, v3o14, v3o18, State, Context);

%% Multiple representations?
% (also where body generation for GET and HEAD is done)
decision(v3o18, State, Context) ->
    {ProcessResult, SBody, CBody} = case is_GET_HEAD(Context) of
        true ->
            {S1, C1} = etag_etc_helper(State, Context),
            process_helper(undefined, S1, C1);
        false ->
            % If a response body was set then also set etag, modified, etc.
            case cowmachine_req:has_resp_body(Context) of
                true ->
                    {S1, C1} = etag_etc_helper(State, Context),
                    {nop, S1, C1};
                false ->
                    {nop, State, Context}
            end
    end,
    case ProcessResult of
        {error, _} -> error_response(ProcessResult, SBody, CBody);
        {error, _,_} -> error_response(ProcessResult, SBody, CBody);
        {halt, Code} -> respond(Code, SBody, CBody);
        _ -> d(v3o18b, SBody, CBody)
    end;

decision(v3o18b, State, Context) ->
    decision_test(controller_call(multiple_choices, State, Context), true, 300, 200);

%% Response includes an entity?
decision(v3o20, State, Context) ->
    decision_test(cowmachine_req:has_resp_body(Context), true, v3o18, 204, State, Context);

%% Conflict?
decision(v3p3, State, Context) ->
    {IsConflict, S1, C1} = controller_call(is_conflict, State, Context),
    case IsConflict of
        true -> respond(409, S1, C1);
        _ ->
            {Res, SHelp, CHelp} = accept_helper(S1, C1),
            case Res of
                {halt, Code} -> respond(Code, SHelp, CHelp);
                {error, _, _} -> error_response(Res, SHelp, CHelp);
                {error, _} -> error_response(Res, SHelp, CHelp);
                _ -> d(v3p11, SHelp, CHelp)
            end
    end;

%% New controller?  (at this point boils down to "has location header")
decision(v3p11, State, Context) ->
    case cowmachine_req:get_resp_header(<<"location">>, Context) of
        undefined -> d(v3o20, State, Context);
        _ -> respond(201, State, Context)
    end.


%% Check if method is get or head, they have body generation on last stage.
is_GET_HEAD(Context) ->
    case cowmachine_req:method(Context) of
        <<"GET">> -> true;
        <<"HEAD">> -> true;
        _ -> false
    end.

%% Check if the request content-type is acceptable - if acceptable then also call the
%% controller's process function.
accept_helper(State, Context) ->
     {M1, M2, MParams} = CTParsed = case cowmachine_req:get_req_header(<<"content-type">>, Context) of
         undefined ->
            {<<"application">>, <<"octet-stream">>, []};
         CTHeader ->
            cow_http_hd:parse_content_type(CTHeader)
     end,
    {ok, RdMParams} = cowmachine_req:set_metadata(mediaparams, MParams, Context),
    {ContentTypesAccepted, State1, Context1} = controller_call(content_types_accepted, State, RdMParams),
    case cowmachine_util:is_media_type_accepted(ContentTypesAccepted, CTParsed) of
        false ->
            {{halt, 415}, State1, Context1};
        true ->
            process_helper(<<M1/binary, $/, M2/binary>>, State1, Context1)
    end.

process_helper(ContentType, State, Context) ->
    {Res, S2, C2} = Result = controller_call_process(ContentType, State, Context),
    case Res of
        {halt, _} -> Result;
        {error, _, _} -> Result;
        {error, _} -> Result;
        true -> Result;
        RespBody ->
            C3 = cowmachine_req:set_resp_body(RespBody, C2),
            {body, S2, C3}
    end.

etag_etc_helper(State, Context) ->
    {Etag, SEtag, CEtag0} = controller_call(generate_etag, State, Context),
    CEtag = case Etag of
        undefined -> CEtag0;
        ETag -> cowmachine_req:set_resp_header(<<"etag">>, cowmachine_util:quoted_string(ETag), CEtag0)
    end,

    {LastModified, SLM, CLM0} = controller_call(last_modified, SEtag, CEtag),
    CLM = case LastModified of
        undefined -> CLM0;
        LM -> cowmachine_req:set_resp_header(<<"last-modified">>,
                    z_convert:to_binary(httpd_util:rfc1123_date(calendar:universal_time_to_local_time(LM))),
                    CLM0)
    end,

    {Expires, SExp, CExp0} = controller_call(expires, SLM, CLM),
    CExp = case Expires of
        undefined -> CExp0;
        Exp -> cowmachine_req:set_resp_header(<<"expires">>,
                    z_convert:to_binary(httpd_util:rfc1123_date(calendar:universal_time_to_local_time(Exp))),
                    CExp0)
    end,
    CIfRange = check_if_range(Etag, LastModified, CExp),
    {SExp, CIfRange}.


% Only called for 'GET' and 'HEAD' - check if 206 result is allowed
check_if_range(Etag, LastModified, Context) ->
    IsRangeOk = is_if_range_ok(cowmachine_req:get_req_header(<<"if-range">>, Context), Etag, LastModified),
    cowmachine_req:set_range_ok(IsRangeOk, Context).

is_if_range_ok(undefined, _ETag, _LM) ->
    true;
is_if_range_ok(<<"W/\"", _/binary>>, _ETag, _LM) ->
    false;
is_if_range_ok(<<"w/\"", _/binary>>, _ETag, _LM) ->
    false;
is_if_range_ok(<<$", _/binary>>, undefined, _LM) ->
    false;
is_if_range_ok(<<$", _/binary>> = IfETag, ETag, _LM) ->
    ETags = cowmachine_util:split_quoted_strings(IfETag),
    lists:member(ETag, ETags);
is_if_range_ok(Date, _ETag, LM) ->
    ErlDate = cowmachine_util:convert_request_date(Date),
    ErlDate =/= undefined andalso ErlDate >= LM.


choose_content_encoding(AccEncHdr, State, Context) ->
    {EncodingsProvided, Rs1, Rd1} = controller_call(content_encodings_provided, State, Context),
    case cowmachine_util:choose_encoding(EncodingsProvided, AccEncHdr) of
        none ->
            {none, Rs1, Rd1};
        ChosenEnc ->
            RdEnc = case ChosenEnc of
                        <<"identity">> -> Rd1;
                        _ -> cowmachine_req:set_resp_header(<<"content-encoding">>,ChosenEnc, Rd1)
                    end,
            RdEnc1 = cowmachine_req:set_resp_content_encoding(ChosenEnc,RdEnc),
            {ChosenEnc, Rs1, RdEnc1}
    end.

choose_transfer_encoding(AccEncHdr, State, Context) ->
    choose_transfer_encoding(cowmachine_req:version(Context), AccEncHdr, State, Context).

choose_transfer_encoding({1,0}, _AccEncHdr, State, Context) ->
    {State, Context};
choose_transfer_encoding({1,1}, AccEncHdr, State, Context) ->
    {EncodingsProvided, Rs1, Rd1} = controller_call(transfer_encodings_provided, State, Context),
    EncList = [ Enc || {Enc, _Func} <- EncodingsProvided ],
    case cowmachine_util:choose_encoding(EncList, AccEncHdr) of
        none ->
            {Rs1, Rd1};
        <<"identity">> ->
            {Rs1, Rd1};
        ChosenEnc ->
            Enc = lists:keyfind(ChosenEnc, 1, EncodingsProvided),
            RdEnc = cowmachine_req:set_resp_transfer_encoding(Enc,Rd1),
            {Rs1, RdEnc}
    end;
choose_transfer_encoding(_, _AccEncHdr, State, Context) ->
    {State, Context}.


choose_charset(AccCharHdr, State, Context) ->
    {CharsetsProvided, Rs1, Rd1} = controller_call(charsets_provided, State, Context),
    case CharsetsProvided of
        no_charset ->
            {no_charset, Rs1, Rd1};
        CL ->
            CSets = [maybe_old_tuple_value(CSet) || CSet <- CL],
            case cowmachine_util:choose_charset(CSets, AccCharHdr) of
                none ->
                    {none, Rs1, Rd1};
                Charset ->
                    RdCSet = cowmachine_req:set_resp_chosen_charset(Charset, Rd1),
                    {Charset, Rs1, RdCSet}
            end
    end.

maybe_old_tuple_value({A, _}) -> A;
maybe_old_tuple_value(A) -> A.

choose_upgrade(UpgradeHdr, State, Context) ->
    {UpgradesProvided, Rs1, Rd1} = controller_call(upgrades_provided, State, Context),
    Provided1 = [ {z_string:to_lower(Prot), Prot, PFun} || {Prot, PFun} <- UpgradesProvided],
    Requested = [ z_string:to_lower(z_string:trim(Up)) || Up <- binary:split(UpgradeHdr, <<",">>, [global]) ],
    {choose_upgrade1(Requested, Provided1), Rs1, Rd1}.

choose_upgrade1([], _) ->
    none;
choose_upgrade1([Req|Requested], Provided) ->
    case lists:keysearch(Req, 1, Provided) of
        false ->
            choose_upgrade1(Requested, Provided);
        {value, {_, Protocol, UpgradeFun}} ->
            {Protocol, UpgradeFun}
    end.


variances(State, Context) ->
    {ContentTypesProvided, Rs1, Rd1} = controller_call(content_types_provided, State, Context),
    Accept = case length(ContentTypesProvided) of
        1 -> [];
        0 -> [];
        _ -> [<<"accept">>]
    end,
    {EncodingsProvided, Rs2, Rd2} = controller_call(content_encodings_provided, Rs1, Rd1),
    AcceptEncoding = case length(EncodingsProvided) of
        1 -> [];
        0 -> [];
        _ -> [<<"accept-encoding">>]
    end,
    {CharsetsProvided, Rs3, Rd3} = controller_call(charsets_provided, Rs2, Rd2),
    AcceptCharset = case CharsetsProvided of
        no_charset -> 
            [];
        CP ->
            case length(CP) of
                1 -> [];
                0 -> [];
                _ -> [<<"accept-charset">>]
            end
    end,
    {Variances, Rs4, Rd4} = controller_call(variances, Rs3, Rd3),
    {Accept ++ AcceptEncoding ++ AcceptCharset ++ Variances, Rs4, Rd4}.


contains_token(Token, HeaderString) ->
    Tokens = lists:map(fun (T) ->
                            z_string:trim(z_string:to_lower(T))
                       end,
                       binary:split(HeaderString, <<",">>, [global])),
    lists:member(Token, Tokens).

