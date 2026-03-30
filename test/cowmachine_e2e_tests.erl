%% @hidden

-module(cowmachine_e2e_tests).

-include_lib("eunit/include/eunit.hrl").

%% Run with: rebar3 eunit -v -m cowmachine_e2e_tests
%%
%% This module acts as both a Cowboy middleware and a Cowmachine controller.
%% Per-test controller behaviour is configured via the application env key
%% 'test_opts' in the 'cowmachine' application (a map).
%%
%% The tests follow the decision flow documented in
%% docs/http-headers-status-cowmachine.dot, exercising each branch.

-export([
    execute/2,
    process/4,
    service_available/1,
    uri_too_long/1,
    allowed_methods/1,
    malformed_request/1,
    is_authorized/1,
    forbidden/1,
    known_content_type/1,
    valid_entity_length/1,
    resource_exists/1,
    content_types_provided/1,
    generate_etag/1,
    last_modified/1,
    moved_permanently/1,
    moved_temporarily/1,
    previously_existed/1,
    delete_completed/1,
    options/1
]).

%%
%% Tests — follow the top-to-bottom order of the HTTP decision flow.
%%

%% -- Happy path -------------------------------------------------------

%% Basic GET returns 200 with body
get_ok_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{}),
    Port = start_listener(e2e_get_ok),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _, "Hello World"}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_get_ok).

%% HEAD returns 200 with an empty body (stripped by the HTTP layer)
head_ok_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{}),
    Port = start_listener(e2e_head_ok),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _, ""}},
        httpc:request(head, {url(Port), []}, [], [])),
    cowboy:stop_listener(e2e_head_ok).

%% -- service_available (v3b13) ----------------------------------------

%% service_available = false  →  503 Service Unavailable
service_unavailable_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{service_available => false}),
    Port = start_listener(e2e_service_unavailable),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 503, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_service_unavailable).

%% -- uri_too_long (v3b11) ---------------------------------------------

%% uri_too_long = true  →  414 Request-URI Too Long
uri_too_long_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{uri_too_long => true}),
    Port = start_listener(e2e_uri_too_long),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 414, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_uri_too_long).

%% -- allowed_methods (v3b10) ------------------------------------------

%% POST to a resource that only allows GET/HEAD  →  405 + Allow header
method_not_allowed_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{}),
    Port = start_listener(e2e_method_not_allowed),
    {ok, {{"HTTP/1.1", 405, _}, Headers, _}} =
        httpc:request(post, {url(Port), [], "text/plain", "body"}, [], []),
    ?assert(lists:keymember("allow", 1, Headers)),
    cowboy:stop_listener(e2e_method_not_allowed).

%% -- malformed_request (v3b9b) ----------------------------------------

%% malformed_request = true  →  400 Bad Request
malformed_request_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{malformed_request => true}),
    Port = start_listener(e2e_malformed_request),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 400, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_malformed_request).

%% -- is_authorized (v3b8) --------------------------------------------

%% is_authorized = false  →  401 + www-authenticate header
unauthorized_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{is_authorized => false}),
    Port = start_listener(e2e_unauthorized),
    {ok, {{"HTTP/1.1", 401, _}, Headers, _}} = httpc:request(url(Port)),
    ?assert(lists:keymember("www-authenticate", 1, Headers)),
    cowboy:stop_listener(e2e_unauthorized).

%% is_authorized returns a binary challenge  →  401 with that challenge
unauthorized_with_challenge_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{is_authorized => <<"Bearer realm=\"api\"">>}),
    Port = start_listener(e2e_unauthorized_challenge),
    {ok, {{"HTTP/1.1", 401, _}, Headers, _}} = httpc:request(url(Port)),
    {_, Challenge} = lists:keyfind("www-authenticate", 1, Headers),
    ?assertEqual("Bearer realm=\"api\"", Challenge),
    cowboy:stop_listener(e2e_unauthorized_challenge).

%% -- forbidden (v3b7) -------------------------------------------------

%% forbidden = true  →  403 Forbidden
forbidden_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{forbidden => true}),
    Port = start_listener(e2e_forbidden),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 403, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_forbidden).

%% -- known_content_type (v3b5) ----------------------------------------

%% known_content_type = false  →  415 Unsupported Media Type
unsupported_media_type_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{known_content_type => false}),
    Port = start_listener(e2e_unsupported_media_type),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 415, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_unsupported_media_type).

%% -- valid_entity_length (v3b4) ---------------------------------------

%% valid_entity_length = false  →  413 Request Entity Too Large
request_entity_too_large_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{valid_entity_length => false}),
    Port = start_listener(e2e_entity_too_large),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 413, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_entity_too_large).

%% -- OPTIONS (v3b3) ---------------------------------------------------

%% OPTIONS request  →  200 with custom headers from options/1
options_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{
        allowed_methods => [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],
        options => [{<<"x-custom-option">>, <<"enabled">>}]
    }),
    Port = start_listener(e2e_options),
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, _}} =
        httpc:request(options, {url(Port), []}, [], []),
    ?assert(lists:keymember("x-custom-option", 1, Headers)),
    cowboy:stop_listener(e2e_options).

%% -- content negotiation (v3c3/v3c4) ----------------------------------

%% Accept header matches content_types_provided  →  200 with matching Content-Type
accept_negotiation_ok_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{content_types_provided => [{<<"text">>, <<"html">>, []}]}),
    Port = start_listener(e2e_accept_ok),
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, _}} =
        httpc:request(get, {url(Port), [{"accept", "text/html"}]}, [], []),
    {_, CT} = lists:keyfind("content-type", 1, Headers),
    ?assert(lists:prefix("text/html", CT)),
    cowboy:stop_listener(e2e_accept_ok).

%% Accept header does not match content_types_provided  →  406 Not Acceptable
accept_negotiation_not_acceptable_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{content_types_provided => [{<<"text">>, <<"plain">>, []}]}),
    Port = start_listener(e2e_not_acceptable),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 406, _}, _, _}},
        httpc:request(
            get, {url(Port), [{"accept", "application/json"}]}, [], [])),
    cowboy:stop_listener(e2e_not_acceptable).

%% -- resource_exists (v3g7) -------------------------------------------

%% resource_exists = false, not previously existed  →  404 Not Found
resource_not_found_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{resource_exists => false}),
    Port = start_listener(e2e_not_found),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 404, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_not_found).

%% -- previously_existed / moved_permanently / moved_temporarily -------

%% resource_exists = false, previously_existed = true, not moved  →  410 Gone
previously_existed_gone_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{resource_exists => false, previously_existed => true}),
    Port = start_listener(e2e_gone),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 410, _}, _, _}},
        httpc:request(url(Port))),
    cowboy:stop_listener(e2e_gone).

%% moved_permanently  →  301 with Location header
moved_permanently_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{
        resource_exists    => false,
        previously_existed => true,
        moved_permanently  => {true, <<"http://example.com/new">>}
    }),
    Port = start_listener(e2e_moved_permanently),
    {ok, {{"HTTP/1.1", 301, _}, Headers, _}} =
        httpc:request(get, {url(Port), []}, [{autoredirect, false}], []),
    {_, Location} = lists:keyfind("location", 1, Headers),
    ?assertEqual("http://example.com/new", Location),
    cowboy:stop_listener(e2e_moved_permanently).

%% moved_temporarily  →  307 with Location header
moved_temporarily_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{
        resource_exists    => false,
        previously_existed => true,
        moved_temporarily  => {true, <<"http://example.com/temp">>}
    }),
    Port = start_listener(e2e_moved_temporarily),
    {ok, {{"HTTP/1.1", 307, _}, Headers, _}} =
        httpc:request(get, {url(Port), []}, [{autoredirect, false}], []),
    {_, Location} = lists:keyfind("location", 1, Headers),
    ?assertEqual("http://example.com/temp", Location),
    cowboy:stop_listener(e2e_moved_temporarily).

%% -- ETag / caching (v3g11, v3k13, v3l17, etag_etc_helper) -----------

%% generate_etag set  →  ETag header present in 200 response
etag_in_response_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{generate_etag => <<"abc123">>}),
    Port = start_listener(e2e_etag),
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, _}} = httpc:request(url(Port)),
    ?assert(lists:keymember("etag", 1, Headers)),
    cowboy:stop_listener(e2e_etag).

%% last_modified set  →  Last-Modified header present in 200 response
last_modified_in_response_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{last_modified => {{2020, 6, 15}, {12, 0, 0}}}),
    Port = start_listener(e2e_last_modified),
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, _}} = httpc:request(url(Port)),
    ?assert(lists:keymember("last-modified", 1, Headers)),
    cowboy:stop_listener(e2e_last_modified).

%% If-None-Match: "etag"  matches generate_etag  →  304 Not Modified (v3k13)
if_none_match_not_modified_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{generate_etag => <<"abc123">>}),
    Port = start_listener(e2e_if_none_match),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 304, _}, _, _}},
        httpc:request(
            get, {url(Port), [{"if-none-match", "\"abc123\""}]}, [], [])),
    cowboy:stop_listener(e2e_if_none_match).

%% If-None-Match: *  with an existing resource and GET  →  304 (v3i13 / v3j18)
if_none_match_star_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{}),
    Port = start_listener(e2e_if_none_match_star),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 304, _}, _, _}},
        httpc:request(
            get, {url(Port), [{"if-none-match", "*"}]}, [], [])),
    cowboy:stop_listener(e2e_if_none_match_star).

%% If-Modified-Since after last_modified date  →  304 Not Modified (v3l17)
if_modified_since_not_modified_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    %% Resource was last modified in 2000; client says "only send if newer than 2022".
    set_opts(#{last_modified => {{2000, 1, 1}, {0, 0, 0}}}),
    Port = start_listener(e2e_if_modified_since),
    %% Jan 1 2022 is a Saturday.
    ImsDate = "Sat, 01 Jan 2022 12:00:00 GMT",
    ?assertMatch(
        {ok, {{"HTTP/1.1", 304, _}, _, _}},
        httpc:request(
            get, {url(Port), [{"if-modified-since", ImsDate}]}, [], [])),
    cowboy:stop_listener(e2e_if_modified_since).

%% -- DELETE (v3m16 / v3m20 / v3m20b) ---------------------------------

%% DELETE  →  204 No Content (delete_completed = true, no body)
delete_no_content_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{allowed_methods => [<<"GET">>, <<"HEAD">>, <<"DELETE">>]}),
    Port = start_listener(e2e_delete_no_content),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 204, _}, _, _}},
        httpc:request(delete, {url(Port), []}, [], [])),
    cowboy:stop_listener(e2e_delete_no_content).

%% DELETE with delete_completed = false  →  202 Accepted (async deletion)
delete_accepted_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{
        allowed_methods  => [<<"GET">>, <<"HEAD">>, <<"DELETE">>],
        delete_completed => false
    }),
    Port = start_listener(e2e_delete_accepted),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 202, _}, _, _}},
        httpc:request(delete, {url(Port), []}, [], [])),
    cowboy:stop_listener(e2e_delete_accepted).

%% -- POST (v3n16 / v3n11) --------------------------------------------

%% POST to existing resource  →  200 with echoed body
post_to_resource_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    set_opts(#{allowed_methods => [<<"GET">>, <<"HEAD">>, <<"POST">>]}),
    Port = start_listener(e2e_post),
    ?assertMatch(
        {ok, {{"HTTP/1.1", 200, "OK"}, _, "hello post"}},
        httpc:request(
            post, {url(Port), [], "text/plain", "hello post"}, [], [])),
    cowboy:stop_listener(e2e_post).

%%
%% Controller implementation
%%

cowboy_opts() ->
    #{middlewares => [?MODULE, cowmachine]}.

%% Cowboy middleware — places this module in the request as controller.
execute(Req, Env) ->
    {ok, Req, Env#{cowmachine_controller => ?MODULE}}.

%% Unified request processor (replaces webmachine's delete_resource,
%% process_post, and named content-type handler functions).
process(<<"GET">>,    _AcceptedCT, _ProvidedCT, Context) ->
    {cfg(response_body, <<"Hello World">>), Context};
process(<<"HEAD">>,   _AcceptedCT, _ProvidedCT, Context) ->
    {cfg(response_body, <<"Hello World">>), Context};
process(<<"POST">>,   _AcceptedCT, _ProvidedCT, Context) ->
    {Body, Context1} = cowmachine_req:req_body(Context),
    {Body, Context1};
process(<<"DELETE">>, _AcceptedCT, _ProvidedCT, Context) ->
    {true, Context};
process(_,            _AcceptedCT, _ProvidedCT, Context) ->
    {{halt, 405}, Context}.

%% Controller callbacks — each reads its value from the test options map,
%% falling back to the same default that cowmachine_controller uses.
service_available(Context)     -> {cfg(service_available,     true),                     Context}.
uri_too_long(Context)          -> {cfg(uri_too_long,          false),                    Context}.
allowed_methods(Context)       -> {cfg(allowed_methods,       [<<"GET">>, <<"HEAD">>]),   Context}.
malformed_request(Context)     -> {cfg(malformed_request,     false),                    Context}.
is_authorized(Context)         -> {cfg(is_authorized,         true),                     Context}.
forbidden(Context)             -> {cfg(forbidden,             false),                    Context}.
known_content_type(Context)    -> {cfg(known_content_type,    true),                     Context}.
valid_entity_length(Context)   -> {cfg(valid_entity_length,   true),                     Context}.
resource_exists(Context)       -> {cfg(resource_exists,       true),                     Context}.
content_types_provided(Context)-> {cfg(content_types_provided,[{<<"text">>,<<"html">>,[]}]),Context}.
generate_etag(Context)         -> {cfg(generate_etag,         undefined),                Context}.
last_modified(Context)         -> {cfg(last_modified,         undefined),                Context}.
moved_permanently(Context)     -> {cfg(moved_permanently,     false),                    Context}.
moved_temporarily(Context)     -> {cfg(moved_temporarily,     false),                    Context}.
previously_existed(Context)    -> {cfg(previously_existed,    false),                    Context}.
delete_completed(Context)      -> {cfg(delete_completed,      true),                     Context}.
options(Context)               -> {cfg(options,               []),                       Context}.

%%
%% Helpers
%%

set_opts(Opts) ->
    application:set_env(cowmachine, test_opts, Opts).

cfg(Key, Default) ->
    maps:get(Key, application:get_env(cowmachine, test_opts, #{}), Default).

url(Port) ->
    "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/".

start_listener(Name) ->
    {ok, _} = cowboy:start_clear(Name, [{port, 0}], cowboy_opts()),
    ranch:get_port(Name).
