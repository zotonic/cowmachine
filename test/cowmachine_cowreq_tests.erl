%% @hidden

-module(cowmachine_cowreq_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M, cowmachine_cowreq).

%%
%% Reading and setting the map: cowboy_req's own functions
%%

reads_the_request_test() ->
    Req = cowboy_req(),
    ?assertEqual(<<"GET">>, ?M:method(Req)),
    ?assertEqual('HTTP/1.1', ?M:version(Req)),
    ?assertEqual(<<"http">>, ?M:scheme(Req)),
    ?assertEqual(<<"example.com">>, ?M:host(Req)),
    ?assertEqual(undefined, ?M:host_info(Req)),
    ?assertEqual(8080, ?M:port(Req)),
    ?assertEqual(<<"/a/b">>, ?M:path(Req)),
    ?assertEqual(undefined, ?M:path_info(Req)),
    ?assertEqual(<<"x=1&f">>, ?M:qs(Req)),
    ?assertEqual(<<"http://example.com:8080/a/b?x=1&f">>, iolist_to_binary(?M:uri(Req))),
    ?assertEqual(<<"//example.com:8080/a/b">>, iolist_to_binary(?M:uri(Req, #{ scheme => undefined, qs => undefined }))),
    ?assertEqual({{127, 0, 0, 1}, 5000}, ?M:peer(Req)),
    ?assertEqual({{127, 0, 0, 1}, 8080}, ?M:sock(Req)),
    ?assertEqual(undefined, ?M:cert(Req)),
    ?assertEqual(<<"text/html">>, ?M:header(<<"accept">>, Req)),
    ?assertEqual(none, ?M:header(<<"x-none">>, Req, none)),
    ?assertEqual(maps:get(headers, Req), ?M:headers(Req)),
    ?assertEqual([{<<"text">>, <<"html">>, []}], [{T, S, P} || {{T, S, P}, _, _} <- ?M:parse_header(<<"accept">>, Req)]),
    ?assertEqual(none, ?M:parse_header(<<"content-type">>, Req, none)),
    ?assertEqual([{<<"x">>, <<"1">>}, {<<"f">>, true}], ?M:parse_qs(Req)),
    ?assertEqual(#{ x => <<"1">> }, ?M:match_qs([x], Req)),
    ?assertEqual([{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}], ?M:parse_cookies(Req)),
    ?assertEqual([{<<"a">>, <<"1">>}], ?M:parse_cookies(?M:filter_cookies([<<"a">>], Req))),
    ?assertEqual(#{ a => <<"1">> }, ?M:match_cookies([a], Req)),
    ?assertEqual(<<"7">>, ?M:binding(id, Req)),
    ?assertEqual(none, ?M:binding(other, Req, none)),
    ?assertEqual(#{ id => <<"7">> }, ?M:bindings(Req)),
    ?assertEqual(false, ?M:has_body(Req)),
    ?assertEqual(0, ?M:body_length(Req)).

sets_the_response_test() ->
    Req0 = cowboy_req(),
    Req1 = ?M:set_resp_header(<<"x-a">>, <<"1">>, Req0),
    Req2 = ?M:set_resp_headers(#{ <<"x-b">> => <<"2">> }, Req1),
    Req3 = ?M:set_resp_cookie(<<"c">>, <<"v">>, Req2),
    Req4 = ?M:set_resp_cookie(<<"d">>, <<"w">>, Req3, #{ path => <<"/">> }),
    Req5 = ?M:set_resp_body(<<"body">>, ?M:delete_resp_header(<<"x-b">>, Req4)),
    ?assertEqual(<<"1">>, ?M:resp_header(<<"x-a">>, Req5)),
    ?assertEqual(none, ?M:resp_header(<<"x-b">>, Req5, none)),
    ?assertEqual(#{ <<"x-a">> => <<"1">> }, ?M:resp_headers(Req5)),
    ?assert(?M:has_resp_header(<<"x-a">>, Req5)),
    ?assert(?M:has_resp_body(Req5)),
    ?assertEqual([<<"c">>, <<"d">>], lists:sort(maps:keys(maps:get(resp_cookies, Req5)))).

%%
%% The connection: the request's server
%%

reply_test() ->
    Req = req(),
    ?assertMatch(#{ has_sent_resp := true },
                 ?M:reply(200, #{ <<"a">> => <<"b">> }, [<<"x">>, "y"], Req)),
    ?assertEqual({reply, 200, #{ <<"a">> => <<"b">> }, <<"xy">>}, call()).

reply_with_resp_body_test() ->
    %% As cowboy_req:reply/2,3: the body set on the request, or none.
    _ = ?M:reply(404, ?M:set_resp_body(<<"gone">>, req())),
    ?assertEqual({reply, 404, #{}, <<"gone">>}, call()),
    _ = ?M:reply(200, #{ <<"x">> => <<"1">> }, ?M:set_resp_body(<<"ok">>, req())),
    ?assertEqual({reply, 200, #{ <<"x">> => <<"1">> }, <<"ok">>}, call()),
    _ = ?M:reply(204, req()),
    ?assertEqual({reply, 204, #{}, <<>>}, call()).

inform_test() ->
    ok = ?M:inform(103, req()),
    ?assertEqual({inform, 103, #{}}, call()),
    ok = ?M:inform(103, #{ <<"link">> => <<"</a.css>">> }, req()),
    ?assertEqual({inform, 103, #{ <<"link">> => <<"</a.css>">> }}, call()).

stream_test() ->
    Req = ?M:stream_reply(200, req()),
    ?assertEqual({stream_reply, 200, #{}}, call()),
    _ = ?M:stream_reply(200, #{ <<"x">> => <<"1">> }, req()),
    ?assertEqual({stream_reply, 200, #{ <<"x">> => <<"1">> }}, call()),
    ok = ?M:stream_body(<<"part">>, nofin, Req),
    ?assertEqual({stream_body, <<"part">>, nofin}, call()),
    ok = ?M:stream_events(#{ data => <<"one">> }, nofin, Req),
    ?assertEqual({stream_body, <<"data: one\n\n">>, nofin}, call()),
    ok = ?M:stream_events([#{ data => <<"two">> }], nofin, Req),
    ?assertEqual({stream_body, <<"data: two\n\n">>, nofin}, call()),
    ok = ?M:stream_trailers(#{ <<"x-sum">> => <<"1">> }, Req),
    ?assertEqual({stream_trailers, #{ <<"x-sum">> => <<"1">> }}, call()).

read_body_test() ->
    Req = req(#{ read_body => {ok, <<"body">>} }),
    ?assertEqual({ok, <<"body">>, Req}, ?M:read_body(Req)),
    ?assertEqual({read_body, #{}}, call()),
    ?assertEqual({ok, <<"body">>, Req}, ?M:read_body(Req, #{ length => 10 })),
    ?assertEqual({read_body, #{ length => 10 }}, call()).

read_urlencoded_body_test() ->
    Req = req(#{ read_body => {ok, <<"a=1&b">>} }),
    ?assertEqual({ok, [{<<"a">>, <<"1">>}, {<<"b">>, true}], Req}, ?M:read_urlencoded_body(Req)),
    ?assertEqual({read_body, #{ length => 64000, period => 5000 }}, call()),
    ?assertExit({request_error, urlencoded_body, _},
                ?M:read_urlencoded_body(req(#{ read_body => {ok, <<"%zz">>} }))),
    %% A body still coming in when the read ends: short of the length is a
    %% timeout, at the length it is too large.
    ?assertExit({request_error, timeout, _},
                ?M:read_urlencoded_body(req(#{ read_body => {more, <<"a=">>} }))),
    ?assertExit({request_error, payload_too_large, _},
                ?M:read_urlencoded_body(req(#{ read_body => {more, <<"a=1">>} }), #{ length => 3 })),
    ?assertExit({request_error, timeout, _},
                ?M:read_urlencoded_body(req(#{ read_body => {more, <<"a=">>} }), #{})).

read_and_match_urlencoded_body_test() ->
    Req = req(#{ read_body => {ok, <<"a=1">>} }),
    ?assertMatch({ok, #{ a := <<"1">> }, _}, ?M:read_and_match_urlencoded_body([a], Req)),
    ?assertExit({request_error, {read_and_match_urlencoded_body, _}, _},
                ?M:read_and_match_urlencoded_body([a, b], Req, #{})).

read_part_test() ->
    Req = req(#{ read_part => {ok, #{ <<"x">> => <<"1">> }}, read_part_body => {more, <<"p">>} }),
    ?assertEqual({ok, #{ <<"x">> => <<"1">> }, Req}, ?M:read_part(Req)),
    ?assertEqual({read_part, #{ length => 64000, period => 5000 }}, call()),
    ?assertEqual({ok, #{ <<"x">> => <<"1">> }, Req}, ?M:read_part(Req, #{})),
    ?assertEqual({read_part, #{}}, call()),
    ?assertEqual({more, <<"p">>, Req}, ?M:read_part_body(Req)),
    ?assertEqual({read_part_body, #{}}, call()),
    ?assertEqual({more, <<"p">>, Req}, ?M:read_part_body(Req, #{ length => 1 })),
    ?assertEqual({read_part_body, #{ length => 1 }}, call()),
    ?assertEqual({done, req(#{ read_part => done })}, ?M:read_part(req(#{ read_part => done }))).

set_options_test() ->
    ok = ?M:set_options(#{ idle_timeout => infinity }, req()),
    ?assertEqual({set_options, #{ idle_timeout => infinity }}, call()).

%%
%% Helpers
%%

%% A request map as Cowboy builds it.
cowboy_req() ->
    #{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"http">>,
        host => <<"example.com">>,
        port => 8080,
        path => <<"/a/b">>,
        qs => <<"x=1&f">>,
        peer => {{127, 0, 0, 1}, 5000},
        sock => {{127, 0, 0, 1}, 8080},
        cert => undefined,
        headers => #{
            <<"accept">> => <<"text/html">>,
            <<"cookie">> => <<"a=1; b=2">>
        },
        host_info => undefined,
        path_info => undefined,
        bindings => #{ id => <<"7">> },
        has_body => false,
        body_length => 0
    }.

req() ->
    req(#{}).

req(Results) ->
    flush(),
    #{ cowmachine_server => cowmachine_fake_server, owner => self(), fake_results => Results }.

flush() ->
    receive
        {cowmachine_fake_server, _} -> flush()
    after 0 ->
        ok
    end.

call() ->
    receive
        {cowmachine_fake_server, Call} -> Call
    after 1000 ->
        error(no_call)
    end.
