%% @hidden

-module(cowmachine_server_cowboy_tests).

-include_lib("eunit/include/eunit.hrl").

-export([execute/2]).

-define(ANY6, {0, 0, 0, 0, 0, 0, 0, 0}).

%% A listener runs its middlewares on Cowboy, and reports each request to
%% the metrics callback with the user data set while it was served.
metrics_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    Self = self(),
    {ok, _} = cowmachine_server_cowboy:start_listener(cowboy_metrics, #{
        port => 0,
        middlewares => [?MODULE],
        env => #{ body => <<"metrics">> },
        request_timeout => 5000,
        metrics_callback => fun(Metrics) -> Self ! {metrics, Metrics} end,
        metrics_req_filter => fun(Req) -> maps:with([method], Req) end
    }),
    try
        Port = cowmachine_server_cowboy:port(cowboy_metrics),
        ?assertMatch({200, <<"metrics">>}, get(gen_tcp, {127, 0, 0, 1}, Port, [])),
        receive
            {metrics, Metrics} ->
                ?assertMatch(#{
                    resp_status := 200,
                    user_data := #{ site := test },
                    req := #{ method := <<"GET">> }
                }, Metrics),
                ?assertEqual([method], maps:keys(maps:get(req, Metrics)))
        after 5000 ->
            error(no_metrics)
        end
    after
        ok = cowmachine_server_cowboy:stop_listener(cowboy_metrics)
    end.

%% With `ipv6_v6only', an IPv4 and an IPv6 listener share a port.
ipv4_and_ipv6_share_a_port_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    {ok, _} = cowmachine_server_cowboy:start_listener(cowboy_v4, #{
        port => 0,
        ip => {0, 0, 0, 0},
        backlog => 64,
        max_connections => 100,
        num_acceptors => 2,
        middlewares => [?MODULE],
        env => #{ body => <<"v4">> }
    }),
    Port = cowmachine_server_cowboy:port(cowboy_v4),
    try
        {ok, _} = cowmachine_server_cowboy:start_listener(cowboy_v6, #{
            port => Port,
            ip => ?ANY6,
            ipv6_v6only => true,
            middlewares => [?MODULE],
            env => #{ body => <<"v6">> }
        }),
        try
            ?assertEqual({200, <<"v4">>}, get(gen_tcp, {127, 0, 0, 1}, Port, [])),
            ?assertEqual({200, <<"v6">>}, get(gen_tcp, {0, 0, 0, 0, 0, 0, 0, 1}, Port, []))
        after
            ok = cowmachine_server_cowboy:stop_listener(cowboy_v6)
        end
    after
        ok = cowmachine_server_cowboy:stop_listener(cowboy_v4)
    end.

%% The listener's `options' are Cowboy's own, and win over the shared keys.
options_win_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    {ok, _} = cowmachine_server_cowboy:start_listener(cowboy_options, #{
        port => 0,
        middlewares => [?MODULE],
        env => #{ body => <<"shared">> },
        options => #{ env => #{ body => <<"own">> } }
    }),
    try
        Port = cowmachine_server_cowboy:port(cowboy_options),
        ?assertEqual({200, <<"own">>}, get(gen_tcp, {127, 0, 0, 1}, Port, []))
    after
        ok = cowmachine_server_cowboy:stop_listener(cowboy_options)
    end.

%% A listener reports its state, its address and its connections.
info_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    {ok, _} = cowmachine_server_cowboy:start_listener(cowboy_info, #{
        port => 0,
        ip => {127, 0, 0, 1},
        max_connections => 10,
        middlewares => [?MODULE]
    }),
    try
        Port = cowmachine_server_cowboy:port(cowboy_info),
        ?assertEqual({ok, #{
            status => running,
            ip => {127, 0, 0, 1},
            port => Port,
            active_connections => 0,
            all_connections => 0,
            max_connections => 10
        }}, cowmachine_server_cowboy:info(cowboy_info)),
        %% An open connection is counted.
        {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 5000),
        ok = wait_for_connections(cowboy_info, 1),
        ok = gen_tcp:close(Socket)
    after
        ok = cowmachine_server_cowboy:stop_listener(cowboy_info)
    end,
    ?assertEqual({error, not_found}, cowmachine_server_cowboy:info(cowboy_gone)).

%% `cowmachine_cowreq''s own versions of Cowboy functions that would read the
%% body or send around the server answer as Cowboy's do.
cowboy_equivalence_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    {ok, _} = cowmachine_server_cowboy:start_listener(cowboy_equivalence, #{
        port => 0,
        middlewares => [?MODULE],
        env => #{ equivalence => true }
    }),
    try
        Port = cowmachine_server_cowboy:port(cowboy_equivalence),
        Cases = [
            {<<"POST">>, <<"urlencoded">>, <<"a=1&b">>},
            {<<"POST">>, <<"urlencoded">>, <<"%zz">>},
            {<<"POST">>, <<"match">>, <<"a=1">>},
            {<<"POST">>, <<"match">>, <<"b=1">>},
            {<<"GET">>, <<"events">>, <<>>},
            {<<"HEAD">>, <<"events">>, <<>>}
        ],
        [?assertEqual(
            request(Port, Method, <<"/cowboy/", Action/binary>>, Body),
            request(Port, Method, <<"/ours/", Action/binary>>, Body))
         || {Method, Action, Body} <- Cases]
    after
        ok = cowmachine_server_cowboy:stop_listener(cowboy_equivalence)
    end.

%% With `tls', the listener serves HTTPS.
tls_test() ->
    {ok, _} = application:ensure_all_started(cowmachine),
    {ok, _} = application:ensure_all_started(ssl),
    #{ server_config := ServerConfig, client_config := ClientConfig } = public_key:pkix_test_data(#{
        server_chain => #{ root => rsa(), intermediates => [], peer => rsa() },
        client_chain => #{ root => rsa(), intermediates => [], peer => rsa() }
    }),
    {ok, _} = cowmachine_server_cowboy:start_listener(cowboy_tls, #{
        port => 0,
        tls => ServerConfig,
        middlewares => [?MODULE],
        env => #{ body => <<"tls">> }
    }),
    try
        Port = cowmachine_server_cowboy:port(cowboy_tls),
        ?assertEqual({200, <<"tls">>}, get(ssl, {127, 0, 0, 1}, Port, [{server_name_indication, net_adm:localhost()} | ClientConfig]))
    after
        ok = cowmachine_server_cowboy:stop_listener(cowboy_tls)
    end.

%% The middleware. For `cowboy_equivalence_test', it does what the path names
%% with `cowboy_req' or `cowmachine_cowreq'; otherwise it replies with the
%% environment's body, with the metrics' user data set first.
execute(#{ path := Path } = Req, #{ equivalence := true }) ->
    [<<>>, Which, Action] = binary:split(Path, <<"/">>, [global]),
    Module = case Which of
        <<"cowboy">> -> cowboy_req;
        <<"ours">> -> cowmachine_cowreq
    end,
    {stop, equivalence(Module, Action, Req)};
execute(Req, #{ body := Body }) ->
    ok = cowmachine_cowreq:set_options(#{ metrics_user_data => #{ site => test } }, Req),
    {stop, cowmachine_cowreq:reply(200, #{}, Body, Req)}.

equivalence(Module, <<"events">>, Req0) ->
    Req = cowboy_req:stream_reply(200, #{ <<"content-type">> => <<"text/event-stream">> }, Req0),
    ok = Module:stream_events(#{ data => <<"one">> }, nofin, Req),
    ok = Module:stream_events([#{ data => <<"two">> }], fin, Req),
    Req;
equivalence(Module, Action, Req) ->
    try read(Module, Action, Req) of
        {ok, Result, Req1} ->
            cowboy_req:reply(200, #{}, io_lib:format("~0p", [Result]), Req1)
    catch
        exit:{request_error, Reason, _Message} ->
            cowboy_req:reply(400, #{}, io_lib:format("~0p", [Reason]), Req)
    end.

read(Module, <<"urlencoded">>, Req) -> Module:read_urlencoded_body(Req);
read(Module, <<"match">>, Req) -> Module:read_and_match_urlencoded_body([a], Req).

%% Send a request and read the whole response, without its `date' header.
request(Port, Method, Path, Body) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 5000),
    ok = gen_tcp:send(Socket, [
        Method, <<" ">>, Path, <<" HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n">>,
        <<"content-type: application/x-www-form-urlencoded\r\n">>,
        <<"content-length: ">>, integer_to_binary(byte_size(Body)), <<"\r\n\r\n">>, Body
    ]),
    Response = recv_all(gen_tcp, Socket, <<>>),
    ok = gen_tcp:close(Socket),
    re:replace(Response, <<"(?i)\r\ndate: [^\r]*">>, <<>>, [{return, binary}]).

wait_for_connections(Ref, N) ->
    case cowmachine_server_cowboy:info(Ref) of
        {ok, #{ active_connections := N, all_connections := N }} -> ok;
        {ok, _} -> wait_for_connections(Ref, N)
    end.

rsa() ->
    [{key, {rsa, 2048, 65537}}].

%% Send a `GET /' and read the whole response, which the server ends by
%% closing the connection. Returns its status and body.
get(Transport, IP, Port, Opts) ->
    {ok, Socket} = Transport:connect(IP, Port, [binary, {active, false} | Opts], 5000),
    ok = Transport:send(Socket, <<"GET / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n">>),
    Response = recv_all(Transport, Socket, <<>>),
    ok = Transport:close(Socket),
    [<<"HTTP/1.1 ", Code:3/binary, _/binary>>, Body] = binary:split(Response, <<"\r\n\r\n">>),
    {binary_to_integer(Code), Body}.

recv_all(Transport, Socket, Acc) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} -> recv_all(Transport, Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.
