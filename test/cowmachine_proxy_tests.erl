%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_proxy_tests).

-include_lib("eunit/include/eunit.hrl").

cowmachine_metadata_test() ->
    Req = #{
        peer => {{127,0,0,1},1234},
        scheme => <<"http">>,
        port => 8000,
        host => <<"local.dev">>,
        headers => #{
        }
    },
    #{
        cowmachine_proxy := false,
        cowmachine_forwarded_host := <<"local.dev">>,
        cowmachine_forwarded_port := 8000,
        cowmachine_forwarded_proto := <<"http">>,
        cowmachine_remote_ip := {127,0,0,1},
        cowmachine_remote := <<"127.0.0.1">>
    } = cowmachine_proxy:update_env(Req, #{}),
    ok.


cowmachine_forwarded_test() ->
    Req = #{
        peer => {{127,0,0,1},1234},
        scheme => <<"http">>,
        port => 8000,
        headers => #{
            <<"host">> => <<"local.dev">>,
            <<"forwarded">> => <<"for=192.0.2.60; Proto=HTTPS; by=203.0.113.43; host=\"example.com\"">>
        }
    },
    #{
        cowmachine_proxy := true,
        cowmachine_forwarded_host := <<"example.com">>,
        cowmachine_forwarded_port := 443,
        cowmachine_forwarded_proto := <<"https">>,
        cowmachine_remote_ip := {192,0,2,60},
        cowmachine_remote := <<"192.0.2.60">>
    } = cowmachine_proxy:update_env(Req, #{}),
    ok.

cowmachine_forwarded_ipv6_test() ->
    Req = #{
        peer => {{127,0,0,1},1234},
        scheme => <<"http">>,
        port => 8000,
        headers => #{
            <<"host">> => <<"local.dev">>,
            <<"forwarded">> => <<"for=\"[2001:db8:cafe::17]:4711\"; Proto=HTTPS; by=203.0.113.43; host=\"example.com\"">>
        }
    },
    #{
        cowmachine_proxy := true,
        cowmachine_forwarded_host := <<"example.com">>,
        cowmachine_forwarded_port := 443,
        cowmachine_forwarded_proto := <<"https">>,
        cowmachine_remote_ip := {16#2001,16#db8,16#cafe,0,0,0,0,16#17},
        cowmachine_remote := <<"2001:db8:cafe::17">>
    } = cowmachine_proxy:update_env(Req, #{}),
    ok.

cowmachine_forwarded_host_default_test() ->
    Req = #{
        peer => {{127,0,0,1},1234},
        scheme => <<"http">>,
        port => 8000,
        headers => #{
            <<"host">> => <<"local.dev">>,
            <<"forwarded">> => <<"for=192.0.2.60; proto=https;port=8443;by=203.0.113.43">>
        }
    },
    #{
        cowmachine_proxy := true,
        cowmachine_forwarded_host := <<"local.dev">>,
        cowmachine_forwarded_port := 8443,
        cowmachine_forwarded_proto := <<"https">>,
        cowmachine_remote_ip := {192,0,2,60},
        cowmachine_remote := <<"192.0.2.60">>
    } = cowmachine_proxy:update_env(Req, #{}),
    ok.

cowmachine_x_forwarded_test() ->
    Req = #{
        peer => {{127,0,0,1},1234},
        scheme => <<"http">>,
        port => 8000,
        host => <<"local.dev">>,
        headers => #{
            <<"x-forwarded-for">> => <<"192.0.2.60">>,
            <<"x-forwarded-host">> => <<"example.com">>,
            <<"x-forwarded-proto">> => <<"https">>,
            <<"x-forwarded-port">> => <<"8443">>
        }
    },
    #{
        cowmachine_proxy := true,
        cowmachine_forwarded_host := <<"example.com">>,
        cowmachine_forwarded_port := 8443,
        cowmachine_forwarded_proto := <<"https">>,
        cowmachine_remote_ip := {192,0,2,60},
        cowmachine_remote := <<"192.0.2.60">>
    } = cowmachine_proxy:update_env(Req, #{}),
    ok.

cowmachine_forwarded_untrusted_test() ->
    Req = #{
        peer => {{1,2,3,4},1234},
        scheme => <<"http">>,
        port => 8000,
        host => <<"local.dev">>,
        headers => #{
            <<"forwarded">> => <<"for=192.0.2.60; proto=https; by=203.0.113.43; host=\"example.com\"">>
        }
    },
    #{
        cowmachine_proxy := false,
        cowmachine_forwarded_host := <<"local.dev">>,
        cowmachine_forwarded_port := 8000,
        cowmachine_forwarded_proto := <<"http">>,
        cowmachine_remote_ip := {1,2,3,4},
        cowmachine_remote := <<"1.2.3.4">>
    } = cowmachine_proxy:update_env(Req, #{}),
    ok.
