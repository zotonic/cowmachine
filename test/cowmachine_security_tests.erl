%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_security_tests).

-include_lib("eunit/include/eunit.hrl").

cowmachine_metadata_test() ->
    Req = #{
        peer => {{127,0,0,1},1234},
        scheme => <<"http">>,
        port => 8000,
        host => <<"local.dev">>,
        headers => #{},
        resp_headers => #{}
    },
    Context = cowmachine_req:init_context(Req, #{}, undefined),
    _ = cowmachine_req:set_resp_header(<<"sample-header">>, <<"foobar">>, Context),
    ok = try
        cowmachine_req:set_resp_header(<<"sample-header">>, <<"foo", 13, 10, "bar">>, Context)
    catch
        throw:{http_invalid_header, _, _} -> ok
    end,
    ok = try
        cowmachine_req:set_resp_header(<<"Foo-Bar">>, <<"foobar">>, Context)
    catch
        throw:{http_invalid_header, _, _} -> ok
    end,
    ok = try
        cowmachine_req:set_resp_header(<<"Foo Bar">>, <<"foobar">>, Context)
    catch
        throw:{http_invalid_header, _, _} -> ok
    end,
    ok = try
        cowmachine_req:set_resp_header(<<"Foo", 13, 10, "Bar">>, <<"foobar">>, Context)
    catch
        throw:{http_invalid_header, _, _} -> ok
    end,
    ok = try
        cowmachine_req:set_resp_header(<<"location">>, <<"javascript:foobar">>, Context)
    catch
        throw:{http_invalid_location, _, _} -> ok
    end.