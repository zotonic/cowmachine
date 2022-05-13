%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_security_tests).

-include_lib("eunit/include/eunit.hrl").

% Run test module
% $ rebar3 eunit -v -m cowmachine_security_tests

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
    
	?assertMatch(
		 #{cowenv := #{},
		   cowreq := #{
			headers := #{},
			host := <<"local.dev">>,
			peer := {{127,0,0,1},1234},
			port := 8000,
			resp_headers := #{<<"sample-header">> := <<"foobar">>},
			scheme := <<"http">>
			}
		},
		cowmachine_req:set_resp_header(<<"sample-header">>, <<"foobar">>, Context)
	),
	
	?assertThrow(
		{http_invalid_header, _, _},
		cowmachine_req:set_resp_header(<<"sample-header">>, <<"foo", 13, 10, "bar">>, Context)
	),
	
	?assertThrow(
		{http_invalid_header, _, _},
		cowmachine_req:set_resp_header(<<"Foo-Bar">>, <<"foobar">>, Context)
	),
	
	?assertThrow(
		{http_invalid_header, _, _},
		cowmachine_req:set_resp_header(<<"Foo Bar">>, <<"foobar">>, Context)
	),
	
	?assertThrow(
		{http_invalid_header, _, _},
		cowmachine_req:set_resp_header(<<"Foo", 13, 10, "Bar">>, <<"foobar">>, Context)
	),
	
	?assertThrow(
		{http_invalid_location, _, _},
		cowmachine_req:set_resp_header(<<"location">>, <<"javascript:foobar">>, Context)
	).