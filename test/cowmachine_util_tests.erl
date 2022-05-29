%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_util_tests).

-include_lib("eunit/include/eunit.hrl").

% Run test module
% $ rebar3 eunit -v -m cowmachine_util_tests

cowmachine_parse_test() ->
	?assertEqual(
		[ {<<"a">>, <<"b">>} ],
		cowmachine_util:parse_qs(<<"a=b">>)
	),
	?assertEqual(
		[ {<<"a">>, <<>>}, {<<"b">>, <<>>} ],
		cowmachine_util:parse_qs(<<"a&b">>)
	),
	?assertEqual(
		[ {<<"a">>, <<"b">>}, {<<"ab">>, <<"cd">>} ],
		cowmachine_util:parse_qs(<<"a=b&ab=cd">>)
	),
	?assertEqual(
		[ {<<"a b">>, <<"b c">>} ],
		cowmachine_util:parse_qs(<<"a+b=b+c">>)
	),
	?assertEqual(
		[ {<<"a b">>, <<"b c">>} ],
		cowmachine_util:parse_qs(<<"a%20b=b%20c">>)
	).

cowmachine_parse_error_test() ->
	?assertThrow(
		invalid_qs_name,
		cowmachine_util:parse_qs(<<"=b">>)
	),
	?assertThrow(
		invalid_percent_encoding,
		cowmachine_util:parse_qs(<<"b%2">>)
	),
	?assertThrow(
		invalid_percent_encoding,
		cowmachine_util:parse_qs(<<"a%2=b">>)
	).
	
cowmachine_normalize_content_type_test() ->
	Expected = {<<"text">>, <<"plain">>, []},
	Result = cowmachine_util:normalize_content_type(<<"text/plain">>),
	%?debugFmt("Result = ~p~n",[Result]),
	?assertEqual(Expected, Result).