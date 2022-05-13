%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_accept_language_tests).

-include_lib("eunit/include/eunit.hrl").

% Run test module
% $ rebar3 eunit -v -m cowmachine_accept_language_tests

all_cowmachine_accept_language_test_() ->
    [{"Check selected languages based on priority", fun cowmachine_accept_language_0/0},
     {"Check fallback languages if primary language does not match", fun cowmachine_accept_language_1/0},
     {"Select main language if variant is requested", fun cowmachine_accept_language_2/0}].

cowmachine_accept_language_0() ->
	
    ?assertEqual(
		{ok, <<"en">>}, 
		cowmachine_accept_language:accept_header([{<<"en">>, []}], <<"en">>)
	),
    ?assertEqual(
		{ok, <<"en">>}, 
		cowmachine_accept_language:accept_header([{<<"en">>, []}], <<"en-us">>)
	),
	
	Hdr0 = <<"en;q=0.8,nl;q=0.9,pt-br,*;q=0.1">>,
    ?assertEqual(
		{ok, <<"en">>}, 
		cowmachine_accept_language:accept_header([{<<"en">>, []}], Hdr0)
	),
    ?assertEqual(
		{ok, <<"nl">>}, 
		cowmachine_accept_language:accept_header([{<<"en">>, []}, {<<"nl">>, []}], Hdr0)
	),
    ?assertEqual(
		{ok, <<"pt">>}, 
		cowmachine_accept_language:accept_header(
							[{<<"en">>, []}, {<<"nl">>, []}, {<<"pt">>, []}], Hdr0)
	),
    ?assertEqual(
		{ok, <<"pt-br">>}, 
		cowmachine_accept_language:accept_header(
							[{<<"en">>, []}, {<<"nl">>, []}, {<<"pt">>, []}, {<<"pt-br">>, []}], Hdr0)
	),
    ?assertEqual(
		{ok, <<"es">>}, 
		cowmachine_accept_language:accept_header(
							[{<<"ru">>, []}, {<<"es">>, [<<"pt-br">>]}], Hdr0)
	),
    
	?assertEqual(
		{error, nomatch}, 
		cowmachine_accept_language:accept_header(
							[{<<"ru">>, []}, {<<"de">>, []}], Hdr0)
	).
						
cowmachine_accept_language_1() ->
	Hdr1 = <<"pt">>,
	?assertEqual(
		{ok, <<"foo">>}, 
		cowmachine_accept_language:accept_header(
							[{<<"en">>, []}, {<<"nl">>, []}, {<<"foo">>, [ <<"pt">> ]}], Hdr1)
	).

cowmachine_accept_language_2() ->
	Hdr2 = <<"pt-br">>,
	?assertEqual(
		{ok, <<"pt">>}, 
		cowmachine_accept_language:accept_header(
							[{<<"en">>, []}, {<<"nl">>, []}, {<<"pt">>, []}], Hdr2)
	).
