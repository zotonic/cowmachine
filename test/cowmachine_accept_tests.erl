%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_accept_tests).

-include_lib("eunit/include/eunit.hrl").

% Run test module
% $ rebar3 eunit -v -m cowmachine_accept_tests

all_cowmachine_accept_test_() ->
    [{"Test if the controller provided content-types is accepted by the request.", fun cowmachine_accept/0},
     {"Test if the request content-type is accepted by the controller", fun cowmachine_provide/0}].

cowmachine_accept() ->
    Acceptable1 = [
        <<"text/json">>,
        <<"text/html">>
    ],
    ?assertNot(cowmachine_util:is_media_type_accepted(Acceptable1, {<<"application">>, <<"octet-stream">>, []})),
	?assert(cowmachine_util:is_media_type_accepted(Acceptable1, {<<"text">>, <<"html">>, []})),

    Acceptable2 = [
        <<"text/json">>,
        <<"text/html">>,
        <<"*/*">>
    ],
	?assert(cowmachine_util:is_media_type_accepted(Acceptable2, {<<"application">>, <<"octet-stream">>, []})),
	?assert(cowmachine_util:is_media_type_accepted(Acceptable2, {<<"text">>, <<"html">>, []})),

    CTHeader = <<"multipart/form-data; boundary=----WebKitFormBoundaryyXNhNBBDn1j4diST">>,
    CTParsed = cow_http_hd:parse_content_type(CTHeader),
    ContentTypesAccepted =  [
        {<<"application">>, <<"json">>, []},
        {<<"application">>, <<"javascript">>, []},
        {<<"text">>, <<"javascript">>, []},
        {<<"text">>, <<"x-ubf">>, []},
        {<<"application">>, <<"x-bert">>, []},
        {<<"application">>, <<"x-www-form-urlencoded">>, []},
        {<<"multipart">>, <<"form-data">>, []}
    ],
	?assertEqual({<<"multipart">>, <<"form-data">>, []}, 
		cowmachine_util:choose_media_type_accepted(ContentTypesAccepted, CTParsed)),
	?assert(cowmachine_util:is_media_type_accepted(ContentTypesAccepted, CTParsed)).

cowmachine_provide() ->
    AcceptHdr = <<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5">>,
    Provided1 = [
        {<<"text">>, <<"plain">>},
        {<<"text">>, <<"html">>, [ {<<"foo">>, <<"bar">>} ]},
        {<<"text">>, <<"html">>, [ {<<"level">>, <<"2">>} ]},
        {<<"text">>, <<"html">>, []}
    ],
    ?assertMatch({<<"text">>, <<"html">>, _}, 
		cowmachine_util:choose_media_type_provided(Provided1, AcceptHdr)),
	
	Provided2 = [
        {<<"text">>, <<"plain">>},
        {<<"text">>, <<"html">>, [ {<<"foo">>, <<"bar">>} ]},
        {<<"text">>, <<"html">>, [ {<<"level">>, <<"1">>} ]},
        {<<"text">>, <<"html">>, []}
    ],
	?assertEqual({<<"text">>, <<"html">>, [ {<<"level">>, <<"1">>} ]}, 
		cowmachine_util:choose_media_type_provided(Provided2, AcceptHdr)),

    Provided3 = [
        {<<"text">>, <<"plain">>},
        {<<"text">>, <<"html">>, [ {<<"foo">>, <<"bar">>} ]},
        {<<"text">>, <<"html">>, [ {<<"level">>, <<"3">>} ]}
    ],
	
	?assertEqual({<<"text">>, <<"plain">>, []}, 
		cowmachine_util:choose_media_type_provided(Provided3, AcceptHdr)).
