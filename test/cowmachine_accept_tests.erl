%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_accept_tests).

-include_lib("eunit/include/eunit.hrl").


% Test if the controller provided content-types is accepted by the request.
cowmachine_accept_test() ->
    Acceptable1 = [
        <<"text/json">>,
        <<"text/html">>
    ],
    false = cowmachine_util:is_media_type_accepted(Acceptable1, {<<"application">>, <<"octet-stream">>, []}),
    true = cowmachine_util:is_media_type_accepted(Acceptable1, {<<"text">>, <<"html">>, []}),
    Acceptable2 = [
        <<"text/json">>,
        <<"text/html">>,
        <<"*/*">>
    ],
    true = cowmachine_util:is_media_type_accepted(Acceptable2, {<<"application">>, <<"octet-stream">>, []}),
    true = cowmachine_util:is_media_type_accepted(Acceptable2, {<<"text">>, <<"html">>, []}),

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
    {<<"multipart">>, <<"form-data">>, []} = cowmachine_util:choose_media_type_accepted(ContentTypesAccepted, CTParsed),
    true = cowmachine_util:is_media_type_accepted(ContentTypesAccepted, CTParsed),
    ok.

% Test if the request content-type is accepted by the controller
cowmachine_provide_test() ->
    AcceptHdr = <<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5">>,
    Provided1 = [
        {<<"text">>, <<"plain">>},
        {<<"text">>, <<"html">>, [ {<<"foo">>, <<"bar">>} ]},
        {<<"text">>, <<"html">>, [ {<<"level">>, <<"2">>} ]},
        {<<"text">>, <<"html">>, []}
    ],
    <<"text/html">> = cowmachine_util:choose_media_type_provided(Provided1, AcceptHdr),
    Provided2 = [
        {<<"text">>, <<"plain">>},
        {<<"text">>, <<"html">>, [ {<<"foo">>, <<"bar">>} ]},
        {<<"text">>, <<"html">>, [ {<<"level">>, <<"1">>} ]},
        {<<"text">>, <<"html">>, []}
    ],
    <<"text/html;level=1">> = cowmachine_util:choose_media_type_provided(Provided2, AcceptHdr),
    Provided3 = [
        {<<"text">>, <<"plain">>},
        {<<"text">>, <<"html">>, [ {<<"foo">>, <<"bar">>} ]},
        {<<"text">>, <<"html">>, [ {<<"level">>, <<"3">>} ]}
    ],
    <<"text/plain">> = cowmachine_util:choose_media_type_provided(Provided3, AcceptHdr),
    ok.
