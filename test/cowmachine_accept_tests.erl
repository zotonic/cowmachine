%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_accept_tests).

-include_lib("eunit/include/eunit.hrl").

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
    ok.
