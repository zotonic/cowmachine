%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_accept_language_tests).

-include_lib("eunit/include/eunit.hrl").

cowmachine_accept_language_test() ->
    % Check selected languages based on priority
    Hdr0 = <<"en;q=0.8,nl;q=0.9,pt-br,*;q=0.1">>,
    {ok, <<"en">>} = cowmachine_accept_language:accept_header([{<<"en">>, []}], <<"en">>),
    {ok, <<"en">>} = cowmachine_accept_language:accept_header([{<<"en">>, []}], <<"en-us">>),
    {ok, <<"en">>} = cowmachine_accept_language:accept_header([{<<"en">>, []}], Hdr0),
    {ok, <<"nl">>} = cowmachine_accept_language:accept_header(
                        [{<<"en">>, []}, {<<"nl">>, []}], Hdr0),
    {ok, <<"pt">>} = cowmachine_accept_language:accept_header(
                        [{<<"en">>, []}, {<<"nl">>, []}, {<<"pt">>, []}], Hdr0),
    {ok, <<"pt-br">>} = cowmachine_accept_language:accept_header(
                        [{<<"en">>, []}, {<<"nl">>, []}, {<<"pt">>, []}, {<<"pt-br">>, []}], Hdr0),
    {ok, <<"es">>} = cowmachine_accept_language:accept_header(
                        [{<<"ru">>, []}, {<<"es">>, [<<"pt-br">>]}], Hdr0),
    {error, nomatch} = cowmachine_accept_language:accept_header(
                        [{<<"ru">>, []}, {<<"de">>, []}], Hdr0),

    % Check fallback languages if primary language does not match
    Hdr1 = <<"pt">>,
    {ok, <<"foo">>} = cowmachine_accept_language:accept_header(
                        [{<<"en">>, []}, {<<"nl">>, []}, {<<"foo">>, [ <<"pt">> ]}], Hdr1),


    % Select main language if variant is requested
    Hdr2 = <<"pt-br">>,
    {ok, <<"pt">>} = cowmachine_accept_language:accept_header(
                        [{<<"en">>, []}, {<<"nl">>, []}, {<<"pt">>, []}], Hdr2),
    ok.
