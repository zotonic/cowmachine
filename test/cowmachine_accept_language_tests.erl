%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_accept_language_tests).

-include_lib("eunit/include/eunit.hrl").

cowmachine_accept_language_test() ->
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
    ok.
