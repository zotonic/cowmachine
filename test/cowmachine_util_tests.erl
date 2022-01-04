%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(cowmachine_util_tests).

-include_lib("eunit/include/eunit.hrl").


cowmachine_parse_test() ->
    [ {<<"a">>, <<"b">>} ] = cowmachine_util:parse_qs(<<"a=b">>),
    [ {<<"a">>, <<>>}, {<<"b">>, <<>>} ] = cowmachine_util:parse_qs(<<"a&b">>),
    [ {<<"a">>, <<"b">>}, {<<"ab">>, <<"cd">>} ] = cowmachine_util:parse_qs(<<"a=b&ab=cd">>),
    [ {<<"a b">>, <<"b c">>} ] = cowmachine_util:parse_qs(<<"a+b=b+c">>),
    [ {<<"a b">>, <<"b c">>} ] = cowmachine_util:parse_qs(<<"a%20b=b%20c">>),
    ok.

cowmachine_parse_error_test() ->
    error = try
        cowmachine_util:parse_qs(<<"=b">>)
    catch
        throw:invalid_qs_name -> error
    end,
    error = try
        cowmachine_util:parse_qs(<<"b%2">>)
    catch
        throw:invalid_percent_encoding -> error
    end,
    error = try
        cowmachine_util:parse_qs(<<"a%2=b">>)
    catch
        throw:invalid_percent_encoding -> error
    end,
    ok.