%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2019 Marc Worrell
%%
%% @doc Accept-Language handling.
%% @end

%% Copyright 2017-2019 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(cowmachine_accept_language).

-author("Marc Worrell <marc@worrell.nl").

-export([
    accept_header/2,
    accept_list/2
    ]).

-spec accept_header(AvailableLangs, AcceptHeader) -> Result when
    AvailableLangs :: [{binary(),[ binary() ]}],
	AcceptHeader :: cowmachine_req:context() | binary() | undefined,
	Result :: {ok, binary()} | {error, nomatch | header}.
accept_header(_AvailableLangs, undefined) ->
    {error, nomatch};
accept_header(AvailableLangs, AcceptHeader) when is_binary(AcceptHeader) ->
    case parse(AcceptHeader) of
        error -> {error, header};
        List ->
            Sorted = sort_accept(List),
            case [ Lang || {Lang,_Prio} <- Sorted, Lang =/= <<"*">> ] of
                [] -> {error, nomatch};
                AcceptList ->
                    AcceptList1 = ensure_baselangs(AcceptList),
                    accept_list(AvailableLangs, AcceptList1)
            end
    end;
accept_header(AvailableLangs, Context) ->
    accept_header(AvailableLangs, cowmachine_req:get_req_header(<<"accept-language">>, Context)).

-spec parse(AcceptHeader) -> Result when
	AcceptHeader :: binary(),
	Result :: [{binary(), cow_http_hd:qvalue()}] | error.
parse(AcceptHeader) ->
    try
        cow_http_hd:parse_accept_language(AcceptHeader)
    catch
        _:_ -> error
    end.


-spec accept_list(AvailableLangs, AcceptableLangs) -> Result when	
	AvailableLangs :: [{binary(), [ binary() ]}], 
	AcceptableLangs :: [binary()],
	Result :: {ok, binary()} | {error, nomatch}.
accept_list(AvailableLangs, AcceptableLangs) ->
    case match_language(AvailableLangs, AcceptableLangs) of
        {ok, _} = OK -> OK;
        error ->
            % Map language variations like "en-gb" to "en"
            Main = main_languages(AcceptableLangs),
            case match_language(AvailableLangs, Main) of
                {ok, _} = OK -> OK;
                error -> {error, nomatch}
            end
    end.

-spec sort_accept(List) -> Result when
	List :: list(),
	Result :: list().
sort_accept([]) -> [];
sort_accept(List) ->
    lists:keysort(2, fix_order(List,1,[])).

-spec ensure_baselangs(Langs) -> Result when
	Langs :: list(),
	Result :: list().
ensure_baselangs(Langs) ->
    lists:foldl(
        fun
            (<<A,B,$-,_/binary>> = Lang, Acc) ->
                BaseLang = <<A,B>>,
                case lists:member(BaseLang, Langs) of
                    true -> [Lang|Acc];
                    false -> [Lang,BaseLang|Acc]
                end;
            (Lang, Acc) ->
                [Lang|Acc]
        end,
        [],
        Langs).


% Modify the priority so that for languages with equal priority the first mentioned
% will be chosen.

-spec fix_order(LangList, N, Acc) -> Result when
	LangList :: [LangItem],
	LangItem :: {Lang, Prio},
	Lang :: binary(), 
	Prio :: integer(),
	N :: non_neg_integer(),
	Acc :: LangList,
	Result :: Acc.
fix_order([], _N, Acc) ->
    Acc;
fix_order([{Lang,Prio}|Langs], N, Acc) ->
    fix_order(Langs, N+1, [{Lang, Prio*100-N}|Acc]).

-spec match_language(AvailableLangs, AcceptList) -> Result when
	AvailableLangs :: [{binary(), [ binary() ]}], 
	AcceptList :: [binary()],
	Result :: {ok, binary()} | error.
match_language(AvailableLangs, AcceptList) ->
    case firstmap(fun(Lang) -> available_language(Lang, AvailableLangs) end, AcceptList) of
        {ok, _} = OK -> OK;
        error ->
            firstmap(fun(Lang) -> fallback_language(Lang, AvailableLangs) end, AcceptList)
    end.

firstmap(_Fun, []) ->
    error;
firstmap(Fun, [H|T]) ->
    case Fun(H) of
        {ok, Found} -> {ok, Found};
        error -> firstmap(Fun, T)
    end.

available_language(Lang, AvailableLangs) ->
    case lists:keymember(Lang, 1, AvailableLangs) of
        true -> {ok, Lang};
        false -> error
    end.

-spec fallback_language(Lang, [{AvailableLang,FallbackLangs}]) -> Result when
	Lang :: binary(), 
	AvailableLang :: binary(),
	FallbackLangs :: [binary()],
	Result :: error | {ok, AvailableLang}.
fallback_language(_Lang, []) ->
    error;
fallback_language(Lang, [{AvailableLang,FallbackLangs}|AvailableLangs]) ->
    case lists:member(Lang, FallbackLangs) of
        true -> {ok, AvailableLang};
        false -> fallback_language(Lang, AvailableLangs)
    end.

-spec main_languages(Accept) -> Result when
	Accept :: [binary()],
	Result :: [binary()].
main_languages(Accept) ->
    Accept1 = lists:foldl(
        fun
            (<<A,B,$-,_/binary>>, Acc) -> [<<A,B>>|Acc];
            (_, Acc) -> Acc
        end,
        [],
        Accept),
    lists:reverse(Accept1).
