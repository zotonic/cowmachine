-module(prop_cowmachine_util_parse_qs).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% shell command for a test: rebar3 proper -p prop_parse_qs
prop_parse_qs() ->
    ?FORALL(
		{
			{{FormattedName1, FormattedValue1},{Name1,Value1}},
			{{FormattedName2, FormattedValue2},{Name2,Value2}}
		},
		{
			pair_gen(),
			pair_gen()
		},
        begin
		
			Pair1 = FormattedName1 ++ "=" ++ FormattedValue1,
			PairBinary1 = list_to_binary(Pair1),
			ResultName1 = list_to_binary(replace(Name1, "\\+", " ")),
			ResultValue1 = list_to_binary(replace(Value1, "\\+", " ")),
			
			Pair2 = FormattedName2 ++ "=" ++ FormattedValue2,
			PairBinary2 = list_to_binary(Pair2),
			ResultName2 = list_to_binary(replace(Name2, "\\+", " ")),
			ResultValue2 = list_to_binary(replace(Value2, "\\+", " ")),
			
			PairBinary = erlang:iolist_to_binary([PairBinary1, <<"&">>, PairBinary2]),
			
			Result = cowmachine_util:parse_qs(PairBinary),
			%io:format("{~p,~p}, Result =~p~n",[ResultName,ResultValue,Result]),
			[{ResultName1, ResultValue1},{ResultName2, ResultValue2}] = Result,
			
			[{ResultName1, <<>>},{ResultName2, <<>>}] ==
			cowmachine_util:parse_qs(
			erlang:iolist_to_binary([FormattedName1, <<"&">>, FormattedName2])
			)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% Non-alphanumeric characters are replaced by `%HH', a percent sign and 
%% two hexadecimal digits representing the ASCII code of the character.

replace(String) ->
	StringWithoutSpaces = replace(String, " ", "+"),
	Result = lists:flatten(
	lists:map(fun(Elem)->
		% Find [^+0-1a-zA-Z] and do replace
		case Elem of
			Elem when Elem =:= 43 ->
				Elem;
			Elem when Elem >= 48, Elem =< 57 ->
				Elem;
			Elem when Elem >= 65, Elem =< 90 ->
				Elem;
			Elem when Elem >= 97,Elem =< 122 ->
				Elem;
			_ ->
				"%" ++ io_lib:format("~2.16.0B", [Elem])
		
		end
	end, StringWithoutSpaces)
	),
	%io:format("String =~p, Result =~p~n",[String, Result]),
	Result
	.

replace(String, RE, Replacement) ->
	StringWithoutSpaces = re:replace(String, RE, Replacement, [{return, list}, global]),
	StringWithoutSpaces.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
pair_gen() ->
	?LET({Name, Value} = Input, {item(), item()}, 
		begin
			FormattedName = replace(Name),
			FormattedValue = replace(Value),
			{{FormattedName, FormattedValue}, Input}
		end). 

item() -> non_empty(list(integer(0,127))).