%% Author: bin.liu
%% Created: 2009-11-30
%% Description: TODO: Add description to util
-module(util).
-compile(export_all).

 delSubstr(S,Con) ->
	L=string:tokens(S, Con),
	Ret=getRet([],L),
	Ret.
 getRet(Ret,[H|E])->
	TRet=Ret++H;
 getRet(Ret,[])->
    Ret.

contact(Acc, [H|R])->
	contact([H|Acc],R);
contact(Acc, [])->
	Acc.

replace(List, Find, Replace)->
	replace(List, Find, Replace, []).

compare([C|L], [C|R], Acc)->
	compare(L, R, [C|Acc]);
compare(L, [], _)->
	{true, L, []};
compare(L, _, Acc)->
	{false, L, lists:reverse(Acc)}.

replace([C|L], [C|R] = Find, Replace, Acc)->
	Result = compare(L, R, [C]),
	case Result of
		{true, Rest1, _} ->
			replace(Rest1, Find, Replace, contact(Acc, Replace));
		{_, Rest2, Result2}->
			replace(Rest2,  Find, Replace, contact(Acc, Result2))
	end;
replace([C|L], Find, Replace, Acc)->
	replace(L, Find, Replace, [C|Acc]);
replace([], _, _, Acc)->
	lists:reverse(Acc).
getSDateTime() ->
	{A, B, C} = erlang:now(),
    random:seed(A, B, C),
    Rand = random:uniform(1000),
	{{Y,M,D},{HH,MM,_}}=erlang:localtime(),
	Ret=integer_to_list(Y)++"_"++getS(M)++"_"++getS(D)
        ++"-"++getS(HH)++"_"++getS(MM)++"r"++getS(Rand),
	Ret.
getS(I)->
	S=integer_to_list(I),
	case length(S) of
		1 ->
			Ret="0"++S;
	    _ ->
			 Ret=S
	end,
	Ret.

br2n(String) ->
	String_br = replace(String,"<br>","\n"),
	replace(String_br,"<br/>","\n").
