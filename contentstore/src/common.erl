-module(common).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-include("config.hrl").

index(String, C)->
    index(String, C, 1).

index([C|_], C, Index)->
    Index;
index([_|R], C, Index)->
    index(R, C, Index + 1);
index([], _, _)->
    0.

indexOfReverse(String, C)->
    indexOfReverse(String, C, 1, 0).

indexOfReverse([C|R], C, Count, _)->
    indexOfReverse(R, C, Count + 1, Count);
indexOfReverse([_|R], C, Count, Index)->
    indexOfReverse(R, C, Count + 1, Index);
indexOfReverse([], _, _, Index)->
    Index.

trace(X, Module, Line)->
    {Hour, Minute, Second} = time(),
    io:format("~p:~p:~p [~p.erl](~p)=>~p~n", [Hour, Minute, Second, Module, Line, X]).
trace(X, Y, Module, Line)->
    {Hour, Minute, Second} = time(),
    io:format("~p:~p:~p [~p.erl](~p)=>" ++ X, [Hour, Minute, Second, Module, Line] ++ Y).

execQLC(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

getStrID() ->
%    {MegaSecs, Secs, MicroSecs} = now(),
%    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
%    list_to_integer(lists:flatten(io_lib:format("~1..0w~2..0w~2..0w~2..0w~2..0w~2..0w~6..0w",[Year rem 1000, Month, Day, Hour, Minute, Second, MicroSecs]))).
     % 63113904000 = calendar:datetime_to_gregorian_seconds({{2000, 1, 1},{0, 0, 0}})
     TimeSpan = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 63113904000,
     {X, Y, Z} = erlang:now(),
     MicroSecs = Z,
     random:seed(X, Y, Z),
     Random = random:uniform(9),
     lists:flatten(io_lib:format("~9..0w~6..0w~1..0w",[TimeSpan, MicroSecs, Random])).
     
getIntID() ->
    ID = getStrID(),
    list_to_integer(ID).
          

getUTCTime() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0wZ",[Year, Month, Day, Hour, Minute, Second, MicroSecs div 1000])).
    
qsort([Pivot|T], Index, Fun1, Fun2) ->
    qsort([X || X <- T, Fun1(element(Index, X), element(Index, Pivot))], Index, Fun1, Fun2)
    ++ [Pivot] 
    ++ qsort([X || X <- T,Fun2(element(Index, X), element(Index, Pivot))], Index, Fun1, Fun2);
qsort([], _, _, _) -> [].
    
sortRecord(SortFun, Condition, Records)->
    Order = [{Key, Operator, Value} || {Key, Operator, Value} <- Condition, Key =:= "order", Operator =:= "="],
    if 
        length(Order) == 1 ->
            [{_, _, Field}|_] = Order,
            SortFun(Field, Records);
        true->
            Records
    end.
    
list_to_string([H], Space,Acc) -> list_to_string([], Space,[H|Acc]);
list_to_string([H|R], Space,Acc)->  list_to_string(R, Space,[Space,H|Acc]);
list_to_string([], _,Acc) -> lists:reverse(Acc).

list_to_string(List,Space) ->
  lists:flatten(list_to_string(List,Space,[])).

extractRecord([], Records)->
    From = 0,
    To = 100, 
    Total = length(Records),
    SubRecords = lists:sublist(Records, From + 1, To),
    {ok, {From, To, Total, SubRecords}};
extractRecord(Condition, Records)->
    FromCondition = [{Key, Operator, Value} || {Key, Operator, Value} <- Condition, Key =:= "from", Operator =:= "="],
    ToCondition = [{Key, Operator, Value} || {Key, Operator, Value} <- Condition, Key =:= "to", Operator =:= "="],

    Total = length(Records),
    {From, To} = if
        length(FromCondition) == 1, length(ToCondition) == 1->
            [{_, _, FromStr}|_] = FromCondition,
            [{_, _, ToStr}|_] = ToCondition,
            FromValue = list_to_integer(FromStr),
            ToValue = list_to_integer(ToStr),
            if 
                FromValue >= 0, ToValue > FromValue ->
                    {FromValue, ToValue};
                true ->
                    {0, Total}
            end;
        true->
            BeginCondition = [{Key, Operator, Value} || {Key, Operator, Value} <- Condition, Key =:= "begin", Operator =:= "="],
            EndCondition = [{Key, Operator, Value} || {Key, Operator, Value} <- Condition, Key =:= "end", Operator =:= "="],
            if length(BeginCondition) == 1, length(EndCondition) == 1 ->
                    [{_, _, BeginStr}|_] = BeginCondition,
                    [{_, _, EndStr}|_] = EndCondition,
                    BeginValue = list_to_integer(BeginStr),
                    EndValue = list_to_integer(EndStr),
                    if 
                        BeginValue >= 0, EndValue > BeginValue ->
                            {BeginValue, EndValue};
                        true ->
                            {0, Total}
                    end;
                true->
                    {0, 100}
             end
      end,
      
      if
          From + 1 > Total ->
              {ok, {From, To, Total, []}};
          true ->
              SubRecords = lists:sublist(Records, From + 1, To - From),
              {ok, {From, To, Total, SubRecords}}
      end.

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
	{false, L, Acc}.

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
      