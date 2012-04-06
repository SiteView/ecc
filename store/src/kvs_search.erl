-module(kvs_search).
-compile(export_all).

-include("kvs_define.hrl").

%%%-record(log, {time, facility, level, machine, program, pid, message, master_decoder, sub_decoder, my}).
%%format
%%%[{time, [20100328091700, 20100417021000]}, {facility, Facility}, {level, Level}, {Key, Value}]
%%%my field name not cpmflict with the fixed field

init()->
	case lists:member(kvs_search, ets:all()) of
		false ->
			ets:new(kvs_search, [public, set, named_table]);
		_ ->
			ets:delete_all_objects(kvs_search)
	end.

stop()->
	ets:delete(kvs_search),
	done.

split(Condition)->
	split(Condition, []).

split([{time, Interval}|Rest], Acc)->
	{ok, {Interval, lists:reverse(Acc) ++ Rest}};
split([Item|Rest], Acc)->
	split(Rest, [Item|Acc]);
split([], _)->
	{error, format}.

%%[2010-03-25 12:45:00, 2010-03-29 12:45:00]
%%2010-03-25 12:45:00    2010-03-25 24:00:00
%%2010-03-26 00:00:00    2010-03-26 24:00:00
%%2010-03-27 00:00:00    2010-03-27 24:00:00
%%2010-03-28 00:00:00    2010-03-28 24:00:00
%%2010-03-29 00:00:00    2010-03-29 21:36:00

%%2010-03-25
%%2010-03-29
%%Interval = 4

time2date(Time)->
	Str =  integer_to_list(Time),
	Year = list_to_integer(lists:sublist(Str, 1, 4)),
	Month = list_to_integer(lists:sublist(Str, 5, 2)),
	Day = list_to_integer(lists:sublist(Str, 7, 2)),
	Hour = list_to_integer(lists:sublist(Str, 9, 2)),
	Minute = list_to_integer(lists:sublist(Str, 11, 2)),
	Second = list_to_integer(lists:sublist(Str, 13, 2)),
	{{Year, Month, Day}, {Hour, Minute, Second}}.

date2time({Year, Month, Day}, {Hour, Minute, Second})->
	list_to_integer(lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [Year, Month, Day, Hour, Minute, Second]))).

time(Start, Days, 0, Acc)->
	{Year, Month, Day} = calendar:gregorian_days_to_date(Days),
	ID = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day])),
	End = date2time({Year, Month, Day}, {24, 0, 0}),
	[{ID, 1, Start, End}|Acc];
time(Start, Days, Interval, Acc)->
	{Year, Month, Day} = calendar:gregorian_days_to_date(Days + Interval),
	ID = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day])),
	Start1 = date2time({Year, Month, Day}, {0, 0, 0}),
	End1 = date2time({Year, Month, Day}, {24, 0, 0}),
	time(Start, Days, Interval - 1, [{ID, 0, Start1, End1}|Acc]).

time([Start, End])->
	{{StartYear, StartMonth, StartDay}, _} = time2date(Start),
	StartDays = calendar:date_to_gregorian_days(StartYear, StartMonth, StartDay),
	{{EndYear, EndMonth, EndDay}, _} = time2date(End),
	EndDays = calendar:date_to_gregorian_days(EndYear, EndMonth, EndDay),

	ID = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [EndYear, EndMonth, EndDay])),
	End1 = date2time({EndYear, EndMonth, EndDay}, {0, 0, 0}),
	
	Interval = EndDays - StartDays,
	case Interval > 0 of
		true->
			time(Start, StartDays, Interval - 1, [{ID, 1, End1, End}]);
		_ ->
			[{ID, 1, Start, End}]
	end.

%%ets:insert(Table, {{N, element(N, Conetnt)}, {Table, Position}, Time});
%%%[{time, [20100328091700, 20100417021000]}, {facility, Facility}, {level, Level}, {Key, Value}]

read(Table, [ID|Rest], Acc)->
	case kvs_storage:read(Table, ID) of
		null->
			read(Table, Rest, Acc);
		Record->
			read(Table, Rest, [Record|Acc])
	end;
read(_, [], Acc)->
	Acc.

read(Table, N, [{ID, N}|Rest], Acc)->
	case kvs_storage:read(Table, ID) of
		null->
			read(Table, N, Rest, Acc);
		Record->
			read(Table, N, Rest, [Record|Acc])
	end;
read(Table, N, [_|Rest], Acc)->
	read(Table, N, Rest, Acc);
read(_, _, [], Acc)->
	Acc.

read(Table, N, Index, Start, End, [{ID, N}|Rest], Acc)->
	case kvs_storage:read(Table, ID) of
		null->
			read(Table, N, Index, Start, End, Rest, Acc);
		Record->
			Time = element(Index, Record),
			case Time >= Start andalso Time =< End of
				true->
					read(Table, N, Index, Start, End, Rest, [Record|Acc]);
				_ ->
					read(Table, N, Index, Start, End, Rest, Acc)
			end
	end;
read(Table, N, Index, Start, End, [_|Rest], Acc)->
	read(Table, N, Index, Start, End, Rest, Acc);
read(_, _, _, _, _, [], Acc)->
	Acc.

read(Table, N, Index, Start, End)->
	read(Table, N, Index, Start, End, get(), []).

read(Table, N) when is_integer(N)->
	read(Table, N, get(), []);
read(Table, IDList) when is_list(IDList)->
	read(Table, IDList, []);
read(_, _)->
	[].

filter([{_, ID}|Rest], P, N)->
	case get(ID) =:= P of
		true->
			put(ID, N);
		_ ->
			nothing
	end,
	filter(Rest, P, N);
filter([], _, _)->
	done.

search([{Key, Value}|Condition], Table, N)->
	Target = {kvs_index:map(Key), Value},
	case kvs_storage:lookup(Table, Target) of
		[]->
			0;
		Records->
			filter(Records, N - 1, N),
			search(Condition, Table, N + 1)
	end;
search([], _, N)->
	N - 1.

search(Table, [{Key, Value}|Rest])->
	Target = {kvs_index:map(Key), Value},
	case kvs_storage:lookup(Table, Target) of
		[]->
			0;
		Records->
			filter(Records, undefined, 1),
			search(Rest, Table, 2)
	end;
search(_, [])->
	0.

search({Storage, Flag, Start, End, []})->
	Index = kvs_index:map(time),
	IndexTable = Storage#storage.index_table,
	DataTable = Storage#storage.data_table,
	IDList =case Flag =:= 1 of
		true ->
			kvs_storage:select(IndexTable, [{{{Index, '$1'},'$2'}, [{'>=','$1',Start},{'=<','$1', End}], ['$2']}]);
		_ ->
			kvs_storage:select(IndexTable, [{{{Index, '$1'},'$2'}, [], ['$2']}])
	end,
	read(DataTable, IDList);
search({Storage, Flag, Start, End, Condition})->
	Index = kvs_index:map(time),
	IndexTable = Storage#storage.index_table,
	DataTable = Storage#storage.data_table,
	case search(IndexTable, Condition) of
		0->
			[];
		N->
			case Flag =:= 1 of
				true ->
					read(DataTable, N, Index, Start, End);
				_ ->
					read(DataTable, N)
			end
	end.

pmap(Tasks) ->   
	Self = self(),  
	Pids = lists:map(fun(Task) -> spawn( fun() -> job(Self, Task) end)  end, Tasks),  
	gather(Pids). 
   
gather([H|T]) ->  
	receive
		{H, Result} -> [Result|gather(T)]  
	end;
gather([]) ->  
	[].  

job(Parent, Task) ->             
     Parent ! {self(), (catch search(Task))}. 

preload(Str)->
	ID = list_to_integer(Str),
	case kvs_storage:get(ID) of
		null->
			Storage = kvs_storage:id2storage(Str),
			case {filelib:is_file(Storage#storage.index_name), filelib:is_file(Storage#storage.data_name)} of
				{true, true}->
					kvs_storage:open(Storage);
				_ ->
					{error, file_not_exist}
			end;
		Result ->
			{ok, Result}
	end.

preload([{ID, Flag, Start, End}|Rest], Condition, Acc)->
	case preload(ID) of
		{ok, Storage}->
			preload(Rest, Condition, [{Storage, Flag, Start, End, Condition}|Acc]);
		_ ->
			preload(Rest, Condition, Acc)
	end;
preload([], _, Acc)->
	Acc.

preload(Intervals, Condition)->
	preload(Intervals, Condition, []).
	
execute(Condition)->
	case split(Condition) of
		{ok, {TimeCondition, OtherCondition}}->
			Intervals = time(TimeCondition),
			Tasks = preload(Intervals, OtherCondition),
			Result = pmap(Tasks),
			Records = lists:flatten(Result),
			{ok, Records};
		Error->
			Error
	end.
