-module(wmic).
-compile(export_all).

-include("../include/define.hrl").

sleep(Time)->
	receive
		after Time ->
			ok
	end.
	
parse(Result)->
	case Result of
		"NTSTATUS: " ++ Rest1->
			{error, Rest1};
		"/bin/sh: " ++ Rest2->
			{error, Rest2};
		_ ->
			List = lists:map(fun(X)->list_to_tuple(string:tokens(X, "|")) end, string:tokens(Result, "\n")),
			case List of
				[]->
					{error, "query nothing return"};
				_ ->
					{ok, List}
			end
	end.
	
parseWithEmpty(Result)->
	case Result of
		"NTSTATUS: " ++ Rest1->
			{error, Rest1};
		"/bin/sh: " ++ Rest2->
			{error, Rest2};
		_ ->
			{ok, lists:map(fun(X)->list_to_tuple(string:tokens(X, "|")) end, string:tokens(Result, "\n"))}
	end.
	
process(?CPU, Value)->
	List = tl(Value),
	DetailUtilization = string:strip(lists:append(lists:flatmap(fun({V1, V2})->[V1, ":", V2, "-"] end, List)), right, $-),
	{Count, Acc} = lists:foldl(fun({_, V}, {CountIn, AccIn})-> {CountIn + 1, AccIn + list_to_integer(V)} end, {0, 0}, List),
	Utilization = integer_to_list(Acc div Count),
	Result = lists:append(["detailutilization=", DetailUtilization ,";utilization=", Utilization]),
	{ok, Result};
process(?DISK, Value)->
	List = tl(Value),
	{ok, lists:append(lists:flatmap(fun({X})-> [X, "$"] end, List))};
process(?SERVICE, Value)->
	List = tl(Value),
	{ok, lists:append(lists:flatmap(fun({X, _})-> [X, "$"] end, List))};
process(?PROCESS, Value)->
	List = tl(Value),
	{ok, lists:append(lists:flatmap(fun({X})-> [X, "$"] end, List))};
process(?NETWORK, Value)->
	List = tl(Value),
	{ok, lists:append(lists:flatmap(fun({X})-> [X, "$"] end, List))}.

process(?MEMORY, 1, Value)->
	Tuple = hd(tl(Value)),
	FreePhyMem = list_to_integer(element(1, Tuple)) / 1024,
	FreeVirMem = list_to_integer(element(2, Tuple)) / 1024,
	TotalVirMem = list_to_integer(element(6, Tuple)) / 1024,
	TotalPhyMem = list_to_integer(element(7, Tuple)) / 1024,
	lists:flatten(io_lib:format("TotalPhyMem=~.2f$TotalVirMemory=~.2f$FreePhyMem=~.2f$FreeVirMem=~.2f$PhyMemUsage=~.2f$PercentUsed=~.2f$", 
							[TotalPhyMem, TotalVirMem, FreePhyMem, FreeVirMem, (TotalPhyMem-FreePhyMem)/TotalPhyMem*100, (TotalVirMem-FreeVirMem)/TotalVirMem*100]));
process(?MEMORY, _, Value)->
	Tuple = hd(tl(Value)),
	Frequency_PerfTime = list_to_integer(element(1, Tuple)),
	PagesPersec = list_to_integer(element(2, Tuple)),
	Timestamp_PerfTime = list_to_integer(element(3, Tuple)),
	{Frequency_PerfTime, PagesPersec, Timestamp_PerfTime};
process(?NETWORK, _, Value)->
	Tuple = hd(tl(Value)),
	BytesReceivedPersec = list_to_integer(element(1, Tuple)),
	BytesSentPersec = list_to_integer(element(2, Tuple)),
	Frequency_PerfTime = list_to_integer(element(3, Tuple)),
	PacketsOutboundErrors = list_to_integer(element(5, Tuple)),
	PacketsReceivedErrors = list_to_integer(element(6, Tuple)),
	Timestamp_PerfTime = list_to_integer(element(7, Tuple)),
	{BytesReceivedPersec, BytesSentPersec, Frequency_PerfTime, PacketsOutboundErrors, PacketsReceivedErrors, Timestamp_PerfTime};
process(?PROCESS, 1, Value)->
	List = tl(Value),
	lists:foldl(fun(X, Acc)-> Acc + list_to_integer(element(2, X)) end, 0, List);
process(?PROCESS, 2, Value)->
	List = tl(Value),
	PercentProcessorTime = lists:foldl(fun(X, Acc)-> Acc + list_to_integer(element(2, X)) end, 0, List),
	PrivateBytes = lists:foldl(fun(X, Acc)-> Acc + list_to_integer(element(3, X)) end, 0, List),
	ThreadCount = lists:foldl(fun(X, Acc)-> Acc + list_to_integer(element(4, X)) end, 0, List),
	WorkingSet = lists:foldl(fun(X, Acc)-> Acc + list_to_integer(element(5, X)) end, 0, List),
	Count = length(List),
	{Count, PercentProcessorTime, PrivateBytes, ThreadCount, WorkingSet};
process(?PROCESS, _, Value)->
	Tuple = hd(tl(Value)),
	TotalVirMem = list_to_integer(element(6, Tuple)),
	TotalPhyMem = list_to_integer(element(7, Tuple)),
	{TotalVirMem, TotalPhyMem}.

process(_, _, _, _, ?DISK, _, Value)->
	Tuple = hd(tl(Value)),
	FreeSize = list_to_integer(element(2, Tuple)),
	Size = list_to_integer(element(3, Tuple)),
	String = lists:flatten(io_lib:format("percentFull=~.2f$Mbfree=~.2f$TotalSize=~.2f$",
										[(Size - FreeSize) / Size * 100 , FreeSize/1048576, Size/1048576])),
	{ok, String};
process(_, Host, User, Password, ?MEMORY, _, Value)->
	Memory = process(?MEMORY, 1, Value),
	Query = wql:string(?MEMORY, 1),
	CMD = lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " \"", Query, "\""]),
	Return1 = os:cmd(CMD),
	case parse(Return1) of
		{ok, Result1} ->
			{_, PagesPersec1, Timestamp_PerfTime1} = process(?MEMORY, 2, Result1),
			sleep(1000),
			Return2 = os:cmd(CMD),
			case parse(Return2) of
				{ok, Result2} ->
					{Frequency_PerfTime, PagesPersec2, Timestamp_PerfTime2} = process(?MEMORY, 2, Result2),
					PagesPersec = (PagesPersec2-PagesPersec1)/((Timestamp_PerfTime2-Timestamp_PerfTime1)/Frequency_PerfTime),
					{ok, Memory ++ lists:flatten(io_lib:format("PagesPerSec=~.2f$", [PagesPersec]))};
				Error2 ->
					Error2
			end;
		Error1 ->
			Error1
	end;
process(_, Host, User, Password, ?NETWORK, Parameter, Value)->
	{BytesReceivedPersec1, BytesSentPersec1, _, _, _, Timestamp_PerfTime1} = process(?NETWORK, 1, Value),
	{_, _, Query} = wql:string([?NETWORK|Parameter]),
	CMD = lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " \"", Query, "\""]),
	sleep(1000),
	Return = os:cmd(CMD),
	case parse(Return) of
		{ok, Result} ->
			{BytesReceivedPersec2, BytesSentPersec2, Frequency_PerfTime, PacketsOutboundErrors, PacketsReceivedErrors, Timestamp_PerfTime2} = process(?NETWORK, 1, Result),
			Interval = (Timestamp_PerfTime2-Timestamp_PerfTime1)/Frequency_PerfTime,
			String = lists:flatten(io_lib:format("BytesReceivedPerSec=~.2f$BytesSentPerSec=~.2f$PacketsErrors=~p", 
												[(BytesReceivedPersec2-BytesReceivedPersec1)/Interval, (BytesSentPersec2-BytesSentPersec1)/Interval, PacketsOutboundErrors + PacketsReceivedErrors])),
			{ok, String};
		Error ->
			Error
	end;
process(_, Host, User, Password, ?PROCESS, Parameter, Value)->
	PercentProcessorTime1 = process(?PROCESS, 1, Value),
	{_, _, Query1} = wql:string([?PROCESS|Parameter]),
	CMD1 = lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " \"", Query1, "\""]),
	sleep(1000),
	Return1 = os:cmd(CMD1),
	case parse(Return1) of
		{ok, Result1} ->
			{Count, PercentProcessorTime2, PrivateBytes, ThreadCount, WorkingSet} = process(?PROCESS, 2, Result1),
			{_, _, Query2} = wql:string([?MEMORY]),
			CMD2 = lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " \"", Query2, "\""]),
			sleep(1000),
			Return2 = os:cmd(CMD2),
			case parse(Return2) of
				{ok, Result2} ->
					{TotalVirMem, TotalPhyMem} = process(?PROCESS, 3, Result2),
					Temp = PercentProcessorTime2 - PercentProcessorTime1,
					PercentProcessorTime = if 
												Temp < 0 -> 0;
												Temp > 100 -> 100;
												true -> Temp
											end,
					Result = lists:flatten(io_lib:format("ProcessCount=~p$ThreadCount=~p$PercentProcessorTime=~.2f$WorkingSet=~.2f$MemUtilization=~.2f$PrivateBytes=~.2f$VirUtilization=~.2f$",
														[Count, ThreadCount, PercentProcessorTime / 1, WorkingSet / 1 , WorkingSet/TotalPhyMem*100, PrivateBytes / 1, PrivateBytes/TotalVirMem*100])),
					{ok, Result};
				Error2 ->
					Error2
			end;
		Error1 ->
			Error1
	end;
process(_, Host, User, Password, ?SERVICE, _, Value)->
	Tuple = hd(tl(Value)),
	ProcessId = element(2, Tuple),
	State = element(3, Tuple),
	Status = element(4, Tuple),
	case ProcessId of
		"0"->
			{ok, lists:append(["Processes=0$Started=0$ProcessName=NA$", "State=", State, "$", "Status=", Status ,"$"])};
		_ ->
			Query = wql:string(?SERVICE, ProcessId),
			CMD = lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " \"", Query, "\""]),
			Return = os:cmd(CMD),
			case parse(Return) of
				{ok, Result} ->
					ProcessName = element(1, hd(tl(Result))),
					{ok, lists:append(["Processes=1$Started=1$ProcessName=", ProcessName,"$", "State=", State, "$", "Status=", Status ,"$"])};						
				Error ->
					Error
			end
	end;
process(_, Host, User, Password, ?DIRECTORY, [Path, Recursive, Match], Value)->
	case Value of
		[]->
			{ok, "count=0$size=0$min=0$max=0"};
		_ ->
			QueryAuth = lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " "]),
			{Drive, Current} = directory(Path),
			Result = case Recursive of
				"true"->
					case getSubDirectory(QueryAuth, Drive,[Current]) of
						{ok, Children}->
							{ok, [Current|Children]};
						_ ->
							{error, "get sub directory error"}
					end;
				_ ->
					{ok, [Current]}
			end,
			case Result of
				{ok, Directories}->
					case count(QueryAuth, Drive, Directories, Match) of
						{ok, {Count, Size, Min, Max}}->
							String = lists:flatten(io_lib:format("count=~p$size=~p$min=~p$max=~p",[Count, Size, Min, Max])),
							{ok, String};
						_ ->
							{ok, "count=-1$size=0$min=0$max=0"}
					end;
				_ ->
					{ok, "count=-1$size=0$min=0$max=0"}
			end
	end.

lower([C|R], Acc)-> 
	lower(R, [string:to_lower(C)|Acc]);
lower([], Acc)->Acc.

parseDirectory(_, [])->
	[];
parseDirectory(Parent, Value)->
	parseDirectory(Parent, tl(Value), []).
	
parseDirectory(_, [], Acc)->
	Acc;
parseDirectory(Parent, [{FileName, _}|Rest], Acc)->
	parseDirectory(Parent, Rest, [Parent ++ FileName ++ "\\\\\\\\"|Acc]).
		
getSubDirectory(QueryAuth, Drive, Parents)->
	getSubDirectory(QueryAuth, Drive, Parents, []).
	
getSubDirectory(_, _, [], Acc)->
	{ok, Acc};
getSubDirectory(QueryAuth, Drive, [Parent|Rest], Acc)->
	CMD = lists:append([QueryAuth, "\"SELECT FileName FROM Win32_Directory WHERE Drive='", Drive, "' AND Path='", Parent, "'\""]),
	Return = os:cmd(CMD),
	case parseWithEmpty(Return) of
		{ok, Result} ->
			Children = parseDirectory(Parent, Result),
			case getSubDirectory(QueryAuth, Drive, Children, []) of
				{ok, GrandChildren}->
					getSubDirectory(QueryAuth, Drive, Rest, Acc ++ Children ++ GrandChildren);
				Error2->
					Error2
			end;
		Error1 ->
			Error1
	end.

path([])->
	[$\\, $\\, $\\, $\\];
path([$\\|Rest])->
	[$\\, $\\|path(Rest)];
path([C|Rest])->
	[C|path(Rest)].
	
directory(Path)->
	Directory = string:strip(Path, right, $\\),
	case length(Directory) of 
		2 -> 
			{Directory, "\\\\\\\\"};
		_ ->
			P1 = lists:sublist(Directory, 1, 2),
			P2 = tl(tl(Directory)),
			{P1, path(P2)}
	end.

subMatch(_, _, [])->
    true;
subMatch([$\\|_], _, _)->
	false;
subMatch([C|LRest], Match, [C|RRest])->
	subMatch(LRest, Match, RRest);
subMatch([C|LRest], Match, [_|_])->
    match(LRest, Match);
subMatch( _, _, _)->
    false.

match([$\\|_], _)->
	false;
match([C|LRest], [C|RRest] = Match)->
    subMatch(LRest, Match, RRest);
match([C|LRest], Match)->
    match(LRest, Match);
match([], _)->
    false.

parseFile(Files, Match, Count, Size, Min, Max)->
	case Match of
		[]->
			parseFileWithoutMatch(Files, Count, Size, Min, Max);
		_ ->
			parseFileWithMatch(Files, Match, Count, Size, Min, Max)
	end.

parseFileWithoutMatch([], Count, Size, Min, Max)->
	{Count, Size, Min, Max};
parseFileWithoutMatch([{FileSize, LastModified, _}|Rest], Count, Size, Min, Max)->
	Time = lists:sublist(LastModified, 1, 14),
	NewMax = if
				Time > Max->Time;
				true->Max
			end,
	NewMin = if
				Time < Min->Time;
				true->Min
			end,
	NewSize = Size + list_to_integer(FileSize),
	parseFileWithoutMatch(Rest, Count+1, NewSize, NewMin, NewMax).

parseFileWithMatch([], _, Count, Size, Min, Max)->
	{Count, Size, Min, Max};
parseFileWithMatch([{FileSize, LastModified, Name}|Rest], Match, Count, Size, Min, Max)->
	case match(lists:reverse(Name), Match) of
		true->
			Time = lists:sublist(LastModified, 1, 14),
			NewMax = if
						Time > Max->Time;
						true->Max
					end,
			NewMin = if
						Time < Min->Time;
						true->Min
					end,
			NewSize = Size + list_to_integer(FileSize),
			parseFileWithMatch(Rest, Match, Count+1, NewSize, NewMin, NewMax);
		_ ->
			parseFileWithMatch(Rest, Match, Count, Size, Min, Max)
	end.
	
count(QueryAuth, Drive, Directories, Match)->
	count(QueryAuth, Drive, Directories, lower(Match, []), 0, 0, "21000101000000", "0").

count(_, _, [], _, Count, Size, Min, Max)->
	{ok, {Count, Size, Min, Max}};
count(QueryAuth, Drive, [Path|Rest], Match, Count, Size, Min, Max)->
	CMD = lists:append([QueryAuth, "\"SELECT FileSize, LastModified FROM CIM_DataFile WHERE Drive='", Drive, "' AND Path='", Path, "'\""]),
	Return = os:cmd(CMD),
	case parseWithEmpty(Return) of
		{ok, Result} ->
			case Result of
				[]->
					count(QueryAuth, Drive, Rest, Match, Count, Size, Min, Max);
				_ ->
					{NewCount, NewSize, NewMin, NewMax} = parseFile(tl(Result), Match, Count, Size, Min, Max),
					count(QueryAuth, Drive, Rest, Match, NewCount, NewSize, NewMin, NewMax)
			end;
		Error1 ->
			Error1
	end.

execute(OS, Host, User, Password, Parameters)->
	{Type, Process, Query} = wql:string(Parameters),
	CMD = lists:append([?WMIC, " -d -1 -U ", User, "%", Password, " //", Host, " \"", Query, "\""]),
	Return = os:cmd(CMD),
	case parse(Return) of
		{ok, Result} ->
			case Process of
				[] ->
					process(Type, Result);
				Rest ->
					process(OS, Host, User, Password, Type, Rest, Result)
			end;
		Error ->
			Error
	end.
