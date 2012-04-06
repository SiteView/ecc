-module(proxy).

-compile(export_all).

-define(SNAME, "wmi").

-export([cpu/4, memory/4, disk/4, disk/5, service/4, service/5, process/4, process/5, network/4, network/5, directory/5, directory/6, directory/7]).

get_proxy_nodes()->
	[get_proxy_node()].
get_proxy_node()->
	case server_conf:getWmiNode() of
		undefined->
			{ok, Host} = inet:gethostname(),
			list_to_atom( ?SNAME ++ "@" ++ Host);
		Node->
			Node
	end.

parserNode(Node) ->
	Node_List = atom_to_list(Node),
	Len = string:str(Node_List,"@")+1,
	Size = string:len(Node_List),
	string:substr(Node_List,Len,Size).

host_process(Host)->
	Ip_String = string:strip(Host, left, $\\),
	Node_addrs = get_proxy_node(),
	{ok,Node_Ip} = inet:getaddr(parserNode(Node_addrs),inet),
	{IP1,IP2,IP3,IP4} = Node_Ip,
	Ip_S = integer_to_list(IP1) ++ "." ++ integer_to_list(IP2) ++ "." ++ integer_to_list(IP3) ++ "." ++ integer_to_list(IP4),
	case Ip_S =:= Ip_String of
		true ->
			"127.0.0.1";
		_ ->
			Ip_String
	end.

disk_process(Disk)->
	case length(Disk) of
		1 ->
			string:to_upper(Disk ++ ":");
		_ ->
			string:to_upper(Disk)
	end.


convert_cpu_result(Result)->
	KVList = lists:map(fun(X)->list_to_tuple(string:tokens(X, "=")) end, string:tokens(Result, ";")),
	{_, Utilization} = lists:keyfind("utilization", 1, KVList),
	{_, Detailutilization} = lists:keyfind("detailutilization", 1, KVList),
	CPUList = lists:map(fun(X)->string:tokens(lists:nthtail(3, X), ":") end, string:tokens(Detailutilization, "-")),
	SortedCPU = lists:keysort(1, lists:map(fun([X, Y])->{list_to_integer(X), list_to_integer(Y)} end, CPUList)),
	CPUS = lists:map(fun({_, V})-> V end, SortedCPU),
	[list_to_integer(Utilization), 0, 0, length(CPUS)|CPUS].

cpu(OS, Host, User, Password)->
	case rpc:call(get_proxy_node(), wmi, cpu, [OS, host_process(Host), User, Password]) of
		{ok, Result} ->
				convert_cpu_result(Result);
			Error ->
				io:format("wmi->cpu error is ~p~n", [Error]),
				[-1,0,0,1]
	end.

convert_memory_result(Result)->
	%%"TotalPhyMem=1918.40$TotalVirMemory=2047.88$FreePhyMem=1106.44$FreeVirMem=2004.46$PhyMemUsage=42.32$PercentUsed=2.12$PagesPerSec=0.00$"
	KVList = lists:map(fun(X)->list_to_tuple(string:tokens(X, "=")) end, string:tokens(Result, "$")),
	{_, PagesPerSec} = lists:keyfind("PagesPerSec", 1, KVList),
	%~ {_, PercentUsed} = lists:keyfind("PercentUsed", 1, KVList),
	{_, TotalPhyMem} = lists:keyfind("TotalPhyMem", 1, KVList),
	{_, TotalVirMemory} = lists:keyfind("TotalVirMemory", 1, KVList),
	{_, FreeVirMem} = lists:keyfind("FreeVirMem", 1, KVList),
	{_, FreePhyMem} = lists:keyfind("FreePhyMem", 1, KVList),
	Total = list_to_float(TotalPhyMem) + list_to_float(TotalVirMemory),
	PercentUsed = (Total- list_to_float(FreePhyMem) - list_to_float(FreeVirMem)) / Total * 100,
	[PercentUsed, list_to_float(TotalVirMemory), list_to_float(FreeVirMem), list_to_float(PagesPerSec)].

memory(OS, Host, User, Password)->
	case rpc:call(get_proxy_node(), wmi, memory, [OS, host_process(Host), User, Password]) of
		{ok, Result} ->
				io:format("memory come here ~p~n",[Result]),
				NewResult = convert_memory_result(Result),
				io:format("NewResult come here ~p~n",[NewResult]),
				{ok, NewResult};
			Error ->
				io:format("wmi->memory error is ~p~n", [Error]),
				{error, "no data"}
	end.

disk(OS, Host, User, Password)->
	case rpc:call(get_proxy_node(), wmi, disk, [OS, host_process(Host), User, Password]) of
		{ok, Result} ->
				string:tokens(Result, ":$");
			Error ->
				io:format("wmi->disk error is ~p~n", [Error]),
				{error, "no data"}
	end.

convert_float([])->
	0.0;
convert_float(Value)->
	Result = case lists:any(fun(X)-> X =:= $. end, Value) of
		false ->
			Value ++ ".0";
		_ ->
			Value
	end,
	 {Float, _} = string:to_float(Result),
	 Float.

disk(OS, Host, User, Password, Disk)->
	case rpc:call(get_proxy_node(), wmi, disk, [OS, host_process(Host), User, Password, disk_process(Disk)]) of
		{ok, Result} ->
				%%{ok,"percentFull=90.94$Mbfree=3621.41$TotalSize=39990.25$"}
				List = lists:map(fun(X)->list_to_tuple(string:tokens(X, "=")) end, string:tokens(Result, "$")),
				{_, Full} = lists:keyfind("percentFull", 1, List),
				{_, Free} = lists:keyfind("Mbfree", 1, List),
				{_, TotalSize} = lists:keyfind("TotalSize", 1, List),
				[round(convert_float(Full)), (convert_float(TotalSize) - convert_float(Free)) * 1000000, convert_float(TotalSize)  * 1000000];
			Error ->
				io:format("wmi->disk error is ~p~n", [Error]),
				{error, "no data"}
	end.

service(OS, Host, User, Password)->
	case rpc:call(get_proxy_node(), wmi, service, [OS, host_process(Host), User, Password]) of
		{ok, Result}->
			{ok, string:tokens(Result, "$")};
		Error ->
			io:format("wmi->service error is ~p~n", [Error]),
			{error, "no data"}
	end.


convert_service_result(Result)->
	%%"Processes=0$Started=0$ProcessName=NA$State=Stopped$Status=OK$"
	KVList = lists:map(fun(X)->list_to_tuple(string:tokens(X, "=")) end, string:tokens(Result, "$")),
	{_, State} = lists:keyfind("State", 1, KVList),
	case State of
		"Running"->
			[1, 0, {false,false}];
		"Stopped"->
			[-1, 0, {false,false}];
		"Paused"->
			[-2, 0, {false,false}];
		_ ->
			[0, 0, {false,false}]
	end.

service(OS, Host, User, Password, Service)->
	case rpc:call(get_proxy_node(), wmi, service, [OS, host_process(Host), User, Password, Service]) of
		{ok, []} ->
			[0, 0, {false,false}];
		{ok, Result} ->
			convert_service_result(Result);
		Error ->
			io:format("wmi->service error is ~p~n", [Error]),
			[0, 0, {false,false}]
	end.

process(OS, Host, User, Password)->
	rpc:call(get_proxy_node(), wmi, process, [OS, host_process(Host), User, Password]).

convert_process_result(Result)->
	%%"ProcessCount=3$ThreadCount=26$PercentProcessorTime=0.00$WorkingSet=99896.00$\t\t\t\t\t\t MemUtilization=5.09$PrivateBytes=122488.00$VirUtilization=5.84$"
	KVList = lists:map(fun(X)->list_to_tuple(string:tokens(X, "=")) end, string:tokens(Result, "$")),
	{_, ProcessCount} = lists:keyfind("ProcessCount", 1, KVList),
	{_, PercentProcessorTime} = lists:keyfind("PercentProcessorTime", 1, KVList),
	{_, PrivateBytes} = lists:keyfind("PrivateBytes", 1, KVList),
	[list_to_float(PercentProcessorTime), list_to_integer(ProcessCount), 0, 0, list_to_float(PrivateBytes) * 1024].

process(OS, Host, User, Password, Process)->
	case rpc:call(get_proxy_node(), wmi, process, [OS, host_process(Host), User, Password, Process]) of
		{ok, Result} ->
			convert_process_result(Result);
		Error ->
			io:format("wmi->process error is ~p~n", [Error]),
			[0, 0, 0, 0, 0]
	end.

network(OS, Host, User, Password)->
	rpc:call(get_proxy_node(), wmi, network, [OS, host_process(Host), User, Password]).

network(OS, Host, User, Password, Network)->
	rpc:call(get_proxy_node(), wmi, network, [OS, host_process(Host), User, Password, Network]).


directory(OS, Host, User, Password, Path)->
	directory(OS, Host, User, Password, Path, false, []).

directory(OS, Host, User, Password, Path, Recursive)->
	directory(OS, Host, User, Password, Path, Recursive, []).

path_process(Path)->
	path_process(string:strip(Path, right, $\\), []).

path_process([], Acc)->
	lists:reverse(Acc);
path_process([$\\|Rest], Acc)->
	path_process(Rest, [$\\, $\\|Acc]);
path_process([C|Rest], Acc)->
	path_process(Rest, [C|Acc]).

recursive_process(false)->
	"true";
recursive_process(_)->
	"false".

time_process("0")->
	0;
time_process(String)->
	Year = list_to_integer(lists:sublist(String, 1, 4)),
	Month = list_to_integer(lists:sublist(String, 5, 2)),
	Day = list_to_integer(lists:sublist(String, 7, 2)),
	Hour = list_to_integer(lists:sublist(String, 9, 2)),
	Minute = list_to_integer(lists:sublist(String, 11, 2)),
	Second = list_to_integer(lists:sublist(String, 13, 2)),
	calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}).
time_process1(String)->
	Year = lists:sublist(String, 1, 4),
	Month = lists:sublist(String, 5, 2),
	Day = lists:sublist(String, 7, 2),
	Hour = lists:sublist(String, 9, 2),
	Minute = lists:sublist(String, 11, 2),
	Second = lists:sublist(String, 13, 2),
	Year++"-"++Month++"-"++Day++" "++Hour++":"++Minute++":"++Second.	
convert_directory_result(Result)->
	KVList = lists:map(fun(X)->list_to_tuple(string:tokens(X, "=")) end, string:tokens(Result, "$")),
	{_, Count} = lists:keyfind("count", 1, KVList),
	{_, Size} = lists:keyfind("size", 1, KVList),
	{_, Max} = lists:keyfind("max", 1, KVList),
	{_, Min} = lists:keyfind("min", 1, KVList),
	MinValue = time_process1(Min),
	MaxValue = time_process1(Max),
	{true, true, list_to_integer(Count), list_to_integer(Size), MaxValue, MinValue}.
%% 	Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
%% 	case {MinValue, MaxValue} of
%% 		{0, 0}->
%% 			{false, false, list_to_integer(Count), list_to_integer(Size), MaxValue, MinValue};
%% 		{_, 0}->
%% 			{true, true, list_to_integer(Count), list_to_integer(Size), MaxValue, MaxValue};
%% 		_ ->
%% 			{true, true, list_to_integer(Count), list_to_integer(Size), (Now - MaxValue) div 60, (Now - MinValue) div 60}
%% 	end.

directory(OS, Host, User, Password, Path, Recursive, Match)->
	case rpc:call(get_proxy_node(), wmi, directory, [OS, host_process(Host), User, Password, Path, recursive_process(Recursive), Match]) of
		{ok, Result} ->
			convert_directory_result(Result);
		Error ->
			io:format("wmi->directory error is ~p~n", [Error]),
			{true, false, 0, 0, 0, 0}
	end.

