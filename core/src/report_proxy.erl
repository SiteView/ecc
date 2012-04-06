%% ---
%%your comment
%%
%%---
-module(report_proxy).
-behaviour(gen_server).

-compile(export_all).

-define(SNAME, "master").
%% -define(MasterNode, 'debug@192.168.6.166').

-include("monitor.hrl").
-include("historyreport.hrl").
-include("monitor_template.hrl").
-define(TIMEOUT,5).


-export([start_link/0,stop/0]).

%-record(state, {}).
-record(state, {conn}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([get_all_monitors/0,find_object/1,get_parent_id/1,qc/3,q/3,get_object/1,get_template/1,queryReportData/8,qdstr/3,qbrowser/3,q_bycount/4]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE,stop).


init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{}}.
%% 	case pgsql:connect(localhost, "root", "root", [{database,"postgres"}]) of
%% 		{ok, C}->
%% 			{ok, #state{conn = C}};
%% 		_->
%% 			{ok, #state{}}
%% 	end.

qbrowser(StartDate,EndDate,Ids) ->
	gen_server:call(?MODULE, {qbrowser,StartDate,EndDate,Ids}, infinity).

qdstr(StartDate,EndDate,Ids) ->
	gen_server:call(?MODULE, {qdstr,StartDate,EndDate,Ids}, infinity).

queryReportData(Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount)->
	io:format("queryReportData PocessId: ~p ~n", [self()]),
	gen_server:call(?MODULE, {queryReportData,Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount}, infinity).

%% @spec get_AllSelMonitors(X) -> ok 
%% Obj = term()
%% @doc  
filter_MonitorStr([], AllMonitors,Ret) ->
	Ret;
filter_MonitorStr([H|T], AllMonitors, Ret) ->	
%% 	io:format("filter_MonitorStr: ~p ~n", [H]),
    case lists:keysearch(H, 2, AllMonitors) of 
	    {value,{_, ID}} ->
%% 			Monitor = report_proxy:find_object(ID),
			filter_MonitorStr(T, AllMonitors, Ret ++ [ID]);
		_ ->
			filter_MonitorStr(T, AllMonitors, Ret)
	end.

print_process_memory([], _)->
	done;
print_process_memory(_, 0)->
	done;
print_process_memory([{PID, Memory}|R], N)->
	io:format("P:~p M:~p Info:~p~n", [PID, Memory, erlang:process_info(PID)]),
	print_process_memory(R, N-1).

print_process_memory_topN(N)->
	PL = processes(),
	ML = lists:map(fun(X)->{X, element(2, erlang:process_info(X, memory))} end, PL),
	SL = lists:sort(fun(X1, X2)-> element(2, X1) > element(2, X2) end, ML),
	io:format("Process---->~p~n Memory---->~p~n", [length(PL), erlang:memory()]),
	print_process_memory(SL, N).

split([])->
	done;
split([H|R])->
	MonitorID = H#monitorlog.id,
	Records = get({value, MonitorID}),
	case Records of
		undefined->
			put({value, MonitorID}, [H]);
		_ ->
			put({value, MonitorID}, [H|Records])
	end,
	put({key, MonitorID}, MonitorID),
	split(R).

precidate({{key, X}, X})->
	true;
precidate(_)->
	false.

clear()->
	L = get(),
	DL = lists:filter(fun(X)->precidate(X) end, L),
	lists:foreach(fun({_, X})-> erase({key, X}), erase({value, X}) end, DL).

handle_call({queryReportData, Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount}, _, State) ->
	io:format("queryReportData1111: ~p ~n", [Ids]),
	C = get_connect(State),
	io:format("queryReportData1: ~p ~n", [Ids]),
	Parms = [{comperss,Compress},{startTime, calendar:datetime_to_gregorian_seconds(StartTime)},{endTime, calendar:datetime_to_gregorian_seconds(EndTime)},{dstrNeed,DstrNeed}] ++
	[{dstrStatusNoNeed, DstrStatusNoNeed},{return_value_filter, Return_value_filter},{truedata, ByCount}, {count, 20}],
	MonitorStrSrc = string:tokens(Ids, ","),
	AllMonitors = report_proxy:get_all_monitors(),
	MonitorStr = filter_MonitorStr(MonitorStrSrc, AllMonitors, []),
%% 	io:format("queryReportData333: ~p ~n", [MonitorStr]),
%	try-1	
%% 	case length(MonitorStr) of
%% 		1->
%% 			Ret = queryReportData_child1(MonitorStr, Parms, [], []);			
%% 		_->	
%% 			{_, AllData} = report_proxy:q(StartTime, EndTime, MonitorStr),
%% %% 			{_, AllData} = report_proxy:q(StartTime, EndTime, MonitorStr),		
%% %% 			io:format("queryReportData1ddddddddd: ~p ~n", [allDatatoData(AllData, [])]),			
%% 			Ret = queryReportData_child1(MonitorStr, Parms, allDatatoData(AllData, []), [])
%% 	end,

%% 	try-2
	case length(MonitorStr) == 1 andalso (ByCount == true) of
		true->
			Ret = queryReportData_child1(MonitorStr, Parms, C, [], []);			
		_->	
%% 			io:format("Start-----------------------~n"),
%% 			print_process_memory_topN(3),
%% 			{_, AllData} = report_proxy:q2(StartTime, EndTime, MonitorStr, State),			
			AllData = report_proxy:q_bytime(StartTime, EndTime, MonitorStr, C),
%% 			io:format("==============================~p~n", [AllData]),
			split(AllData),			
%% 			print_process_memory_topN(3),
%% 			io:format("---------------------------end~n"),
%% 			Ret = queryReportData_child1(MonitorStr, Parms, allDatatoData(AllData, []), []),
%% 			Data = allDatatoData(AllData, []),
			
%% 			Ret2 = lists:map(F, MonitorStr),
							
			F4 = fun(X)->
						Records = case get({value, list_to_atom(X)}) of
							  undefined->
								  [];
							  Result->
								  lists:reverse(Result)
						 end,
						 {X, Records}
				 end,
			ML = lists:map(F4, MonitorStr),
%% 			io:format("queryReportData ML: ~p ~n", [ML]),
			F = fun({X, Records})->
%% 				io:format("-------------------------->~p~n", [X]),
%% 				Records = case get({value, list_to_atom(X)}) of
%% 							  undefined->
%% 								  [];
%% 							  Result->
%% 								  lists:reverse(Result)
%% 						  end,
				case Records of
					[]->
						Obj = reportdata:new();
					_->	
%% 						io:format("queryReportData1 AllData: ~p ~n", [X]),
						%%Obj = reportdata:new(filter_data(AllData, [{id,'=',list_to_atom(X)}]))
						Obj = reportdata:new(Records)
				end,					
%% 				Obj = reportdata:new([]),
				Obj:readFromHashMap([{monitors, X}|Parms]),
				Ret = Obj:createFromQuery(""),
				Obj:delete(),
				Ret
			end,			
 			Ret2 = pmap(F, ML),		
			clear(),
%% 			Ret2 = pmap(F, [{X, Parms} || X <- MonitorStr]),
			Ret = makeReportData_child(Ret2, []),
			io:format("queryReportData 2 Process Total Data Size: ~p ~n", [length(AllData)])
	end,
%	try-3	
%% 	Ret = queryReportData_child(MonitorStr, Parms, []),
	
%	try-4
%% 	F = fun(X)->
%% 		Obj = reportdata:new([]),
%% 		Obj:readFromHashMap(Parms ++ [{monitors, X}]),
%% 		Ret = Obj:createFromQuery(""),
%% 		Obj:delete(),
%% 		erlang:garbage_collect(),
%% 		Ret
%% 	end,
%% 	Ret2 = lists:map(F, MonitorStr),
%%  	Ret2 = pmap(F, MonitorStr),	
%% 	Ret = makeReportData_child(Ret2, []),	
	io:format("queryReportData 2 StartTime: ~p ~n", [StartTime]),
	io:format("queryReportData 2 EndTime : ~p ~n", [EndTime]),
	{reply,Ret,State#state{conn=C}};
handle_call(connection, _, State) ->
	{reply,{ok,State},State};
handle_call({qdstr,StartDate,EndDate,Ids}, _, State) -> 	 
	C = get_connect(State),
	Temids=string:tokens(Ids, ","),
	 F = fun(X)->
		io:format("qdstr 1: ~p ~n", [X]), 
%% 		 {_,DataSrc}=q(StartDate,EndDate,[X]),
		 DataSrc = report_proxy:q_bytime(StartDate,EndDate,[X], C),
%% 		io:format("qdstr 2: ~p ~n", [Tmp]),
%% 		 Datalog=builddata(Tmp,[]),
	 	 Data = getDstrList(DataSrc, [X], []),
		 Data
	 end,
 	Ret2 = pmap(F, Temids),
	Ret = makeReportData_child(Ret2, []),
	{reply,Ret,State#state{conn=C}};
handle_call({qbrowser,StartDate,EndDate,Ids}, _, State) ->
	C = get_connect(State),	
	io:format("qbrowser 12: ~p ~n", [Ids]),
	Temids=string:tokens(Ids, ","),
%% 	 F = fun(X)->
%% %% 		io:format("qdstr 12: ~p ~n", [X]), 
%% 		 {_,DataSrc}=q(StartDate,EndDate,[X]),
%% %% 		io:format("qdstr 2: ~p ~n", [X]),
%% %% 		 Datalog=builddata(Tmp,[]),
%% 	 	 Data = getBDstrList(DataSrc, [X], []),
%% %% 		erlang:garbage_collect(),
%% 		 Data
%% 	 end,
%% 			io:format("Start------------------------~n"),
%% 			print_process_memory_topN(3),
%% 	{_, AllData} = report_proxy:q(StartDate,EndDate, Temids),
	AllData = report_proxy:q_bytime(StartDate,EndDate,Temids, C),
	split(AllData),
	F4 = fun(X)->
				Records = case get({value, list_to_atom(X)}) of
					  undefined->
						  [];
					  Result->
						  lists:reverse(Result)
				 end,
				 {X, Records}
		 end,
	ML = lists:map(F4, Temids),
	F = fun({X, Records})->
		getBDstrList(Records, [X], [])
	end,
	Ret2 = pmap(F, ML),			
	clear(),
	Ret = makeReportData_child(Ret2, []),
	{reply,Ret,State#state{conn=C}};
handle_call({q, Date}, _, State) ->
	{reply,Date,State};
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_connect(State)->
	State#state.conn.
%% 	case erlang:is_pid(State#state.conn) andalso erlang:is_process_alive(State#state.conn) of
%% 		false->
%% 			case pgsql:connect(localhost, "root", "root", [{database,"postgres"}]) of
%% 				{ok, C} -> 
%% 					C;
%% 				_->
%% 					undefined
%% 			end;
%% 		_->
%% 			State#state.conn
%% 	end.

encode_measurement(C)->
	term_to_binary(C).
%% 	binary_to_list(base64:encode(term_to_binary(C))).
	
decode_measurement(E)->
%% 	binary_to_term(base64:decode(E)).
	binary_to_term(E).
%% 	base64:decode(E).
	
convert_row([],Acc)->lists:reverse(Acc);
convert_row([R|T],Acc)->
	Id = list_to_atom(binary_to_list(element(1,R))),
%% 	Name = binary_to_list(element(3,R)),
	Name = element(3,R),
%% 	Time = decode_time(binary_to_list(element(4,R))),
	
	Temp = element(2, element(4,R)),
	Seconds = round(element(3,Temp)),
	Time = {element(1, element(4, R)), setelement(3, Temp, Seconds)},
	
	Category = list_to_atom(binary_to_list(element(5,R))),
%% 	Desc = binary_to_list(element(6,R)),
	Desc = element(6,R),
%% 	Measure = decode_measurement(element(7,R)),
 	Measure = element(7,R),
	Log = #monitorlog{id=Id,name=Name,time=Time,category=Category,desc=Desc,measurement=Measure}, 
	convert_row(T,[Log|Acc]).
	
encode_time({{Y,M,D},{HH,MM,SS}})->
	lists:flatten(io_lib:format("~w-~w-~w ~w:~w:~w",[Y,M,D,HH,MM,SS])).

decode_time(TimeStr)->
	[Date,Time] = string:tokens(TimeStr," "),
	[Y,M,D] = string:tokens(Date,"-"),
	[HH,MM,SS] = string:tokens(Time,":"),
	{{list_to_integer(Y),list_to_integer(M),list_to_integer(D)},
	{list_to_integer(HH),list_to_integer(MM),list_to_integer(SS)}}.
convert_ofbizrow([],Acc)->lists:reverse(Acc);
convert_ofbizrow([R|T],Acc)->
	[Id,Time,Category,Desc,Measure,Name]=R,
	Log = #monitorlog{id=Id,name=Name,time=Time,category=Category
					 ,desc=Desc,measurement=Measure }, 
	convert_ofbizrow(T,[Log|Acc]).
q_bycount(Ids, Count, DaysLimit, C)->
Params=[Ids,"","",Count],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			[];
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					[];
				{_,R} ->
					Ret=convert_ofbizrow(R,[])
			end
	end.

q_bytime(StartTime, EndTime, Ids, C)->
Params=[Ids,StartTime,EndTime,""],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 [];
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					[];
				{_,R} ->
					Ret=convert_ofbizrow(R,[])
			end
	end.

%% q_bycount(Ids, Count, DaysLimit, C)->
%% %% 	{_, State} = gen_server:call(?MODULE, connection, infinity),
%% %% 	C = get_connect(State),
%% 	EndDate =  date(),
%% 	StartDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(EndDate)-DaysLimit+1),
%%  	Dates = lists:reverse(nextPath([], StartDate, EndDate)),
%% 	IdsStr =  string:join(lists:map(fun(X)-> "'" ++ X ++ "'" end,Ids),","),
%% 	App = atom_to_list(dbcs_base:get_app()),
%% 	F = fun(X,R)->
%% 			{Y,M,D} = X,
%% 			Left = Count - length(R),
%% 			if
%% 				Left > 0 ->
%% 					TblName = lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D])),
%% 					Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and id in (~s) order by time desc limit ~w;",
%% 										[TblName,App,IdsStr,Left])),
%% 					case pgsql:equery(C,Sql,[]) of
%% 						{ok,_,Rows}->
%% 							lists:reverse(convert_row(Rows,[])) ++ R;							
%% 						_->
%% 							R
%% 					end;
%% 				true->
%% 					R
%% 			end
%% 	end,	
%% 	Ret = lists:foldl(F,[],Dates),
%% 	Ret.
%% 
%% q_bytime(StartTime, EndTime, Ids, C)->
%% 	%{_, State} = gen_server:call(?MODULE, connection, infinity),
%% %% 	C = get_connect(State),
%% 	{StartDate, _} = StartTime,
%% 	{EndDate, _} = EndTime,	
%% 	Dates = nextPath([], StartDate, EndDate),
%% 	IdsStr =  string:join(lists:map(fun(X)-> lists:flatten(io_lib:format("'~s'",[X])) end,Ids),","),
%% 	App = atom_to_list(dbcs_base:get_app()),
%% 	F = fun(X,R)->
%% 			{Y,M,D} = X,
%% 			TblName = lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D])),
%% 			Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and id in (~s) and time >= '~s' and time < '~s';",
%% 								[TblName,App,IdsStr,encode_time(StartTime),encode_time(EndTime)])),
%% 			case pgsql:equery(C,Sql,[]) of
%% 				{ok,_,Rows}->
%% %% 					[convert_row(Rows,[])|R];
%% 					R ++ convert_row(Rows,[]);
%% 				_->
%% 					R
%% 			end 
%% 		end,	
%% 	Ret = lists:foldl(F,[],Dates),	
%% 	Ret.

compare(StartTime, EndTime) ->
	L = calendar:date_to_gregorian_days(StartTime),
	L1 = calendar:date_to_gregorian_days(EndTime),
	if(L > L1) ->
		true;
	true ->
		false
	end.

nextPath(Times, StartTime, EndTime) ->
	case compare(StartTime, EndTime) of 
		true -> 
			Times;
		false ->
			T2 = Times ++ [StartTime],
			nextPath(T2, sv_datetime:next_date(StartTime), EndTime)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeReportData_child([], Ret)->
	Ret;
makeReportData_child([H|T], Ret)->
	case is_list(H) of
		true->
			makeReportData_child(T, Ret ++ H);
		_->
			io:format("makeReportData_child Error H:~p~n", [H]),
			makeReportData_child(T, Ret)
%% 			nothing
	end.
	
pmap(F, L) ->   
   S = self(),  
   Pids = lists:map(fun(I) ->   
     spawn(fun() -> do_fun(S, F, I) end)  
   end, L),  
   gather(Pids).  
   
gather([H|T]) ->  
   receive  
     {H, Result} -> [Result|gather(T)]  
   end;  
gather([]) ->  
   [].  
   
do_fun(Parent, F, I) ->
%% 	io:format("do_fun: ~p ~n", [I]),
     Parent ! {self(), (catch F(I))}.

filter_data([],_)->
	[];
filter_data(Data,[])->
	Data;
filter_data(Data,[H|T])->
	F=fun(X)->
		case H of
			{id,'in',V11}->
				lists:member(atom_to_list(X#monitorlog.id), V11);
			{id,'=',V1}->
				X#monitorlog.id=:=V1;
			{id,'<',V2}->
				X#monitorlog.id<V2;
			{id,'>',V3}->
				X#monitorlog.id>V3;
			{name,'=',V4}->
				X#monitorlog.name=:=V4;
			{time,'>',V5}->
				X#monitorlog.time>V5;
			{time,'>=',V6}->
				(X#monitorlog.time>V6) or (X#monitorlog.time =:= V6);
			{time,'=',V7}->
				X#monitorlog.time =:= V7;
			{time,'<',V8}->
				X#monitorlog.time < V8;
			{time,'<=',V9}->
				(X#monitorlog.time<V9) or (X#monitorlog.time=:=V9);
			{category,'=',V10}->
				X#monitorlog.category =:= V10;
			_->
				false
		end
	end,
	filter_data(lists:filter(F,Data),T).

allDatatoData([], Ret)->
	Ret;
allDatatoData([H|T], Ret)->	
	case H of
		{eof, ChildData} ->
			case is_list(ChildData) of
				true ->
					allDatatoData(T, Ret ++ ChildData);
				_ ->
					allDatatoData(T, Ret)
			end;
		_ ->
%% 			io:format("allDatatoData: ~p ~n", ["2"]),
			allDatatoData(T, Ret)
	end.

queryReportData_child1([], Parms, DbCon, Data, Ret)->
	Ret;
queryReportData_child1([H|T], Parms, DbCon, Data, Ret)->	
	case Data of
		[]->
			Obj = reportdata:new();
		_->
%% 			io:format("queryReportData_child22222: ~p ~n", [H]),
			Obj = reportdata:new(filter_data(Data, [{id,'=',H}]))
	end,
	
	Obj:readFromHashMap(Parms ++ [{monitors, H}] ++ [{dbCon, DbCon}]),
%%     io:format("queryReportData_child2: ~p ~n", [H]),
	Ret1 = Obj:createFromQuery(""),
%% 	io:format("queryReportData_child3: ~p ~n", [H]),
	Obj:delete(),
	queryReportData_child1(T, Parms, DbCon, Data, Ret ++ Ret1).


%% queryReportData(Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount)->
%% 	io:format("queryReportData1: ~p ~n", [Ids]),
%% 	Parms = [{comperss,Compress},{startTime, calendar:datetime_to_gregorian_seconds(StartTime)},{endTime, calendar:datetime_to_gregorian_seconds(EndTime)},{dstrNeed,DstrNeed}] ++
%% 	[{dstrStatusNoNeed, DstrStatusNoNeed},{return_value_filter, Return_value_filter},{truedata, ByCount}, {count, 20}],
%% 	MonitorStr = string:tokens(Ids, ","),
%% 	case length(MonitorStr) of
%% 		1->
%% 			Ret = queryReportData_child(MonitorStr, Parms, [], []);			
%% 		_->	
%% 			{_, AllData} = report_proxy:q(StartTime,	EndTime, MonitorStr),		
%% %% 			io:format("queryReportData1ddddddddd: ~p ~n", [allDatatoData(AllData, [])]),			
%% 			Ret = queryReportData_child(MonitorStr, Parms, allDatatoData(AllData, []), [])
%% 	end,
%% %% 	Ret = queryReportData_child(MonitorStr, Parms, []),
%% 	io:format("queryReportData2: ~p ~n", [Ids]),
%% 	io:format("queryReportData 2 StartTime: ~p ~n", [StartTime]),
%% 	io:format("queryReportData 2 EndTime : ~p ~n", [EndTime]),
%% 	Ret.
%% 

%% queryReportDataOld(Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount)->
%% 	Parms=[{monitors,Ids},{comperss,Compress},{startTime, calendar:datetime_to_gregorian_seconds(StartTime)},{endTime, calendar:datetime_to_gregorian_seconds(EndTime)},{dstrNeed,DstrNeed}] ++
%% 	[{dstrStatusNoNeed, DstrStatusNoNeed},{return_value_filter, Return_value_filter},{truedata, ByCount}, {count, 20}],
%% 	Obj = reportdata:new(),
%% 	Obj:readFromHashMap(Parms),
%%     Ret = Obj:createFromQuery(""),
%% 	Obj:delete(),
%% 	Ret.
%% 

queryReportData_child([], Parms, Ret)->
	Ret;
queryReportData_child([H|T], Parms, Ret)->
	Obj = reportdata:new([]),
	Obj:readFromHashMap(Parms ++ [{monitors, H}]),
	Ret1 = Obj:createFromQuery(""),
	Obj:delete(),
	queryReportData_child(T, Parms, Ret ++ Ret1).

%% queryReportData(Ids, Compress, StartTime, EndTime, DstrNeed, DstrStatusNoNeed, Return_value_filter, ByCount)->
%% 	io:format("queryReportData1: ~p ~n", [Ids]),
%% 	Parms = [{comperss,Compress},{startTime, calendar:datetime_to_gregorian_seconds(StartTime)},{endTime, calendar:datetime_to_gregorian_seconds(EndTime)},{dstrNeed,DstrNeed}] ++
%% 	[{dstrStatusNoNeed, DstrStatusNoNeed},{return_value_filter, Return_value_filter},{truedata, ByCount}, {count, 20}],
%% 	MonitorStr = string:tokens(Ids, ","),
%% 	Ret = queryReportData_child(MonitorStr, Parms, []),
%% 	io:format("queryReportData 2 StartTime: ~p ~n", [StartTime]),
%% 	io:format("queryReportData 2 EndTime : ~p ~n", [EndTime]),
%% 	Ret.
%% 
%%=============================qbrowser====================== 
%% 
%% @spec getDstrList(X) -> ok 
%% Obj = term()
%% @doc
getBDstrList(DstrList, [], MonitorDstrList)->
	MonitorDstrList;
getBDstrList(DstrList, [H|T], MonitorDstrList)->
    ChildSrcDstrList=[X||X<-DstrList,X#monitorlog.id=:=list_to_atom(H)],
	ChildDesDstrList = getBMonitorDstrList(ChildSrcDstrList, []),
	getBDstrList(DstrList, T, MonitorDstrList ++ [{H, ChildDesDstrList}]).

%% @spec getDstrListChild(X) -> ok 
%% Obj = term()
%% @doc
getBMonitorDstrList([], DstrList)->
	DstrList;
getBMonitorDstrList([H|T], DstrList)->
	getBMonitorDstrList(T, DstrList ++ [{H#monitorlog.time,H#monitorlog.category}]).
%%
%%=============================qbrowser======================
%% 
%% 
%%=============================dstr =========================
%% 

builddata([],Data)->
	Data;
builddata([H|T],Data)->
	builddata(T,Data++element(2,H)).

%% @spec getDstrListChild(X) -> ok 
%% Obj = term()
%% @doc
getMonitorDstrList([], DstrList)->
	DstrList;
getMonitorDstrList([H|T], DstrList)->
	getMonitorDstrList(T, DstrList ++ [{H#monitorlog.time,{H#monitorlog.category, H#monitorlog.desc, measurementtoString(H#monitorlog.measurement, [])}}]).

%% @spec measurementtoString(X) -> ok 
%% Obj = term()
%% @doc
measurementtoString([], Str)->
	Str;
measurementtoString([H|T], Str)->
	{Key, Value} = H,
%% 	measurementtoString(T, Str ++ to_list(Key) ++ "=" ++ to_list(Value) ++ ",").
	measurementtoString(T, Str ++ [{Key, to_value(Value)}]).
  		
%% @spec getDstrList(X) -> ok 
%% Obj = term()
%% @doc
getDstrList(DstrList, [], MonitorDstrList)->
	MonitorDstrList;
getDstrList(DstrList, [H|T], MonitorDstrList)->
%% 	F=fun(X)->
%% 		case X#monitorlog.id of
%% 			H ->
%% 				true;
%% 			_  ->
%% 				false
%% 		end
%% 	end,
%% 	ChildSrcDstrList = lists:filter(F, DstrList),
    ChildSrcDstrList=[X||X<-DstrList,X#monitorlog.id=:=list_to_atom(H)],
	ChildDesDstrList = getMonitorDstrList(ChildSrcDstrList, []),
	getDstrList(DstrList, T, MonitorDstrList ++ [{"(dstr)" ++ H, ChildDesDstrList}]).

to_value(X) when is_float(X)->
  io_lib:format("~.2f", [X]);
to_value(X) when is_list(X)->
	try list_to_float(X) of
		V->V
	catch
		_:E ->
			try list_to_integer(X) of
	            V1->V1
            catch
	           _:Ex ->
				X
            end
	end;
to_value(X) ->
  X.
%% 
%%=============================dstr =========================
%% 

get_master_nodes()->
	[get_master_node()].
get_master_node()->
%% 	'debug@192.168.6.166'.
	case server_conf:getServerConf(master_node) of
		undefined->
			{ok, Host} = inet:gethostname(),
			list_to_atom(?SNAME ++ "@" ++ Host);
		Node->
			Node
	end.

get_all_monitors()->
%% 	io:format("report_proxy->get_all_monitors 1 is ~p~n", [get_master_node()]),
	case rpc:call(get_master_node(), api_monitor, get_all_monitors, []) of
		{badrpc, Reason}  ->
			io:format("report_proxy->get_all_monitors error is ~p~n", [Reason]),
			[0, 0, 0, 0, 0];
		Result ->
			Result
	end.

find_object(Id)->
	case rpc:call(get_master_node(), api_siteview, find_object, [Id]) of
		{badrpc, Reason}  ->
			io:format("report_proxy->find_object error is ~p~n", [Reason]),
			[0, 0, 0, 0, 0];
		Result ->
			Result	
	end.

get_parent_id(Id)->
	case rpc:call(get_master_node(), api_siteview, find_object, [Id]) of
		{badrpc, Reason}  ->
			io:format("report_proxy->get_parent_id error is ~p~n", [Reason]),
			[0, 0, 0, 0, 0];
		Result ->
			Result	
	end.

qc(MonitorStr, Count, DaysLimit)->
	monitor_logger:qc(MonitorStr, Count, DaysLimit).
	% case rpc:call(get_master_node(), monitor_logger, qc, [MonitorStr, Count, DaysLimit]) of
		% {badrpc, Reason}  ->
			% io:format("report_proxy->qc error is ~p~n", [Reason]),
			% [0, 0, 0, 0, 0];
		% Result ->
			% Result
	% end.
  
q(StartTime, EndTime, MonitorStr)->
	monitor_logger:q(StartTime, EndTime, MonitorStr).
%% 	io:format("report_proxy->get_all_monitors 1 is ~p~n", [get_master_node()]),
	% case rpc:call(get_master_node(), monitor_logger, q, [StartTime, EndTime, MonitorStr]) of
		% {badrpc, Reason}  ->
			% io:format("report_proxy->q error is ~p~n", [Reason]),
			% [0, 0, 0, 0, 0];		
		% Result ->
			% Result
	% end.
  
get_object(Id)->
	case rpc:call(get_master_node(), siteview, get_object, [Id]) of
		{badrpc, Reason}  ->
			io:format("report_proxy->get_object error is ~p~n", [Reason]),
			[0, 0, 0, 0, 0];		
		Result ->
			Result
	end.

get_template(Class)->
	case rpc:call(get_master_node(), api_monitor_template, get_template, [Class]) of
		{badrpc, Reason}  ->
			io:format("report_proxy->get_template error is ~p~n", [Reason]),
			[0, 0, 0, 0, 0];		
		Result ->
			Result
	end.

handle_cast(stop, State) ->	
	pgsql:close(State#state.conn),
	{stop,normal,State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.