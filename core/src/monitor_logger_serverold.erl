%% ---
%% monitor_logger_server
%%
%%---
-module(monitor_logger_serverold).
-behaviour(gen_server).

-compile(export_all).

-include("monitor.hrl").

-export([start_link/0,stop/0,
		log/1,logdb/1,q/1,q/2,q/3,qc/3,qdstr/3,
		read/1,read/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {parent,file,mdate,app}).

-define(LOGDIR,"logs").
-define(LOGNAME,monitor_log).
-define(ONCE_MAX,1000).
-define(MAX_READ,20000).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).


init([]) ->
	process_flag(trap_exit, true),
	do_init().


do_init()->
do_init("localhost").  %%默认在写log时会建立一个localhost的子目录，并开启当天的log文件操作


do_init(App) ->
	{Year,Month,Day} = date(),
    %%监测日志的多应用区分,以应用名作为日志文件的上层目录
    file:make_dir(?LOGDIR++"/"++App),
    
	FileName = lists:flatten(io_lib:format(?LOGDIR ++ "/" ++ App ++ "/siteview_~p-~p-~p.log",[Year,Month,Day])),

	case disk_log:open([{name,?LOGNAME},{file,FileName},{mode,read_write}]) of
		{error,_}->
            {stop, disk_log_open_error};
		_->
            {ok, #state{file=?LOGNAME,mdate={Year,Month,Day},app=App}}
	end.


%% @spec log(Terms)->{ok,Log} | {error,Reason}
%% where
%%	Terms = record()
%%	Log = string()
%% Reason = string()
%% @doc write monitor log file
log(Terms)->
    App = atom_to_list(dbcs_base:get_app()),
	call({log,Terms,App}).
    
%%暂时未进行多应用区分
logdb({Id, Time, Terms})->
	call({logdb,{Id, Time, Terms}}).

%% @spec q(Date)->{ok,Log} | {error,Reason}
%% where
%%	Date = string()
%%	Log = {eof,[MonitorLogs]}
%% MonitorLogs = record()
%% Reason = string()
%% Reason = string()
%% @doc get monitor log by Date
%%date format is:Y-M-D,example:2010-3-12,注意日和月的长度不一定是两位
q(Date)->
    App = atom_to_list(dbcs_base:get_app()),
	call({q,Date,App}).
    
    
%% @spec q(Date,Id)->{ok,Log} | {error,Reason}
%% where
%%	Date = string()
%% Id = atom()
%%	Log = {eof,[MonitorLogs]}
%% MonitorLogs = record()
%% Reason = string()
%% @doc get monitor log by Date and monitor Id
q(Date,Id)->
    App = atom_to_list(dbcs_base:get_app()),
	call({q,Date,Id,App}).


%% @spec q(StartDate,EndDate,Ids)->{ok,Log} | {error,Reason}
%% where
%%	StartDate = EndDate = {Date, Time}
%% Date = {Y,M,D}
%% Time = {H,M,S}
%% Y = M = D = H = M = S = integer()
%% Ids = [Id]
%% Id = atom()
%%	Log = {eof,[MonitorLogs]}
%% MonitorLogs = record()
%% Reason = string()
%% @doc get monitor log by Date between start to end,and monitor ids
q(StartDate,EndDate,Ids)->
    App = atom_to_list(dbcs_base:get_app()),
	%%io:format("monitor_logger:q: ~p ~n", [App]),
	call({q,StartDate,EndDate,Ids,App}).
	

%% 取一个时间段的日志
q(Id,StartDate,StartTime,EndDate,EndTime)->
	App = atom_to_list(dbcs_base:get_app()),
	call({q,Id,StartDate,StartTime,EndDate,EndTime,App}).
	
ql(StartDate,StartTime, EndDate,EndTime, Params)->
	 App = atom_to_list(dbcs_base:get_app()),
	 call({ql,StartDate,StartTime, EndDate,EndTime, Params, App}).
	
%%
%%
%%
qc(Ids, Count, DaysLimit)->
	App = atom_to_list(dbcs_base:get_app()),
	call({qc, Ids, Count, DaysLimit, App}).
%% 
%%=============================dstr =========================
%% 
qdstr(StartDate,EndDate,Ids) ->
 	 Temids=string:tokens(Ids, ","),
	 App = atom_to_list(dbcs_base:get_app()),
	%%io:format("monitor_logger:q: ~p ~n", [App]),
	{ok,Tmp}=call({q,StartDate,EndDate,Temids,App}),
	 Datalog=builddata(Tmp,[]),
	 getDstrList(Datalog, Temids, []).
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
%% @spec read(Count)->{Des,Logs} | {error,Reason}
%% where
%% Count = integer()
%%	Des = atom()|tuple()
%%	Logs = [MonitorLogs]
%% MonitorLogs = record()
%% Reason = string()
%% @doc read numbers of logs from start
read(Count)->
    App = atom_to_list(dbcs_base:get_app()),
	read(start,Count,App).

%% @spec read(Pos,Count)->{Des,Logs} | {error,Reason}
%% where
%% Pos = atom()
%% Count = integer()
%%	Des = atom()|tuple()
%%	Logs = [MonitorLogs]
%% MonitorLogs = record()
%% Reason = string()
%% @doc read numbers of logs from the position you decided
read(Pos,Count)->
    App = atom_to_list(dbcs_base:get_app()),
    read(Pos,Count,App).
    
read(Pos,Count,App)->
	call({read,Pos,Count,App}).

handle_call({log,Terms,App}, _, State) ->
    Mapp = State#state.app,
	Mdate = State#state.mdate,
    IsSomeApp = (Mapp==App),
    IsSomeDate = (date()==Mdate),
    IsFileOpend = (IsSomeApp and IsSomeDate),
    jdbc_Logger:log(Terms),
    if
        IsFileOpend ->
            {reply,disk_log:log(State#state.file,Terms),State};  %%当为同一app，同一日期文件时继续对初始化以打开的文件操作
        true ->
            disk_log:close(?LOGNAME),
            case do_init(App) of  %%关闭当前文件操作，重新初始化
				{ok,St}->
					disk_log:log(State#state.file,Terms),
					{reply,ok,St};
				Err->
					{reply,Err,#state{}}
			end
	end;
    

%% handle_call({logdb,{Id, Terms}}, _, State) ->
%% 	Table = Id,
%% 	Logdata = [{Table, calendar:local_time(), Terms}],
%% 	case lists:member(Table, mnesia:system_info(tables)) of
%% 		true ->
%% 			db_mnesia:insert_data(Logdata),
%% 			{reply, ok, State};
%% 		_->
%% 			case db_mnesia:create(Table, [id, logdata]) of
%% 				{atomic, ok} ->
%% 					db_mnesia:insert_data(Logdata),
%% 					{reply, ok, State};
%% 			_W ->				
%% 				{reply,_W, #state{}}
%% 			end
%% 	end;

handle_call({logdb,{Id, Time, Terms}}, _, State) ->
	Table = Id,
	Logdata = [{Table, Time, Terms}],
	case lists:member(Table, mnesia:system_info(tables)) of
		true ->
			db_mnesia:insert_data(Logdata),
			{reply, ok, State};
		_->
			case db_mnesia:create(Table, [id, logdata]) of
				{atomic, ok} ->
					db_mnesia:insert_data(Logdata),
					{reply, ok, State};
			_W ->				
				{reply,_W, #state{}}
			end
	end;


%%假设取log的进程已经含有了当前多应用的信息
%%当直接读?LOGNAME时，需要当前打开的文件索引为目前应用时有效，如果刚切换到另一个应用此时在没有及时更新文件索引时会出现打开上一个应用log的情况
%%handle_call({q,""}, _, State) ->
%%	{reply,read_log(start,?LOGNAME),State};
handle_call({q,Date,App}, _, State) ->
	Log = make_ref(),
    %%
    Day = if
        Date=="" -> %%默认为当天log
            {Y,M,D} = date(),
            lists:flatten(io_lib:format("~p-~p-~p",[Y,M,D]));
        true ->
            Date
    end,
	File = lists:flatten(io_lib:format(?LOGDIR ++ "/" ++ App ++ "/siteview_~s.log",[Day])),
    %%
	case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
		{ok,_}->
			Ret = read_log(start,Log),
			disk_log:close(Log),
			{reply,Ret,State};
		Else->
			{reply,Else,State}
	end;
handle_call({q,Date,Id,App}, _, State) ->
	Log = make_ref(),
    %%
    Day = if
        Date=="" -> %%默认为当天log
            {Y,M,D} = date(),
            lists:flatten(io_lib:format("~p-~p-~p",[Y,M,D]));
        true ->
            Date
    end,
	File = lists:flatten(io_lib:format(?LOGDIR ++ "/" ++ App ++ "/siteview_~s.log",[Day])),
    %%
	case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
		{ok,_}->
			Ret = read_log(start,Log,[{id,'=',Id}]),
			disk_log:close(Log),
			{reply,Ret,State};
		Else->
			{reply,Else,State}
	end;
handle_call({q, StartTime, EndTime, Ids, App}, _, State) ->
	{StartDate, _} = StartTime,
	{EndDate, _} = EndTime,	
	Dates = nextPath([], StartDate, EndDate),
	AllData = getAllData(Dates, [], Ids, StartTime, EndTime,App),
	case AllData of
		{ok,_} ->
			{reply,AllData,State};
		Else->
			{reply,Else,State}
	end;
	
handle_call({ql,StartDate,StartTime, EndDate,EndTime, Params, App}, _, State) ->
	{reply,query_data(App,StartDate,StartTime,EndDate,EndTime,Params),State};
	
handle_call({read, Pos, Count, App}, _, State) ->  %%不能使用读LOGNAME的方法原因同上
    Log = make_ref(),
    {Y,M,D} = date(),
    Date = lists:flatten(io_lib:format("~p-~p-~p",[Y,M,D])),
	File = lists:flatten(io_lib:format(?LOGDIR ++ "/" ++ App ++ "/siteview_~s.log",[Date])),
    case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
		{ok,_}->
			Ret = read_log2(Pos,Log,Count),
			disk_log:close(Log),
			{reply,Ret,State};
		Else->
			{reply,Else,State}
	end;
handle_call({qc, Ids, Count, DaysLimit, App}, _, State) ->
%% 	StartDate = date(),	
	EndDate =  date(),
	io:format("qc :~p~n", [EndDate]),
%% 	Dates = nextPath([], StartDate, EndDate),
	AllData = getAllData1(EndDate, [], Ids, 0, 0,DaysLimit, Count, App),
	case AllData of
		{ok,_} ->
			{reply,AllData,State};
		Else->
			{reply,Else,State}
	end;
%% 	Log = make_ref(),
%%     {Y,M,D} = date(),
%%     Date = lists:flatten(io_lib:format("~p-~p-~p",[Y,M,D])),
%% 	File = lists:flatten(io_lib:format(?LOGDIR ++ "/" ++ App ++ "/siteview_~s.log",[Date])),
%% 	
%%     %%
%% 	case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
%% 		{ok,_}->
%% 			Ret = read_log(start,Log,[{id,'in',Ids}]),
%% 			disk_log:close(Log),
%% 			{reply,Ret,State};
%% 		Else->
%% 			{reply,Else,State}
%% 	end;

handle_call({q,Id,StartDate,StartTime,EndDate,EndTime,App}, _, State) ->
	Dates = nextPath([], StartDate, EndDate),
	AllData = getAllData2(Dates, [], Id,App),
	case AllData of
		{ok,_} ->
			{reply,AllData,State};
		Else->
			{reply,Else,State}
	end;
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.

handle_cast(stop, State) ->
	disk_log:close(State#state.file),
	{stop,normal,State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.
	
	
query_data(App,StartDate,StartTime,EndDate,EndTime,Params)->
	Dates = get_dates(StartDate,EndDate),
	query_data_loop(Dates,StartDate,StartTime,EndDate,EndTime,Params,App,[]).
	
	% F = fun(X,R)->
		% {Y,M,D} = X,
		% Log = make_ref(),
		% File = lists:flatten(io_lib:format(?LOGDIR ++ "/" ++ App ++ "/siteview_~p-~p-~p.log",[Y,M,D])),
		% case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
			% {ok,_}->
				% Ret  = 
				% case X of
					% StartDate->
						% read_log(start,Log,[{time,'>',{X,StartTime}}] ++ Params);
					% EndDate->
						% read_log(start,Log,[{time,'<',{X,EndTime}}] ++ Params);
					% _->
						% read_log(start,Log, Params)
				% end,
				% disk_log:close(Log),
				% case Ret of
					% {error,_}->
						% R;
					% {_,Data}->
						% R ++ Data;
					% _->
						% R
				% end;
			% _->
				% R
		% end
	% end,
	% {eof,lists:foldl(F,[],Dates)}.

query_data_loop([],_,_,_,_,_,_,R)->{eof,R};	
query_data_loop([X|T],StartDate,StartTime,EndDate,EndTime,Params,App,R)->
	if
		length(R) >= ?MAX_READ->
			query_data_loop([],StartDate,StartTime,EndDate,EndTime,Params,App,R);
		true->
			{Y,M,D} = X,
			Log = make_ref(),
			File = lists:flatten(io_lib:format(?LOGDIR ++ "/" ++ App ++ "/siteview_~p-~p-~p.log",[Y,M,D])),
			case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
				{ok,_}->
					Ret  = 
					case X of
						StartDate->
							read_log(start,Log,[{time,'>',{X,StartTime}}] ++ Params);
						EndDate->
							read_log(start,Log,[{time,'<',{X,EndTime}}] ++ Params);
						_->
							read_log(start,Log, Params)
					end,
					disk_log:close(Log),
					case Ret of
						{error,_}->
							query_data_loop(T,StartDate,StartTime,EndDate,EndTime,Params,App,R);
						{_,Data}->
							query_data_loop(T,StartDate,StartTime,EndDate,EndTime,Params,App,lists:append(R,Data));
						_->
							query_data_loop(T,StartDate,StartTime,EndDate,EndTime,Params,App,R)
					end;
				_->
					query_data_loop(T,StartDate,StartTime,EndDate,EndTime,Params,App,R)
			end
	end.
		
	
get_dates(StartDate,EndDate) when StartDate > EndDate ->
	[];
get_dates(StartDate,EndDate) when StartDate == EndDate ->
	[StartDate];
get_dates(StartDate,EndDate)->
	[StartDate]
	++ get_dates(sv_datetime:next_date(StartDate),EndDate).

getAllData1(Enddate, Ret, Ids, Total, Days, DaysLimit, CountLimit, _) when (Total >= CountLimit) or (Days >= DaysLimit) ->
	{ok, Ret};
getAllData1(Enddate, Ret, Ids, Total, Days, DaysLimit, CountLimit, App) ->			
	Log = make_ref(),
	{Year,Month,Day} = Enddate,
	File = lists:flatten(io_lib:format(?LOGDIR++ "/" ++ App  ++ "/siteview_~p-~p-~p.log",[Year,Month,Day])),
	case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
		{ok,_}->
			{_, Data} = read_log(start,Log,[{id,'in',Ids}]),
			disk_log:close(Log),
			getAllData1(sv_datetime:prev_date({Year,Month,Day}), Data ++ Ret , Ids, Total + length(Data), Days+1, DaysLimit, CountLimit, App);
		Else->
			getAllData1(sv_datetime:prev_date({Year,Month,Day}), Ret, Ids, Total, Days+1, DaysLimit, CountLimit, App)
	end.

getAllData([], Ret, Ids,StartTime, EndTime,_) ->
	{ok, Ret};
getAllData([H|T], Ret, Ids,StartTime, EndTime,App) ->
	if 
		length(Ret) >= ?MAX_READ->
			getAllData([],Ret,Ids,StartTime,EndTime,App);
		true->
			Log = make_ref(),
			{Year,Month,Day} = H, 
			File = lists:flatten(io_lib:format(?LOGDIR++ "/" ++ App  ++ "/siteview_~p-~p-~p.log",[Year,Month,Day])),
			case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
				{ok,_}->
					RetOld = read_log(start,Log,[{id,'in',Ids},{time,'>=',StartTime},{time,'<=',EndTime}]),
					disk_log:close(Log),
					case RetOld of
						{_, Data} ->
							getAllData(T, Ret ++ Data, Ids,StartTime, EndTime,App);
						_ ->
							getAllData(T, Ret, Ids,StartTime, EndTime,App)
					end;	
				Else->
					getAllData(T, Ret, Ids,StartTime, EndTime,App)
			end
	end.
	
	
getAllData2([], Ret, _,_) ->
	{ok, Ret};
getAllData2(more, Ret, _,_) ->
	{more, Ret};
getAllData2([H|T], Ret, Id,App) ->
	if 
		length(Ret) >= ?MAX_READ->
			getAllData2(more,Ret,Id,App);
		true->
			Log = make_ref(),
			{Year,Month,Day} = H, 
			File = lists:flatten(io_lib:format(?LOGDIR++ "/" ++ App  ++ "/siteview_~p-~p-~p.log",[Year,Month,Day])),
			case disk_log:open([{name,Log},{file,File},{mode,read_only}]) of
				{ok,_}->
					case read_log(start,Log,[{id,'=',Id}]) of
						{error,_}->
							disk_log:close(Log),
							getAllData2(T, Ret, Id,App);
						{eof,Data}->
							disk_log:close(Log),
							getAllData2(T, Ret ++ Data, Id,App);
						{_,Data}->
							disk_log:close(Log),
							getAllData2(more, Ret ++ Data, Id,App);
						_->
							disk_log:close(Log),
							getAllData2(T, Ret, Id,App)
					end;
				Else->
					getAllData2(T, Ret, Id,App)
			end
	end.

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

read_log(Con,File)->
	read_log(Con,File,?MAX_READ,[],[]).

read_log(Con,File,Where)->
	read_log(Con,File,?MAX_READ,Where,[]).
	
read_log(Con,File,Count,Cond,Data)->
	if 
		Count > 0 ->
			case disk_log:chunk(File,Con,?ONCE_MAX) of
				{error,Reason}->
					{error,Reason};
				eof->
					{eof,Data};
				{Con2,Ret}->
					%%io:format("read_log:~p,~p~n",[Count,Ret]),
					Ret4 = filter_data(Ret,Cond),
%% 					io:format("read_log:~p~n",[Ret4]),
					Len = length(Ret4),
					read_log(Con2,File,Count-Len,Cond,lists:append(Data,Ret4))

			end;
		true ->
			{Con,Data}
	end.
	
	
read_log2(Con,File,Count)->
	if 
		Count > 0 ->
			if 
				Count < ?ONCE_MAX ->
					case disk_log:chunk(File,Con,Count) of
						{error,Reason}->
							{error,Reason};
						eof->
							{eof,[]};
						{Con2,Ret}->
							{Con2,Ret}
					end;
				true ->
					read_log(File,Con,?ONCE_MAX,[],[])
			end;
		true->
			{Con,[]}
	end.

filter_data([],_)->[];
filter_data(Data,[])->Data;
filter_data(Data,[C|T])->
	F=fun(X)->
		case C of
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
%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	disk_log:close(_State#state.file),
    ok.

code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


call(Req) ->
    gen_server:call(?MODULE, Req, infinity).

cast(Msg) ->
    gen_server:cast(?MODULE, Msg).

