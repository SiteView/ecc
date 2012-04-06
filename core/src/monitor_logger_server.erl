%% ---
%% monitor_logger_server
%%
%%---
-module(monitor_logger_server).
-behaviour(gen_server).

-compile(export_all).

-include("monitor.hrl").

-export([start_link/0,stop/0,
		log/1,logdb/1,q/1,q/2,q/3,qc/3,qdstr/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {conn}).

-define(LOGDIR,"logs").
-define(LOGNAME,monitor_log).
-define(ONCE_MAX,1000).
-define(MAX_READ,20000).
-define(MBox,eccadmin).
-define(OfbizNode,server_conf:get_ofbiz_node()).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).


init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{conn=ok}}.



%% @spec log(Terms)->{ok,Log} | {error,Reason}
%% where
%%	Terms = record()
%%	Log = string()
%% Reason = string()
%% @doc write monitor log file
log(Terms)->
    App = atom_to_list(dbcs_base:get_app()),
	call({log,Terms,App}).
    
%%Not yet distinguish between multiple applications
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
%%date format is:Y-M-D,example:2010-3-12,Note that the length of day and month are not necessarily two
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
	

%% Take a time log
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

convert_ofbizrow([],Acc)->lists:reverse(Acc);
convert_ofbizrow([R|T],Acc)->
	[Id,Time,Category,Desc,Measure,Name]=R,
	Log = #monitorlog{id=Id,name=Name,time=Time,category=Category,desc=Desc,measurement=Measure}, 
	convert_ofbizrow(T,[Log|Acc]).		
handle_call({log,Terms,App}, _, State) ->
	io:format("monitor_logger_server:~p~n", ["log"]),
	{{Y,M,D},_} = Terms#monitorlog.time,
	Params = [Terms#monitorlog.id,App,Terms#monitorlog.name,Terms#monitorlog.time,Terms#monitorlog.category
					,Terms#monitorlog.desc,Terms#monitorlog.measurement],
	{?MBox,?OfbizNode} ! {self(),"LogMonitor","monitorLogger",Params},				
	%% register(log_monitor,spawn(fun()-> ofbiz:log_monitor(Params) end)),
	{reply, {ok,ok}, #state{conn=ok}}

;
handle_call({q,Date,App}, _, State) ->
	Params=["",Date,Date,""],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, q}, State};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, State};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},State}
			end
	end;

handle_call({q,Date,Id,App}, _, State) ->
	Params=[Id,Date,Date,""],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, q}, State};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, State};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},State}
			end
	end;
handle_call({q, StartTime, EndTime, Ids, App}, _, State) ->
	Params=[Ids,StartTime,EndTime,""],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, q}, State};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, State};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},State}
			end;
		Else->
			io:format("NNNNNNNNNNNNNNNNN:~p~n", [Else])
	end;
	
handle_call({ql,StartDate,StartTime, EndDate,EndTime, Params, App}, _, State) ->
	Params=["",{StartDate,StartTime},{EndDate,EndTime},""],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, q}, State};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, State};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},State}
			end
	end;
	

handle_call({qc, Ids, Count, DaysLimit, App}, _, State) ->
	Params=[Ids,"","",Count],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, qc}, State};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, State};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},State}
			end
	end;


handle_call({q,Id,StartDate,StartTime,EndDate,EndTime,App}, _, State) ->
	Params=[Id,{StartDate,StartTime},{EndDate,EndTime},""],
	case java_node:ofbizcall("loggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, q}, State};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, State};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},State}
			end
	end;
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.

handle_cast(stop, State) ->
	{stop,normal,State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.
	

get_dates(StartDate,EndDate) when StartDate > EndDate ->
	[];
get_dates(StartDate,EndDate) when StartDate == EndDate ->
	[StartDate];
get_dates(StartDate,EndDate)->
	[StartDate]
	++ get_dates(sv_datetime:next_date(StartDate),EndDate).


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

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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

