%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(historyreport,[BASE]).
-extends(siteview_object).
-compile(export_all).
-include("ecc_oem.hrl").
-include("monitor.hrl").
-include("historyreport.hrl").
-include("monitor_template.hrl").

-define(MaxRecords,200).
-define(DefaultBgColor,"#DDDDDD").
-define(DefaultBorder,"1").
%% -export([createHistoryReportObject/1, init/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for historyreport
new()->
	Obj = siteview_object:new(),

	Obj:set_attribute(collectors,[]),
	Obj:set_attribute(errorlist,[]),
	Obj:set_attribute(goodlist,[]),	
	Obj:set_attribute(warninglist,[]),
%% 	Obj:set_attribute(nodataColor,""),
	
	Obj:set_property(monitors, ""),
	Obj:set_property(format, ""),
	Obj:set_property(startDay, "today"),
	Obj:set_property(title, "testhistoryreport"),
	Obj:set_property(schedule, "repeat"),
	Obj:set_property(truedata, false),
	
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
readFromHashMap(X) ->
	F = fun(I)->
		{Key, Value} = I,
		THIS:set_property(Key, Value)
	end,
	lists:foreach(F, X),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
isDisabled()->
	{ok,{_, Disabled}} = THIS:get_property(disabled),
	Disabled.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
schedule()->
%% %% 	L = 0,
%% 	ReoportSch = case siteview:get_report_scheduler() of
%% 		{ok,Sche}->
%% 				Sche;
%% 		_->
%% 			{error,no_found_scheduler}
%% 		end,
%% 	Action = generatehistoryreport:new(THIS),
%% 	{ok,{_, PSchedule}}= THIS:get_property(schedule),
%% 	THIS:set_attribute(action, Action),
%% 	ReoportSch:scheduleRepeatedAction(Action, PSchedule, sv_datetime:now()),
%% %% 	ReoportSch:testreportPeriodicAction(Action,2*60*1000),	
%% %% 	ReoportSch:scheduleAction(Action, PSchedule, L, jj),
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
unschedule(History)->
%% 	{ok,{_, Action}} = History:get_attribute(action),
%% 	ReoportSch = case siteview:get_report_scheduler() of
%% 		{ok,Sche}->
%% 				Sche;
%% 		_->
%% 			{error,no_found_scheduler}
%% 		end,
%% 
%% 	ReoportSch:unschedule(Action),
%% 	Action:delete(),
	ok.

delete()->
	Scheduler = case siteview:get_report_scheduler() of
					{ok,Sche}->
						Sche;
					_->
						{error,no_found_scheduler}
				end,
	case THIS:get_attribute(action) of
		{ok,{action,Action}}->
			Scheduler:unschedule(Action),
			THIS:remove_attribute(action),
			Action:delete();
		_->
			pass
	end,

	case THIS:get_attribute(collectors) of
		{ok,{action,Collectors}}->
 			F = fun(X)->
 				ets:delete(X:get_tid())
 			end,
 			lists:foreach(F, Collectors),
			THIS:remove_attribute(collectors);
		_->
			pass
	end,	
	
	BASE:delete(). 

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
initialize()->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
    io:format("~n Collectors:~p ~n",[Collectors]),
	T1 = [],
	THIS:set_attribute(collectors, T1),	
	THIS:initializeSampleCollectors(),
	THIS:initializeTimeParameters(),
	THIS:initializeFilePaths(),	
    %% text report initialize
    THIS:set_property(reportDelimiter, ","),
    THIS:set_property(lineseparator, "|"),
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
createFromQuery(Query)->
	case is_atom(THIS:get_app()) of
		true->
			put(hostname, THIS:get_app());	
		_->
			put(hostname, list_to_atom(THIS:get_app()))
	end,
    
%% 	dbcs_base:set_app(THIS:get_app()),
	THIS:initialize(),
	
	{ok,{_, P}} = THIS:get_property(reportPath),
%% 	case file:open(P ++ ".html", write) of
	case file:open(P, write) of
		{ok, S} ->
			THIS:createReport(S);
%% 			file:close(S);
		_->
			io:format("eror   title    file: ~p ~n", [P])
	end,
	
%% 	generateIndexPage(THIS, "user", THIS:get_property(id), false, ""),
	case THIS:get_property(isadhoc) of
		{error,{_, _}} ->
			{ok,{_, ReportDirectory}} = THIS:get_property(reportDirectory),
			historyreport_static:generateIndexPage(THIS, P, ReportDirectory, flag, s2);			
		_->
			continue
	end,
%%  close all middle run obj (ets:close)?....	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
addCollector(CollectorMap, Monitor, Property, KeepSampleBuffer, TrueData)->
	{ok,{id, FullId}} = Monitor:get_property(id),
%% 	{ok,{_, Name}} = Property:get_property(name),
	Name = Property,
%% 	Key = {FullId} + atom_to_list(Name),
%% 	Key,	
	case TrueData of
		true->
			Collector = samplecollectordata:new(Monitor, Property, false, false, KeepSampleBuffer);
		_->
			Collector = samplecollector:new(Monitor, Property, false, false, KeepSampleBuffer)
	end,
		
%% 	Collector = samplecollectorproc:new(Monitor, Property, false, false, KeepSampleBuffer),	
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	T1 = Collectors ++ [Collector],
	
	THIS:set_attribute(collectors, T1),
	ok.
%% 	String key = monitor.getFullId() + " " + property.getName(); 
%%     if (collectorMap.get(key) == null) {
%%        SampleCollector collector = new SampleCollector((Monitor)monitor.toSiteViewObject(), property, keepSampleBuffer, this.reportObjectConfig.isWarningNotIncluded(), this.reportObjectConfig.isFailureNotIncluded());
%%  
%%        collectorMap.put(key, collector);
%%        this.collectors.add(collector);

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
get_AllSelGruopMonitors([], Monitors,Ret) ->
	Ret;
get_AllSelGruopMonitors([H|T], Monitors, Ret) ->
  	ID = api_siteview:get_parent_id(H),
	case lists:keysearch(id, 1, Monitors) of 
   	{value,{"id", N}} ->
		Monitor = api_siteview:find_object(N)
%% 		get_AllSelGruopMonitors(T, Monitors, dict:append(N, Monitor, Ret)) 
   	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
get_AllSelMonitors([], AllMonitors,Ret) ->
	Ret;
get_AllSelMonitors([H|T], AllMonitors, Ret) ->	
	Monitor = api_siteview:find_object(H),
	get_AllSelMonitors(T, AllMonitors, dict:append(H, Monitor, Ret)).
%%     case lists:keysearch(H, 2, AllMonitors) of 
%%     {value,{_, ID}} ->
%% 		Monitor = api_siteview:find_object(ID),
%% 		get_AllSelMonitors(T, AllMonitors, dict:append(ID, Monitor, Ret)) 
%%     end.
%% 	Monitor = dbcs_monitor:get_monitor(H),

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
getLogProperties(T)->
%% 	Temp = T:get_template_property(T),
	Temp = api_monitor_template:get_template(T),
	[X#property.name || X<-Temp,X#property.state=:=true].

iscounters(false,_) ->
	false;
iscounters(true,_) ->
	true;
iscounters(Ret,Monitor) ->
	case is_tuple(Monitor) of
		true ->
			H=element(1,Monitor),
			E=element(2,Monitor),
			case H of
				application_base ->
					iscounters(true,Monitor);
				_ -> 
					iscounters(continue,E)
			end;
		false ->
			iscounters(false,Monitor)
	end.
	
	
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
initializeSampleCollectors()->
	{ok,{_, T}} = THIS:get_property(monitors),
	{ok,{_, TrueData}} = THIS:get_property(truedata),
	MonitorStr = string:tokens(T, ","),
%% 	put(hostname, THIS:get_app()),
	io:format("initializeSampleCollectors: ~p ~n", [THIS:get_app()]),
	AllMonitors = api_monitor:get_all_monitors(),
%% 	io:format("AllMonitors:~p ~n", [AllMonitors]),
	Monitors = get_AllSelMonitors(MonitorStr, AllMonitors, dict:new()),		
%% 	SelMonitors = get_AllSelMonitors(MonitorStr, AllMonitors, dict:new()),
%% 	SelGroupMonitors = get_AllSelGruopMonitors(AllMonitors, MonitorStr, dict:new()),
%% 	Monitors = lists:append(SelMonitors, SelGroupMonitors),
%% 	keepSampleBuffer = this.reportObjectConfig.isLineGraphReportType();
	F = fun(X)->
%% 			[Monitor] = dict:fetch(X, Monitors),
%% 			{value,{class, Class}} = lists:keysearch(class, 1, Monitor),
%% 			Props = THIS:getLogProperties(Class),
			[[Monitor]] = dict:fetch(X, Monitors),
%% 			Isc= iscounters("",Monitor),
%% 			case Isc of
%% 						true ->
%% 							Dynamic=[ list_to_atom(element(1,X))||X<-Monitor:getCountersContent()],
%% 				            TProps = Monitor:getLogProperties(Monitor)++Dynamic,
%%                             Props=[ X||X<-TProps,X=/=status ];
%% 						false ->
							Props=Monitor:getLogProperties(Monitor),
%% 					end,
			
			
%% 			io:format("Props:~p ~n", [Props]),
			F1 = fun(X1)->
			    THIS:addCollector(collectorMap, Monitor, X1, false, TrueData)
			end,
			lists:foreach(F1, Props)
	end,
	lists:foreach(F, dict:fetch_keys(Monitors)),
	ok.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% private %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

realTimeReportingMonitor()->
	ok.

printReportTop()->
	ok.

printReportBottom()->
	ok.

getCustomFormat(String)->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
get_list_data(H, MaxError, MaxGood, MaxWarning) ->
	case H#monitorlog.category of
		good ->
			{ok,{_, List}} = THIS:get_attribute(goodlist),
			case length(List) =< MaxGood of
				true ->					
					T1 = List ++ [H],
					THIS:set_attribute(goodlist, T1);
				 _->
					error
			end;
		error ->
			{ok,{_, List}} = THIS:get_attribute(errorlist),
			case length(List) =< MaxError of
				true ->					
					T1 = List ++ [H],	
					THIS:set_attribute(errorlist, T1);
				 _->
					error			
			end;
		warning ->
			{ok,{_, List}} = THIS:get_attribute(warninglist),
			case length(List) =< MaxWarning of
				true ->					
					T1 = List ++ [H],	
					THIS:set_attribute(warninglist, T1);
				 _->
					error			
			end;

		 _ ->
			ok
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
process_sub_1([], Ret, RequestedStartTime, RequestedEndTime, Precision) ->
	Ret;
process_sub_1([H|T], Ret, RequestedStartTime, RequestedEndTime, Precision) ->
	L = calendar:datetime_to_gregorian_seconds(RequestedStartTime),
	L1 = calendar:datetime_to_gregorian_seconds(RequestedEndTime),
%% 	H:createBuckets(L, L1, Precision * 60 * 2),
 	H:createBuckets(L, L1, Precision),
	Monitor = H:getMonitor(),
%% 	io:format("~p ~n", [L1]),
%% 	{ok,{_, OwnerID}} = Monitor:get_property(ownerid),
	{ok,{_, ID}} = Monitor:get_property(id),
%% 	Key =  OwnerID + "/" + ID,
	Key = ID,
	process_sub_1(T, dict:append(Key, H, Ret), RequestedStartTime, RequestedEndTime, Precision).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
process_sub_2([], Ret, RequestedStartTime, RequestedEndTime, Precision, Length) ->
	Ret;
process_sub_2([H|T], Ret, RequestedStartTime, RequestedEndTime, Precision, Length) ->
	L = calendar:datetime_to_gregorian_seconds(RequestedStartTime),
	L1 = calendar:datetime_to_gregorian_seconds(RequestedEndTime),
%% 	H:createBuckets(L, L1, Precision * 60 * 2),
 	H:createBuckets(L, L1, Precision, Length),
	Monitor = H:getMonitor(),
%% 	io:format("~p ~n", [L1]),
%% 	{ok,{_, OwnerID}} = Monitor:get_property(ownerid),
	{ok,{_, ID}} = Monitor:get_property(id),
%% 	Key =  OwnerID + "/" + ID,
	Key = ID,
	process_sub_2(T, dict:append(Key, H, Ret), RequestedStartTime, RequestedEndTime, Precision, Length).

getTuplelist([],Ret)->
	Ret;
getTuplelist([H|E],Ret)->
	Index=string:rstr(H,"<")-1,
	Subs=string:sub_string(H, 1,Index),
	Tup = string:tokens(Subs,"="),
    Key=list_to_atom(string:strip(hd(Tup),both,$ )),
	Tvalue=string:strip(lists:last(Tup),both,$ ),
%% 	io:format("Tvalue: ~p ~n", [Tvalue]),
	Value= 
	try
		list_to_float(Tvalue) of
		V ->V
	catch _:_ ->
			  try
				  list_to_integer(Tvalue) of
				  V1 ->V1
			  catch _:_ ->
						0
			  end
	end,
	case Ret of
		[] ->
			TRet=[{Key,Value}];
		_->
			TRet=Ret++[{Key,Value}]
	end,
getTuplelist(E,TRet).	

getMeasurement(State)->
	Liststate=string:tokens(State, ">"),
	THIS:getTuplelist(Liststate,[]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
process(S, CollectorList, Data, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc) ->
	MonitorMap = THIS:process_sub_1(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision),
%% 	io:format("~p ~n", [MonitorMap]),
	io:format("~p ~n", ["haha"]),
	F = fun(X)->		
		case X of
			{eof, ChildData} ->
%% 				io:format("~p ~n", [ChildData]),
				F1 = fun(X1)->
%% 					io:format("~p ~n", [X1]),
					case is_list(X1) of
						true ->
							F2 = fun(X2)->
								K2 = X2#monitorlog.id,
								DESC=X2#monitorlog.desc,
%% 								io:format("Id:~p ~n", [K2]),
								M=siteview:get_object(K2),
                                Monitor=hd(M),
%% 								Isc= iscounters("",Monitor),
%% 								case Isc of
%% 									true ->
%% 										Measurement=getMeasurement(DESC);
%% 									false ->
										Measurement=X2#monitorlog.measurement,
%% 								end,
									  
%% 								Measurement=X2#monitorlog.measurement,
%%  								io:format("Measurement:~p ~n", [Measurement]),
%% 								io:format("~p ~n", [K2]),
								case dict:find(K2, MonitorMap) of
									{ok, Connectors}->
%% 										io:format(S, "~p ~n", [X2]),
%% 										F3 = fun(X3)->
											case (((calendar:datetime_to_gregorian_seconds(X2#monitorlog.time) - calendar:datetime_to_gregorian_seconds(RequestedStartTime)) >= 0) and
													((calendar:datetime_to_gregorian_seconds(RequestedEndTime) - calendar:datetime_to_gregorian_seconds(X2#monitorlog.time)) >= 0)) of
												true ->
													{Day, Time} = X2#monitorlog.time,
													case schedule_property:history_is_enabled(ScheduleMap, calendar:day_of_the_week(Day), sv_datetime:time(Time)) of
														true ->														
															THIS:get_list_data(X2, ?MaxRecords, ?MaxRecords, ?MaxRecords),															
															F3 = fun(X3) ->					
																X3:add_2(X2#monitorlog.time, Measurement, X2#monitorlog.category, X2#monitorlog.category, bestCaseCalc)
															end,
															lists:foreach(F3, Connectors);
														_ ->
															F4 = fun(X4) ->
																X4:add_2(X2#monitorlog.time, Measurement, filtered, filtered, bestCaseCalc)
															end,
															lists:foreach(F4, Connectors),															
															ok
													end;
												_->
%% 											 		io:format("~p ~n",["ok"])
													ok
											end;							
%% 										end,
%% 										lists:foreach(F3, Connectors);
%% 										get_list_data([X2], ErrorsList, WarningList, GoodList);
							 		_->
%% 										io:format("~p ~n", [K2])
								 		ok
								end
							 end,
							 lists:foreach(F2, X1);
				 	 	 false ->
							ok
					end					
			    end,
%% 			    io:format("~p ~n", ["haha1111"]),
			    lists:foreach(F1, [ChildData]);
			_ ->
			    io:format("~p ~n", ["No Data Log:"]),
				io:format("~p ~n", [X])
		end
	end,	
	lists:foreach(F, Data),
%% 	{MonitorMap, ErrorsList, WarningList, GoodList}.
	MonitorMap.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
processTrueData(S, CollectorList, Data, Count, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc) ->
%% 	[{eof, ChildData1}] = Data,c
	MonitorMap = THIS:process_sub_2(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision, Count),		
%% 	io:format("~p ~n", [MonitorMap]),
	io:format("~p ~n", ["hahadddd"]),
	F = fun(X)->		
%% 			F2 = fun(X2)->
				K2 = X#monitorlog.id,
				DESC=X#monitorlog.desc,
				M=siteview:get_object(K2),
	    		Monitor=hd(M),
%% 				Isc= iscounters("",Monitor),
%% 				case Isc of
%% 					true ->
%% 						Measurement=getMeasurement(DESC);
%% 					false ->
						Measurement=X#monitorlog.measurement,
%% 				end,
				case dict:find(K2, MonitorMap) of
				{ok, Connectors}->
					{Day, Time} = X#monitorlog.time,
					case schedule_property:history_is_enabled(ScheduleMap, calendar:day_of_the_week(Day), sv_datetime:time(Time)) of
						true ->														
							THIS:get_list_data(X, ?MaxRecords, ?MaxRecords, ?MaxRecords),
							F3 = fun(X3) ->					
								X3:add_2(X#monitorlog.time, Measurement, X#monitorlog.category, X#monitorlog.category, bestCaseCalc)
							end,
							lists:foreach(F3, Connectors);
						_ ->
							F4 = fun(X4) ->
								X4:add_2(X#monitorlog.time, Measurement, filtered, filtered, bestCaseCalc)
							end,
							lists:foreach(F4, Connectors),														
							ok
						end;
					_->
						ok
				end
%% 			end,
%% 			lists:foreach(F2, X)
	end,	
	lists:foreach(F, Data),
	MonitorMap.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
count_Con(Map, [], Cons, I) ->
	{I, Cons};	
count_Con(Map, [H|T], Cons, I) ->
	{ok, V} = dict:find(H, Map),	
	count_Con(Map, T, Cons ++ V, I + length(V)).

getBackColor(Format)->
    Value = string:tokens(Format,","),
    BackColorString = lists:filter(fun(X)-> string:rstr(X,"bgcolor")>0 end,Value),
    case BackColorString of 
        []  ->  ?DefaultBgColor;
        V   ->  
            S=string:tokens(lists:nth(1,V),":"),
            lists:nth(2,S)
    end.

getBorder(Format)->
    Value = string:tokens(Format,","),
    BackColorString = lists:filter(fun(X)-> string:rstr(X,"border")>0 end,Value),
    case BackColorString of 
        []  ->  ?DefaultBorder;
        V   ->  
            S=string:tokens(lists:nth(1,V),":"),
            lists:nth(2,S)
    end.

createReportData(S)->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	{ok,{_, Precision}} = THIS:get_property(precision),
	io:format("Precision:~p ~n", [Precision]),
	{ok,{_, StartTime}} = THIS:get_property(startTime),
 	io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(StartTime)]),
	{ok,{_, EndTime}} = THIS:get_property(endTime),
	io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(EndTime)]),

	{ok,{_, SchedFilter}} = THIS:get_property(schedFilter),
	io:format("~p ~n", [SchedFilter]),
	ScheduleMap	= schedule_manager:get_history_schedule_filter(SchedFilter),
	case ScheduleMap of
		null ->
		    SchedName = "all";
		_->
			SchedName = dbcs_schedule:get_name(SchedFilter)
	end,
	io:format("~p ~n", [ScheduleMap]),
	{ok,{_, Description}} = THIS:get_property(description),
	io:format("~p ~n",[Description]),
	case Description == [] of
		true->
		    Des = "";
		_->
			Des = Description
	end,
	
	{ok,{_, StatusFilter}} = THIS:get_property(statusFilter),
	io:format("monitor_logger start: ~p ~n", [calendar:local_time()]),
	{ok,{_, T}} = THIS:get_property(monitors),
	MonitorStr = string:tokens(T, ","),	
	
	{ok,{_, Truedata}} = THIS:get_property(truedata),	
	Count = 20,
	case Truedata of
		true->
		{_, AllData} = monitor_logger:qc(MonitorStr, Count);
		_->
		{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
				calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr)
	end,
	io:format("monitor_logger end: ~p ~n", [calendar:local_time()]),

	io:format("process start: ~p ~n", [calendar:local_time()]),
	case Truedata of
		true->	
		Data = AllData,
		case length(Data) > Count of
			true ->
				Count1 = Count,
				Data1 = lists:sublist(Data, length(Data) - Count, Count);
			_->
				Count1 = length(Data),
				Data1 = Data
		end,
		Map = THIS:processTrueData(S, Collectors, Data1, Count1, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, ScheduleMap, bestCaseCalc);	
		_->
		Map = THIS:process(S, Collectors, AllData, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, ScheduleMap, bestCaseCalc)	
	end,
	io:format("process end: ~p ~n", [calendar:local_time()]),

	{ok,{_, Title}} = THIS:get_property(title),
	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	
	{ok,{_, ShowThresholdSummary}} = THIS:get_property(showReportThresholdSummary),
	{ok,{_, ShowErrorTimeSummary}} = THIS:get_property(showReportErrorTimeSummary),
	
	{ok,{_, Disabled}} = THIS:get_property(disabled),
	
	{ok,{_, ShowReportAlerts}} = THIS:get_property(showReportAlerts),
	{ok,{_, WarningNotIncluded}} = THIS:get_property(warningNotIncluded),
	{ok,{_, UpTimeIncludeWarning}} = THIS:get_property(upTimeIncludeWarning),		
	
	{ok,{_, HideReportSummary}} = THIS:get_property(hideReportSummary),
	{ok,{_, HideReportTables}} = THIS:get_property(hideReportTables),
	{ok,{_, HideReportErrors}} = THIS:get_property(hideReportErrors),
	{ok,{_, HideReportWarnings}} = THIS:get_property(hideReportWarnings),
	{ok,{_, HideReportGoods}} = THIS:get_property(hideReportGoods),
    {ok,{_, Format}} = THIS:get_property(format),
    BgColor = getBackColor(Format),
    Border =  getBorder(Format),
%%    io:format("BgColor:~p ~n", [BgColor]),
%%     io:format("Border:~p ~n", [Border]),
	X = ["<TABLE ALIGN=CENTER BORDER=0>" , "<TR><TD><H2 ALIGN=CENTER> " , Title , 
		 "</H2><TD></TR>" , "<TR><TD>" , "<P ALIGN=CENTER>" , ReportPeriod , "<BR/><FONT=-1>"++gettext:key2str("Filter data By Schedule","zh-cn")++":",  
		 SchedName, "</FONT>", "<BR/><FONT=-1> "++gettext:key2str("Filter data by status","zh-cn")++":", StatusFilter, "</FONT>", "</P></TD></TR>" , "<TR><TD><P ALIGN=CENTER><B>" , 
		 Des, "</B></P></TD></TR></TABLE>"],
	
	io:format(S, "~s ~n", [lists:flatten([X])]),
	{_, Consold} = THIS:count_Con(Map, dict:fetch_keys(Map), [], 0),
	
%%  printwriter.print("\n<!--TIMEPERIOD\n" + dateStringDate(date) + "\t" + dateStringTime(date) + "\t" + dateStringDate(date1) + "\t" + dateStringTime(date1) + "\nENDTIMEPERIOD-->\n");	
%% 	ReportPeriod,	
%%  printwriter.print("\n<!--OVERVIEW\n" + getSummaryString() + "ENDOVERVIEW-->\n");
	
%% 	case THIS:get_property(isadhoc) of
%% 		{error,{_, _}} -> 
%% 			{ok,{_, FileName}} = THIS:get_property(filename),
%% 			{ok,{_, ReportDirectory}} = THIS:get_property(reportDirectory),	 
%% 			DataPath = ReportDirectory ++ FileName ++ ".data",
%% 			SummaryList = getSummaryListByMap(Map, dict:fetch_keys(Map), []),
%% 			unconsult(DataPath, SummaryList);
%% 		_->
%% 			continue
%% 	end,
	
	Cons = THIS:filterDataWithStatus(Consold, "all", []),
    ConsError = THIS:filterDataWithStatus(Consold, "error", []),
%%    ConsWarning = THIS:filterDataWithStatus(Consold, "warning", []),
%%    ConsGood = THIS:filterDataWithStatus(Consold, "good", []),
	
	case ShowThresholdSummary of
		true->
			ThresholdInfo = THIS:createThresholdSummary();
		_->
			[{thresholdinfo, []}]
	end,
	
	case HideReportSummary of
		false->
			THIS:hTMLSummaryTable(S, Cons,BgColor,Border);
		_->
			[{uptimeSummary, []}],
			[{thresholdinfo, []}]
	end,
	
	case ShowErrorTimeSummary of
		true->
 			THIS:hTMLErrorTimeSummary(S,ConsError,BgColor,Border);
		_->
			[{errorTimeSummary, []}]
	end,

	case Disabled of
		true->
			ok;
		_->
			ok
	end,
	
	case WarningNotIncluded of
		true->
			ok;
		_->
			ok
	end,
	
	case UpTimeIncludeWarning of
		true->
			ok;
		_->
			ok
	end,
	
	THIS:printImage(S, Cons),
	

	case Truedata of
		true->
			ok;			
		_->
			case HideReportTables of
				false->
					THIS:hTMLTables(S, Cons, length(Cons),BgColor,Border);
				_->
					ok
			end
	end,
		
	case HideReportErrors of
		false->
			THIS:hTMLErrorTable(S,BgColor,Border);
		_->
			ok
	end,

	case HideReportWarnings of
		false->
			THIS:hTMLWarningTable(S,Border);
		_->
			ok
	end,

	case HideReportGoods of
		false->
			THIS:hTMLGoodTable(S,Border);
		_->
			ok
	end,
	
	io:format("createReport ok: ~p ~n", [calendar:local_time()]),

	ok.
  
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
createReport(S)->
	io:format(S, "~s ~n", ["<html>"]),
	io:format(S, "~s ~n", ["<head>"]),
	io:format(S, "~s ~n", ["<META http-equiv='Content-Type' content='text/html; charset=UTF-8'>"]),
	io:format(S, "~s ~n", ["<link rel='stylesheet' href='/css/SiteView.css' type='text/css' media='screen' charset='utf-8'>"]),	
	io:format(S, "~s ~n", ["</head>"]),
	io:format(S, "~s ~n", ["<body BGCOLOR=#ffffff LINK=#1155bb ALINK=#1155bb VLINK=#1155bb>"]),
	LL = 	["<TABLE WIDTH=100% BORDER=0>  
	<TR>
	<TD ALIGN='LEFT' WIDTH='33%'>"] ++ 
	["   <A HREF='#historyTable'>"++gettext:key2str("historyTable","zh-cn")++"</A><BR/>"] ++ 
	["   <A HREF='#errorTable'>"++gettext:key2str("errorTable","zh-cn")++"</A><BR/>"] ++ 
	["   <A HREF='#warningTable'>"++gettext:key2str("warningTable","zh-cn")++"</A><BR/>"] ++ 
	["   <A HREF='#goodTable'>"++gettext:key2str("goodTable","zh-cn")++"</A><BR/>"] ++ 
%% 	["   <A HREF='#alertReport'>"++gettext:key2str("alertReport","zh-cn")++"</A>"] ++ 
	["  </TD>
	  <TD ALIGN=CENTER WIDTH=34%>"] ++ 
	["   <A HREF='javascript:history.go(-1)'>"++gettext:key2str("Report Index","zh-cn")++"</A>"] ++ 
	["  </TD>
	  <TD ALIGN=RIGHT WIDTH=33%>
	   &nbsp;
	  </TD>
	 </TR>
	</TABLE><p/>"],
	io:format(S, "~s ~n", [lists:flatten(LL)]),
	
%% 	io:format("gettext:key2str(warningTable):~p ~n ", [gettext:key2str("warningTable","zh-cn")]),
%% 	io:format(S, "~s ", ["<br>"]),
%% 	io:format(S, "~s ~n", ["properties"]),
%% 	
%% 	Props = THIS:get_properties(),
%% 	F = fun(X)->
%% 		io:format(S, "~s ", ["<br>"]),
%% 		io:format(S, "~p ~n", [X])
%% 	end,
%% 	lists:foreach(F, Props),
%% 	
%% 	io:format(S, "~s ", ["<br>"]),
%% 	io:format(S, "~s ~n", ["attributes"]),
%% 	Attrs = THIS:get_attributes(),
%% 	lists:foreach(F, Attrs),
%% 	
%% 	io:format(S, "~s ", ["<br>"]),
%% 	io:format(S, "~s ~n", ["monitordata"]),
	
%% 	{Year,Month,Day} = date(),
%% 	Date = io_lib:format("~p-~p-~p",[Year,Month,Day]),

%% 	F1 = fun(X2)->
%% 		io:format(S, "~s ", ["<br>"]),
%% 		io:format(S, "~p ~n", [X2]),
%% 		io:format(S, "~s ", ["<br>"]),
%% 	    {eof, Data} = monitor_logger:q(Date, list_to_atom(X2)),
%% %% 		F2 = fun(X3)->
%% %% %%			{_,ID,Name, RecordTime, State, Des, Returns} = X3,
%% %% %%			io:format(S, "~s ~n", [Name, RecordTime, State, Des, Returns]),
%% %% 			io:format(S, "~s ~n", [X3]),
%% %% 			io:format(S, "~s ", ["<br>"])
%% %% 		end,
%% %% 		lists:foreach(F2, [Data])
%%  	io:format(S, "~p ~n", [Data])
%% %% 		file:write(S, [Data])
%% 	end,
	
%% 	{eof, AllData} = monitor_logger:q(""),
	io:format("history report 1: ~p ~n", [THIS:get_app()]),
%% 	THIS:set_app(dbcs_base:get_app()),
	io:format("history report 2: ~p ~n", [dbcs_base:get_app()]),
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	{ok,{_, Precision}} = THIS:get_property(precision),
	io:format("Precision:~p ~n", [Precision]),
	{ok,{_, StartTime}} = THIS:get_property(startTime),
%% 	io:format("~p ~n", [StartTime]),
 	io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(StartTime)]),
	{ok,{_, EndTime}} = THIS:get_property(endTime),
	io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(EndTime)]),

	{ok,{_, SchedFilter}} = THIS:get_property(schedFilter),
	io:format("~p ~n", [SchedFilter]),
	ScheduleMap	= schedule_manager:get_history_schedule_filter(SchedFilter),
	case ScheduleMap of
		null ->
		    SchedName = "all";
		_->
			SchedName = dbcs_schedule:get_name(SchedFilter)
	end,
	io:format("~p ~n", [ScheduleMap]),
	{ok,{_, Description}} = THIS:get_property(description),
	io:format("~p ~n",[Description]),
	case Description == [] of
		true->
		    Des = "";
		_->
			Des = Description
	end,
	
	{ok,{_, StatusFilter}} = THIS:get_property(statusFilter),
	io:format("monitor_logger start: ~p ~n", [calendar:local_time()]),
	{ok,{_, T}} = THIS:get_property(monitors),
	MonitorStr = string:tokens(T, ","),	
	
	{ok,{_, Truedata}} = THIS:get_property(truedata),	
	Count = 20,
	case Truedata of
		true->
		{_, AllData} = monitor_logger:qc(MonitorStr, Count);
		_->
		{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
				calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr)
	end,
%% 	io:format("monitor_logger data: ~p ~n", [AllData]),
	io:format("monitor_logger end: ~p ~n", [calendar:local_time()]),
	
%% 	F2 = fun(X2)->
%% 		case X2 of
%% 			{eof, ChildData} ->
%% 				io:format("~p ~n", [ChildData]);
%% 			Else ->
%% 				io:format("~p ~n", [Else])
%% 		end
%% 	end,
%% 	lists:foreach(F2, AllData),
	
%% 	io:format(S, "~p ~n", [AllData]),
	io:format("process start: ~p ~n", [calendar:local_time()]),
	case Truedata of
		true->	
		Data = AllData,
		case length(Data) > Count of
			true ->
				Count1 = Count,
				Data1 = lists:sublist(Data, length(Data) - Count, Count);
			_->
				Count1 = length(Data),
				Data1 = Data
		end,
		Map = THIS:processTrueData(S, Collectors, Data1, Count1, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, ScheduleMap, bestCaseCalc);	
		_->
		Map = THIS:process(S, Collectors, AllData, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, ScheduleMap, bestCaseCalc)	
	end,

%% 	case Truedata of
%% 		true->
%% 			lists:foreach(fun(X)->X:dropwhileBucket()end, Collectors);
%% 		_->
%% 			ok
%% 	end,
	
	io:format("process end: ~p ~n", [calendar:local_time()]),
	{ok,{_, Title}} = THIS:get_property(title),
	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
 	L = [Title, ReportPeriod],	
%% 	io:format("~p ~n", ["Hahdddda"]),
	
	{ok,{_, ShowThresholdSummary}} = THIS:get_property(showReportThresholdSummary),
	{ok,{_, ShowErrorTimeSummary}} = THIS:get_property(showReportErrorTimeSummary),
	
	{ok,{_, Disabled}} = THIS:get_property(disabled),
	
	{ok,{_, ShowReportAlerts}} = THIS:get_property(showReportAlerts),
	{ok,{_, WarningNotIncluded}} = THIS:get_property(warningNotIncluded),
	{ok,{_, UpTimeIncludeWarning}} = THIS:get_property(upTimeIncludeWarning),		
	
	{ok,{_, HideReportSummary}} = THIS:get_property(hideReportSummary),
	{ok,{_, HideReportTables}} = THIS:get_property(hideReportTables),
	{ok,{_, HideReportErrors}} = THIS:get_property(hideReportErrors),
	{ok,{_, HideReportWarnings}} = THIS:get_property(hideReportWarnings),
	{ok,{_, HideReportGoods}} = THIS:get_property(hideReportGoods),
    {ok,{_, Format}} = THIS:get_property(format),
    io:format("Format:~p ~n", [Format]),
     BgColor = getBackColor(Format),
     Border =  getBorder(Format),
%% 	io:format("ShowThresholdSummary:~p ~n", [ShowThresholdSummary]),
%% 	io:format("ShowErrorTimeSummary:~p ~n", [ShowErrorTimeSummary]),
%% 	io:format("Disabled:~p ~n", [Disabled]),
%% 	
%% 	io:format("ShowReportAlerts:~p ~n", [ShowReportAlerts]),
%% 	io:format("WarningNotIncluded:~p ~n", [WarningNotIncluded]),
%% 	io:format("UpTimeIncludeWarning:~p ~n", [UpTimeIncludeWarning]),
%% 	
%% 	io:format("HideReportTables:~p ~n", [HideReportTables]),
%% 	io:format("HideReportErrors:~p ~n", [HideReportErrors]),
%% 	io:format("HideReportWarnings:~p ~n", [HideReportWarnings]),
%% 	io:format("HideReportGoods:~p ~n", [HideReportGoods]),
	
%% 	io:format("~p ~n",[L]),
	X = ["<TABLE ALIGN=CENTER BORDER=0>" , "<TR><TD><H2 ALIGN=CENTER> " , Title , 
		 "</H2><TD></TR>" , "<TR><TD>" , "<P ALIGN=CENTER>" , ReportPeriod , "<BR/><FONT=-1>"++gettext:key2str("Filter data By Schedule","zh-cn")++":",  
		 SchedName, "</FONT>", "<BR/><FONT=-1> "++gettext:key2str("Filter data by status","zh-cn")++":", StatusFilter, "</FONT>", "</P></TD></TR>" , "<TR><TD><P ALIGN=CENTER><B>" , 
		 Des, "</B></P></TD></TR></TABLE>"],
%% 	io:format("~p ~n", ["Description"]),	
	io:format(S, "~s ~n", [lists:flatten([X])]),
	{_, Consold} = THIS:count_Con(Map, dict:fetch_keys(Map), [], 0),
	
%%  printwriter.print("\n<!--TIMEPERIOD\n" + dateStringDate(date) + "\t" + dateStringTime(date) + "\t" + dateStringDate(date1) + "\t" + dateStringTime(date1) + "\nENDTIMEPERIOD-->\n");	
%% 	ReportPeriod,
	
%%  printwriter.print("\n<!--OVERVIEW\n" + getSummaryString() + "ENDOVERVIEW-->\n");
	case THIS:get_property(isadhoc) of
		{error,{_, _}} -> 
			{ok,{_, FileName}} = THIS:get_property(filename),
			{ok,{_, ReportDirectory}} = THIS:get_property(reportDirectory),	 
			DataPath = ReportDirectory ++ FileName ++ ".data",
			SummaryList = getSummaryListByMap(Map, dict:fetch_keys(Map), []),
			unconsult(DataPath, SummaryList);
		_->
			continue
	end,
	
%% 	historyreport_static:generateIndexPage(THIS, s, ReportDirectory, flag, s2),
	
%% 	T = file:consult(DataPath),
%% 	io:format("~p ~n", [T]),
%% 	io:format("ffffff:  ~s ~n", [lists:flatten(["\n<!--OVERVIEW\n"] ++ SummaryList ++ ["ENDOVERVIEW-->\n"])]),
%% 	io:format(S, "~s ~n", [lists:flatten(["\n<!--OVERVIEW\n"] ++ SummaryList ++ ["ENDOVERVIEW-->\n"])]),
	
%% 	Cons = THIS:filterDataWithStatus(Consold, StatusFilter, []),	
	Cons = THIS:filterDataWithStatus(Consold, "all", []),
    ConsError = THIS:filterDataWithStatus(Consold, "error", []),
 %%   ConsWarning = THIS:filterDataWithStatus(Consold, "warning", []),
 %%   ConsGood = THIS:filterDataWithStatus(Consold, "good", []),
    
	case ShowThresholdSummary of
		true->
			THIS:hTMLThresholdSummary(S,BgColor,Border);
		_->
			ok
	end,
	
	case HideReportSummary of
		false->
			THIS:hTMLSummaryTable(S, Cons,BgColor,Border);
		_->
			ok
	end,
	
	case ShowErrorTimeSummary of
		true->
			THIS:hTMLErrorTimeSummary(S, ConsError,BgColor,Border);
		_->
			ok
	end,

	case Disabled of
		true->
			ok;
		_->
			ok
	end,
	
	case WarningNotIncluded of
		true->
			ok;
		_->
			ok
	end,
	
	case UpTimeIncludeWarning of
		true->
			ok;
		_->
			ok
	end,
	
	THIS:printImage(S, Cons),
	

	case Truedata of
		true->
			ok;			
		_->
			case HideReportTables of
				false->
					THIS:hTMLTables(S, Cons, length(Cons),BgColor,Border);
				_->
					ok
			end
	end,
		
	case HideReportErrors of
		false->
			THIS:hTMLErrorTable(S,BgColor,Border);
		_->
			ok
	end,

	case HideReportWarnings of
		false->
			THIS:hTMLWarningTable(S,Border);
		_->
			ok
	end,

	case HideReportGoods of
		false->
			THIS:hTMLGoodTable(S,Border);
		_->
			ok
	end,
	
 	case ShowReportAlerts of
 		true->
 			THIS:hTMLAlertReport(S);
 		_->
 			ok
 	end,
%% 	AlertReport.generateReport(...),	 
%% 	io:format("~p ~n", [THIS:gatherAlertInfo({2009,11,17}, {2009,11,17}, scheduleMap)]),
	
	THIS:addLogo(S),	
	io:format(S, "~s ~n", ["</body>"]),	
	io:format(S, "~s ~n", ["</html>"]),	
	io:format("~p ~n", ["createReport ok"]),	
	file:close(S),
	
%% 	F4 = fun(X4)->
%% 		ets:delete(X4:get_tid())
%% 	end,
%% 	lists:foreach(F4, Consold),
    %%
    %% xmlHistoryTable ��� 
    %%
    PXML = 
        case THIS:get_property(xmlfile) of
            {ok,{_, _PXML}} ->
                _PXML;
            _ ->
                false
        end,
    if
        PXML =:= true ->
            xmlHistoryTable([],[],[],[],[]);
        true ->
            ok
    end,
    PSpreadsheet = 
        case THIS:get_property(tabfile) of
            {ok,{_, _PSpreadsheet}} ->
                _PSpreadsheet;
            _ ->
                false
        end,
    
    %% textHistoryTable �ı��������
    io:format("PSpreadsheet !!!!!!!!!!!!~n~p~n", [PSpreadsheet]),
    if
        PSpreadsheet =:= true ->
            textHistoryTable([]);
        true ->
            ok
    end,
	io:format("createReport ok: ~p ~n", [calendar:local_time()]),
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
printImage(File,Map)->
%% 	GraphCollectors=THIS:getCollectorList(Map,dict:fetch_keys(Map)),
	GraphCollectors=Map,
%% HistoryReportHTML,Files,ConCollectorList,[H|E],CollectorMap,SimilarProperties,MultipleMonitors,Imgcount
    {ok,{_,ReportType}} = THIS:get_property(reportType),
	SimilarProperties = string:rstr(ReportType, "similarProperties")>0,
	MultipleMonitors = string:rstr(ReportType, "multipleMonitors")>0,
	LineGraph=string:rstr(ReportType, "lineGraph")>0,
	io:format(File, "~s ~n", ["<center>"]),	
	io:format(File, "~s ~n", ["<table>"]),
	case LineGraph of 
		true ->
			RR=htmlChart([],File,GraphCollectors,GraphCollectors,[],SimilarProperties,MultipleMonitors,1);
		false ->
			RR=buildimageBarGraph(File,GraphCollectors,1)
	end,
	io:format(File, "~s ~n", ["</center>"]),	
	io:format(File, "~s ~n", ["</table>"]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
getCollectorList(Map,Keys)->
	V=[dict:fetch(Key, Map) ||Key<-Keys],
	F= fun(X, Sum) -> X ++ Sum end,
	NL=lists:foldl(F, [], V),
	NL.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
%%
%%HistoryReportHTML
%%Files
%%CollectorList
%%AllPropertiesSame true or false
%%AllMonitorsSame   true or false
%%ImageCount
%%
imageLineGraph(HistoryReportHTML,Files,CollectorList,AllPropertiesSame,AllMonitorsSame,ImageCount,Width,Height )->
     LineCount =length(CollectorList),
	if LineCount=:=0 ->
		   Ret=[];
	   LineCount>0 ->
%%  Names=  [ Samplecollector:getPropertyLabel() || Samplecollector<-CollectorList],
%% 		 F=fun(Samplec) ->
%% 				M=Samplec:getMonitorName(),
%%               if (AllPropertiesSame=/=true)->
%%                      Name=M++"("++ Samplec:getPropertyLabel()++")";
%% 				  AllPropertiesSame=:=true ->
%% 					  Name=M
%%                end,
%% 		      Average=Samplec:getAverage(),
%% 			  Maximum=Samplec:getMaximum(),
%% 			  MaximumTime=Samplec:getMaximumTime(),
%% 			 [{_,Data}] = ets:lookup(Samplec:get_tid(), buckets),
%% 			  AveragedSamples=Samplec:samplesAreAveraged(),
%% 				if (AllMonitorsSame=:=true)->
%% 					   TotalTime=Samplec:getTotalTime(),
%% 				       ErrorTime=Samplec:getErrorTime(),
%% 					   ErrorPercentage=ErrorTime/TotalTime*100;
%% 			        AllMonitorsSame=/=true ->
%% 		               TotalTime=null
%% 				end
%% 		 end,
%% 		 lists:foreach(F, CollectorList),
		 Params=imageLineGraph_1(CollectorList,[],AllPropertiesSame,AllMonitorsSame,1,0),
 		 FirstCollector=hd(CollectorList),
		 {ok,{_, ReportType}} = THIS:get_property(reportType),
		 IsMultipleMonitors=string:rstr(ReportType, "multipleMonitors")>0,
		  {ok,{_, Reporttitle}} = THIS:get_property(title),
		 case  IsMultipleMonitors of
			 true ->
				 if LineCount>1 ->
					 Title =Reporttitle;
					true->
					   Title=FirstCollector:getMonitorName()
				 end;
			 false ->
				 Title=FirstCollector:getMonitorName()
		 end,
%% 		 case IsMultipleMonitors andalso LineCount>1 of
%%             true ->
%%                Title =Reporttitle;
%%             false->
%%                Title=FirstCollector:getMonitorName()
%%          end,
		 PropertyName=FirstCollector:getPropertyLabel(),
		{value,{_,High}} =lists:keysearch(high, 1, Params),
		 {ok,{_, Vmax}} = THIS:get_property(vmax),
	    AutoScale=case Vmax of
				      "default"->
					     V=High;
				       _  ->
					     V=list_to_atom(Vmax) 
			      end,
	     if V<1 ->
		      VertMax=1;
	      true ->
		      VertMax=V
	      end,
%% 		 VertMax=1,
		 StartTime=FirstCollector:getStartTime(),
		 EndTime=FirstCollector:getEndTime(),
		 P=FirstCollector:getProperty(),
		 Monitorid=FirstCollector:getMonitorFullID(),
	     TmpParams=Params++[{title,Title},{startTime,StartTime},{endTime,EndTime},{propertyName,PropertyName},{vertMax,VertMax},{ncount,integer_to_list(ImageCount)},{monitorid,Monitorid},{ret,P}],
          bulidImage(TmpParams,true,Files,plot2d)
		 end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
%% no maxdata 
%% N=1 default
%%High =0 default
imageLineGraph_1([H|E],Params,AllPropertiesSame,AllMonitorsSame,N,High)->
	        Samplec=H,
	    	M=Samplec:getMonitorName(),
			 P1=Samplec:getPropertyLabel(),
				 PropertyLabel=case is_atom(P1) of
								  true ->
									  RetP=atom_to_list(P1);
								  false->
									  RetP=P1
							  end,
              if (AllPropertiesSame=/=true)->
                      Name=M++"("++ PropertyLabel ++")";
				  AllPropertiesSame=:=true ->
					  Name=M
               end,
	          Average=Samplec:getAverage(),
			  Maximum=Samplec:getMaximum(),
			  Minimum=Samplec:getMinimum(),
			  if Maximum>High ->
					THigh= Maximum;
				 true ->
					 THigh=High
			  end,
			  MaximumTime=Samplec:getMaximumTime(),
			 [{_,Data}] = ets:lookup(Samplec:get_tid(), buckets),
%% 			{ok,{_,Data}} = Samplec:get_property(buckets),	
			  AveragedSamples=Samplec:samplesAreAveraged(),
			  Strn=integer_to_list(N),
			 if    AllMonitorsSame->
					   TotalTime=Samplec:getTotalTime(),
				       ErrorTime=Samplec:getErrorTime(),
					   case TotalTime of
						   0 ->
						      TErrorPercentage=0;
						   _ ->  
							   TErrorPercentage=ErrorTime/TotalTime*100
					   end,
					   if (TErrorPercentage>100)->
							ErrorPercentage=100;
                           true ->
							   ErrorPercentage=TErrorPercentage
					   end,
					 
				       TmpParams=Params++[{"name"++Strn,Name},{"average"++Strn,Average},{"maximum"++Strn,Maximum},{"minimum"++Strn,Minimum}]++
								 [{"maximumTime"++Strn,MaximumTime},{"data"++Strn,Data},{"averagedSamples"++Strn,AveragedSamples}]++
								 [{"errorTime"++Strn,ErrorTime},{"errorPercentage"++Strn,ErrorPercentage}];
			        not AllMonitorsSame ->
		               TmpParams=Params++[{"name"++Strn,Name},{"average"++Strn,Average},{"maximum"++Strn,Maximum},{"minimum"++Strn,Minimum}]++
								 [{"maximumTime"++Strn,MaximumTime},{"data"++Strn,Data},{"averagedSamples"++Strn,AveragedSamples}]
				end,
	imageLineGraph_1(E,TmpParams,AllPropertiesSame,AllMonitorsSame,N+1,THigh);
imageLineGraph_1([],Params,_,_,N,High) ->
  Params++[{n,N},{high,High}].
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
%%ReportObjectConfig,
%%default value
%%Imgcount=1
%%CollectorMap=[]
%%getMonitorFullID()--getIDString()
htmlChart(HistoryReportHTML,Files,ConCollectorList,[H|E],CollectorMap,SimilarProperties,MultipleMonitors,Imgcount)->
	Samplecollector=H,
	case Samplecollector:isNumeric() of
           true ->
  			     MonitorId=Samplecollector:getMonitorFullID(),
				 P1=Samplecollector:getProperty(),
				 Propertyname=case is_atom(P1) of
								  true ->
									  RetP=atom_to_list(P1);
								  false->
									  RetP=P1
							  end,
				 MonitorIdPropertyname=atom_to_list(MonitorId)++Propertyname,
			   case lists:keymember(MonitorIdPropertyname, 1, CollectorMap) of
				   false ->
					 Ret=bulidNewCollectorList(Samplecollector,ConCollectorList,CollectorMap,[],SimilarProperties,MultipleMonitors,true,true,[],[]),
					 {value,{_,NewCollectorList}}=lists:keysearch(collectorList, 1, Ret),
				     {value,{_,AllPropertiesSame}}=lists:keysearch(allPropertiesSame, 1, Ret),
					 {value,{_,AllMonitorsSame}}=lists:keysearch(allMonitorsSame, 1, Ret),
					 {value,{_,TCollectorMap}}=lists:keysearch(collectorMap, 1, Ret),
					 case length(NewCollectorList)>0 of
						 true->
 							TmpParams=imageLineGraph(HistoryReportHTML,Files,NewCollectorList,AllPropertiesSame,AllMonitorsSame,Imgcount,600,300),
							Tmpimagecount=Imgcount+1;
					      false->
	                       Tmpimagecount=Imgcount
					 end;
				   true ->
					   Tmpimagecount=Imgcount,
					   TCollectorMap=CollectorMap
			   end;
		      false ->
				  Tmpimagecount=Imgcount,
				  TCollectorMap=CollectorMap 
	end,
	htmlChart(HistoryReportHTML,Files,ConCollectorList,E,TCollectorMap,SimilarProperties,MultipleMonitors,Tmpimagecount);
htmlChart(_,_,_,[],_,_,_,_)->
	htmlChartok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
bulidNewCollectorList(SampleCollector,[H|E],CollectorMap,CollectorList,SimilarProperties,MultipleMonitors,AllPropertiesSame,AllMonitorsSame,LastMonitor,LastProperty) ->
	Coll=H,
	case Coll:isNumeric() of
           true ->
				 MonitorId=Coll:getMonitorFullID(),
				 P1=Coll:getProperty(),
				 Propertyname=case is_atom(P1) of
								  true ->
									  RetP=atom_to_list(P1);
								  false->
									  RetP=P1
							  end,
				 MonitorIdPropertyname=atom_to_list(MonitorId)++Propertyname,
			   case lists:keymember(MonitorIdPropertyname, 1, CollectorMap) of
				   false ->
					   case ((SampleCollector:getMonitor()=:=Coll:getMonitor()) orelse MultipleMonitors) of
                            true ->
								 Ret1=SampleCollector:getProperty()=:=Coll:getProperty(),
                              case (Ret1 orelse SimilarProperties) of
								  true ->
									  TmpCollectorList=CollectorList++[Coll],
									  case AllPropertiesSame of true ->
									     case is_atom(LastProperty) andalso LastProperty=/=Coll:getProperty() of
                                           true ->
										    	 TmpAllPropertiesSame=false;
										    false->
											     TmpAllPropertiesSame=true
                                          end;
										  false ->
											 TmpAllPropertiesSame=false 
									  end,
									   case AllMonitorsSame of true ->
							              case is_atom(LastMonitor) andalso LastMonitor=/=Coll:getMonitor() of
                                               true ->
											       TmpAllMonitorsSame=false;
										        false->
											       TmpAllMonitorsSame=true
                                               
                                           end;
										   false ->
											  TmpAllMonitorsSame=false  
									  end,
									 
									  TmpCollectorMap=CollectorMap++[{MonitorIdPropertyname,"graphed"}];
								  false ->
									  TmpCollectorMap=CollectorMap,
			                          TmpCollectorList=CollectorList,
			       					  TmpAllPropertiesSame=AllPropertiesSame,
			       					  TmpAllMonitorsSame=AllMonitorsSame
							  end;
						        false ->
									  TmpCollectorMap=CollectorMap,
			                          TmpCollectorList=CollectorList,
			       					  TmpAllPropertiesSame=AllPropertiesSame,
			       					  TmpAllMonitorsSame=AllMonitorsSame
                       end;
				 true ->
                    TmpCollectorMap=CollectorMap,
			        TmpCollectorList=CollectorList,
			        TmpAllPropertiesSame=AllPropertiesSame,
			        TmpAllMonitorsSame=AllMonitorsSame
			   end ;
	       false ->
			   TmpCollectorMap=CollectorMap,
			   TmpCollectorList=CollectorList,
			   TmpAllPropertiesSame=AllPropertiesSame,
			   TmpAllMonitorsSame=AllMonitorsSame
			   
   end,
	TmpLastMonitor=Coll:getMonitor(),
	TmpLastProperty=Coll:getProperty(),
	bulidNewCollectorList(SampleCollector,E,TmpCollectorMap,TmpCollectorList,SimilarProperties,MultipleMonitors,TmpAllPropertiesSame,TmpAllMonitorsSame,TmpLastMonitor,TmpLastProperty);
bulidNewCollectorList(_,[],CollectorMap,CollectorList,_,_,AllPropertiesSame,AllMonitorsSame,_,_) ->
	[{collectorList,CollectorList},{allPropertiesSame,AllPropertiesSame},{allMonitorsSame,AllMonitorsSame},{collectorMap,CollectorMap}].

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
buildimageBarGraph(_,[],_)->
	ok;
buildimageBarGraph(Files,[H|E],ImageCount)->
	imageBarGraph(Files,H,ImageCount),
	buildimageBarGraph(Files,E,ImageCount+1).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
imageBarGraph(Files,SampleCollector,ImageCount)->
	Label=SampleCollector:getPropertyLabel(),
    Propertyname=case is_atom(Label) of
				   true ->
					 RetP=atom_to_list(Label)++integer_to_list(ImageCount);
				   false->
					 RetP=Label++integer_to_list(ImageCount)
				end,
	MonitorName=SampleCollector:getMonitorName(),
	Name=MonitorName ++"("++Propertyname++")",
	Average=SampleCollector:getAverage(),
	Maximum=SampleCollector:getMaximum(),
	
	if (Maximum>0) ->
		   High=Maximum;
	   true ->
		   High=0
	end,
	MaximumTime=SampleCollector:getMaximumTime(),
	Minimum=SampleCollector:getMinimum(),
	MinimumTime=SampleCollector:getMinimumTime(),
	[{_,Data}] = ets:lookup(SampleCollector:get_tid(), buckets),
%% 	{ok,{_,Data}} = SampleCollector:get_property(buckets),
	MaxData="",
	TotalTime=SampleCollector:getTotalTime(),
	ErrorTime=SampleCollector:getErrorTime(),
	case TotalTime of
		0 ->
			TErrorPercentage=0;
		_ ->
			TErrorPercentage=ErrorTime/TotalTime*100
	end,
	if TErrorPercentage>100 ->
		   ErrorPercentage=100;
	   true ->
		   ErrorPercentage=TErrorPercentage
	end,
	AveragedSamples=SampleCollector:samplesAreAveraged(),
	BucketCount=SampleCollector:getBucketCount(),
	{ok,{_, Vmax}} = THIS:get_property(vmax),
	AutoScale=case Vmax of
				  "default"->
					  V=High;
				  _  ->
					  V=list_to_atom(Vmax) 
			  end,
			    
	if V<1 ->
		  VertMax=1;
	   true ->
		   VertMax=V
	end,
	
	Title=SampleCollector:getMonitorName(),
	StartTime="",
	EndTime="",
	PropertyName=SampleCollector:getPropertyLabel(),
	Monitorid=SampleCollector:getMonitorFullID(),
	Params=  [{"name1",Name},{"average1",Average},{"maximum1",Maximum}]
		   ++[{"maximumTime1",MaximumTime},{minimum,Minimum},{minimumTime,MinimumTime}]
           ++[{errorTime,ErrorTime},{errorPercentage,ErrorPercentage},{"averagedSamples1",AveragedSamples}]
           ++[{bucketCount,BucketCount},{vertMax,VertMax},{propertyName,Propertyname},{ncount,integer_to_list(ImageCount)}]
           ++[{title,Title},{startTime,StartTime},{endTime,EndTime},{"data1",Data},{n,2},{monitorid,Monitorid},{ret,Label}],
	bulidImage(Params,false,Files,bar2d).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
bulidImage(Params,IsLineChart,File,Type) ->
	{value,{_,Title}}=lists:keysearch(title, 1, Params),
	{value,{_,TPropertyname}}=lists:keysearch(propertyName, 1, Params),
		if is_atom(TPropertyname)->
			 APropertyname=atom_to_list(TPropertyname);
		   true ->
			  APropertyname=TPropertyname
		end,
	Propertyname=util:delSubstr(APropertyname,"/"),
	{value,{_,N}}=lists:keysearch(n, 1, Params), 
	case IsLineChart of 
		false ->
	        {value,{_,TMax}}=lists:keysearch("maximum1", 1, Params),
     	    {value,{_,TMin}}=lists:keysearch(minimum, 1, Params),
	        {value,{_,TAverage}}=lists:keysearch("average1", 1, Params),
%% 	        {value,{_,Name}}=lists:keysearch(name, 1, Params),
			Max=THIS:to_list(TMax),
			Min=THIS:to_list(TMin),
			Average=THIS:to_list(TAverage),
	        SubTitle="Max:"++hd(Max)++" Min:"++hd(Min)++" Average:"++hd(Average);
		true ->
			if N=:=2 ->
				    {value,{_,TMax}}=lists:keysearch("maximum1", 1, Params),
	                {value,{_,TAverage}}=lists:keysearch("average1", 1, Params),
					{value,{_,TMin}}=lists:keysearch("minimum1", 1, Params),
					
		         	Max=THIS:to_list(TMax),
					Min=THIS:to_list(TMin),
					Average=THIS:to_list(TAverage),
	        		SubTitle="Max:"++hd(Max)++" Min:"++hd(Min)++" Average:"++hd(Average);
			   true ->
			        SubTitle="" 
			end
%% 			Name=Propertyname
    end,
%% 	realTime
	{value,{_,Ncount}}=lists:keysearch(ncount, 1, Params),
	{_,{_,RealTime}} = THIS:get_property(realTime),
	case RealTime of 
		true ->
			{_,{_,ReportDirectory}}=THIS:get_property(reportDirectory),
			{_,{_,Filename}}=THIS:get_property(filename),
			PathFile=ReportDirectory++Filename++Ncount++".png",
			PathF=Filename++Ncount++".png";
		_   ->
			{_,{_,ReportDirectory}}=THIS:get_property(reportDirectory),
			{_,{_,Filename}}=THIS:get_property(filename),
			PathFile=ReportDirectory++Filename++Ncount++".png",
			PathF=Filename++Ncount++".png"
	end,
	{value,{_,MP}}=lists:keysearch(ret, 1, Params),
	{value,{_,Monitorid}}=lists:keysearch(monitorid, 1, Params),
  Tthreshold=api_monitor:get_all_classifier(Monitorid),
  TrmpLThreshold=lists:keysearch(error, 1, Tthreshold),
	case TrmpLThreshold of 
		false ->
			Threshold=0;
		_ ->
			{value,{_,LThreshold}} =TrmpLThreshold,
			TemThreshold= lists:keysearch(MP, 1, LThreshold),
			case TemThreshold of 
				false ->
					Threshold=0;
				_ ->
					{value,{_,_,Threshold}}=TemThreshold
			end
	end,
	{_,{_,Trends_line}}=THIS:get_property(trends_line),
%% 	io:format("Trends_line:~p ~n", [Trends_line]),
%%     {trends_line, true}
case Trends_line of
	true ->
	Options=[{width, 900}, {height, 350}, {threshold, Threshold},{x_data_type,datetime}]
            ++[{margin, 60}, {title1, Title}, {title2, SubTitle},{input_data, [""]}]
            ++[{save_file, PathFile},{trends_line, true}]
            ++[{plot, Type}, {run_function, png}],
		 {value,{_,Name}}=lists:keysearch("name1", 1, Params);
	_ ->
		Options=[{width, 900}, {height, 350}, {threshold, Threshold},{x_data_type,datetime}]
            ++[{margin, 60}, {title1, Title}, {title2, SubTitle},{input_data, [""]}]
            ++[{save_file, PathFile}]
            ++[{plot, Type}, {run_function, png}],
		 {value,{_,Name}}=lists:keysearch("name1", 1, Params)
	end,
 	TMData=buildImageData([],Params,N,1),
	io:format(File, "~s ~n", ["<tr>"]),
	io:format(File, "~s ~n", ["<td>"]),
	io:format(File, "~s ~n", ["<img src="++PathF++" />"]),
	io:format(File, "~s ~n", ["</tr>"]),
	io:format(File, "~s ~n", ["</td>"]),
    eplot_main:getimage(TMData,Options).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
buildImageData(Ret,_,_,0) ->
	Ret;
buildImageData(Ret,Params,Nmax,N)  ->
		 if Nmax>N ->
		   {value,{_,Name}}=lists:keysearch("name"++integer_to_list(N), 1, Params),
		   {value,{_,Data}}=lists:keysearch("data"++integer_to_list(N), 1, Params),
		   Bucketdatas=buildbuckets([],Data,1),
		   Tmp=Ret++[{Name,Bucketdatas}],
		   TN=N+1;
%% 		   buildImageData(Tmp,Params,Nmax,TN);
			true ->
				Tmp=Ret,
				TN=0
%% 				buildImageData(Ret,Params,0,N)
		 end,
		buildImageData(Tmp,Params,Nmax,TN).
%% 	io:format(S, "~p ~n", [{calendar:gregorian_seconds_to_datetime(Cons:getBucketStartTime(I)), H:getMaximum(), H:getMinimum(), H:getWorstCategory(), H:getTotalValue(), 
%% 			H:getAverage(), H:getTotal(), H:getTotalSamples()}]),

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
buildbuckets(RetData,[H|E],I)->	
	Bucket=H,
%% 	io:format("Bucket:~p ~n", [Bucket]),
	{_, {{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_}, {connector, SampleCollector}}} = Bucket,
%%  	{_, {{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_}, {_,_}, {connector, SampleCollector}}} = Bucket,	
%% 	io:format("SampleCollector:~p ~n", [SampleCollector]),
%% 	{_,_,SampleCollector}=Bucket,
%% 	SampleCollector=getConnector
	Time=SampleCollector:getBucketStartTime(I),
%% 	Value=Bucket:getAverage(),
	Value=SampleCollector:getBucketData(Bucket, getAverage),
	TRetData=RetData++[{Time,Value}],
     buildbuckets(TRetData,E,I+1);
buildbuckets(RetData,[],_)->
	RetData.
	
getColorProperty(String, Color)->
	ok.

getStringProperty(String, String)->
	ok.

getGraphMaximum(StringProperty, Monitor, float)->
	ok.

makeColor(String)->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLTableHeader_Child([], S, StartColumn, Columns, Index, Count, BucketSize)->
	ok;
hTMLTableHeader_Child([H|T], S, StartColumn, Columns, Index, Count, BucketSize)->
	case (Count >= StartColumn) and (Count < (StartColumn+Columns)) of 
	true->
		L = ["<TH  WIDTH="] ++ [to_list(80/Columns)] ++ ["%>"] ++ [to_list(H:getMonitorName())]
		++ ["  "] ++ [to_list(H:getPropertyLabel())], 
		io:format(S, "~s ~n", [lists:flatten(L)]),
		hTMLTableHeader_Child(T, S, StartColumn, Columns, Index, Count+1, BucketSize);
	_->
%% 		io:format("~p ~n", [H]),
		hTMLTableHeader_Child(T, S, StartColumn, Columns, Index, Count+1, BucketSize)
   end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLTableHeader(Cons, S, StartColumn, Columns, Index, Count, BucketSize,Border)->  
	case Index =/= 0 of
	true ->
		L1 = "</TABLE><br>",
		io:format(S, "~s ~n", [lists:flatten(L1)]);
		_->
		L1 = ["<A name=historyTable></A>"] ,
		io:format(S, "~s ~n", [lists:flatten(L1)])
	end,
	L = ["<P><TABLE WIDTH=100% Border="++[Border]++" CELLSPACING=0>"] ++ ["<TR BGCOLOR=#88AA99><TH WIDTH=20%>"++gettext:key2str("Time","zh-cn")++"</TH>"],
	io:format(S, "~s ~n", [lists:flatten(L)]),
	hTMLTableHeader_Child(Cons, S, StartColumn, Columns, Index, Count, BucketSize),
	io:format(S, "~s ~n", [lists:flatten(["</TH></TR>"])]).
    
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
get_Cons_Bucket_I([], BucketIndex, S, StartColumn, Columns, Count, Data,BgColor,Border) ->	
	Data;
get_Cons_Bucket_I([H|T], BucketIndex, S, StartColumn, Columns, Count, Data,BgColor,Border) ->
  case (Count >= StartColumn) and (Count < (StartColumn+Columns)) of 
	true->		
		case (Count == StartColumn) of
			true ->
				Head = ["<TR BgColor="++[BgColor]++">"] ++ ["<TD>"] ++ [sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(H:getBucketStartTime(BucketIndex)))] ++ ["</TD>"];
			_->  
				Head = [""]
		end,
		case (Count == StartColumn+Columns-1) of
			true ->
				End = ["</TR>"];
			_->  
				End = [""]
		end,		
		case H:getWorstCategory(BucketIndex) of
		good->
			Data1 = Head ++ Data ++ ["<TD BGCOLOR=#88CC99 ALIGN=RIGHT><B>"] ++ [to_list(H:getAverage(BucketIndex))] ++ ["</B></TD>"] ++ End;
		warning->
  			Data1 = Head ++ Data ++ ["<TD BGCOLOR=#FFFF77 ALIGN=RIGHT><B>"] ++ [to_list(H:getAverage(BucketIndex))] ++ ["</B></TD>"] ++ End;
  		error->
			Data1 = Head ++ Data ++ ["<TD BGCOLOR=#EE7777 ALIGN=RIGHT><B>"] ++ [to_list(H:getAverage(BucketIndex))] ++ ["</B></TD>"] ++ End;
		_->
			 Data1 = Head ++ Data ++ ["<TD BGCOLOR=#EE7777 ALIGN=RIGHT><B>"] ++ [to_list(H:getAverage(BucketIndex))] ++ ["</B></TD>"] ++ End
		end,			
		get_Cons_Bucket_I(T, BucketIndex, S, StartColumn, Columns, Count+1, Data1,BgColor,Border);
	_->
		get_Cons_Bucket_I(T, BucketIndex, S, StartColumn, Columns, Count+1, Data,BgColor,Border)
   end.	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
print_Cons_Buckets(Cons, S, StartColumn, Columns, PageIndex, Index, BucketSize,BgColor,Border)
	when (Index > BucketSize)->
	ok;
print_Cons_Buckets(Cons, S, StartColumn, Columns, PageIndex, Index, BucketSize,BgColor,Border) ->
%% 	io:format("~p ~n", [Index]),
%% 	io:format("~p ~n", [PageIndex]),
	case (Index > PageIndex*50) and (Index =< (PageIndex+1)*50) of
	true ->
		Buckete_I = get_Cons_Bucket_I(Cons, Index, S, StartColumn, Columns, 1, [],BgColor,Border),
		io:format(S, "~s ~n", [lists:flatten(Buckete_I)]);
	_->
		continue
	end,
	print_Cons_Buckets(Cons, S, StartColumn, Columns, PageIndex, Index+1, BucketSize,BgColor,Border).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLTable(S, StartColumn, Columns, Cons, Index, BucketSize,BgColor,Border) when (Index*50) >= BucketSize ->
	ok;
hTMLTable(S, StartColumn, Columns, Cons, Index, BucketSize,BgColor,Border)->
	THIS:hTMLTableHeader(Cons, S, StartColumn, Columns, Index, 1, BucketSize,Border),
	THIS:print_Cons_Buckets(Cons, S, StartColumn, Columns, Index, Index*50+1, BucketSize,BgColor,Border),
	case (Index+1)*50 > BucketSize of
	true->
%% 		End = ["</TABLE><br>"];
		io:format(S, "~s ~n", [lists:flatten(["</TABLE><br>"])]);
	_->
%% 		End = [""]
		continue
	end,	
	hTMLTable(S, StartColumn, Columns, Cons, Index+1, BucketSize,BgColor,Border).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLTable_Child(S, StartColumn, Columns, ColumnsLeft, Cons, BucketSize,BgColor,Border) when (ColumnsLeft =< 0) ->	
	ok;
hTMLTable_Child(S, StartColumn, Columns, ColumnsLeft, Cons, BucketSize,BgColor,Border)->	
	case ColumnsLeft < Columns of
		true->			
			THIS:hTMLTable(S, StartColumn, ColumnsLeft, Cons, 0, BucketSize,BgColor,Border),
			hTMLTable_Child(S, StartColumn + ColumnsLeft, ColumnsLeft, ColumnsLeft - Columns, Cons, BucketSize,BgColor,Border);
		_->
			THIS:hTMLTable(S, StartColumn, Columns, Cons, 0, BucketSize,BgColor,Border),
			hTMLTable_Child(S, StartColumn + Columns, Columns, ColumnsLeft - Columns, Cons, BucketSize,BgColor,Border)
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLTables(S, Cons, ConSize,BgColor,Border)->
	case ConSize > 0 of
		true->
			io:format(S, "~s ~n", [lists:flatten("<CENTER>")]),
			BucketSize = (lists:nth(1, Cons)):getBucketCount(),	
			hTMLTable_Child(S, 1, 5, ConSize, Cons, BucketSize,BgColor,Border),
			io:format(S, "~s ~n", [lists:flatten("</TABLE><br><br></CENTER>")]);
		_->
			ok
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
print_DataList(S, Color, Count, [], Max)->
	ok;
print_DataList(S, Color, Count, T, Max) when Count > Max ->
	ok;
print_DataList(S, Color, Count, [H|T], Max) ->	
	L = ["<tr BGCOLOR="] ++ [Color] ++ [">", "<td>"] ++ [sv_datetime:tostr(H#monitorlog.time)] ++ ["</td>", "<td>"] ++ to_list(H#monitorlog.name)
		++ ["</td>" ,"<td>"] ++	to_list(H#monitorlog.desc) ++ ["</td>", "</tr>"],	
	io:format(S, "~s ~n", [lists:flatten(L)]),
	print_DataList(S, Color, Count+1, T, Max).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLGoodTable(S,Border)->
	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
%% 	{ok,{_, List}} = THIS:get_attribute(goodlist),
	{ok,{_, List1}} = THIS:get_attribute(goodlist),
	List = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
	
	X = ["<A name=goodTable><P><TABLE WIDTH=100% Border="++[Border]++"  CELLSPACING=0><CAPTION><B>"++gettext:key2str("first 100 good","zh-cn") , ReportPeriod, 
	"</B></CAPTION>	<TR BGCOLOR=#88AA99><TH WIDTH=20%>"++gettext:key2str("Time","zh-cn")++"</TH><TH>"++gettext:key2str("Name","zh-cn")++"</TH><TH>"++gettext:key2str("Description","zh-cn")++"</TH></TR>"],
	io:format(S, "~s ~n", [lists:flatten([X])]),
	case length(List) == 0 of
		true ->
			return;
		_ ->
	  		case length(List) > ?MaxRecords of
				true ->
					THIS:print_DataList(S, "#88CC99", 1, List, ?MaxRecords);
				_->
					THIS:print_DataList(S, "#88CC99", 1, List, length(List)),
					error
			end			
	end,
	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLWarningTable(S,Border)->
	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
%% 	{ok,{_, List}} = THIS:get_attribute(warninglist),
	{ok,{_, List1}} = THIS:get_attribute(warninglist),
	List = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
	X = ["<A name=warningTable><P><TABLE WIDTH=100% Border="++[Border]++"  CELLSPACING=0><CAPTION><B>"++gettext:key2str("first 100 warning","zh-cn") , ReportPeriod, 
	"</B></CAPTION>	<TR BGCOLOR=#88AA99><TH WIDTH=20%>"++gettext:key2str("Time","zh-cn")++"</TH><TH>"++gettext:key2str("Name","zh-cn")++"</TH><TH>"++gettext:key2str("Description","zh-cn")++"</TH></TR>"],
	io:format(S, "~s ~n", [lists:flatten([X])]),
	case length(List) == 0 of
		true ->
			return;		
		_ ->
	  		case length(List) > ?MaxRecords of
				true ->
					THIS:print_DataList(S, "#FFFF77", 1, List, ?MaxRecords);
				_->
					THIS:print_DataList(S, "#FFFF77", 1, List, length(List)),
					error
			end			
	end,
	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLErrorTable(S,BgColor,Border)->
	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	{ok,{_, List1}} = THIS:get_attribute(errorlist),
%% 	{ok,{_, List1}} = THIS:get_attribute(goodlist),
	List = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
	X = ["<A name=errorTable><P><TABLE WIDTH=100% Border="++[Border]++"  CELLSPACING=0><CAPTION><B>"++gettext:key2str("first 100 error","zh-cn") , ReportPeriod, 
	"</B></CAPTION>	<TR BGCOLOR=#88AA99><TH WIDTH=20%>"++gettext:key2str("Time","zh-cn")++"</TH><TH>"++gettext:key2str("Name","zh-cn")++"</TH><TH>"++gettext:key2str("Description","zh-cn")++"</TH></TR>"],
	io:format(S, "~s ~n", [lists:flatten([X])]),
	case length(List) == 0 of
		true ->
			return;		
		_ ->
	  		case length(List) > ?MaxRecords of
				true ->
					THIS:print_DataList(S, "#EE7777", 1, List, ?MaxRecords);
				_->
					THIS:print_DataList(S, "#EE7777", 1, List, length(List)),
					error
			end			
	end,
	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]),
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
calculateUpTime(UpTime, WarningTime, ErrorTime, TotalTime, Precision) ->
	case TotalTime of
		0 ->
			UpTimePercentage=0,
			WarningTimePercentage=0, 
			ErrorTimePercentage=0;
		_ ->
	T_UpTimePercentage = (UpTime * 100) /  TotalTime,
	T_WarningTimePercentage = (WarningTime * 100) /  TotalTime,  
	T_ErrorTimePercentage = (ErrorTime * 100) /  TotalTime,
	case T_UpTimePercentage > 100 of
		true -> 
			UpTimePercentage = 100;
		_ ->
			case T_UpTimePercentage < 0 of
				true ->
					UpTimePercentage = 0;
				_ ->
					UpTimePercentage = T_UpTimePercentage
			end				
	end,
	case T_WarningTimePercentage > 100 of
		true -> 
			WarningTimePercentage = 100;
		_ ->
			case T_WarningTimePercentage < 0 of
				true ->
					WarningTimePercentage = 0;
				_ ->
					WarningTimePercentage = T_WarningTimePercentage
			end
	end,
	case T_ErrorTimePercentage > 100 of
		true -> 
			ErrorTimePercentage = 100;
		_ ->
			case T_ErrorTimePercentage < 0 of
				true ->
					ErrorTimePercentage = 0;
				_ ->
					ErrorTimePercentage = T_ErrorTimePercentage
			end				
	end
	end,
	{UpTimePercentage, WarningTimePercentage, ErrorTimePercentage, Precision}.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
print_uptimeSummary([], S, Id,BgColor,Border) ->
	ok;
print_uptimeSummary([H|T], S, Id,BgColor,Border) ->
	case (H:getMonitorFullID() =/= Id) or (Id == '') of
		true ->
%%			X1:getLastTime()
%% 			X1:getNaTime()
			{UpTimePercentage, WarningTimePercentage, ErrorTimePercentage, _} = THIS:calculateUpTime(H:getGoodTime(), H:getWarningTime(), H:getErrorTime(), H:getTotalTime(), precision),
			L = ["<tr BgColor="++[BgColor]++">", "<td>"] ++ to_list(H:getMonitorName()) ++ ["</td>", "<td>"] ++ to_list(UpTimePercentage) ++ ["</td>" ,"<td>"] ++ 
					to_list(ErrorTimePercentage) ++ ["</td>" , "<td>"] ++ to_list(WarningTimePercentage) ++ ["</td>" , "<td>"] ++ to_list(H:getLastCategory()) ++ ["</td>", "</tr>"],
			io:format(S, "~s ~n", [lists:flatten(L)]),
			print_uptimeSummary(T, S, H:getMonitorFullID(),BgColor,Border);
		_ ->
			print_uptimeSummary(T, S, Id,BgColor,Border)
	end.	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
hTMLSummaryTable(S , Map,BgColor,Border)->	
	io:format(S, "~s ~n", ["<P><CENTER><A NAME=uptimeSummary> </A>
		<TABLE  WIDTH=100% Border="++[Border]++" CELLSPACING=0><CAPTION><B>"++gettext:key2str("uptimeSummary","zh-cn")++"</B></CAPTION>
		<TR BGCOLOR=#88AA99><TH>"++gettext:key2str("Name","zh-cn")++"</TH><TH>"++gettext:key2str("Good Time %","zh-cn")++"</TH><TH>"++gettext:key2str("Error Time %","zh-cn")++"</TH><TH>"++gettext:key2str("Warning %","zh-cn")++"</TH><TH>"++gettext:key2str("Last State","zh-cn")++"</TH></TR>"]),
	THIS:print_uptimeSummary(Map, S, '',BgColor,Border),
	
	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]),
	io:format(S, "~s ~n", ["<P><CENTER><A NAME=readingsSummary> </A>
	<TABLE  WIDTH=100% Border="++[Border]++" CELLSPACING=0><CAPTION><B>"++gettext:key2str("readingsSummary","zh-cn")++"</B></CAPTION>
	<TR BGCOLOR=#88AA99><TH>"++gettext:key2str("Name","zh-cn")++"</TH><TH>"++gettext:key2str("Measurement","zh-cn")++"</TH><TH>"++gettext:key2str("Max","zh-cn")++"</TH><TH>"++gettext:key2str("Average","zh-cn")++"</TH><TH>"++gettext:key2str("Last","zh-cn")++"</TH></TR>"]),	
	F4 = fun(X4)->
		Name=to_list(X4:getMonitorName()),		
%% 		Lable=X4:getPropertyLabel(),getProperty
		Lable=to_list(X4:getProperty()),
		L = ["<tr BgColor="++[BgColor]++">", "<td>"] ++Name ++ ["</td>", "<td>"] ++ Lable ++ ["</td>" ,"<td>"] ++ 
			to_list(X4:getMaximum()) ++ ["</td>" , "<td>"] ++ to_list(X4:getAverage()) ++ ["</td>" , "<td>"] ++ to_list(X4:getLastValue()) ++ ["</td>", "</tr>"],
		io:format(S, "~s ~n", [lists:flatten(L)])
	end,
	lists:foreach(F4, Map),
	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
print_errorTimeSummary([], S, Id,BgColor,Border) ->
	ok;
print_errorTimeSummary([H|T], S, Id,BgColor,Border) ->
	case (H:getMonitorFullID() =/= Id) or (Id == '') of
		true ->			
			L = ["<tr BgColor="++[BgColor]++">", "<td>"] ++ to_list(H:getMonitorName()) ++ ["</td>", "<td>"] ++ to_list(H:getErrorTime()/60.0) ++ ["</td>", "</tr>"],
			io:format(S, "~s ~n", [lists:flatten(L)]),
			print_errorTimeSummary(T, S, H:getMonitorFullID(),BgColor,Border);
		_ ->
			print_errorTimeSummary(T, S, Id,BgColor,Border)
	end.	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLErrorTimeSummary(S, Map,BgColor,Border)->
	io:format(S, "~s ~n", ["<P><CENTER><A NAME=errorTimeSummary> </A>
		<TABLE  WIDTH=100% CELLSPACING=0 Border="++[Border]++"><CAPTION><B>"++gettext:key2str("errorTimeSummary","zh-cn")++"</B></CAPTION>
		<TR BGCOLOR=#88AA99><TH>"++gettext:key2str("Name","zh-cn")++"</TH><TH>"++gettext:key2str("Error Time(minutes)","zh-cn")++"</TH></TR>"]),

	THIS:print_errorTimeSummary(Map, S, '',BgColor,Border),
	
	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLAlertReport(S)->
	io:format(S, "~s ~n", ["<P><CENTER><A name=alertReport></A><P>
    <TABLE  WIDTH=100% CELLSPACING=0><CAPTION><B>Alerts From </B></CAPTION><TR BGCOLOR=#88AA99>
	<TH>Time</TH><TH>Type</TH><TH>Message</TH><TH>Monitor</TH><TH>Group</TH></TR></TABLE>"]).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
getThresholdList([], Ret) ->
	Ret;
getThresholdList([H|T], Ret) ->
	L = api_monitor:get_all_classifier(list_to_atom(H)),
	[M] = api_monitor:find_object(list_to_atom(H)),
	{ok,{_,Name}} = M:get_property(name),
	getThresholdList(T, Ret ++ [{Name, L}]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
createThresholdSummary() ->
	{ok,{_, T}} = THIS:get_property(monitors),
	MonitorStr = string:tokens(T, ","),
	getThresholdList(MonitorStr, []).
    
%% ���� text����ķ�ֵ����
createThresholdSummary(String, Flag) ->
    %% Ŀǰû�п��� Boolean Ϊtrue�����
    {ok,{_, S1}} = THIS:get_property(lineseparator),
    {ok,{_, Collectors}} = THIS:get_attribute(collectors),
    Array = createThresholdSummary(),
    I = 0,
    J = 0,
    K = 0,
    L = 0,
    Name = textutils:appendStringRightJustify([], "Name", I),
    ErrorIf = textutils:appendStringRightJustify([], "Error if", J),
    WarningIf = textutils:appendStringRightJustify([], "Warning if", K),
    GoodIf = textutils:appendStringRightJustify([], "Good if", L),
    FunArray = 
        fun(B) ->
            case B of
                {S5, Hashmap} ->
                    case Hashmap of
                        [] ->
                            [];
                        HashMap when erlang:is_list(Hashmap) ->
                            case string:len(Hashmap) of
                                3 ->
                                    S2 =
                                    case lists:keysearch(error, 1, Hashmap) of
                                        {value, {error, ErrorList}} ->
                                            classifierToStr(ErrorList, "\n");
                                        _ ->
                                            ""
                                    end,
                                    S3 = 
                                    case lists:keysearch(warning, 1, Hashmap) of
                                        {value, {warning, WarningList}} ->
                                            classifierToStr(WarningList, "\n");
                                        _ ->
                                            ""
                                    end,
                                    S4 =
                                    case lists:keysearch(good, 1, Hashmap) of
                                        {value, {good, GoodList}} ->
                                            classifierToStr(GoodList, "\n");
                                        _ ->
                                            ""
                                    end,
                                    lists:append([ textutils:appendStringRightJustify([], S5, I),
                                                   String,
                                                   textutils:appendStringRightJustify([], S2, J),
                                                   String,
                                                   textutils:appendStringRightJustify([], S3, K),
                                                   String,
                                                   textutils:appendStringRightJustify([], S4, L),
                                                   S1]);
                                _ ->
                                    []
                            end;
                        _ ->
                            []
                    end;
                _ ->
                    []
            end
        end,
    RowArrayList = lists:map(FunArray, Array),
    Rows = lists:append(RowArrayList),
    lists:append([Name, String, ErrorIf, String, WarningIf, String, GoodIf, S1, Rows]).

    


to_list(X) when is_atom(X)->
	atom_to_list(X);
to_list(X) when is_float(X)->
	io_lib:format("~.2f", [X]);
to_list(X) when is_integer(X)->	
    integer_to_list(X);
to_list(X) ->
  [X].

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
threshold2Str(T) ->
    Restult=case T of
                    {Con, X, Value}  ->
                        case (X == '') or (Value == '') of
                            true ->
                                "";
                            _ ->
                                 io_lib:format("~s", [to_list(Con)++to_list(X)++to_list(Value)])
                        end;
                    _   ->  ""
                 end,
%% 	io:format("~p ~n", [Restult]),
	Restult.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
isthresholdNull(T) ->
    case T of
        {Con, X, Value} ->
            case (X == '') or (Value == '') of
                true ->
                    true;
                _ ->
                     false
            end;
        _   ->  true
    end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
printThresholds(ID, [], [], [], S,BgColor,Border) ->
	ok;
printThresholds(ID,Temp1 , Temp2, Temp3, S,BgColor,Border) ->
   {H,T} = case Temp1 of
            [K|KK] ->    {K,KK};
            _   ->  {[],[]}
        end,
    {H1,T1} = case Temp2 of
        [K1|KK1] -> {K1,KK1};
        _   ->  {[],[]}
    end,
    {H2,T2} = case Temp3 of
        [K2|KK2] ->    {K2,KK2};
        _   ->  {[],[]}
    end,
	case isthresholdNull(H) and isthresholdNull(H1) and isthresholdNull(H2) of
		true ->
			continue;
		_ ->
			L = ["<tr  BgColor="++[BgColor]++" Border="++[Border]++">", "<td>"] ++ [ID] ++ 
				["</td>", "<td>"] ++ THIS:threshold2Str(H) ++ ["</td>" ,"<td>"] ++ THIS:threshold2Str(H1) ++ ["</td>" , "<td>"] ++ THIS:threshold2Str(H2) ++ ["</td>", "</tr>"],
				io:format(S, "~s ~n", [lists:flatten(L)])
	end,	
	printThresholds("", T, T1, T2, S,BgColor,Border).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
hTMLThresholdSummary(PrintWriter,BgColor,Border)->
	ThresholdInfo = THIS:createThresholdSummary(),
	io:format(PrintWriter, "~s ~n", ["<P><CENTER><A NAME=thresholdSummary> </A>
	<TABLE WIDTH=100%  Border="++[Border]++" CELLSPACING=0><CAPTION><B>thresholdSummary</B></CAPTION>
	<TR BGCOLOR=#88AA99 Border="++[Border]++"><TH>Name</TH><TH> Error Condition</TH><TH>Warning Condition</TH><TH>Good Condition</TH></TR>"]),	

	F = fun(X)->
		{ID, [{error, Error}, {warning, Warning}, {good, Good}]} = X,
        io:format("~n ID:~p, Error:~p , Warning:~p , Good:~p , PrintWriter:~p  ~n",[ID,Error,Warning,Good,PrintWriter]),
		printThresholds(ID, Error, Warning, Good, PrintWriter,BgColor,Border)
	end,
	lists:foreach(F, ThresholdInfo),	
	io:format(PrintWriter, "~s ~n", ["</TABLE></CENTER><P>"]),
	ok.

dateString(Date)->
	ok.
 
createEmailSummaryMessage()->
	ok.


createSpreadsheetSummary()->
	ok.

%% text����������Ϣ
createSpreadsheetSummary(String)->
	createSummaryMessage(String, false).

%% ����ʱ������
createErrorTimeSpreadsheetSummary(String)->
	createErrorTimeSummary(String, false).

%% ���� text����ķ�ֵ����
createThresholdSpreadsheetSummary(String)->
	createThresholdSummary(String, false).

%% ���� text����Ĵ����嵥
createErrorSummary(String) ->
    {ok,{_, S1}} = THIS:get_property(lineseparator),
    {ok,{_, Collectors}} = THIS:get_attribute(collectors),
    {ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	{ok,{_, List1}} = THIS:get_attribute(errorlist),
    SortList = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
    case SortList of
        [] ->
            [];
        ErrorList when erlang:is_list(ErrorList) ->
            ErrorLength = string:len(ErrorList),
            Title =
            if
                ErrorLength >= ?MaxRecords ->
                    lists:append(["First ", to_list(?MaxRecords), " "]);
                true ->
                    []
            end ++
            lists:append(["", "Errors from ", ReportPeriod]) ++
            lists:append([S1, "Time", String, "Monitor", String, "Status", S1]),
            Count = string:len(SortList),
            TxtList = print_TxtList(Count, SortList, ?MaxRecords, String, S1),
            lists:append([Title] ++ TxtList);
        _ ->
            []
    end.
    
    
%% text��������
createSummaryMessage(String, Boolean)->
	%% Ŀǰû�п��� Boolean Ϊtrue�����
    {ok,{_, S2}} = THIS:get_property(lineseparator),
    {ok,{_, Collectors}} = THIS:get_attribute(collectors),
    I = 0,
    J = 0,
    K = 0,
    L = 0,
    I1 = 0,
    Byte0 = 0,
    Byte1 = 0,
    Name = textutils:appendStringRightJustify([], "Name", I1),
    Uptime = textutils:appendStringRightJustify([], "Uptime %", Byte1),
    Error = textutils:appendStringRightJustify([], "Error %", Byte1),
    Warning = textutils:appendStringRightJustify([], "Warning %", Byte1),
    Last = textutils:appendStringRightJustify([], "Last", Byte0),
    RowsList = createSummaryMessage_t(Collectors, "", Byte0, String, S2, I1),
    Measurement = textutils:appendStringRightJustify([], "Measurement", L),
    Max = textutils:appendStringRightJustify([], "Max", J),
    Avg = textutils:appendStringRightJustify([], "Avg", I),
    Last = textutils:appendStringRightJustify([], "Last", K),
    Fun = 
        fun(A) ->
            Samplecollector2 = A,
            Names = textutils:appendStringRightJustify([], Samplecollector2:getMonitorName(), I1),
            Label = textutils:appendStringRightJustify([], Samplecollector2:getPropertyLabel(), L),
            IsNumeric = Samplecollector2:isNumeric(),
            if
                IsNumeric =:= true ->
                    Maxs = textutils:appendStringRightJustify([], to_list(Samplecollector2:getMaximum()), J),
                    Avgs = textutils:appendStringRightJustify([], to_list(Samplecollector2:getAverage()), I),
                    Lasts = textutils:appendStringRightJustify([], to_list(Samplecollector2:getLastValue()), K),
                    lists:append([Names, String, Label, String, Maxs, String, Avgs, String, Lasts, S2]);
                true ->
                    Totals = textutils:appendStringRightJustify([], to_list(Samplecollector2:getTotalValue()), J),
                    Space = textutils:appendStringRightJustify([], " ", I),
                    Lasts = textutils:appendStringRightJustify([], to_list(Samplecollector2:getLastValue()), K),
                    lists:append([Names, String, Label, String, Totals, String, Space, String, Lasts, S2])
            end
        end,
    ListsCo = lists:map(Fun, Collectors),
    lists:append([Name, String, Uptime, String, Error, String, Warning, String, Last, S2] ++ RowsList ++ [S2, Name, String, Measurement,
                    String, Max, String, Avg, String, Last, S2] ++ ListsCo).
createSummaryMessage_t([], MName, Byte0, String, S2, I1) ->
    [];
createSummaryMessage_t([A|B], MName, Byte0, String, S2, I1) ->
    Samplecollector1 = A,
    NMName = Samplecollector1:getMonitorName(),
    if
        MName =/= NMName ->
            Name = textutils:appendStringRightJustify([], NMName, I1),
            {ok,{_, UpTimeIncludeWarning}} = THIS:get_property(upTimeIncludeWarning),	
            Flag1 = UpTimeIncludeWarning,
            J11 = Samplecollector1:getGoodTime(),
            J1 = 
                case Flag1 of
                    true ->
                        J11 + Samplecollector1:getWarningTime();
                    _ ->
                        J11 + 0
                end,
            L1 = Samplecollector1:getTotalTime(),
            As = THIS:calculateUpTime(J1, Samplecollector1:getWarningTime(), Samplecollector1:getErrorTime(), L1, precision),
            case As of
                {As1, As2, As3, _} ->
                    [Uptime] = to_list(As1),
                    [Error] = to_list(As2),
                    [Warning] = to_list(As3),
                    S66 = Samplecollector1:getLastCategory(),
                    S6 = 
                    if
                        S66 =/= 'good' ->
                            string:to_upper(to_list(S66));
                        true ->
                            to_list(S66)
                    end,
                    Category = textutils:appendStringRightJustify([], S6, Byte0),
                    [lists:append([Name, String, Uptime, String, Error, String, Warning, String, Category, S2])];
                _ ->
                    []
            end;
        true ->
            []
    end ++ 
    createSummaryMessage_t(B, NMName, Byte0, String, S2, I1).
    
    
%% ����ʱ������
createErrorTimeSummary(String, Boolean)->
	%% Ŀǰû�п��� Boolean Ϊtrue�����
    {ok,{_, S1}} = THIS:get_property(lineseparator),
    {ok,{_, Collectors}} = THIS:get_attribute(collectors),
    I = 0,
    J = 0,
    K = 0,
    Name = textutils:appendStringRightJustify([], "Name", J),
    TimeInError = textutils:appendStringRightJustify([], "Time in Error", I),
    Fun = 
        fun(A) ->
            Samplecollector1 = A,
            MName = textutils:appendStringRightJustify([], Samplecollector1:getMonitorName(), J),
            [ErrT] = to_list(Samplecollector1:getErrorTime()/60.0),
            ErrorTime = textutils:appendStringRightJustify([], ErrT, K),
            lists:append([MName, String, ErrorTime, S1])
        end,
    RowsList = lists:map(Fun, Collectors),
    lists:append([Name,String,TimeInError,S1]++RowsList).
    

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
getAlertLogList([], StartDate, EndDate, Ret) ->
	Ret;
getAlertLogList([H|T], StartDate, EndDate, Ret) ->
%%  io:format("~p ~n", H),
	F = fun(X)->
			AId = proplists:get_value(id,X),
			AId
	end,
	[Id|_] = lists:map(F,H),
	io:format("~p ~n", [Id]),
	io:format("~p ~n", [StartDate]),
	Logs = api_alert:get_log(StartDate),
	io:format("~p ~n", [Logs]),
	getAlertLogList(T, StartDate, EndDate, Ret ++ [Logs]).	
%% 	case lists:keysearch(id, 1, H) of
%% 		{value,N} ->			
%%  			Logs = alert_logger:q(StartDate, list_to_atom(N)),
%% 			getAlertLogList(T, StartDate, EndDate, Ret ++ [Logs]);
%% 		_ ->
%% 			
%% 			getAlertLogList(T, StartDate, EndDate, Ret)
%% 	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
getMonitorAlertList([], Ret) ->
	Ret;
getMonitorAlertList([H|T], Ret) ->	
	RuleIds = dbcs_rule:get_monitor_rule(list_to_atom(H)),	
%% 	io:format("~p ~n", [RuleIds]),
	getMonitorAlertList(T, Ret ++ [RuleIds]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
gatherAlertInfo(StartDate, EndDate, ScheduleMap) ->
	{ok,{_, T}} = THIS:get_property(monitors),	
	MonitorStr = string:tokens(T, ","),
	RuleLists = getMonitorAlertList(MonitorStr, []),
	getAlertLogList(RuleLists, StartDate, EndDate, []).

%% �����嵥
createXMLWarningSummary()->
	WarningBegin = "<warnings>\n",
    {ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	{ok,{_, List1}} = THIS:get_attribute(warninglist),
	List = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
    ListLength = string:len(List),
    S =
    if
        ListLength >= ?MaxRecords ->
            "First " ++ to_list(?MaxRecords) ++ " " ++
            "Warnings from " ++ ReportPeriod;
        true ->
            lists:append(["Warnings from ", ReportPeriod])
    end,
    Description = textutils:escapeXML("description", S),
    LogList = print_XmlList(1, List, ?MaxRecords),
    WarningEnd = "</warnings>\n",
	lists:append([WarningBegin, Description] ++ LogList ++ [WarningEnd]).
    

%% ����xml�����е�������Ϣ��summary �ֶ�
createXMLSummaryMessage()->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
    SummaryBegin = "<summary>\n",
    S = "",
    SummaryRows = createXMLSummaryMessage_t(Collectors, S),
    Fun =
        fun(A) ->
            Samplecollector1 = A,
            MeasurementBegin = "<measurement>\n",
            MonitorBegin = "<monitor>",
            Monitor = to_list(Samplecollector1:getMonitorName()),
            MonitorEnd = "</monitor>\n",
            LabelBegin = "<label>",
            Label = to_list(Samplecollector1:getProperty()),
            LabelEnd = "</label>\n",
            MeasurementEnd = "</measurement>\n",
            IsNumeric = Samplecollector1:isNumeric(),
            if
                IsNumeric =:= true ->
                    MaxBegin = "<max>",
                    Max = to_list(Samplecollector1:getMaximum()),
                    MaxEnd = "</max>",
                    AveBegin = "<ave>",
                    Ave = to_list(Samplecollector1:getAverage()),
                    AveEnd = "</ave>",
                    LastBegin = "<last>",
                    Last = to_list(Samplecollector1:getLastValue()),
                    LastEnd = "</last>\n",
                    lists:append([MeasurementBegin, MonitorBegin, Monitor, MonitorEnd, LabelBegin, Label, LabelEnd, MaxBegin,
                                    Max, MaxEnd, AveBegin, Ave, AveEnd, LastBegin, Last, LastEnd, MeasurementEnd]);
                true ->
                    TotalBegin = "<total>",
                    Total = to_list(Samplecollector1:getTotalValue()),
                    TotalEnd = "</total>\n",
                    LastBegin = "<last>",
                    Last = to_list(Samplecollector1:getLastValue()),
                    LastEnd = "</last>\n",
                    lists:append([MeasurementBegin, MonitorBegin, Monitor, MonitorEnd, LabelBegin, Label, LabelEnd, TotalBegin,
                                    Total, TotalEnd, LastBegin, Last, LastEnd, MeasurementEnd])
            end
        end,
    SummaryList = lists:map(Fun, Collectors),
    SummaryEnd = "</summary>\n",
    lists:append([SummaryBegin, SummaryRows] ++ SummaryList ++ [SummaryEnd]).
createXMLSummaryMessage_t([], SMName) ->
    [];
createXMLSummaryMessage_t([A|B], SMName) ->
    Samplecollector = A,
    MName = Samplecollector:getMonitorName(),
    NSMName = MName,
    if
        SMName =/= MName ->
            RowBegin = "<row>\n",
            ColumnName = lists:append(["<name>", MName, "</name>\n"]),
            L = Samplecollector:getTotalTime(),
            {ok,{_, Flag}} = THIS:get_property(upTimeIncludeWarning),	
            I = Samplecollector:getGoodTime(),
            Ii = 
                case Flag of
                    true ->
                        I + Samplecollector:getWarningTime();
                    _ ->
                        I + 0
                end,
            {UpTimePercentage, WarningTimePercentage, ErrorTimePercentage, _} = calculateUpTime(Ii, Samplecollector:getWarningTime(), Samplecollector:getErrorTime(), L, precision),
            ColumnUptime = textutils:appendStringRightJustify([], to_list(UpTimePercentage), 0),
            ColumnErrorUptime = textutils:appendStringRightJustify([], to_list(ErrorTimePercentage), 0),
            ColumnWarningUptime = textutils:appendStringRightJustify([], to_list(WarningTimePercentage), 0),
            S1 = Samplecollector:getLastCategory(),
            S11 =
            if
                S1 =/= good ->
                    string:to_upper(to_list(S1));
                true ->
                    to_list(S1)
            end,
            ColumnLast = textutils:appendStringRightJustify([], S11, 0),
            RowEnd = "</row>\n",
            lists:append([RowBegin, ColumnName, "<uptime>", ColumnUptime, "</uptime><error>", ColumnErrorUptime, "</error><warning>",
                            ColumnWarningUptime, "</warning><last>", ColumnLast, "</last>\n", "</row>\n"]);
        true ->
            []
    end ++
    createXMLSummaryMessage_t(B, NSMName).

%% ����ʱ��ժҪ
createXMLErrorTimeSummaryMessage()->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
    ErrorTimeSummaryBegin = "<errorTimeSummary>\n",
    Fun =
        fun(A) ->
            Samplecollector = A,
            RowBegin = "<row>\n",
            NameBegin = "<name>",
            Name = Samplecollector:getMonitorName(),
            NameEnd = "</name>\n",
            ErrorTimeBegin = "<errorTime>",
            ErrorTime = to_list(Samplecollector:getErrorTime()/60.0),
            ErrorTimeEnd = "</errorTime>",
            RowEnd = "</row>\n",
            lists:append([RowBegin, NameBegin, Name, NameEnd, ErrorTimeBegin, ErrorTime, ErrorTimeEnd, RowEnd])
        end,
    RowList = lists:map(Fun, Collectors),
    ErrorTimeSummaryEnd = "</errorTimeSummary>\n",
    lists:append([ErrorTimeSummaryBegin] ++ RowList ++ [ErrorTimeSummaryEnd]).


%% xml �����嵥
createXMLErrorSummary()->
    ErrorsBegin = "<errors>\n",
    {ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	{ok,{_, List1}} = THIS:get_attribute(errorlist),
	List = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
    ListLength = string:len(List),
    S =
    if
        ListLength >= ?MaxRecords ->
            "First " ++ to_list(?MaxRecords) ++ " " ++
            "Errors from " ++ ReportPeriod;
        true ->
            lists:append(["Errors from ", ReportPeriod])
    end,
    Description = textutils:escapeXML("description", S),
    LogList = print_XmlList(1, List, ?MaxRecords),
    ErrorsEnd = "</errors>\n",
	lists:append([ErrorsBegin, Description] ++ LogList ++ [ErrorsEnd]).
    
    
%% ���xml����Ŀ�ʼ������Ϣ
%% monitorThresholdSummary  xml�ֶ�
createXMLThresholdSummaryMessage()->
    {ok,{_, Collectors}} = THIS:get_attribute(collectors),
    ThresholdSummaryBegin = "<monitorThresholdSummary>\n",
    S = "",
    RowThreshholdList = createXMLThresholdSummaryMessage_t(Collectors, S),
    ThresholdSummaryEnd = "</monitorThresholdSummary>\n",
    lists:append([ThresholdSummaryBegin]++RowThreshholdList++[ThresholdSummaryEnd]).
createXMLThresholdSummaryMessage_t([], SMName) ->
    [];
createXMLThresholdSummaryMessage_t([A|B], SMName) ->
    Samplecollector = A,
    MName = Samplecollector:getMonitorName(),
    NSMName = MName,
    if
        SMName =/= MName ->
            Monitor = Samplecollector:getMonitor(),
            {ok,{name,S1}} = Monitor:get_property(name),
            {ok,{id,Id}} = Monitor:get_property(id),
            IdStr = erlang:atom_to_list(Id),
            Array = getThresholdList([IdStr], []),
            FunArray = 
                fun(B) ->
                    case B of
                        {_, Hashmap} ->
                            case Hashmap of
                                [] ->
                                    [];
                                HashMap when erlang:is_list(Hashmap) ->
                                    case string:len(Hashmap) of
                                        3 ->
                                            S2 =
                                            case lists:keysearch(error, 1, Hashmap) of
                                                {value, {error, ErrorList}} ->
                                                    classifierToStr(ErrorList, ",");
                                                _ ->
                                                    ""
                                            end,
                                            S3 = 
                                            case lists:keysearch(warning, 1, Hashmap) of
                                                {value, {warning, WarningList}} ->
                                                    classifierToStr(WarningList, ",");
                                                _ ->
                                                    ""
                                            end,
                                            S4 =
                                            case lists:keysearch(good, 1, Hashmap) of
                                                {value, {good, GoodList}} ->
                                                    classifierToStr(GoodList, ",");
                                                _ ->
                                                    ""
                                            end,
                                            lists:append(["<row>\n",textutils:escapeXML("name", S1), "\n", textutils:escapeXML("errorThreshold", S2),
                                                            textutils:escapeXML("warningThreshold", S3), textutils:escapeXML("goodThreshold", S4), "</row>\n"]);
                                        _ ->
                                            []
                                    end;
                                _ ->
                                    []
                            end;
                        _ ->
                            []
                    end
                end,
            RowArrayList = lists:map(FunArray, Array),
            [lists:append(RowArrayList)];
        true ->
            []
    end ++
    createXMLThresholdSummaryMessage_t(B, NSMName).

createXMLThresholdSummaryMessage(String)->
	ok.

%% txt����warning�嵥
createWarningSummary(String)->
    {ok,{_, S1}} = THIS:get_property(lineseparator),
    {ok,{_, Collectors}} = THIS:get_attribute(collectors),
    {ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	{ok,{_, List1}} = THIS:get_attribute(warninglist),
    SortList = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
    ErrorLength = string:len(SortList),
    Title =
        if
            ErrorLength >= ?MaxRecords ->
                lists:append(["First ", to_list(?MaxRecords), " "]);
            true ->
                []
        end ++
        lists:append(["", "Warnings from ", ReportPeriod]) ++
        lists:append([S1, "Time", String, "Monitor", String, "Status", S1]),
    Body = 
    case SortList of
        [] ->
            [];
        WarningList when erlang:is_list(WarningList) ->
            Count = string:len(SortList),
            TxtList = print_TxtList(Count, SortList, ?MaxRecords, String, S1),
            TxtList;
        _ ->
            []
    end,
    lists:append([Title] ++ Body).

%% txt���嵥
createGoodSummary(String)->
    {ok,{_, S1}} = THIS:get_property(lineseparator),
    {ok,{_, Collectors}} = THIS:get_attribute(collectors),
    {ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	{ok,{_, List1}} = THIS:get_attribute(goodlist),
    SortList = lists:sort(fun(A, B) -> A#monitorlog.id < B#monitorlog.id end, List1),
    ErrorLength = string:len(SortList),
    Title =
        if
            ErrorLength >= ?MaxRecords ->
                lists:append(["First ", to_list(?MaxRecords), " "]);
            true ->
                []
        end ++
        lists:append(["", "Goods from ", ReportPeriod]) ++
        lists:append([S1, "Time", String, "Monitor", String, "Status", S1]),
    Body = 
    case SortList of
        [] ->
            [];
        WarningList when erlang:is_list(WarningList) ->
            Count = string:len(SortList),
            TxtList = print_TxtList(Count, SortList, ?MaxRecords, String, S1),
            TxtList;
        _ ->
            []
    end,
    lists:append([Title] ++ Body).

%% �ı��������
textHistoryTable(PrintWriter)->
    {ok,{_, Flag}} = THIS:get_property(showReportThresholdSummary),            %% �������ֵժҪ     %% monitorThresholdSummary  xml�ֶ�
    {ok,{_, Flag1}} = THIS:get_property(hideReportSummary),                    %% ������ʱ��Ͷ���ժҪ     %% summary �ֶ�
    {ok,{_, Flag2}} = THIS:get_property(showReportErrorTimeSummary),            %% �������ժҪ       %% - <errorTimeSummary>       
    {ok,{_, Flag3}} = THIS:get_property(hideReportTables),                      %% �������ݱ�
    {ok,{_, Flag4}} = THIS:get_property(hideReportErrors),                      %% �����嵥
    {ok,{_, Flag5}} = THIS:get_property(hideReportWarnings),                    %% �����嵥
    {ok,{_, Flag6}} = THIS:get_property(hideReportGoods),             %% ���嵥
    %% ����ָ���
    {ok, {_, Split}} = THIS:get_property(reportDelimiter),
    %%lineseparator
    {ok,{_, _TxtFilePath}} = THIS:get_property(pFilePath),
    TxtFilePath = _TxtFilePath ++ ".txt",
    PrintWriterN = 
        if 
            PrintWriter =:= [] ->
                case file:open(TxtFilePath, write) of
                    {ok, F} ->
                        F;
%% 			file:close(S);
                    _->
                        throw("open txt file error")
                end;
            true ->
                PrintWriter
        end,
    try PrintWriterN of
        S ->
            io:format(S, "~s ~n", ["\n\n<PRE>\n"]),
            {ok,{_, Collectors}} = THIS:get_attribute(collectors),
            case string:len(Collectors) of
                0 ->
                    [];
                ConSize ->
                    if
                        Flag =:= true ->                                            %% monitorThresholdSummary  xml�ֶ�
                            io:format(S, "~s ~n", [""]),
                            TextThreshold = createThresholdSpreadsheetSummary(Split),
                            io:format(S, "~s ~n", [TextThreshold]),
                            ok;
                        true ->
                            ok
                    end,
                    if
                        Flag1 =:= false ->               %% summary �ֶ�
                            TextSummary = createSpreadsheetSummary(Split),
                            io:format(S, "~s ~n", [""]),
                            io:format(S, "~s ~n", [TextSummary]),
                            ok;
                        true ->
                            ok
                    end,
                    if
                        Flag2 =:= true ->           %% <errorTimeSummary>
                            ErrorTimeSpreadsheetSummary = createErrorTimeSpreadsheetSummary(Split),
                            io:format(S, "~s ~n", [""]),
                            io:format(S, "~s ~n", [ErrorTimeSpreadsheetSummary]),
                            ok;
                        true ->
                            ok
                    end,
                    if                                  %% �������ݱ�
                        Flag3 =:= false ->
                            %%Samplecollector = whileSamplecollector(Collectors, Samplecollector0),
                            %%io:format(S, "~s ~n", [lists:append([RowsBegin] ++ TableList ++ [RowsEnd])]);
                            MoniData = txtTables(Collectors, ConSize, Split),
                            io:format(S, "~s ~n", [""]),
                            io:format(S, "~s ~n", [MoniData]),
                            ok;
                        true ->
                            ok
                    end,
                    if
                        Flag4 =:= false ->              %% �����嵥
                            ErrorList = createErrorSummary(Split),
                            io:format(S, "~s ~n", [""]),
                            io:format(S, "~s ~n", [ErrorList]),
                            ok;
                        true ->
                            ok
                    end,
                    if
                        Flag5 =:= false ->               %% �����嵥
                            WarningList = createWarningSummary(Split),
                            io:format(S, "~s ~n", [""]),
                            io:format(S, "~s ~n", [WarningList]),
                            ok;
                        true ->
                            ok
                    end,
                    if
                        Flag6 =:= false ->               %% ���嵥
                            GoodList = createGoodSummary(Split),
                            io:format(S, "~s ~n", [""]),
                            io:format(S, "~s ~n", [GoodList]),
                            ok;
                        true ->
                            ok
                    end,
                    ok
            end
    catch
        _:_ ->
            ok
    after
         file:close(PrintWriterN)
    end,
    %% �����ʼ�
    {ok, {_, S2}} = THIS:get_property(emailData),
    S2Length = string:len(S2),
    if
        S2Length > 0 ->
            S3 = "Comma-delimited file is attached",
            {ok,{_, S5}} = THIS:get_property(title),
            case api_preferences:get_all(email_settings) of
                {ok, Config} ->
                    Result = sendMail(Config, S2, S5, S3, TxtFilePath);
                _ ->
                    error
            end;
        true ->
            ok
    end,
	ok.
    
%% text �����־�б��嵥 ����
print_TxtList(Count, [], Max, S, S1)->
	[];
print_TxtList(Count, T, Max, S, S1) when Count > Max ->
	[];
print_TxtList(Count, [H|T], Max, S, S1) ->	
	Date = sv_datetime:tostr(H#monitorlog.time),
    Monitor = H#monitorlog.name,
    Message = H#monitorlog.desc,
    [Date, S, Monitor, S, Message, S1] ++ 
	print_TxtList(Count+1, T, Max, S, S1).
    

sendMail(HashMap, EmailAddress, Title, Content, Attach)->
    S4 = 
    case lists:keysearch(mailServer, 1, HashMap) of
        {value, {mailServer, _S4}} ->
            _S4;
        _ ->
            []
    end,
    S4Length = string:len(S4),
    if
        S4Length == 0 ->
            {error, "missing mail server"};
        true ->
            sendMailMsg(HashMap, S4, EmailAddress, Title, Content, Attach)
    end.
    
sendMailMsg(HashMap, Server, EmailAddress, Title, Content, Attach) ->
    S1 = EmailAddress,
    As = string:tokens(S1, ","),
    S55 = 
        case lists:keysearch(fromAddress, 1, HashMap) of
            {value, {fromAddress, _S5}} ->
                _S5;
            _ ->
                []
        end,
    S5Length = string:len(S55),
    AsLength = string:len(As),
    S5 =
    if
        S5Length == 0, AsLength > 0 ->
            lists:nth(1, As);
        true ->
            S55
    end,
    
    UserName = 
        case lists:keysearch(mailUser, 1, HashMap) of
            {value, {mailUser, _UserName}} ->
                _UserName;
            _ ->
                []
        end,
    Password = 
        case lists:keysearch(mailPassword, 1, HashMap) of
            {value, {mailPassword, _Password}} ->
                _Password;
            _ ->
                []
        end,
    From = S5,
    Subject = Title,
    Body = Content,
    Attachment = Attach,
    Timeout = 60,
    Fun =
        fun(To) ->
            %%io:format("Server !!!!!!!!!!!!!!!~n~p~n", [Server]),
            %%io:format("Port !!!!!!!!!!!!!!!~n~p~n", [25]),
            %%io:format("UserName !!!!!!!!!!!!!!!~n~p~n", [UserName]),
            %%io:format("From !!!!!!!!!!!!!!!~n~p~n", [From]),
            %%io:format("To !!!!!!!!!!!!!!!~n~p~n", [To]),
            %%io:format("Subject !!!!!!!!!!!!!!!~n~p~n", [Subject]),
            %%io:format("Body !!!!!!!!!!!!!!!~n~p~n", [Body]),
            %%io:format("Attachment !!!!!!!!!!!!!!!~n~p~n", [Attachment]),
            %%io:format("Timeout !!!!!!!!!!!!!!!~n~p~n", [Timeout]),
            mail:smtp_mail_send(Server, 25, UserName, Password, From, To, Subject, Body, Attachment, Timeout)
        end,
    lists:map(Fun, As).

%% xml �����־�б��嵥 ����
print_XmlList(Count, [], Max)->
	[];
print_XmlList(Count, T, Max) when Count > Max ->
	[];
print_XmlList(Count, [H|T], Max) ->	
	Date = textutils:escapeXML("date", sv_datetime:tostr(H#monitorlog.time) ++ "\n"),
    Monitor = textutils:escapeXML("monitor", H#monitorlog.name ++ "\n"),
    Message = textutils:escapeXML("message", H#monitorlog.desc ++ "\n"),
    ["<error>\n", Date, Monitor, Message, "</error>\n"] ++ 
	print_XmlList(Count+1, T, Max).

xmlHistoryTable(PrintWriter, Array, Date, Date, I)->
    {ok,{_, Flag}} = THIS:get_property(showReportThresholdSummary),            %% �������ֵժҪ     %% monitorThresholdSummary  xml�ֶ�
    {ok,{_, Flag1}} = THIS:get_property(hideReportSummary),                    %% ������ʱ��Ͷ���ժҪ     %% summary �ֶ�
    {ok,{_, Flag2}} = THIS:get_property(showReportErrorTimeSummary),            %% �������ժҪ       %% - <errorTimeSummary>       
    {ok,{_, Flag3}} = THIS:get_property(hideReportTables),                      %% �������ݱ�
    {ok,{_, Flag4}} = THIS:get_property(hideReportErrors),                      %% �����嵥
    {ok,{_, Flag5}} = THIS:get_property(hideReportWarnings),                    %% �����嵥
    {ok,{_, Flag6}} = THIS:get_property(showReportAlerts),
    %%
    {ok,{_, _XmlFilePath}} = THIS:get_property(pFilePath),
    XmlFilePath = _XmlFilePath ++ ".xml",
    PrintWriterN = 
        if 
            PrintWriter =:= [] ->
                case file:open(XmlFilePath, write) of
                    {ok, F} ->
                        F;
%% 			file:close(S);
                    _->
                        throw("open xml file error")
                end;
            true ->
                PrintWriter
        end,
    try PrintWriterN of
        S ->
            io:format(S, "~s ~n", ["<?xml version=\"1.0\"?>"]),
            io:format(S, "~s ~n", ["<report>"]),
            {ok,{_, Collectors}} = THIS:get_attribute(collectors),
            %%io:format("Collectors   !!!!!!!!!!!!!!!!~n~p~n", [Collectors]),
            case string:len(Collectors) of
                0 ->
                    io:format(S, "~s ~n", ["<rows>"]),
                    io:format(S, "~s ~n", ["</rows>"]);
                ConSize ->
                    if
                        Flag =:= true ->
                            XMLThresholdSummary = createXMLThresholdSummaryMessage(),       %% monitorThresholdSummary  xml�ֶ�
                            io:format(S, "~s ~n", [XMLThresholdSummary]);
                        true ->
                            ok
                    end,
                    if
                        Flag1 =:= false ->               %% summary �ֶ�
                            XMLSummary = createXMLSummaryMessage(),
                            io:format(S, "~s ~n", [XMLSummary]);
                        true ->
                            ok
                    end,
                    if
                        Flag2 =:= true ->           %% <errorTimeSummary>
                            io:format(S, "~s ~n", [createXMLErrorTimeSummaryMessage()]);
                        true ->
                            ok
                    end,
                    if                                  %% �������ݱ�
                        Flag3 =:= false ->
                            %%Samplecollector = whileSamplecollector(Collectors, Samplecollector0),
                            RowsBegin = "<rows>\n",
                            TableList = xmlTables(Collectors, ConSize),
                            RowsEnd = "</rows>\n",
                            io:format(S, "~s ~n", [lists:append([RowsBegin] ++ TableList ++ [RowsEnd])]);
                        true ->
                            ok
                    end,
                    if
                        Flag4 =:= false ->              %% �����嵥
                            io:format(S, "~s ~n", [createXMLErrorSummary()]);
                        true ->
                            ok
                    end,
                    if
                        Flag5 =:= false ->               %% �����嵥
                            io:format(S, "~s ~n", [createXMLWarningSummary()]);
                        true ->
                            ok
                    end,
                    if
                        Flag6 =:= true ->
                            ok;
                        true ->
                            ok
                    end,
                    ok
            end,
            io:format(S, "~s", ["</report>"])
    catch
        _:_ ->
            ok
    after
         file:close(PrintWriterN)
    end,
    %% �����ʼ�
    {ok, {_, S2}} = THIS:get_property(xmlEmailData),
    
    S2Length = string:len(S2),
    if
        S2Length > 0 ->
            S3 = "XML file is attached",
            {ok,{_, S5}} = THIS:get_property(title),
            case api_preferences:get_all(email_settings) of
                {ok, Config} ->
                    Result = sendMail(Config, S2, S5, S3, XmlFilePath),
                    io:format("MailResult !!!!!!!!!!!!~n~p~n", [Result]);
                _ ->
                    error
            end;
        true ->
            ok
    end,
	ok.

%%forBucketCount(Samplecollector, J, Count, Collectors) when J <= Count ->
%%    RowBegin = "<row>\n",
    
    
    
%%    RowEnd = "</row>\n",
%%forBucketCount(Samplecollector, J, Count, Collectors) ->


%%hTMLTables(S, Cons, ConSize)->
%% txtTables ���
txtTables(Cons, ConSize, String)->
	case ConSize > 0 of
		true->
            TimeHeader = "time",
            Samplecollector = lists:nth(1, Cons),
            Fun = 
                fun(A) ->
                    Samplecollector2 = A,
                    [MonitorName] = to_list(Samplecollector2:getMonitorName()),
                    [Labels] = to_list(Samplecollector2:getPropertyLabel()),
                    S9 = lists:append([MonitorName, "  ", Labels]), 
                    lists:append([String, S9])
                end,
            ConList = lists:map(Fun, Cons),
            ConLists = ConList,
			BucketSize = (Samplecollector):getBucketCount(),	
            BodyList = txtTables_t(Cons, ConSize, Samplecollector, 1, String),
            lists:append([TimeHeader] ++ ConLists ++ ["\n"] ++ BodyList);
			%%hTMLTable_Child(S, 1, 5, ConSize, Cons, BucketSize);
		_->
			[]
	end.
txtTables_t(Cons, ConSize, Samplecollector, J, String) when J =< ConSize ->
    Date = sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(Samplecollector:getBucketStartTime(J))),
    Fun =
        fun(A) ->
            Samplecollector2 = A,
            S11 = Samplecollector2:getWorstCategory(J),
            IsNumeric = Samplecollector2:isNumeric(),
            [Ss10] =
            if
                IsNumeric =:= true ->
                    to_list(Samplecollector2:getAverage(J));
                true ->
                    to_list(Samplecollector2:getTotalValue(J))
            end,
            S10 =
                if
                    S11 =:= nodata ->
                        "no data";
                    true ->
                        Ss10
                end,
            Data = S10,
            lists:append([String, Data])
        end,
    SampleList = lists:map(Fun, Cons),
    SampleList1 = lists:map(Fun, Cons),
    [Date] ++ SampleList ++ ["\n"] ++
    txtTables_t(Cons, ConSize, Samplecollector, J+1, String);
txtTables_t(Cons, ConSize, Samplecollector, J, String) ->
    [].
    
    
    
%%hTMLTables(S, Cons, ConSize)->
%% xmlTables ���
xmlTables(Cons, ConSize)->
	case ConSize > 0 of
		true->
            Samplecollector = lists:nth(1, Cons),
			BucketSize = (Samplecollector):getBucketCount(),	
            xmlTables_t(Cons, ConSize, Samplecollector, 1);
			%%hTMLTable_Child(S, 1, 5, ConSize, Cons, BucketSize);
		_->
			[]
	end.
xmlTables_t(Cons, ConSize, Samplecollector, J) when J =< ConSize ->
    RowBegin = "<row>\n",
    Date = textutils:escapeXML("date", sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(Samplecollector:getBucketStartTime(J)))),
    Fun =
        fun(A) ->
            SampleBegin = "<sample>\n",
            Samplecollector2 = A,
            [MonitorName] = to_list(Samplecollector2:getMonitorName()),
            [Labels] = to_list(Samplecollector2:getPropertyLabel()),
            S9 = lists:append([MonitorName, "  ", Labels]), 
            Label = textutils:escapeXML("label", S9),
            S11 = Samplecollector2:getWorstCategory(J),
            IsNumeric = Samplecollector2:isNumeric(),
            [Ss10] =
            if
                IsNumeric =:= true ->
                    to_list(Samplecollector2:getAverage(J));
                true ->
                    to_list(Samplecollector2:getTotalValue(J))
            end,
            S10 =
                if
                    S11 =:= nodata ->
                        "no data";
                    true ->
                        Ss10
                end,
            Value = textutils:escapeXML("value", S10),
            Data = textutils:escapeXML("data", S10),
            SampleEnd = "</sample>\n",
            lists:append([SampleBegin, Label, Value, Data, SampleEnd])
        end,
    SampleList = lists:map(Fun, Cons),
    RowEnd = "</row>\n",
    [RowBegin, Date] ++ SampleList ++ [RowEnd] ++
    xmlTables_t(Cons, ConSize, Samplecollector, J+1);
xmlTables_t(Cons, ConSize, Samplecollector, J) ->
    [].


%% ת��һ����ֵ���ϳ��ַ���ʽ�����ҽ������ֵ���ʽ��ָ���ָ�������
classifierToStr(Classifier, Separator) ->
    Cf = classifierToStr_t(Classifier),
    string:join(Cf, Separator).
classifierToStr_t([]) ->
    [];
classifierToStr_t([{Field, Operation, Value}|T]) ->
    [lists:append([textutils:any_to_list(Field), textutils:any_to_list(Operation), textutils:any_to_list(Value)])] ++
    classifierToStr_t(T);
classifierToStr_t([H|T]) ->
    classifierToStr_t(T).
    

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
initializeFilePaths()->
%% 	Root = platform:getDirectoryPath(""admin"", ""),
	Root = httputils:replaceAll(platform:getDirectoryPath("", ""),"\\","/"),
    io:format("root is:~p~n",[Root]),
%% 	UrlPath = platform:getURLPath("admin", ""),
	UrlPath = platform:getURLPath("", ""),
	{ok,{_, ID}} = THIS:get_property(id),
	{ok,{_,TApp}}=THIS:get_property(app_),
	case is_atom(TApp) of
		true ->
			App=atom_to_list(TApp);
		_ -> 
			App=TApp
	end,
		
	io:format("~p ~n", [to_list(ID)]),	
    Separator = filename:nativename("/"),
	ReportDirectory = filename:nativename(Root ++  "wwwroot/htdocs/Reports-" ++ to_list(ID) ++"-"++App)++ Separator,
    io:format("directory is:~p~n",[ReportDirectory]),
	case filelib:is_dir(ReportDirectory) of
		true->
			ok;
		_->
 			io:format("make_dir:~p ~n", [ReportDirectory]),
			file:make_dir(ReportDirectory)			
	end,
	
%% 	io:format("~p ~n", [to_list(ReportDirectory ++ "logo.gif")]),
	case filelib:is_file(ReportDirectory ++ "SiteView.css") of
		true->
		    ok;
		_->
		   file:copy(filename:nativename(Root ++ "wwwroot/css/SiteView.css"), ReportDirectory ++ "SiteView.css")
	end,
	
	case filelib:is_file(ReportDirectory ++ "logo.jpg") of
		true->
		    ok;
		_->
		   file:copy(filename:nativename(Root ++ "wwwroot/images/logo.jpg"), ReportDirectory ++ "logo.jpg")
	end,

	case filelib:is_file(ReportDirectory ++ "tt_03.jpg") of
		true->
		    ok;
		_->
		   file:copy(filename:nativename(Root ++ "wwwroot/images/tt_03.jpg"), ReportDirectory ++ "tt_03.jpg")
	end,
	
	case filelib:is_file(ReportDirectory ++ "index.html") of
		true->
		    ok;
		_->
		   file:copy(filename:nativename(Root ++ "wwwroot/reportindexblank.html"), ReportDirectory ++ "index.html")
	end,
	
	{Year,Month,Day} = date(),
	{Time,Minute,Second} = time(),
%% 	io:format("~p ~n", [Year]),
%% 	io:format("~p ~n", [Month]),
%% 	io:format("~p ~n", [Day]),
	FileName = lists:flatten(io_lib:format("Report_~p_~p_~p-~p_~p.html",[Year,Month,Day,Time,Minute])),	
%% 	io:format("~p ~n", [lists:flatten(FileName)]),
	ReportPath =  ReportDirectory ++ FileName,
    
    
	THIS:set_property(reportDirectory, ReportDirectory),
	THIS:set_property(reportPath, ReportPath),
	THIS:set_property(reportVirtual, UrlPath ++ "../htdocs/Reports-" ++ to_list(ID)++"-"++App),
	THIS:set_property(reportURL, UrlPath ++ "../htdocs/Reports-" ++ to_list(ID) ++"-"++App++ "/" ++ FileName),
	THIS:set_property(reportlistVirtual, UrlPath ++ "/htdocs/Reports-" ++ to_list(ID)++"-"++App),
	THIS:set_property(reportlistURL, UrlPath ++ "/Reports-" ++ to_list(ID) ++"-"++App++ "/" ++ FileName),

	
	FileName1 = lists:flatten(io_lib:format("Report_~p_~p_~p-~p_~p",[Year,Month,Day,Time,Minute])),
	THIS:set_property(filename, FileName1),
    
    PFilePath = ReportDirectory ++ FileName1,
    THIS:set_property(pFilePath, PFilePath),
	
%% 	io:format("111:~p ~n", [ReportPath]),
	
%% 	THIS:set_property("textReportURL", ""),
%% 	THIS:set_property("xmlReportURL", ""),
%% 	THIS:set_property("reportIndexURL", ""),
%% 	THIS:set_property("userReportURL", ""),
%% 	THIS:set_property("userTextReportURL", ""),
%% 	THIS:set_property("userXMLReportURL", ""),
%% 	THIS:set_property("userReportIndexURL", ""),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
addLogo(S) ->
	io:format(S, "~s ~n",["<center><IMG SRC=logo.jpg> <table class=fine border=0 cellspacing=0 width=500 align=center><tr><td><p class=fine align=center><br>
	<small>"++?OEM(gettext:key2str("SiteView"))++"</p><p class=fine align=center><a href=http://www.dragonflow.com/company/copyright.html target=web> Copyright &copy; 2009 </a> , All rights reserved.</small></p></td></tr></table></center>"]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
initializeTimeParameters()->
	{ok,{_, StartDayStr}} = THIS:get_property(startDay),
	{ok,{_, StartHourStr}} = THIS:get_property(startHour),
	{ok,{_, WindowStr}} = THIS:get_property(window),
	{ok,{_, PrecisionStr}} = THIS:get_property(precision),
	{ok,{_, Truedata}} = THIS:get_property(truedata),
	case Truedata of
		true ->
			case (string:len(WindowStr) == 0) of 
			  true ->
				Freq = THIS:calculateMinimumFrequency(),
	%% 			io:format("~p ~n", [Freq]),
				THIS:set_property(precision, Freq),
	%% 			DefaultSamples = 48,
				DefaultSamples = 20,
				Window = Freq * DefaultSamples,
				THIS:set_property(window, Window),
				THIS:set_property(endTime, trunc(platform:timeMillis()/1000)),
				THIS:set_property(startTime, trunc(platform:timeMillis()/1000) - Window);
	%%     		setUsingDefaultTimeParameters(true);
			_->	
				{Date, Time} = calendar:local_time(),
	%% 			io:format("~p ~n", [calendar:datetime_to_gregorian_seconds({Date, Time})]),
				case string:equal(WindowStr, "monthToDate") of
					true ->
						{_,_,Day} = Date,
						Window1 = (Day - 1)*86400;
	%% 					IsMonthToDate = true;
					_ ->
						{Window1, _} = string:to_integer(WindowStr)
	%% 					IsMonthToDate = false
				end,
				io:format("~p ~n", [Window1]),
				case (StartDayStr == today) of
					true ->
						T1 = calendar:datetime_to_gregorian_seconds({Date,{0,0,0}}),					
	%% 					io:format("~p ~n", [T1]),
	%% 					io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(T1)]),
						StartDay = T1;
					_ ->
						StartDay = calendar:datetime_to_gregorian_seconds(StartDayStr) 		
				end,
				case string:equal(StartHourStr, "now") of 
					true ->
						{Hour,Min,Sec} = Time, 
						StartHour = Sec + Min * 60 + Hour * 3600; 
					_->
	%% 					StartHour = calendar:datetime_to_gregorian_seconds(StartHourStr)
						{StartHour, _} = string:to_integer(StartHourStr)
				end,
				THIS:set_property(startTime, StartDay + StartHour),
	%% 		 	boolean useAdvancedAlgorithm = ConfigManagerSession.getReadOnlyMasterConfig().getAsBoolean("_useReportAdvancedSamplingAlgorithm");
				UseAdvancedAlgorithm = false,
				case string:equal(PrecisionStr, "default") of 
					true ->
						 case useAdvancedAlgorithm of
							true ->
								Freq1 = THIS:calculateMinimumFrequency(),
								THIS:set_property(precision, Freq1);
							 _ -> 
								case Window1 of
									3600 ->
											THIS:set_property(precision, 60);
									86400 ->
											THIS:set_property(precision, 1800);
									604800 ->
										THIS:set_property(precision, 14400);
									2592000 ->
											THIS:set_property(precision, 43200);
									_->	
										Precision = trunc((Window1 / 40) / 60) * 60,
										case (Precision == 0) of
											true ->
											   THIS:set_property(precision, 60);
											_->
													ok
										end,
										THIS:set_property(precision, Precision)
								end
							end;
					_->
						case is_integer(PrecisionStr) of
							true ->
								Precision = PrecisionStr;
							_ ->
								{Precision, _} = string:to_integer(PrecisionStr)
						end,
						
						io:format("~p ~n", [Precision]),
						io:format("~p ~n", [Window1]),
						MaxBuckets = 100,
						case (Window1 / Precision > MaxBuckets) of
							true ->
								T_Precision = trunc(Window1 / MaxBuckets),
								T_Precision1 = (T_Precision / 60) * 60,
								case T_Precision1 == 0 of
									true ->
										THIS:set_property(precision, 60);
									_ ->
									  THIS:set_property(precision, T_Precision)
								end;
							_ ->
								THIS:set_property(precision, Precision)
						end
				end,

	%%     if ((!(isMonthToDate)) && (window == 2592000)) {
	%%       if (thisDay == 1) {
	%%         label731: int lastMonth = thisMonth - 1;
	%%         if (lastMonth < 0) {
	%%           lastMonth = 11;
	%%         }
	%%         window = 86400 * ReportTimeUtils.MONTH_DAYS[lastMonth];
	%%       } else if (thisDay >= 28) {
	%%         window = 86400 * ReportTimeUtils.MONTH_DAYS[thisMonth];
	%%       }
	%%     }		
				{ok,{_, Relative}} = THIS:get_property(relative),
				{ok,{_, StartTime}} = THIS:get_property(startTime),
	%%  			io:format("startTime: ~p ~n", [StartTime]),
				
				case Relative == -1 of
					true ->
						THIS:set_property(endTime, StartTime),
					    THIS:set_property(startTime, StartTime - Window1),
						THIS:setReportPeriod();
					   _ ->
						THIS:set_property(endTime, StartTime + Window1),
						THIS:setReportPeriod()
				end
		end;
		_ ->
		case (string:len(WindowStr) == 0) of 
			  true ->
				Freq = THIS:calculateMinimumFrequency(),
	%% 			io:format("~p ~n", [Freq]),
				THIS:set_property(precision, Freq),
	%% 			DefaultSamples = 48,
				DefaultSamples = 20,
				Window = Freq * DefaultSamples,
				THIS:set_property(window, Window),
				THIS:set_property(endTime, trunc(platform:timeMillis()/1000)),
				THIS:set_property(startTime, trunc(platform:timeMillis()/1000) - Window);
	%%     		setUsingDefaultTimeParameters(true);
			_->	
				{Date, Time} = calendar:local_time(),
	%% 			io:format("~p ~n", [calendar:datetime_to_gregorian_seconds({Date, Time})]),
				case string:equal(WindowStr, "monthToDate") of
					true ->
						{_,_,Day} = Date,
						Window1 = (Day - 1)*86400;
	%% 					IsMonthToDate = true;
					_ ->
						{Window1, _} = string:to_integer(WindowStr)
	%% 					IsMonthToDate = false
				end,
				io:format("~p ~n", [Window1]),
				case (StartDayStr == today) of
					true ->
						T1 = calendar:datetime_to_gregorian_seconds({Date,{0,0,0}}),					
	%% 					io:format("~p ~n", [T1]),
	%% 					io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(T1)]),
						StartDay = T1;
					_ ->
						StartDay = calendar:datetime_to_gregorian_seconds(StartDayStr) 		
				end,
				case string:equal(StartHourStr, "now") of 
					true ->
						{Hour,Min,Sec} = Time, 
						StartHour = Sec + Min * 60 + Hour * 3600; 
					_->
	%% 					StartHour = calendar:datetime_to_gregorian_seconds(StartHourStr)
						{StartHour, _} = string:to_integer(StartHourStr)
				end,
				THIS:set_property(startTime, StartDay + StartHour),
	%% 		 	boolean useAdvancedAlgorithm = ConfigManagerSession.getReadOnlyMasterConfig().getAsBoolean("_useReportAdvancedSamplingAlgorithm");
				UseAdvancedAlgorithm = false,
				case string:equal(PrecisionStr, "default") of 
					true ->
						 case useAdvancedAlgorithm of
							true ->
								Freq1 = THIS:calculateMinimumFrequency(),
								THIS:set_property(precision, Freq1);
							 _ -> 
								case Window1 of
									3600 ->
											THIS:set_property(precision, 60);
									86400 ->
											THIS:set_property(precision, 1800);
									604800 ->
										THIS:set_property(precision, 14400);
									2592000 ->
											THIS:set_property(precision, 43200);
									_->	
										Precision = trunc((Window1 / 40) / 60) * 60,
										case (Precision == 0) of
											true ->
											   THIS:set_property(precision, 60);
											_->
													ok
										end,
										THIS:set_property(precision, Precision)
								end
							end;
					_->
						case is_integer(PrecisionStr) of
							true ->
								Precision = PrecisionStr;
							_ ->
								{Precision, _} = string:to_integer(PrecisionStr)
						end,
						
						io:format("~p ~n", [Precision]),
						io:format("~p ~n", [Window1]),
						MaxBuckets = 100,
						case (Window1 / Precision > MaxBuckets) of
							true ->
								T_Precision = trunc(Window1 / MaxBuckets),
								T_Precision1 = (T_Precision / 60) * 60,
								case T_Precision1 == 0 of
									true ->
										THIS:set_property(precision, 60);
									_ ->
									  THIS:set_property(precision, T_Precision)
								end;
							_ ->
								THIS:set_property(precision, Precision)
						end
				end,

	%%     if ((!(isMonthToDate)) && (window == 2592000)) {
	%%       if (thisDay == 1) {
	%%         label731: int lastMonth = thisMonth - 1;
	%%         if (lastMonth < 0) {
	%%           lastMonth = 11;
	%%         }
	%%         window = 86400 * ReportTimeUtils.MONTH_DAYS[lastMonth];
	%%       } else if (thisDay >= 28) {
	%%         window = 86400 * ReportTimeUtils.MONTH_DAYS[thisMonth];
	%%       }
	%%     }		
				{ok,{_, Relative}} = THIS:get_property(relative),
				{ok,{_, StartTime}} = THIS:get_property(startTime),
	%%  			io:format("startTime: ~p ~n", [StartTime]),
				
				case Relative == -1 of
					true ->
						THIS:set_property(endTime, StartTime),
					    THIS:set_property(startTime, StartTime - Window1),
						THIS:setReportPeriod();
					   _ ->
						THIS:set_property(endTime, StartTime + Window1),
						THIS:setReportPeriod()
				end
		end
	end,

	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
calculateMinFreq_Child([], Ret) ->
	Ret;
calculateMinFreq_Child([H|T], Ret) ->
	{ok,{_, T1}} = (H:getMonitor()):get_property(?FREQUENCY),	
	case T1 < Ret of 
		 true ->
			calculateMinFreq_Child(T, T1);
	  	 _ ->
		 	calculateMinFreq_Child(T, Ret)
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
calculateMinimumFrequency() ->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
%%  io:format("~nCollectors:~p ~n", [Collectors]),
	Frequency = calculateMinFreq_Child(Collectors, 6000),%%  zanshi 6000
	if 
		Frequency == 0 ->
			600;
	  	true ->
		 	Frequency
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
setReportPeriod() ->
	{ok,{_, StartTime}} = THIS:get_property(startTime),
	{ok,{_, EndTime}} = THIS:get_property(endTime),	

%% 	F = io_lib:format("~p", [lists:flatten(tuple_to_list(element(1, calendar:gregorian_seconds_to_datetime(StartTime))), 
%% 					  tuple_to_list(element(2, calendar:gregorian_seconds_to_datetime(StartTime))))]),
%% 	THIS:set_property(reportPeriod, L),
	
	L = "(from " ++ sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(StartTime))
	++ " to " ++ sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(EndTime)) ++ ")",
 	THIS:set_property(reportPeriod, L).

  
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
filterDataWithStatus([], StatuFilter, Cons) ->
	Cons;
filterDataWithStatus([H|T], StatuFilter, Cons) ->
%% 	io:format("~p ~n",[StatuFilter]),
%% 	io:format("~p ~n",[H:getLastCategory()]),
	case StatuFilter of
		"all" ->			
			filterDataWithStatus(T, StatuFilter, Cons++[H]);
		"error" ->
			case H:getLastCategory() of
				error ->
				filterDataWithStatus(T, StatuFilter, Cons++[H]);
				_->
				filterDataWithStatus(T, StatuFilter, Cons)
			end;
		"warning" ->
			case H:getLastCategory() of
				warning ->
				filterDataWithStatus(T, StatuFilter, Cons++[H]);
				_->
				filterDataWithStatus(T, StatuFilter, Cons)
			end;
		"good" ->
			case H:getLastCategory() of
				good ->
				filterDataWithStatus(T, StatuFilter, Cons++[H]);
				_->
				filterDataWithStatus(T, StatuFilter, Cons)
			end;
		"error,warning" ->
			case H:getLastCategory() of
				error ->
				filterDataWithStatus(T, StatuFilter, Cons++[H]);
				warning ->
				filterDataWithStatus(T, StatuFilter, Cons++[H]);					
				_->
				filterDataWithStatus(T, StatuFilter, Cons)
			end;		
		_->
			filterDataWithStatus(T, StatuFilter, Cons++[H])
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
getSummaryList([], SummaryList) ->
	SummaryList;
getSummaryList([H|T], SummaryList)->
	getSummaryList(T, SummaryList ++ [{H:getMonitorFullID(), H:getMonitorName(), H:getWorstCategory(), H:getMaximum(), H:getAverage()}]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
getSummaryListByMap(Map, [], Cons) ->
	Cons;	
getSummaryListByMap(Map, [H|T], Cons) ->
	{ok, V} = dict:find(H, Map),
	T1 = lists:nth(1,  V),	
	getSummaryListByMap(Map, T, Cons ++ [{T1:getMonitorFullID(), T1:getMonitorName(), T1:getWorstCategory(), T1:getMaximum(), T1:getAverage()}]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc  
unconsult(File, L) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
	file:close(S).

%% @spec createMessage(Dir,Template)->string()
%% where
%%	Dir = string()
%%	Template = string()
%% @doc create message from template file,Dir is the directory contain template file,Tempalte is the file name of template
%%
createMessage(Dir,Template)->
%% 	{ok,{_,Monitor}} = THIS:get_monitor(),
	Path = Dir ++ "/" ++ Template,
	case file:read_file(Path) of
		{error,enoent}->
			"The file does not exist:" ++ Path;
		{error,eacces}->
			"Missing permission for reading the file, or for searching one of the parent directories:" ++ Path;
		{error,eisdir}->
			"A component of the file name is not a directory. On some platforms, enoent is returned instead.";
		{error,enomem}->
			"There is not enough memory for the contents of the file.";
		{ok,Bin}->
			Tmp = binary_to_list(Bin),
%%			io:format("createMessage 1:~p ~n", [Tmp]),
			THIS:process_msg(Tmp)
%% 			io:format("createMessage 2: ~p ~n", [Tmp1])
	end.

process_msg([])->"";
process_msg("<" ++ T)->
%% 	process_msg(T);
	{T1, T2} = lists:splitwith(fun(A) -> A =/= $> end, T),
%% 	io:format("process_msg1:~p ~n", [T1]),	
%% 	io:format("process_msg2:~p ~n", [T2 -- [$>]]),	
	case THIS:get_property(list_to_atom(T1)) of
		{error,{_, _}} ->
			THIS:process_msg(T2 -- [$>]);
		{ok,{_, Value}}->
%% 			THIS:to_list(Value) ++ process_msg(T2 -- [$>])
%% 			io:format("createMessage 2: ~p ~n", THIS:to_list(T1)),

               Value1 = 
                   case T1 =:= "title" of
                        true->
                            "";
                        _  ->  THIS:to_list(Value)
                    end,
                Value1++ THIS:process_msg(T2 -- [$>])
	end;
process_msg([C|T])->
%% 	io:format("createMessage 3: ~p ~n", [C]),
	[C] ++ THIS:process_msg(T).

