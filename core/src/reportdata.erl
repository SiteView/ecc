%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(reportdata,[BASE]).
-extends(siteview_object).
-compile(export_all).
-include("ecc_oem.hrl").
-include("monitor.hrl").
-include("historyreport.hrl").
-include("monitor_template.hrl").

-define(MaxRecords,200).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for reportdata
new()->
	Obj = siteview_object:new(),

	Obj:set_attribute(collectors,[]),
	Obj:set_attribute(errorlist,[]),
	Obj:set_attribute(goodlist,[]),	
	Obj:set_attribute(warninglist,[]),
	Obj:set_attribute(data, []),

	Obj:set_attribute(alllist,[]),
%% 	Obj:set_attribute(nodataColor,""),
	
	Obj:set_property(monitors, ""),
	Obj:set_property(format, ""),
	Obj:set_property(startDay, "today"),
	Obj:set_property(title, "testhistoryreport"),
	Obj:set_property(truedata, false),
	
	{?MODULE,Obj}.

new(Data)->
	Obj = siteview_object:new(),
%% 	io:format("queryReportData1 AllData: ~p ~n", [Data]),
	Obj:set_attribute(collectors,[]),
	Obj:set_attribute(errorlist,[]),
	Obj:set_attribute(goodlist,[]),	
	Obj:set_attribute(warninglist,[]),

	Obj:set_attribute(alllist,[]),
 	Obj:set_attribute(data, Data),
	
	Obj:set_property(monitors, ""),
	Obj:set_property(format, ""),
	Obj:set_property(startDay, "today"),
	Obj:set_property(title, "testhistoryreport"),
	Obj:set_property(truedata, false),
	
	{?MODULE,Obj}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec delete(X) -> ok 
%% Obj = term()
%% @doc  
delete()->
	case THIS:get_attribute(collectors) of
		{ok,{_,Collectors}}->
 			F = fun(X)->
%% 				X:delete()
%% 				io:format("reportdata deleteobj child: ~p ~n", [X:get_tid()]),						
 				ets:delete_all_objects(X:get_tid()),
				ets:delete(X:get_tid())
 			end,
%% 			io:format("reportdata deleteobj: ~p ~n", [Collectors]),
 			lists:foreach(F, Collectors),
			THIS:remove_attribute(collectors);
		_->
			pass
	end,
	
	BASE:delete(). 

%% @spec initialize(X) -> ok 
%% Obj = term()
%% @doc  
initialize()->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	T1 = [],
	THIS:set_attribute(collectors, T1),	
	THIS:initializeSampleCollectors(),
	THIS:initializeTimeParameters(),
%%	THIS:initializeFilePaths(),	
	ok.

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

%% @spec createFromQuery(X) -> ok 
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
%% 	io:format("setNeedTrueData: ~p ~n", [Query]),
	THIS:setNeedTrueData(),
%% 	io:format("initialize: ~p ~n", [Query]),
	THIS:initialize(),
%% 	io:format("createReport: ~p ~n", [Query]),
	THIS:createReport().

%% @spec setNeedTrueData() -> ok 
%% Obj = term()
%% @doc  
setNeedTrueData()->
	nothing.
%% 	{ok,{_, Truedata}} = THIS:get_property(truedata),	
%% 	case Truedata of
%% 		true->
%% 			ok;		
%% 		_->
%% 		{ok,{_, StartTime}} = THIS:get_property(startTime),
%% 		{ok,{_, EndTime}} = THIS:get_property(endTime),
%% %% 		io:format("setNeedTrueData countLogSize1: ~p ~n", [StartTime]),
%% %% 		io:format("setNeedTrueData countLogSize2: ~p ~n", [EndTime]),
%% 		{ok,{_, T}} = THIS:get_property(monitors),
%% 		MonitorStr = string:tokens(T, ","),
%% 		{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 				calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr),
%% 		{ok,{_, Data}} = THIS:get_attribute(data),
%% 		case Data of
%% 			[]->		
%% 				AllData=Data;
%% %% %% 				{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% %% %% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr);
%% %% 				io:format("setNeedTrueData q start: ~p ~n",  [calendar:local_time()]),				
%% %% 				{_, AllData} = report_proxy:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% %% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr),
%% %% %% 				io:format("setNeedTrueData AllData end: ~p ~n",  [AllData]),
%% %% 				io:format("setNeedTrueData q end: ~p ~n",  [calendar:local_time()]);
%% 			_->
%% %% 				io:format("setNeedTrueData Exist Data: ~p ~n", [yes]),
%% 				AllData = Data
%% 		end,
%% 		I = countLogSize(AllData, 0),
%% 		I = length(AllData),
%% 		case I < 200 of
%% 			true ->
%% 				io:format("setNeedTrueData countLogSize: ~p ~n", [I]),
%% %% 				THIS:set_property(truedata, true),
%% %% 				THIS:set_property(count, I);
%% 				THIS:set_attribute(data, AllData);
%% 			_ ->
%% 				io:format("setNeedTrueData countLogSize: ~p ~n", [I]),
%% 				THIS:set_attribute(data, AllData),
%% 				ok
%% 		end
%% 		THIS:set_attribute(data, AllData)
%% 	end.
	
%% @spec addCollector(X) -> ok 
%% Obj = term()
%% @doc  
addCollector(CollectorMap, Monitor, Property, KeepSampleBuffer, TrueData)->
%% 	io:format("addCollector1: ~p ~n", [Monitor]),
	{ok,{id, FullId}} = Monitor:get_property(id),
%% 	{ok,{_, Name}} = Property:get_property(name),
	Name = Property,
%% 	Key = {FullId} + atom_to_list(Name),
%% 	Key,

	case TrueData of
		true->
			Collector = samplecollector:new(Monitor, Property, false, false, KeepSampleBuffer);
		_->
			Collector = samplecollector:new(Monitor, Property, false, false, KeepSampleBuffer)
			
%% 			Collector = samplecollector1:new(Monitor, Property, false, false, KeepSampleBuffer)			
	end,

		
%% 	Collector = samplecollectorproc:new(Monitor, Property, false, false, KeepSampleBuffer),	
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	T1 = Collectors ++ [Collector],
	
	THIS:set_attribute(collectors, T1),

	ok.

%% @spec get_AllSelMonitors(X) -> ok 
%% Obj = term()
%% @doc  
get_AllSelMonitors([], AllMonitors,Ret) ->
	Ret;
get_AllSelMonitors([H|T], AllMonitors, Ret) ->	
%% 	Monitor = api_siteview:find_object(H),
	Monitor = report_proxy:find_object(H),
	get_AllSelMonitors(T, AllMonitors, dict:append(H, Monitor, Ret)).

	
%% @spec initializeSampleCollectors(X) -> ok 
%% Obj = term()
%% @doc  
initializeSampleCollectors()->
	{ok,{_, T}} = THIS:get_property(monitors),
	{ok,{_, TrueData}} = THIS:get_property(truedata),	
	MonitorStr = string:tokens(T, ","),
%% 	io:format("initializeSampleCollectors1: ~p ~n", [MonitorStr]),
	Monitors = get_AllSelMonitors(MonitorStr, [], dict:new()),
%% 	io:format("initializeSampleCollectors2: ~p ~n", [MonitorStr]),

	
	F = fun(X)->
			[[Monitor]] = dict:fetch(X, Monitors),
			Props=Monitor:getLogProperties(Monitor),
			F1 = fun(X1)->
			    THIS:addCollector(collectorMap, Monitor, X1, false, TrueData)
			end,
			lists:foreach(F1, Props)
	end,
	
%% 	F = fun(X)->
%% 			[[Monitor]] = dict:fetch(X, Monitors),
%% 			{_,{_,Class}}=Monitor:get_property(class),
%% 			Template=report_proxy:get_template(Class),
%% %% 			io:format("initializeSampleCollectors1112: ~p ~n", [Template]),
%% 			Props=Monitor:getLogProperties(Monitor),
%% %% 			io:format("initializeSampleCollectors111111: ~p ~n", [Props]),
%% 			F1 = fun(X1)->
%% 				case lists:keysearch(X1, #property.name, Template) of
%% 		 		{V,Plabel} ->
%% 					 case element(4, Plabel) of
%% 						 numeric ->
%% 							 THIS:addCollector(collectorMap, Monitor, X1, false, TrueData);
%% 						 frequency ->
%% 							 THIS:addCollector(collectorMap, Monitor, X1, false, TrueData);
%% 						 scalar ->
%% 							 THIS:addCollector(collectorMap, Monitor, X1, false, TrueData);
%% 						 _->
%% 								ok
%% 					end;
%% 				_ ->
%% 					ok
%% %% 					io:format("initializeSampleCollectors22: ~p ~n", [X1]),
%% %% 			 		THIS:addCollector(collectorMap, Monitor, X1, false, TrueData)
%% 				end
%% 			end,
%% 			lists:foreach(F1, Props)
%% 	end,
	
	lists:foreach(F, dict:fetch_keys(Monitors)),
%% 	lists:foreach(F, dict:fetch_keys(MonitorStr)),
%% 	io:format("initializeSampleCollectors3: ~p ~n", [MonitorStr]),
	ok.
	
%% @spec get_all_list(X) -> ok 
%% Obj = term()
%% @doc
get_all_list(H) ->	
	{ok,{_, List}} = THIS:get_attribute(alllist),
	T1 = List ++ [H],
	THIS:set_attribute(alllist, T1).

%% @spec process_sub_2(X) -> ok 
%% Obj = term()
%% @doc  
process_sub_2([], Ret, RequestedStartTime, RequestedEndTime, Precision) ->
	Ret;
process_sub_2([H|T], Ret, RequestedStartTime, RequestedEndTime, Precision) ->
	L = calendar:datetime_to_gregorian_seconds(RequestedStartTime),
	L1 = calendar:datetime_to_gregorian_seconds(RequestedEndTime),

 	H:createBuckets(L, L1, Precision),
	Monitor = H:getMonitor(),

	{ok,{_, ID}} = Monitor:get_property(id),

	Key = ID,
	process_sub_2(T, dict:append(Key, H, Ret), RequestedStartTime, RequestedEndTime, Precision).

%% %% @spec countLogSize(X) -> ok 
%% %% Obj = term()
%% %% @doc
%% countLogSize([], Count) ->
%% 	Count;
%% countLogSize([H|T], Count) ->
%% 	case H of
%% 		{eof, ChildData} ->
%% 			{eof, ChildData} = H,
%% 			countLogSize(T, Count + erlang:length(ChildData));
%% 		_->
%% 		countLogSize(T, Count)
%% 	end.

%% %% @spec countLogSize(X) -> ok 
%% %% Obj = term()
%% %% @doc 
%% countLogSizeold(Data) ->
%% 	F = fun(X)->		
%% 		case X of
%% 			{eof, ChildData} ->
%% 				F1 = fun(X1)->
%% 					case is_list(X1) of
%% 						true ->
%% 							ok;
%% 						_->
%% 					 		ok
%% 					end					
%% 			    end,
%% 			    lists:foreach(F1, [ChildData]);							
%% 			_ ->
%% 				ok
%% 		end
%% 	end,
%% 	lists:foreach(F, Data),
%% 	ok.

%% @spec process(X) -> ok 
%% Obj = term()
%% @doc  
process(CollectorList, Data, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc, DstrNeed) ->
%% 	io:format("process: ~p ~n", ["Start"]),
	MonitorMap = THIS:process_sub_1(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision),	
	
%% 	I = THIS:countLogSize(Data, 0),	
%% 	MonitorMap = THIS:process_sub_3(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision, I),	
	F = fun(X)->		
		case X of
			{eof, ChildData} ->
%% 				io:format("process ~p ~n", [ChildData]),
				F1 = fun(X1)->
%% 					io:format("~p ~n", [X1]),
					case is_list(X1) of
						true ->
							F2 = fun(X2)->
								K2 = X2#monitorlog.id,
								Measurement=X2#monitorlog.measurement,
									  
								case dict:find(K2, MonitorMap) of
									{ok, Connectors}->
											case (((calendar:datetime_to_gregorian_seconds(X2#monitorlog.time) - calendar:datetime_to_gregorian_seconds(RequestedStartTime)) >= 0) and
													((calendar:datetime_to_gregorian_seconds(RequestedEndTime) - calendar:datetime_to_gregorian_seconds(X2#monitorlog.time)) >= 0)) of
												true ->
															case DstrNeed of
																true->
%% 																	THIS:get_all_list(X2);
																	ok;
																_->
																	ok
															end,
															F3 = fun(X3) ->
																case X3:getPropertyType() of
																	numeric->
																		X3:add_2(X2#monitorlog.time, Measurement, X2#monitorlog.category, X2#monitorlog.category, bestCaseCalc);
																	_->
																		ok
																end
															end,
															lists:foreach(F3, Connectors);
												_->
													ok
											end;							
							 		_->
								 		ok
								end
							 end,
							 lists:foreach(F2, X1);
				 	 	 false ->
							ok
					end
			    end,
			    lists:foreach(F1, [ChildData]);
			_ ->
			    io:format("~p ~n", ["No Data Log:"]),
				io:format("~p ~n", [X])
		end
	end,
	lists:foreach(F, Data),
	MonitorMap.

%% @spec processTrueData(X) -> ok 
%% Obj = term()
%% @doc  
processTrueData(CollectorList, Data, Compress, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc) ->
%% 	[{eof, ChildData1}] = Data,c
%% 	io:format("processTrueData: ~p ~n", ["Start"]),
	MonitorMap = THIS:process_sub_2(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision),
%% 	io:format("~p ~n", [MonitorMap]),
	F = fun(X)->
			{X:getProperty(),X:getPropertyType()}
	end,	
	MeasurementType = lists:map(F, CollectorList),
	{ok,{_, T}} = THIS:get_property(monitors),
	MonitorStr = string:tokens(T, ","),
	io:format("MonitorStr:~p~n", [MonitorStr]),
%% 	io:format("MeasurementType1:~p~n", [MeasurementType]),
%% 	io:format("MeasurementType2:~p~n", [{precisionCompress, RequestedStartTime, RequestedEndTime, Precision}]),	
	case Data of
		[]->
			nothing;
		_->	
			Result = try
				case Compress of
					noCompress ->
						report_statics:report(Data, MeasurementType, noCompress);
					{mutiCompress, Count} ->
						report_statics:report(Data, MeasurementType, {compress, Count});
					{precisionCompress, _} ->
						report_statics:report(Data, MeasurementType, {precisionCompress, RequestedStartTime, RequestedEndTime, Precision})
				end
			catch
				_:_->
%% 					io:format("processTrueData catch : ~p ~n", [MonitorStr]),
					[]
			end,
%% 			io:format("Report_Static Result:~p~n", [Result]),
			THIS:analysisStatData(MonitorMap, dict:fetch_keys(MonitorMap), Result),
			{ok,{_, DstrNeed}} = THIS:get_property(dstrNeed),
			case DstrNeed of
				true->
					THIS:set_attribute(alllist, Data);
				_->
					ok
			end	
	end,
	MonitorMap.

filter_value(X) ->
  case X of
	undefined ->
		0;	
	_ ->
		X
  end.

analysisMonitorStatData([], StatsData)->
	done;
analysisMonitorStatData([H|T], StatsData)->
	TID = H:get_tid(),
	Name = H:getProperty(),
	case lists:keyfind(Name, 1, StatsData) of
		{_, L}->
			lists:foreach(fun({Key, Value})->ets:insert(TID, {Key, filter_value(Value)}) end, L);
		_ ->
			nothing
	end,
	analysisMonitorStatData(T, StatsData).

%% @spec getSummaryList(X) -> ok 
%% Obj = term()
%% @doc
analysisStatData(Map, [], StatsData) ->
	done;
analysisStatData(Map, [H|T], StatsData)->	
	{ok, V} = dict:find(H, Map),
	Temp = THIS:analysisMonitorStatData(V, StatsData),
	analysisStatData(Map, T, StatsData).

%% %% @spec count_Con(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% count_Con(Map, [], Cons, I) ->
%% 	{I, Cons};	
%% count_Con(Map, [H|T], Cons, I) ->
%% 	{ok, V} = dict:find(H, Map),	
%% 	count_Con(Map, T, Cons ++ V, I + length(V)).

getDayLimits()->
	{ok,{_, StartTime}} = THIS:get_property(startTime),
 	{StartDate,_}=calendar:gregorian_seconds_to_datetime(StartTime),
%% 	io:format("getDayLimits1: ~p ~n", [StartDate]),
	StartDays = calendar:date_to_gregorian_days(StartDate),
	{ok,{_, EndTime}} = THIS:get_property(endTime),
	{EndDate,_}=calendar:gregorian_seconds_to_datetime(EndTime),
%% 	io:format("getDayLimits2: ~p ~n", [EndDate]),
	EndDays = calendar:date_to_gregorian_days(EndDate),
%% 	io:format("getDayLimits: ~p ~n", [(EndDays - StartDays) + 1]),
	(EndDays - StartDays) + 1.

computePrecision(I, StartTime, EndTime)->
	case I < 200 of
		true ->
			case I =/= 0 of
				true->
				Freq =  (EndTime - StartTime) / I,
				THIS:set_property(precision, trunc(Freq));
				_->
					ok
			end;
		_->
			Freq =  trunc(I/200) * ((EndTime - StartTime)/I),
			THIS:set_property(precision, trunc(Freq))
	end.

%% @spec createReport(X) -> ok 
%% Obj = term()
%% @doc
createReport()->
%% 	io:format("createReport: ~p ~n", ["start"]),
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),	
	{ok,{_, StartTime}} = THIS:get_property(startTime),
%%  	io:format("StartTime:~p ~n", [calendar:gregorian_seconds_to_datetime(StartTime)]),
	{ok,{_, EndTime}} = THIS:get_property(endTime),
%% 	io:format("EndTime:~p ~n", [calendar:gregorian_seconds_to_datetime(EndTime)]),
	{ok,{_, DstrNeed}} = THIS:get_property(dstrNeed),
%% 	{ok,{_, SchedFilter}} = THIS:get_property(schedFilter),
%% 	io:format("~p ~n", [SchedFilter]),
%% 	ScheduleMap	= schedule_manager:get_history_schedule_filter(SchedFilter),
%% 	case ScheduleMap of
%% 		null ->
%% 		    SchedName = "all";
%% 		_->
%% 			SchedName = dbcs_schedule:get_name(SchedFilter)
%% 	end,
%% 	io:format("~p ~n", [ScheduleMap]),
%% 	{ok,{_, Description}} = THIS:get_property(description),
%% 	io:format("~p ~n",[Description]),
%% 	case Description == [] of
%% 		true->
%% 		    Des = "";
%% 		_->
%% 			Des = Description
%% 	end,
	
%% 	{ok,{_, StatusFilter}} = THIS:get_property(statusFilter),
%% 	io:format("monitor_logger start: ~p ~n", [calendar:local_time()]),
	{ok,{_, T}} = THIS:get_property(monitors),
	MonitorStr = string:tokens(T, ","),	
	
	{ok,{_, Truedata}} = THIS:get_property(truedata),	
%%  	Count = 20,
	{ok,{_, Count}} = THIS:get_property(count),
%% 	io:format("createReport1: ~p ~n", [MonitorStr]),
%% 	io:format("monitor_logger start: ~p ~n", [calendar:local_time()]),
	case Truedata of
		true->
%% 			{_, AllData} = monitor_logger:qc(MonitorStr, Count);
			{ok,{_, DataSrc}} = THIS:get_attribute(data),
			case DataSrc of
				[]->					
%% 					{_, AllData} = monitor_logger:qc(MonitorStr, Count, DaysLimit);
%% 					{_, AllData} = report_proxy:qc(MonitorStr, Count, THIS:getDayLimits()),
%% 					io:format("monitor_logger start: ~p ~n", [MonitorStr]), 
					{ok,{_, DbCon}} = THIS:get_property(dbCon),					
					AllData = report_proxy:q_bycount(MonitorStr, Count, THIS:getDayLimits(), DbCon);
%%	 				io:format("monitor_logger start1: ~p ~n", [AllData]);
				_->
					AllData = DataSrc
			end;
		_->
%% 		{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 				calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr)
			{ok,{_, DataSrc}} = THIS:get_attribute(data),
			AllData = DataSrc
%% 			case DataSrc of
%% 				[]->
%% %% 					{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% %% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr);					
%% 					{_, AllData} = report_proxy:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr);
%% 				_->
%% 					AllData = DataSrc
%% 			end
	end,	
%% 	io:format("monitor_logger end: ~p ~n", [calendar:local_time()]),
%% 	io:format("monitor_logger end11: ~p ~n", [AllData]),

%% 	io:format("process start: ~p ~n", [calendar:local_time()]),
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
		{ok,{_, Precision}} = THIS:get_property(precision),
%% 		io:format("Precision:~p ~n", [Precision]),
		Map = THIS:processTrueData(Collectors, Data1, noCompress, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, scheduleMap, bestCaseCalc);	
		_->
%% 		THIS:computePrecision(length(AllData), StartTime, EndTime),
		{ok,{_, Precision}} = THIS:get_property(precision),		
		Map = THIS:processTrueData(Collectors, AllData, {mutiCompress, 200}, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, scheduleMap, bestCaseCalc)			
%% 		Map = THIS:processTrueData(Collectors, AllData, {precisionCompress, Precision}, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
%% 				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, scheduleMap, bestCaseCalc)	

%% %% 		io:format("process AllData: ~p ~n", [AllData]),
%% %% 		I = countLogSize(AllData, 0),
%% %% 		io:format("process countLogSize: ~p ~n", [I]),
%% 		case I < 200 of
%% 			true ->
%% 				case I =/= 0 of
%% 					true->
%% 					Freq =  (EndTime - StartTime) / I,
%% %% 					io:format("process Freq1: ~p ~n", [Freq]),
%% 					THIS:set_property(precision, Freq);
%% 					_->
%% 						ok
%% 				end;
%% 			_->
%% 				Freq =  trunc(I/200) * ((EndTime - StartTime)/I),
%% %% 				io:format("process Freq2: ~p ~n", [Freq]),
%% 				THIS:set_property(precision, Freq)
%% 		end,
%% 		{ok,{_, Precision}} = THIS:get_property(precision),
%% 		io:format("Precision:~p ~n", [Precision]),
%% %% 		Map = THIS:process(Collectors, AllData, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
%% %% 				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, scheduleMap, bestCaseCalc, DstrNeed)
%% 		Map = THIS:processTrueData(Collectors, AllData, I, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
%% 				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, scheduleMap, bestCaseCalc);
		
	end,
%% 	io:format("process end: ~p ~n", [calendar:local_time()]),
%% 	{ok,{_, Title}} = THIS:get_property(title),
%% 	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	
%% 	{_, Consold} = THIS:count_Con(Map, dict:fetch_keys(Map), [], 0),
	
%% 	Cons = THIS:filterDataWithStatus(Consold, "all", []),
	case AllData of
		[]->
			[];
		_->
%% 	 	io:format("SummaryList Start: ~p ~n",  [calendar:local_time()]),	
		Summary1 = THIS:getSummaryList(Map, dict:fetch_keys(Map), []),
		
		{ok,{_, Collectors}} = THIS:get_attribute(collectors),
		Summary2 = THIS:get_uptimeSummary(Map, Collectors, '', []),
%% 	 	io:format("SummaryList End: ~p ~n",  [calendar:local_time()]),
		case DstrNeed of
			true ->
				{ok,{_, List}} = THIS:get_attribute(alllist),
				%% dstrStatusNoNeed = null,ok,disable,bad
				DstrList = THIS:getDstrList(List, dict:fetch_keys(Map), []),
				Summary1 ++ Summary2 ++ DstrList;
			_->
				Summary1 ++ Summary2
		end
	end.
		
	

%% @spec getMonitorSummaryList(X) -> ok 
%% Obj = term()
%% @doc
getMonitorSummaryList([], I, SummaryList) ->
	SummaryList;  
getMonitorSummaryList([H|T], I, SummaryList) ->	
	Detail = buildbuckets([], H, H:getBuckets(), 1),		
%% 	io:format("getMonitorSummaryList~p~n", [to_list(H:getMonitorName())]),
%% 	Monitor = H:getMonitor(),
%% 	Props = Monitor:getPrimaryStateProperties(Monitor),
%% 	io:format("getMonitorSummaryList1: ~p ~n", [Props]),
%% 	io:format("getMonitorSummaryList2: ~p ~n", [H:getProperty()]),
%% 	case lists:member(H:getProperty(), Props) of
%% 		true ->
%% 			getMonitorSummaryList(T, I+1, SummaryList ++ [{"(Return_" ++ integer_to_list(I) ++ ")" ++ to_list(H:getMonitorFullID()), [{min, to_list(H:getMinimum())}, {detail, ""}, {max, to_list(H:getMaximum())},  
%% 			{when_max, to_list(H:getMaximumTime())}, {"MonitorName", to_list(H:getMonitorName())}, 
%% 			{"ReturnTitle", to_list(H:getProperty())},{"ReturnName", to_list(H:getPropertyLabel())}, {average, to_list(H:getAverage())}, {latest, to_list(H:getLastValue())}, {sv_type, to_list(H:getPropertyType())},  
%% 			{sv_drawimage, "1"},  {sv_drawtable, "1"}, {sv_drawmeasure, "1"}, {sv_primary, "1"}, {sv_baseline, "1"}, {detail, Detail}]}]).
%% 		_  ->
%% 			getMonitorSummaryList(T, I+1, SummaryList ++ [{"(Return_" ++ integer_to_list(I) ++ ")" ++ to_list(H:getMonitorFullID()), [{min, to_list(H:getMinimum())}, {detail, ""}, {max, to_list(H:getMaximum())},  
%% 			{when_max, to_list(H:getMaximumTime())}, {"MonitorName", to_list(H:getMonitorName())}, 
%% 			{"ReturnName", to_list(H:getProperty())},{average, to_list(H:getAverage())}, {latest, to_list(H:getLastValue())},  
%% 			{sv_drawimage, "1"},  {sv_drawtable, "1"}, {sv_drawmeasure, "1"}, {sv_primary, "0"}, {sv_baseline, "1"}, {detail, Detail}]}])
%% 	end.

	case H:getPropertyType() of
		text->
			getMonitorSummaryList(T, I+1, SummaryList ++ [{"(Return_" ++ integer_to_list(I) ++ ")" ++ to_list(H:getMonitorFullID()), [{min, to_list(H:getMinimum())}, {detail, ""}, {max, to_list(H:getMaximum())},  
			{when_max, to_list(H:getMaximumTime())}, {"MonitorName", to_list(H:getMonitorName())}, 
			{"ReturnTitle", to_list(H:getProperty())},{"ReturnName", to_list(H:getPropertyLabel())}, {average, to_list(H:getAverage())}, {latest, to_list(H:getLastValue())}, {sv_type, to_list(H:getPropertyType())},  
			{sv_drawimage, "0"},  {sv_drawtable, "0"}, {sv_drawmeasure, "0"}, {sv_primary, "0"}, {sv_baseline, "0"}, {detail, Detail}]}]);
		_->
			getMonitorSummaryList(T, I+1, SummaryList ++ [{"(Return_" ++ integer_to_list(I) ++ ")" ++ to_list(H:getMonitorFullID()), [{min, to_list(H:getMinimum())}, {detail, ""}, {max, to_list(H:getMaximum())},  
			{when_max, to_list(H:getMaximumTime())}, {"MonitorName", to_list(H:getMonitorName())}, 
			{"ReturnTitle", to_list(H:getProperty())},{"ReturnName", to_list(H:getPropertyLabel())}, {average, to_list(H:getAverage())}, {latest, to_list(H:getLastValue())}, {sv_type, to_list(H:getPropertyType())},  
			{sv_drawimage, "1"},  {sv_drawtable, "1"}, {sv_drawmeasure, "1"}, {sv_primary, "1"}, {sv_baseline, "1"}, {detail, Detail}]}])			
	end.

%% @spec getSummaryList(X) -> ok 
%% Obj = term()
%% @doc
getSummaryList(Map, [], SummaryList) ->
	SummaryList;
getSummaryList(Map, [H|T], SummaryList)->
	{ok, V} = dict:find(H, Map),
	Temp = THIS:getMonitorSummaryList(V, 0, []),
	getSummaryList(Map, T, SummaryList ++ Temp).

%% @spec getDstrListChild(X) -> ok 
%% Obj = term()
%% @doc
getMonitorDstrList([], DstrList)->
	DstrList;
getMonitorDstrList([H|T], DstrList)->
%% 	Measurement = try
%% 		binary_to_term(H#monitorlog.measurement)
%% 	catch
%% 		_:_->
%% %% 			io:format("Record: ~p~n", [binary_to_term(base64:decode(element(7, Record)))]),
%% 			binary_to_term(base64:decode(H#monitorlog.measurement))
%% 	end,
	Measurement=H#monitorlog.measurement,
%% 	 binary_to_list(H#monitorlog.desc)
	getMonitorDstrList(T, DstrList ++ [{H#monitorlog.time,{H#monitorlog.category, H#monitorlog.desc, THIS:measurementtoString(Measurement, [])}}]).

%% @spec measurementtoString(X) -> ok 
%% Obj = term()
%% @doc
measurementtoString([], Str)->
	Str;
measurementtoString([H|T], Str)->
	{Key, Value} = H,
%% 	measurementtoString(T, Str ++ to_list(Key) ++ "=" ++ to_list(Value) ++ ",").
	measurementtoString(T, Str ++ to_list({Key, to_value(Value)})).
  		
%% @spec getDstrList(X) -> ok 
%% Obj = term()
%% @doc
getDstrList(DstrList, [], MonitorDstrList)->
	MonitorDstrList;
getDstrList(DstrList, [H|T], MonitorDstrList)->
	F=fun(X)->
		case X#monitorlog.id of
			H ->
				true;
			_  ->
				false
		end
	end,
	ChildSrcDstrList = lists:filter(F, DstrList),
	ChildDesDstrList = getMonitorDstrList(ChildSrcDstrList, []),
	
	getDstrList(DstrList, T, MonitorDstrList ++ [{"(dstr)" ++ to_list(H), ChildDesDstrList}]).
	
%% @spec buildbuckets(X) -> ok 
%% Obj = term()
%% @doc  
buildbuckets(RetData,SampleCollector, [H|E],I)->	
	Bucket=H,
%% 	{_, {{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_}, {connector, SampleCollector}}} = Bucket,
	Time=SampleCollector:getBucketStartTime(I),
	Value=SampleCollector:getBucketData(Bucket, getAverage),
%% 	TRetData=RetData++[{calendar:gregorian_seconds_to_datetime(Time),Value}],
	TRetData=RetData++[{Time,Value}],
     buildbuckets(TRetData,SampleCollector, E,I+1);
buildbuckets(RetData,_,[],_)->
	RetData.

to_list(X) when is_atom(X)->
	atom_to_list(X);
to_list(X) when is_float(X)->
	io_lib:format("~.2f", [X]);
to_list(X) when is_integer(X)->	
    integer_to_list(X);
to_list(X) ->
  [X].

to_value(X) when is_float(X)->
  io_lib:format("~.2f", [X]);
to_value(X) ->
  X.

%% @spec getReturnValueList(X) -> ok 
%% Obj = term()
%% @doc
getReturnValueList([], I, RetList) ->
	RetList;  
getReturnValueList([H|T], I, RetList) ->	
	getReturnValueList(T, I+1, RetList ++ [{"(Return_" ++ integer_to_list(I) ++ ")" ++ to_list(H:getMonitorFullID()), "ReturnValue"}]).

%% @spec get_uptimeSummary(X) -> ok 
%% Obj = term()
%% @doc  
get_uptimeSummary(Map, [], Id, SummaryList) ->
	SummaryList;
get_uptimeSummary(Map, [H|T], Id, SummaryList) ->
	case (H:getMonitorFullID() =/= Id) or (Id == '') of
		true ->
			{UpTimePercentage, WarningTimePercentage, ErrorTimePercentage, _} = THIS:calculateUpTime(H:getGoodTime(), H:getWarningTime(), H:getErrorTime(), H:getTotalTime(), precision),
%% 			io:format("get_uptimeSummary: UpTimePercentage ~p, WarningTimePercentage ~p , ErrorTimePercentage ~p ~n", [H:getGoodTime(), H:getWarningTime(), H:getErrorTime()]),
			L1 = [{latestCreateTime,  to_list(erlang:localtime())}, {"MonitorName", to_list(H:getMonitorName())}, {errorPercent, to_list(ErrorTimePercentage)},	{warnPercent,to_list(WarningTimePercentage)}, {okPercent,to_list(UpTimePercentage)}, 
				  {latestStatus, to_list(H:getLastCategory())}, {latestDstr, to_list(H:get_last_description())}],
			{ok, V} = dict:find(H:getMonitorFullID(), Map),
			L2 = getReturnValueList(V, 0, []),
%%			getThresholdList(),
			L3 = [{errorCondition, ""}],
			L = [{to_list(H:getMonitorFullID()), L1 ++ L2 ++ L3}],
			get_uptimeSummary(Map, T, H:getMonitorFullID(), SummaryList ++ L);
		_ ->
			get_uptimeSummary(Map, T, Id, SummaryList)
	end.

%% @spec calculateUpTime(X) -> ok 
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

%% @spec initializeTimeParameters(X) -> ok 
%% Obj = term()
%% @doc  
initializeTimeParameters()->
%% 	{ok,{_, StartDayStr}} = THIS:get_property(startDay),
%% 	{ok,{_, StartHourStr}} = THIS:get_property(startHour),
%% 	{ok,{_, WindowStr}} = THIS:get_property(window),
%% 	{ok,{_, PrecisionStr}} = THIS:get_property(precision),
%% 	io:format("initializeTimeParameters: ~p ~n", ["start"]),
	{ok,{_, Truedata}} = THIS:get_property(truedata),
	case Truedata of
		true ->
				Freq = THIS:calculateMinimumFrequency(),

				THIS:set_property(precision, Freq),
				DefaultSamples = 20,
				Window = Freq * DefaultSamples,
				THIS:set_property(window, Window),
				THIS:set_property(endTime, trunc(platform:timeMillis()/1000)),
				THIS:set_property(startTime, trunc(platform:timeMillis()/1000) - Window);
%% 				THIS:setReportPeriod();
		_->	
				Freq1 = THIS:calculateMinimumFrequency(),
				THIS:set_property(precision, Freq1)
%% 				{ok,{_, Relative}} = THIS:get_property(relative),
%% 				{ok,{_, StartTime}} = THIS:get_property(startTime),
				
%% 				case Relative == -1 of
%% 					true ->
%% 						THIS:set_property(endTime, StartTime),
%% 					    THIS:set_property(startTime, StartTime - Window1),
%% 						THIS:setReportPeriod()
%% 					   _ ->
%% %% 						THIS:set_property(endTime, StartTime + Window1),
%% 						THIS:setReportPeriod()
%% 				end
		end,
%% 	io:format("initializeTimeParameters: ~p ~n", ["end"]),
	ok.

%% @spec calculateMinFreq(X) -> ok 
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

%% @spec calculateMinimumFrequency(X) -> ok 
%% Obj = term()
%% @doc  
calculateMinimumFrequency() ->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	Frequency = calculateMinFreq_Child(Collectors, 6000),%%  zanshi 6000
	if 
		Frequency == 0 ->
			600;
	  	true ->
		 	Frequency
	end.

%% @spec setReportPeriod(X) -> ok 
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

  