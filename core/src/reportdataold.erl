%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(reportdataold,[BASE]).
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
%%  				ets:delete_all_objects(X:get_tid()),
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
	{ok,{_, Truedata}} = THIS:get_property(truedata),	
	case Truedata of
		true->
			ok;		
		_->
		{ok,{_, StartTime}} = THIS:get_property(startTime),
		{ok,{_, EndTime}} = THIS:get_property(endTime),
%% 		io:format("setNeedTrueData countLogSize1: ~p ~n", [StartTime]),
%% 		io:format("setNeedTrueData countLogSize2: ~p ~n", [EndTime]),
		{ok,{_, T}} = THIS:get_property(monitors),
		MonitorStr = string:tokens(T, ","),
%% 		{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 				calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr),
		{ok,{_, Data}} = THIS:get_attribute(data),
		case Data of
			[]->				
%% 				{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr);
%% 				io:format("setNeedTrueData q start: ~p ~n",  [calendar:local_time()]),
%% 				{_, AllData} = report_proxy:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr);
%% 				io:format("setNeedTrueData q end: ~p ~n",  [calendar:local_time()]);
				AllData = Data;
			_->
				AllData = Data
		end,
%% 		I = countLogSize(AllData, 0),
		I = length(AllData),
		case I < 200 of
			true ->
%% 				io:format("setNeedTrueData countLogSize: ~p ~n", [I]),
%% 				THIS:set_property(truedata, true),
%% 				THIS:set_property(count, I);
				THIS:set_attribute(data, AllData);
			_ ->
				THIS:set_attribute(data, AllData),
				ok
		end
	end.
	
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
			Collector = samplecollectordata:new(Monitor, Property, false, false, KeepSampleBuffer);
		_->
			Collector = samplecollector:new(Monitor, Property, false, false, KeepSampleBuffer)
			
%% 			Collector = samplecollector1:new(Monitor, Property, false, false, KeepSampleBuffer)			
	end,

%% 	case TrueData of
%% 		true->
%% 			Collector = samplecollectordataproc:new(Monitor, Property, false, false, KeepSampleBuffer);
%% 		_->
%% 			Collector = samplecollectorproc:new(Monitor, Property, false, false, KeepSampleBuffer)
%% 	end,
		
%% 	Collector = samplecollectorproc:new(Monitor, Property, false, false, KeepSampleBuffer),	
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	T1 = Collectors ++ [Collector],
	
	THIS:set_attribute(collectors, T1),

	ok.

%% %% @spec get_AllSelGruopMonitors(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% get_AllSelGruopMonitors([], Monitors,Ret) ->
%% 	Ret;
%% get_AllSelGruopMonitors([H|T], Monitors, Ret) ->
%% %%   	ID = api_siteview:get_parent_id(H),
%% 	ID = report_proxy:get_parent_id(H),
%% 	case lists:keysearch(id, 1, Monitors) of 
%%    	{value,{"id", N}} ->
%% %% 		Monitor = api_siteview:find_object(N)
%% 		Monitor = report_proxy:find_object(N)
%% %% 		get_AllSelGruopMonitors(T, Monitors, dict:append(N, Monitor, Ret)) 
%%    	end.

%% @spec get_AllSelMonitors(X) -> ok 
%% Obj = term()
%% @doc  
get_AllSelMonitors([], AllMonitors,Ret) ->
	Ret;
get_AllSelMonitors([H|T], AllMonitors, Ret) ->	
%% 	Monitor = api_siteview:find_object(H),
	Monitor = report_proxy:find_object(H),
	get_AllSelMonitors(T, AllMonitors, dict:append(H, Monitor, Ret)).
%%     case lists:keysearch(H, 2, AllMonitors) of 
%%     {value,{_, ID}} ->
%% 		Monitor = report_proxy:find_object(ID),
%% 		get_AllSelMonitors(T, AllMonitors, dict:append(ID, Monitor, Ret)) 
%%     end.
%% 	Monitor = dbcs_monitor:get_monitor(H),

%% @spec getLogProperties(X) -> ok 
%% Obj = term()
%% @doc  
%% getLogProperties(T)->
%% %% 	Temp = T:get_template_property(T),
%% 	Temp = api_monitor_template:get_template(T),
%% 	[X#property.name || X<-Temp,X#property.state=:=true].

	
%% @spec initializeSampleCollectors(X) -> ok 
%% Obj = term()
%% @doc  
initializeSampleCollectors()->
	{ok,{_, T}} = THIS:get_property(monitors),
	{ok,{_, TrueData}} = THIS:get_property(truedata),	
	MonitorStr = string:tokens(T, ","),
%% 	io:format("initializeSampleCollectors1: ~p ~n", [MonitorStr]),
%% 	put(hostname, THIS:get_app()),
%% 	io:format("initializeSampleCollectors: ~p ~n", [THIS:get_app()]),
%% 	AllMonitors = api_monitor:get_all_monitors(),
%% 	AllMonitors = report_proxy:get_all_monitors(),
%% 	io:format("AllMonitors:~p ~n", [AllMonitors]),
%% 	Monitors = get_AllSelMonitors(MonitorStr, AllMonitors, dict:new()),
	Monitors = get_AllSelMonitors(MonitorStr, [], dict:new()),
%% 	io:format("initializeSampleCollectors2: ~p ~n", [MonitorStr]),
%% 	SelMonitors = get_AllSelMonitors(MonitorStr, AllMonitors, dict:new()),
%% 	SelGroupMonitors = get_AllSelGruopMonitors(AllMonitors, MonitorStr, dict:new()),
%% 	Monitors = lists:append(SelMonitors, SelGroupMonitors),
%% 	keepSampleBuffer = this.reportObjectConfig.isLineGraphReportType();
	
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

%% %% @spec get_list_data(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% get_list_data(H, MaxError, MaxGood, MaxWarning) ->
%% 	case H#monitorlog.category of
%% 		good ->
%% 			{ok,{_, List}} = THIS:get_attribute(goodlist),
%% 			case length(List) =< MaxGood of
%% 				true ->					
%% 					T1 = List ++ [H],
%% 					THIS:set_attribute(goodlist, T1);
%% 				 _->
%% 					error
%% 			end;
%% 		error ->
%% 			{ok,{_, List}} = THIS:get_attribute(errorlist),
%% 			case length(List) =< MaxError of
%% 				true ->					
%% 					T1 = List ++ [H],	
%% 					THIS:set_attribute(errorlist, T1);
%% 				 _->
%% 					error			
%% 			end;
%% 		warning ->
%% 			{ok,{_, List}} = THIS:get_attribute(warninglist),
%% 			case length(List) =< MaxWarning of
%% 				true ->					
%% 					T1 = List ++ [H],	
%% 					THIS:set_attribute(warninglist, T1);
%% 				 _->
%% 					error			
%% 			end;
%% 		 _ ->
%% 			ok
%% 	end.

%% @spec process_sub_1(X) -> ok 
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

%% @spec process_sub_2(X) -> ok 
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

%% @spec process_sub_2(X) -> ok 
%% Obj = term()
%% @doc  
process_sub_3([], Ret, RequestedStartTime, RequestedEndTime, Precision, Length) ->
	Ret;
process_sub_3([H|T], Ret, RequestedStartTime, RequestedEndTime, Precision, Length) ->
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
	process_sub_3(T, dict:append(Key, H, Ret), RequestedStartTime, RequestedEndTime, Precision, Length).

%% %% @spec getTuplelist(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% getTuplelist([],Ret)->
%% 	Ret;
%% getTuplelist([H|E],Ret)->
%% 	Index=string:rstr(H,"<")-1,
%% 	Subs=string:sub_string(H, 1,Index),
%% 	Tup = string:tokens(Subs,"="),
%%     Key=list_to_atom(string:strip(hd(Tup),both,$ )),
%% 	Tvalue=string:strip(lists:last(Tup),both,$ ),
%% %% 	io:format("Tvalue: ~p ~n", [Tvalue]),
%% 	Value= 
%% 	try
%% 		list_to_float(Tvalue) of
%% 		V ->V
%% 	catch _:_ ->
%% 			  try
%% 				  list_to_integer(Tvalue) of
%% 				  V1 ->V1
%% 			  catch _:_ ->
%% 						0
%% 			  end
%% 	end,
%% 	case Ret of
%% 		[] ->
%% 			TRet=[{Key,Value}];
%% 		_->
%% 			TRet=Ret++[{Key,Value}]
%% 	end,
%% getTuplelist(E,TRet).	
%% 
%% %% @spec getMeasurement(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% getMeasurement(State)->
%% 	Liststate=string:tokens(State, ">"),
%% 	THIS:getTuplelist(Liststate,[]).

%% @spec countLogSize(X) -> ok 
%% Obj = term()
%% @doc
countLogSize([], Count) ->
	Count;
countLogSize([H|T], Count) ->
	case H of
		{eof, ChildData} ->
			{eof, ChildData} = H,
			countLogSize(T, Count + erlang:length(ChildData));
		_->
		countLogSize(T, Count)
	end.

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

%% @spec countLogSize(X) -> ok 
%% Obj = term()
%% @doc 
processNew(CollectorList, Data, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc, DstrNeed) ->	
	I = THIS:countLogSize(Data, 0),	
	MonitorMap = THIS:process_sub_3(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision, I),	
	F = fun(X)->		
		case X of
			{eof, ChildData} ->
				F1 = fun(X1)->
					case is_list(X1) of
						 true ->
							ok;							
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
%% 	{MonitorMap, ErrorsList, WarningList, GoodList}.
	MonitorMap.

%% @spec process(X) -> ok 
%% Obj = term()
%% @doc  
process(CollectorList, Data, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc, DstrNeed) ->
%% 	io:format("process: ~p ~n", ["Start"]),
	MonitorMap = THIS:process_sub_1(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision),
	
	F = fun(X)->		
%% 			F2 = fun(X2)->
				K2 = X#monitorlog.id,
%% 				DESC=X#monitorlog.desc,
%% 				M=siteview:get_object(K2),
%% 				M=report_proxy:get_object(K2),
%% 	    		Monitor=hd(M),
				Measurement=X#monitorlog.measurement,
				case dict:find(K2, MonitorMap) of
				{ok, Connectors}->
%% 					{Day, Time} = X#monitorlog.time,
%% 					case schedule_property:history_is_enabled(ScheduleMap, calendar:day_of_the_week(Day), sv_datetime:time(Time)) of
%% 						true ->														
%% 							THIS:get_list_data(X, ?MaxRecords, ?MaxRecords, ?MaxRecords),
%% 							THIS:get_all_list(X),
							F3 = fun(X3) ->		
%% 								case X3:getPropertyType() of
%% 									numeric->
										X3:add_2(X#monitorlog.time, Measurement, X#monitorlog.category, X#monitorlog.category, bestCaseCalc)
%% 									_->
%% 										ok
%% 								end
%% 								X3:add_2(X#monitorlog.time, Measurement, X#monitorlog.category, X#monitorlog.category, bestCaseCalc)
							end,
							lists:foreach(F3, Connectors);
%% 						_ ->
%% 							F4 = fun(X4) ->
%% 								X4:add_2(X#monitorlog.time, Measurement, filtered, filtered, bestCaseCalc)
%% 							end,
%% 							lists:foreach(F4, Connectors),														
%% 							ok
%% 						end;
					_->
						ok
				end
	end,	
	lists:foreach(F, Data),
	MonitorMap.	
%% %% 	I = THIS:countLogSize(Data, 0),	
%% %% 	MonitorMap = THIS:process_sub_3(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision, I),	
%% 	F = fun(X)->		
%% 		case X of
%% 			{eof, ChildData} ->
%% %% 				io:format("process ~p ~n", [ChildData]),
%% 				F1 = fun(X1)->
%% %% 					io:format("~p ~n", [X1]),
%% 					case is_list(X1) of
%% 						true ->
%% 							F2 = fun(X2)->
%% 								K2 = X2#monitorlog.id,
%% %% 								DESC=X2#monitorlog.desc,
%% %% 								io:format("Id:~p ~n", [K2]),
%% %% 								M=siteview:get_object(K2),
%% %% 								M=report_proxy:get_object(K2),
%% %%                                 Monitor=hd(M),
%% 								Measurement=X2#monitorlog.measurement,
%% 									  
%% 								case dict:find(K2, MonitorMap) of
%% 									{ok, Connectors}->
%% %% 										io:format(S, "~p ~n", [X2]),
%% %% 										F3 = fun(X3)->
%% 											case (((calendar:datetime_to_gregorian_seconds(X2#monitorlog.time) - calendar:datetime_to_gregorian_seconds(RequestedStartTime)) >= 0) and
%% 													((calendar:datetime_to_gregorian_seconds(RequestedEndTime) - calendar:datetime_to_gregorian_seconds(X2#monitorlog.time)) >= 0)) of
%% 												true ->
%% %% 													{Day, Time} = X2#monitorlog.time,
%% %% 													case schedule_property:history_is_enabled(ScheduleMap, calendar:day_of_the_week(Day), sv_datetime:time(Time)) of
%% %% 														true ->														
%% %% 															THIS:get_list_data(X2, ?MaxRecords, ?MaxRecords, ?MaxRecords),
%% 															case DstrNeed of
%% 																true->
%% %% 																	THIS:get_all_list(X2);
%% 																	ok;
%% 																_->
%% 																	ok
%% 															end,
%% 															F3 = fun(X3) ->
%% 																case X3:getPropertyType() of
%% 																	numeric->
%% 																		X3:add_2(X2#monitorlog.time, Measurement, X2#monitorlog.category, X2#monitorlog.category, bestCaseCalc);
%% 																	_->
%% 																		ok
%% 																end
%% 															end,
%% 															lists:foreach(F3, Connectors);
%% %% 														_ ->
%% %% 															F4 = fun(X4) ->
%% %% 																X4:add_2(X2#monitorlog.time, Measurement, filtered, filtered, bestCaseCalc)
%% %% 															end,
%% %% 															lists:foreach(F4, Connectors),															
%% %% 															ok
%% %% 													end;
%% 												_->
%% %% 											 		io:format("~p ~n",["ok"])
%% 													ok
%% 											end;							
%% %% 										end,
%% %% 										lists:foreach(F3, Connectors);
%% %% 										get_list_data([X2], ErrorsList, WarningList, GoodList);
%% 							 		_->
%% %% 										io:format("~p ~n", [K2])
%% 								 		ok
%% 								end
%% 							 end,
%% 							 lists:foreach(F2, X1);
%% 				 	 	 false ->
%% 							ok
%% 					end
%% 			    end,
%% %% 			    io:format("~p ~n", ["haha1111"]),
%% 			    lists:foreach(F1, [ChildData]);
%% 			_ ->
%% 			    io:format("~p ~n", ["No Data Log:"]),
%% 				io:format("~p ~n", [X])
%% 		end
%% 	end,
%% 	lists:foreach(F, Data),
%% %% 	{MonitorMap, ErrorsList, WarningList, GoodList}.
%% 	MonitorMap.

%% @spec processTrueData(X) -> ok 
%% Obj = term()
%% @doc  
processTrueDataNew(CollectorList, Data, Count, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc) ->
%% 	[{eof, ChildData1}] = Data,c
	io:format("processTrueData: ~p ~n", ["Start"]),
	MonitorMap = THIS:process_sub_2(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision, Count),
%% 	io:format("~p ~n", [MonitorMap]),
	
	F = fun(X)->
			{X:getProperty(),X:getPropertyType()}
	end,
	MeasurementType = lists:map(F, CollectorList),
	io:format("MeasurementType1:~p~n", [MeasurementType]),
	Result = report_statics:report(Data, MeasurementType, noCompress),
	io:format("Result:~p~n", [Result]),
	THIS:analysisStatData(MonitorMap, dict:fetch_keys(MonitorMap), Result),
	
	MonitorMap.

analysisMonitorStatData([], SummaryList)->
	done;
analysisMonitorStatData([H|T], SummaryList)->
	ok.

%% @spec getSummaryList(X) -> ok 
%% Obj = term()
%% @doc
analysisStatData(Map, [], StatsData) ->
	done;
analysisStatData(Map, [H|T], StatsData)->
	{ok, V} = dict:find(H, Map),
	Temp = THIS:analysisMonitorStatData(V, []),
	analysisStatData(Map, T, StatsData).

%% @spec processTrueData(X) -> ok 
%% Obj = term()
%% @doc  
processTrueData(CollectorList, Data, Count, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc) ->
%% 	[{eof, ChildData1}] = Data,c
%% 	io:format("processTrueData: ~p ~n", ["Start"]),
	MonitorMap = THIS:process_sub_2(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision, Count),
%% 	io:format("~p ~n", [MonitorMap]),
%% 	io:format("~p ~n", ["hahadddd"]),
	F = fun(X)->		
%% 			F2 = fun(X2)->
				K2 = X#monitorlog.id,
%% 				DESC=X#monitorlog.desc,
%% 				M=siteview:get_object(K2),
%% 				M=report_proxy:get_object(K2),
%% 	    		Monitor=hd(M),
				Measurement=X#monitorlog.measurement,
				case dict:find(K2, MonitorMap) of
				{ok, Connectors}->
%% 					{Day, Time} = X#monitorlog.time,
%% 					case schedule_property:history_is_enabled(ScheduleMap, calendar:day_of_the_week(Day), sv_datetime:time(Time)) of
%% 						true ->														
%% 							THIS:get_list_data(X, ?MaxRecords, ?MaxRecords, ?MaxRecords),
%% 							THIS:get_all_list(X),
							F3 = fun(X3) ->		
%% 								case X3:getPropertyType() of
%% 									numeric->
										X3:add_2(X#monitorlog.time, Measurement, X#monitorlog.category, X#monitorlog.category, bestCaseCalc)
%% 									_->
%% 										ok
%% 								end
%% 								X3:add_2(X#monitorlog.time, Measurement, X#monitorlog.category, X#monitorlog.category, bestCaseCalc)
							end,
							lists:foreach(F3, Connectors);
%% 						_ ->
%% 							F4 = fun(X4) ->
%% 								X4:add_2(X#monitorlog.time, Measurement, filtered, filtered, bestCaseCalc)
%% 							end,
%% 							lists:foreach(F4, Connectors),														
%% 							ok
%% 						end;
					_->
						ok
				end
	end,	
	lists:foreach(F, Data),
	MonitorMap.

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
	io:format("createReport1: ~p ~n", [MonitorStr]),
%% 	io:format("monitor_logger start: ~p ~n", [calendar:local_time()]),
	case Truedata of
		true->
%% 			{_, AllData} = monitor_logger:qc(MonitorStr, Count);
			{ok,{_, DataSrc}} = THIS:get_attribute(data),
			case DataSrc of
				[]->					
%% 					{_, AllData} = monitor_logger:qc(MonitorStr, Count, DaysLimit);
%% 					{_, AllData} = report_proxy:qc(MonitorStr, Count, THIS:getDayLimits());
					AllData = DataSrc;
				_->
					AllData = DataSrc
			end;
		_->
%% 		{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 				calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr)
			{ok,{_, DataSrc}} = THIS:get_attribute(data),
			case DataSrc of
				[]->
%% %% 					{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% %% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr);					
%% 					{_, AllData} = report_proxy:q(calendar:gregorian_seconds_to_datetime(StartTime), 
%% 					calendar:gregorian_seconds_to_datetime(EndTime), MonitorStr);
					AllData = DataSrc;
				_->
					AllData = DataSrc
			end
	end,	
%% 	io:format("monitor_logger end: ~p ~n", [calendar:local_time()]),
%% 	io:format("monitor_logger end11: ~p ~n", [AllData]),

%% 	io:format("process start: ~p ~n", [calendar:local_time()]),
	io:format("createReport2: ~p ~n", [calendar:local_time()]),
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
		Map = THIS:processTrueData(Collectors, Data1, Count1, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, scheduleMap, bestCaseCalc);	
		_->
%% 		io:format("process AllData: ~p ~n", [AllData]),
%% 		I = countLogSize(AllData, 0),
		I = length(AllData),
%% 		io:format("process countLogSize: ~p ~n", [I]),
		case I < 200 of
			true ->
				case I =/= 0 of
					true->
					Freq =  (EndTime - StartTime) / I,
%% 					io:format("process Freq1: ~p ~n", [Freq]),
					THIS:set_property(precision, Freq);
					_->
						ok
				end;
			_->
				Freq =  trunc(I/200) * ((EndTime - StartTime)/I),
%% 				io:format("process Freq2: ~p ~n", [Freq]),
				THIS:set_property(precision, Freq)
		end,
		{ok,{_, Precision}} = THIS:get_property(precision),
%% 		io:format("Precision:~p ~n", [Precision]),
		Map = THIS:process(Collectors, AllData, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, scheduleMap, bestCaseCalc, DstrNeed)	
	end,
%% 	io:format("process end: ~p ~n", [calendar:local_time()]),
%% 	{ok,{_, Title}} = THIS:get_property(title),
%% 	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),
	io:format("createReport3: ~p ~n", [calendar:local_time()]),	
%% 	{_, Consold} = THIS:count_Con(Map, dict:fetch_keys(Map), [], 0),
	
%% 	Cons = THIS:filterDataWithStatus(Consold, "all", []),
%% 	io:format("SummaryList Start: ~p ~n",  [calendar:local_time()]),	
	Summary1 = THIS:getSummaryList(Map, dict:fetch_keys(Map), []),
	
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	Summary2 = THIS:get_uptimeSummary(Map, Collectors, '', []),
%% 	Summary2 = [],
	io:format("SummaryList End: ~p ~n",  [calendar:local_time()]),
	case DstrNeed of
		true ->
%% 			{ok,{_, List}} = THIS:get_attribute(alllist),
%% 			%% dstrStatusNoNeed = null,ok,disable,bad
%% 			DstrList = THIS:getDstrList(List, dict:fetch_keys(Map), []),
%% 			Summary1 ++ Summary2 ++ DstrList;
			ok;
		_->
			Summary1 ++ Summary2
	end.
	
		
	

%% @spec getMonitorSummaryList(X) -> ok 
%% Obj = term()
%% @doc
getMonitorSummaryList([], I, SummaryList) ->
	SummaryList;  
getMonitorSummaryList([H|T], I, SummaryList) ->	
	Detail = buildbuckets([], H:getBuckets(), 1),		
%% 	Monitor = H:getMonitor(),
%% 	Props = Monitor:getPrimaryStateProperties(Monitor),
%% 	io:format("getMonitorSummaryList1: ~p ~n", [Props]),
	io:format("getMonitorSummaryList2: ~p ~n", [H:getProperty()]),
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
	getMonitorDstrList(T, DstrList ++ [{H#monitorlog.time,{H#monitorlog.category, H#monitorlog.desc, THIS:measurementtoString(H#monitorlog.measurement, [])}}]).

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
buildbuckets(RetData,[H|E],I)->	
	Bucket=H,
	{_, {{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_}, {connector, SampleCollector}}} = Bucket,
	Time=SampleCollector:getBucketStartTime(I),
	Value=SampleCollector:getBucketData(Bucket, getAverage),
	TRetData=RetData++[{calendar:gregorian_seconds_to_datetime(Time),Value}],
     buildbuckets(TRetData,E,I+1);
buildbuckets(RetData,[],_)->
	RetData.

%% @spec getThresholdList(X) -> ok 
%% Obj = term()
%% @doc  
%% getThresholdList([], Ret) ->
%% 	Ret;
%% getThresholdList([H|T], Ret) ->
%% 	L = api_monitor:get_all_classifier(list_to_atom(H)),
%% 	[M] = api_monitor:find_object(list_to_atom(H)),
%% 	{ok,{_,Name}} = M:get_property(name),
%% 	getThresholdList(T, Ret ++ [{Name, L}]).

%% @spec createThresholdSummary(X) -> ok 
%% Obj = term()
%% @doc  
%% createThresholdSummary() ->
%% 	{ok,{_, T}} = THIS:get_property(monitors),
%% 	MonitorStr = string:tokens(T, ","),
%% 	getThresholdList(MonitorStr, []).

%% @spec getThresholdSummary(X) -> ok 
%% Obj = term()
%% @doc  
%% getThresholdSummary()->
%% 	ThresholdInfo = THIS:createThresholdSummary(),
%% 	F = fun(X)->
%% 		{ID, [{error, Error}, {warning, Warning}, {good, Good}]} = X		
%% 	end,
%% 	lists:foreach(F, ThresholdInfo),	
%% 	ok.

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


%% %% @spec threshold2Str(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% threshold2Str(T) ->
%% 	{Con, X, Value} = T,
%% 	case (X == '') or (Value == '') of
%% 		true ->
%% 			Restult = "";
%% 		_ ->
%% 			 Restult = io_lib:format("~s", [to_list(Con)++to_list(X)++to_list(Value)])
%% 	end,
%% %% 	io:format("~p ~n", [Restult]),
%% 	Restult.
%% 
%% %% @spec isthresholdNull(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% isthresholdNull(T) ->
%% 	{Con, X, Value} = T,
%% 	case (X == '') or (Value == '') of
%% 		true ->
%% 			true;
%% 		_ ->
%% 			 false
%% 	end.
 
%% @spec getAlertLogList(X) -> ok 
%% Obj = term()
%% @doc  
%% getAlertLogList([], StartDate, EndDate, Ret) ->
%% 	Ret;
%% getAlertLogList([H|T], StartDate, EndDate, Ret) ->
%% %%  io:format("~p ~n", H),
%% 	F = fun(X)->
%% 			AId = proplists:get_value(id,X),
%% 			AId
%% 	end,
%% 	[Id|_] = lists:map(F,H),
%% 	io:format("~p ~n", [Id]),
%% 	io:format("~p ~n", [StartDate]),
%% 	Logs = api_alert:get_log(StartDate),
%% 	io:format("~p ~n", [Logs]),
%% 	getAlertLogList(T, StartDate, EndDate, Ret ++ [Logs]).	


%% @spec getMonitorAlertList(X) -> ok 
%% Obj = term()
%% @doc  
%% getMonitorAlertList([], Ret) ->
%% 	Ret;
%% getMonitorAlertList([H|T], Ret) ->	
%% 	RuleIds = dbcs_rule:get_monitor_rule(list_to_atom(H)),	
%% %% 	io:format("~p ~n", [RuleIds]),
%% 	getMonitorAlertList(T, Ret ++ [RuleIds]).

%% @spec gatherAlertInfo(X) -> ok 
%% Obj = term()
%% @doc  
%% gatherAlertInfo(StartDate, EndDate, ScheduleMap) ->
%% 	{ok,{_, T}} = THIS:get_property(monitors),	
%% 	MonitorStr = string:tokens(T, ","),
%% 	RuleLists = getMonitorAlertList(MonitorStr, []),
%% 	getAlertLogList(RuleLists, StartDate, EndDate, []).


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

  
%% %% @spec filterDataWithStatus(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% filterDataWithStatus([], StatuFilter, Cons) ->
%% 	Cons;
%% filterDataWithStatus([H|T], StatuFilter, Cons) ->
%% %% 	io:format("~p ~n",[StatuFilter]),
%% %% 	io:format("~p ~n",[H:getLastCategory()]),
%% 	case StatuFilter of
%% 		"all" ->			
%% 			filterDataWithStatus(T, StatuFilter, Cons++[H]);
%% 		"error" ->
%% 			case H:getLastCategory() of
%% 				error ->
%% 				filterDataWithStatus(T, StatuFilter, Cons++[H]);
%% 				_->
%% 				filterDataWithStatus(T, StatuFilter, Cons)
%% 			end;
%% 		"warning" ->
%% 			case H:getLastCategory() of
%% 				warning ->
%% 				filterDataWithStatus(T, StatuFilter, Cons++[H]);
%% 				_->
%% 				filterDataWithStatus(T, StatuFilter, Cons)
%% 			end;
%% 		"good" ->
%% 			case H:getLastCategory() of
%% 				good ->
%% 				filterDataWithStatus(T, StatuFilter, Cons++[H]);
%% 				_->
%% 				filterDataWithStatus(T, StatuFilter, Cons)
%% 			end;
%% 		"error,warning" ->
%% 			case H:getLastCategory() of
%% 				error ->
%% 				filterDataWithStatus(T, StatuFilter, Cons++[H]);
%% 				warning ->
%% 				filterDataWithStatus(T, StatuFilter, Cons++[H]);					
%% 				_->
%% 				filterDataWithStatus(T, StatuFilter, Cons)
%% 			end;		
%% 		_->
%% 			filterDataWithStatus(T, StatuFilter, Cons++[H])
%% 	end.
%% 
%% %% @spec unconsult(X) -> ok 
%% %% Obj = term()
%% %% @doc  
%% unconsult(File, L) ->
%% 	{ok, S} = file:open(File, write),
%% 	lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
%% 	file:close(S).

