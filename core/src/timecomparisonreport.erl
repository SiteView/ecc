%% Author: Administrator
%% Created: 2009-12-31
%% Description: TODO: Add description to timecomparisonreport
-module(timecomparisonreport,[BASE]).
-extends(siteview_object).
-compile(export_all).
-include("monitor.hrl").
-include("ecc_oem.hrl").
-include("monitor_template.hrl").

%% -export([createHistoryReportObject/1, init/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for timecomparisonreport
new()->
	Obj = siteview_object:new(),

	Obj:set_attribute(collectors,[]),
	Obj:set_attribute(collectors1,[]),
	Obj:set_attribute(errorlist,[]),
	Obj:set_attribute(goodlist,[]),	
	Obj:set_attribute(warninglist,[]),
%% 	Obj:set_attribute(nodataColor,""),
	
	Obj:set_property(monitors, ""),
	Obj:set_property(format, ""),
	Obj:set_property(startDay, "today"),
	Obj:set_property(title, "testtimecomparisonreport"),
	Obj:set_property(schedule, "repeat"),
	
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
readFromHashMap(X) ->
	F = fun(I)->
		{Key, Value} = I,
		THIS:set_property(Key, Value)
	end,
	lists:foreach(F, X),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
isDisabled()->
	{ok,{_, Disabled}} = THIS:get_property(disabled),
	Disabled.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
schedule()->
%% 	L = 0,
	ReoportSch = case siteview:get_report_scheduler() of
		{ok,Sche}->
				Sche;
		_->
			{error,no_found_scheduler}
		end,
	Action = generatehistoryreport:new(THIS),
	{ok,{_, PSchedule}}= THIS:get_property(schedule),
	THIS:set_attribute(action, Action),
	ReoportSch:scheduleRepeatedAction(Action, PSchedule, sv_datetime:now()),
%% 	ReoportSch:testreportPeriodicAction(Action,2*60*1000),	
%% 	ReoportSch:scheduleAction(Action, PSchedule, L, jj),
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
unschedule()->
	{ok,{_, Action}} = THIS:get_property(action),
	ReoportSch = case siteview:get_report_scheduler() of
		{ok,Sche}->
				Sche;
		_->
			{error,no_found_scheduler}
		end,

	ReoportSch:unschedule(Action),
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
initialize()->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	T1 = [],	
	THIS:set_attribute(collectors, T1),	
	THIS:set_attribute(collectors1, []),	
	THIS:initializeSampleCollectors(),		
	THIS:initializeTimeParameters(),
	THIS:initializeFilePaths(),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
createFromQuery(Query)->
	THIS:initialize(),
	
%% 	S1 = THIS:get_property(reportPath) + ".html",
	{ok,{_, P}} = THIS:get_property(reportPath),
%% 	case file:open(P ++ ".html", write) of
	case file:open(P, write) of
		{ok, S} ->
			THIS:createReport(S);
%% 			file:close(S);
		_->
			io:format("eror   title    file: ~p ~n", [P])
	end.
	
%% 	case THIS:get_property(isadhoc) of
%% 		{error,{_, _}} ->
%% 			{ok,{_, ReportDirectory}} = THIS:get_property(reportDirectory),
%% 			historyreport_static:generateIndexPage(THIS, P, ReportDirectory, flag, s2);			
%% 		_->
%% 			continue
%% 	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
addCollector(CollectorMap, Monitor, Property, KeepSampleBuffer)->
	{ok,{id, FullId}} = Monitor:get_property(id),
%% 	{ok,{_, Name}} = Property:get_property(name),
	Name = Property,
%% 	Key = {FullId} + atom_to_list(Name),
%% 	Key,	
	Collector = samplecollector:new(Monitor, Property, false, false, KeepSampleBuffer),	
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	T1 = Collectors ++ [Collector],
	
	THIS:set_attribute(collectors, T1),
	ok.
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
addCollector1(CollectorMap, Monitor, Property, KeepSampleBuffer)->
	{ok,{id, FullId}} = Monitor:get_property(id),
%% 	{ok,{_, Name}} = Property:get_property(name),
	Name = Property,
%% 	Key = {FullId} + atom_to_list(Name),
%% 	Key,	
	Collector = samplecollector:new(Monitor, Property, false, false, KeepSampleBuffer),	
	{ok,{_, Collectors}} = THIS:get_attribute(collectors1),
	T1 = Collectors ++ [Collector],
	
	THIS:set_attribute(collectors1, T1),
	ok.
%% 	String key = monitor.getFullId() + " " + property.getName(); 
%%     if (collectorMap.get(key) == null) {
%%        SampleCollector collector = new SampleCollector((Monitor)monitor.toSiteViewObject(), property, keepSampleBuffer, this.reportObjectConfig.isWarningNotIncluded(), this.reportObjectConfig.isFailureNotIncluded());
%%  
%%        collectorMap.put(key, collector);
%%        this.collectors.add(collector);

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
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
%% @doc read report info from contentstore 
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
%% @doc read report info from contentstore 
initializeSampleCollectors()->
	{ok,{_, T}} = THIS:get_property(monitors),
	MonitorStr = string:tokens(T, ","),
	
	AllMonitors = api_monitor:get_all_monitors(),
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
			Props = Monitor:getLogProperties(Monitor),	
			F1 = fun(X1)->
						THIS:addCollector(collectorMap, Monitor, X1, false),
				        THIS:addCollector1(collectorMap1, Monitor, X1, false)
				
			end,
			lists:foreach(F1, Props)
	end,
	lists:foreach(F, dict:fetch_keys(Monitors)),
	ok.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% private %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
get_list_data(H, MaxError, MaxGood, MaxWarning) ->
	case H#monitorlog.category of
		good ->
			{ok,{_, List}} = THIS:get_attribute(goodlist),
			case length(List) < MaxGood of
				true ->					
					T1 = List ++ [H],
					THIS:set_attribute(goodlist, T1);
				 _->
					error
			end;
		error ->
			{ok,{_, List}} = THIS:get_attribute(errorlist),
			case length(List) < MaxError of
				true ->					
					T1 = List ++ [H],	
					THIS:set_attribute(errorlist, T1);
				 _->
					error			
			end;
		warning ->
			{ok,{_, List}} = THIS:get_attribute(warninglist),
			case length(List) < MaxWarning of
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
%% @doc read report info from contentstore 
process_sub_1([], Ret, RequestedStartTime, RequestedEndTime, Precision) ->
	Ret;
process_sub_1([H|T], Ret, RequestedStartTime, RequestedEndTime, Precision) ->
	L = calendar:datetime_to_gregorian_seconds(RequestedStartTime),
	L1 = calendar:datetime_to_gregorian_seconds(RequestedEndTime),
 	H:createBuckets(L, L1, Precision),
	Monitor = H:getMonitor(),
	{ok,{_, ID}} = Monitor:get_property(id),
	Key = ID,
	process_sub_1(T, dict:append(Key, H, Ret), RequestedStartTime, RequestedEndTime, Precision).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
process(S, CollectorList, Data, RequestedStartTime, RequestedEndTime, TimezoneOffset, Precision, ErrorsList, WarningList, GoodList, MaxErrors, MaxWarnings, MaxGoods, ScheduleMap, BestCaseCalc) ->
	MonitorMap = THIS:process_sub_1(CollectorList, dict:new(), RequestedStartTime, RequestedEndTime, Precision),
	io:format("~p ~n", ["haha"]),
	F = fun(X)->
		case X of
			{eof, ChildData} ->
				F1 = fun(X1)->
					case is_list(X1) of
						true ->
							F2 = fun(X2)->
								K2 = X2#monitorlog.id,
								case dict:find(K2, MonitorMap) of
									{ok, Connectors}->
											case (((calendar:datetime_to_gregorian_seconds(X2#monitorlog.time) - calendar:datetime_to_gregorian_seconds(RequestedStartTime)) > 0) and
													((calendar:datetime_to_gregorian_seconds(RequestedEndTime) - calendar:datetime_to_gregorian_seconds(X2#monitorlog.time)) > 0)) of
												true ->
													{Day, Time} = X2#monitorlog.time,
													case schedule_property:history_is_enabled(ScheduleMap, calendar:day_of_the_week(Day), sv_datetime:time(Time)) of
														true ->														
%% 															THIS:get_list_data(X2, 100, 100, 100),
															F3 = fun(X3) ->					
																X3:add_2(X2#monitorlog.time, X2#monitorlog.measurement, X2#monitorlog.category, X2#monitorlog.category, bestCaseCalc)
															end,
															lists:foreach(F3, Connectors);
														_ ->
															F4 = fun(X4) ->
																X4:add_2(X2#monitorlog.time, X2#monitorlog.measurement, filtered, filtered, bestCaseCalc)
															end,
															lists:foreach(F4, Connectors),															
															ok
													end;
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

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
count_Con(Map, [], Cons, I) ->
	{I, Cons};	
count_Con(Map, [H|T], Cons, I) ->
	{ok, V} = dict:find(H, Map),	
	count_Con(Map, T, Cons ++ V, I + length(V)).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
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
%% 	["   <A HREF='#historyTable'>historyTable</A><BR/>"] ++ 
	["  </TD>
	  <TD ALIGN=CENTER WIDTH=34%>"] ++ 
	["   <A HREF='javascript:history.go(-1)'>"++gettext:key2str("Time Comparison Report Index","zh-cn")++"</A>"] ++ 
	["  </TD>
	  <TD ALIGN=RIGHT WIDTH=33%>
	   &nbsp;
	  </TD>
	 </TR>
	</TABLE><p/>"],
	io:format(S, "~s ~n", [lists:flatten(LL)]),	
%% 	{eof, AllData} = monitor_logger:q(""),
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	{ok,{_, Collectors1}} = THIS:get_attribute(collectors1),
	{ok,{_, Precision}} = THIS:get_property(precision),
	io:format("Precision:~p ~n", [Precision]),
	{ok,{_, StartTime}} = THIS:get_property(startTime),
	{ok,{_, StartTime1}} = THIS:get_property(startTime1),
%% 	io:format("~p ~n", [StartTime]),
 	io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(StartTime)]),
	io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(StartTime1)]),
	{ok,{_, EndTime}} = THIS:get_property(endTime),
	{ok,{_, EndTime1}} = THIS:get_property(endTime1),
	io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(EndTime)]),
    io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(EndTime1)]),
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
	
%% 	{ok,{_, StatusFilter}} = THIS:get_property(statusFilter),
	{ok,{_, T}} = THIS:get_property(monitors),
	MonitorStr = string:tokens(T, ","),	
	
	{_, AllData} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime), 
			calendar:gregorian_seconds_to_datetime(EndTime),MonitorStr),
	
%% 	io:format(S, "~p ~n", [AllData]),
	Map = THIS:process(S, Collectors, AllData, calendar:gregorian_seconds_to_datetime(StartTime), calendar:gregorian_seconds_to_datetime(EndTime), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, ScheduleMap, bestCaseCalc),	
 
	io:format("process:~p ~n", ["Hahdddda"]),
	
	{_, AllData1} = monitor_logger:q(calendar:gregorian_seconds_to_datetime(StartTime1), 
			calendar:gregorian_seconds_to_datetime(EndTime1), MonitorStr),
	
%% 	io:format(S, "~p ~n", [AllData]),
	Map1 = THIS:process(S, Collectors1, AllData1, calendar:gregorian_seconds_to_datetime(StartTime1), calendar:gregorian_seconds_to_datetime(EndTime1), 
				  timezoneOffset, Precision, [], [], [], maxErrors, maxWarnings, maxGoods, ScheduleMap, bestCaseCalc),	
	{ok,{_, Title}} = THIS:get_property(title),
	{ok,{_, ReportPeriod}} = THIS:get_property(reportPeriod),	
 	L = [Title, ReportPeriod],	
 	io:format("~p ~n", ["Hahdddda"]),
%% 	
	{ok,{_, Disabled}} = THIS:get_property(disabled),
	
	
%% 	io:format("~p ~n",[L]),
	X = ["<TABLE ALIGN=CENTER BORDER=0>" , "<TR><TD><H2 ALIGN=CENTER> " , Title , 
		 "</H2><TD></TR>" , "<TR><TD>" , "<P ALIGN=CENTER>" ,  "<BR/><FONT=-1>"++gettext:key2str("Filter data By Schedule","zh-cn")++":",  
		 SchedName, "</FONT>", "<BR/>", "</P></TD></TR>" , "<TR><TD><P ALIGN=CENTER><B>" , 
		 Des, "</B></P></TD></TR></TABLE>"],
%% 	io:format("~p ~n", ["Description"]),	
	io:format(S, "~s ~n", [lists:flatten([X])]),
	{_, Consold} = THIS:count_Con(Map, dict:fetch_keys(Map), [], 0),
	io:format("Consold:~p ~n", [time()]),
	{_, Consold1} = THIS:count_Con(Map1, dict:fetch_keys(Map1), [], 0),
	io:format("Consold1:~p ~n", [time()]),
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
	
	Cons = THIS:filterDataWithStatus(Consold, "all", []),
	
	Cons1 = THIS:filterDataWithStatus(Consold1, "all", []),
	io:format("Cons:~p ~n", [time()]),
	ZCons= [{X,Y} ||X<-Cons,Y<-Cons1,X:getProperty()=:=Y:getProperty()],
	
 	io:format("ZCons:~p ~n", [time()]),
%% 	THIS:printImage(S, Cons),		
	F=fun(X)->
			E1= element(1, X),
			E2= element(2, X),
			Conlist=[E1]++[E2],
			Ttitle= E1:getPropertyLabel(),
			case is_atom(Ttitle) of
				true ->
					TTitle=atom_to_list(Ttitle);
				false ->
					TTitle=Ttitle
			end,
	        THIS:hTMLSummaryTable(S, Conlist,TTitle),
			THIS:printImage(S,Conlist)
			end,
	lists:foreach(F, ZCons),
%%  	THIS:hTMLSummaryTable(S, Cons),
%%   	THIS:hTMLSummaryTable(S, Cons1),
	
	THIS:addLogo(S),	
	io:format(S, "~s ~n", ["</body>"]),	
	io:format(S, "~s ~n", ["</html>"]),
	io:format("~p ~n", ["createTimeComparisonReport ok"]),
	file:close(S),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
printImage(File,Map)->
%% 	GraphCollectors=THIS:getCollectorList(Map,dict:fetch_keys(Map)),
	GraphCollectors=Map,
%% HistoryReportHTML,Files,ConCollectorList,[H|E],CollectorMap,SimilarProperties,MultipleMonitors,Imgcount
   
	io:format(File, "~s ~n", ["<center>"]),	
	io:format(File, "~s ~n", ["<table>"]),
	RR=bulidImage(File,Map),
%%    RR=buildimageBarGraph([],File,GraphCollectors,1),
	io:format(File, "~s ~n", ["</center>"]),	
	io:format(File, "~s ~n", ["</table>"]).
getfileID() ->
     % 63113904000 = calendar:datetime_to_gregorian_seconds({{2000, 1, 1},{0, 0, 0}})
     TimeSpan = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 63113904000,
     {X, Y, Z} = erlang:now(),
     MicroSecs = Z,
     random:seed(X, Y, Z),
     Random = random:uniform(9),
     lists:flatten(io_lib:format("~9..0w~6..0w~1..0w.png",[TimeSpan, MicroSecs, Random])).
bulidImage(Files,Collectors)->
	{_,{_,ReportDirectory}}=THIS:get_property(reportDirectory),
%% 	io:format("ReportDirectory:~p ~n", [ReportDirectory]),
	{_,{_,Filename}}=THIS:get_property(filename),
%% 	io:format("Filename:~p ~n", [Filename]),
%% 	 {TH,TM,TS}=time(),
	{A, B, C} = erlang:now(),
    random:seed(A, B, C),
    Rand = random:uniform(1000),
	PathFile=ReportDirectory++Filename++integer_to_list(Rand)++".png",
%% 	io:format("Filename:~p ~n", ["1111111111111111"]),
	PathF=Filename++integer_to_list(Rand)++".png",
	[Samplecol1,Samplecol2]=Collectors,
	{_,{_,ComparisonType}}=THIS:get_property(comparisonType),
	Ttitle= Samplecol1:getPropertyLabel(),
			case is_atom(Ttitle) of
				true ->
					Title=atom_to_list(Ttitle);
				false ->
					Title=Ttitle
			end,
%% 	io:format("Filename:~p ~n", ["122222222222222222"]),
	Max1=Samplecol1:getMaximum(),
	Max2=Samplecol2:getMaximum(),
	Min1=Samplecol1:getMinimum(),
	Min2=Samplecol2:getMinimum(),
	Avg1=Samplecol1:getAverage(),
	Avg2=Samplecol2:getAverage(),
	Last1=Samplecol1:getLastValue(),
	Last2=Samplecol2:getLastValue(),
 	io:format("Last1:~p ~n", [Max1]),
	[{_,Data1}] = ets:lookup(Samplecol1:get_tid(), buckets),
	[{_,Data2}] = ets:lookup(Samplecol2:get_tid(), buckets),
 	io:format("Filename:~p ~n", [Max2]),
 	SubTitle="Max:"++ to_list(Max1)++"/"++to_list(Max2)
          ++"Average:"++to_list(Avg1)++"/"++to_list(Avg2) 
           ++"Min:"++ to_list(Min1)++"/"++to_list(Min2),
	io:format("SubTitle:~p ~n", [SubTitle]),
%% 	{x_data_type,datetime}
   Options=[{width, 900}, {height, 350}, {threshold, 0}]
            ++[{margin, 60}, {title1, Title}, {title2, SubTitle},{input_data, [""]}]
            ++[{save_file, PathFile},{contrast_type,list_to_atom(ComparisonType)}]
            ++[{plot, plot2d}, {run_function, png}],
	io:format("builddata:~p ~n", [time()]),
	Bucketdatas1=buildbuckets([],Data1,1),
	io:format("builddata1:~p ~n", [time()]),
	Bucketdatas2=buildbuckets([],Data2,1),
	io:format("builddata2:~p ~n", [time()]),
    io:format(Files, "~s ~n", ["<tr>"]),
	io:format(Files, "~s ~n", ["<td>"]),
	io:format(Files, "~s ~n", ["<img src="++PathF++" />"]),
	io:format(Files, "~s ~n", ["</tr>"]),
	io:format(Files, "~s ~n", ["</td>"]),
	io:format("PathF:~p ~n", ["PathF"]),
	{ok,{_, StartTime}} = THIS:get_property(startTime),
	{ok,{_, StartTime1}} = THIS:get_property(startTime1),
%% 	io:format("~p ~n", [StartTime]),
	{{Ys,Ms,Ds},{HHs,MMs,SSs}}=calendar:gregorian_seconds_to_datetime(StartTime),
	{{Ys1,Ms1,Ds1},{HHs1,MMs1,SSs1}}=calendar:gregorian_seconds_to_datetime(StartTime1),
	{ok,{_, EndTime}} = THIS:get_property(endTime),
	{ok,{_, EndTime1}} = THIS:get_property(endTime1),
	{{Ye,Me,De},{HHe,MMe,SSe}}=calendar:gregorian_seconds_to_datetime(EndTime),
	{{Ye1,Me1,De1},{HHe1,MMe1,SSe1}}=calendar:gregorian_seconds_to_datetime(EndTime1),
	Column1=integer_to_list(Ys)++"-"++integer_to_list(Ms)++"-"++integer_to_list(Ds)++" "
          ++integer_to_list(HHs)++":"++integer_to_list(MMs)++":"++integer_to_list(SSs)++"~"
          ++integer_to_list(Ye)++"-"++integer_to_list(Me)++"-"++integer_to_list(De)++" "
          ++integer_to_list(HHe)++":"++integer_to_list(MMe)++":"++integer_to_list(SSe),
	Column2=integer_to_list(Ys1)++"-"++integer_to_list(Ms1)++"-"++integer_to_list(Ds1)++" "
          ++integer_to_list(HHs1)++":"++integer_to_list(MMs1)++":"++integer_to_list(SSs1)++"~"
          ++integer_to_list(Ye1)++"-"++integer_to_list(Me1)++"-"++integer_to_list(De1)++" "
          ++integer_to_list(HHe1)++":"++integer_to_list(MMe1)++":"++integer_to_list(SSe1),
%%  Data=[{Title,TMData}],
	Data=[{Column1,Bucketdatas1},{Column2,Bucketdatas2}],
	io:format("Data:~p ~n", ["data"]),
   eplot_main:getimage(Data,Options),
	io:format("image:~p ~n", [time()]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
buildbuckets(RetData,[H|E],I)->	
	Bucket=H,
%% 	io:format("Bucket:~p ~n", [Bucket]),
	{_, {{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_}, {connector, SampleCollector}}} = Bucket,
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
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
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
%% @doc read report info from contentstore 
hTMLTableHeader(Cons, S, StartColumn, Columns, Index, Count, BucketSize)->  
	case Index =/= 0 of
	true ->
		L1 = "</TABLE><br>",
		io:format(S, "~s ~n", [lists:flatten(L1)]);
		_->
		L1 = ["<A name=historyTable></A>"] ,
		io:format(S, "~s ~n", [lists:flatten(L1)])
	end,
	L = ["<P><TABLE class='table historytable' WIDTH=100% CELLSPACING=0>"] ++ ["<TR BGCOLOR=#88AA99><TH WIDTH=20%>Time</TH>"],
	io:format(S, "~s ~n", [lists:flatten(L)]),
	hTMLTableHeader_Child(Cons, S, StartColumn, Columns, Index, Count, BucketSize),
	io:format(S, "~s ~n", [lists:flatten(["</TH></TR>"])]).
    
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
get_Cons_Bucket_I([], BucketIndex, S, StartColumn, Columns, Count, Data) ->	
	Data;
get_Cons_Bucket_I([H|T], BucketIndex, S, StartColumn, Columns, Count, Data) ->
  case (Count >= StartColumn) and (Count < (StartColumn+Columns)) of 
	true->		
		case (Count == StartColumn) of
			true ->
				Head = ["<TR BGCOLOR=#DDDDDD>"] ++ ["<TD>"] ++ [sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(H:getBucketStartTime(BucketIndex)))] ++ ["</TD>"];
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
		get_Cons_Bucket_I(T, BucketIndex, S, StartColumn, Columns, Count+1, Data1);
	_->
		get_Cons_Bucket_I(T, BucketIndex, S, StartColumn, Columns, Count+1, Data)
   end.	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
print_Cons_Buckets(Cons, S, StartColumn, Columns, PageIndex, Index, BucketSize)
	when (Index > BucketSize)->
	ok;
print_Cons_Buckets(Cons, S, StartColumn, Columns, PageIndex, Index, BucketSize) ->
%% 	io:format("~p ~n", [Index]),
%% 	io:format("~p ~n", [PageIndex]),
	case (Index > PageIndex*50) and (Index =< (PageIndex+1)*50) of
	true ->
		Buckete_I = get_Cons_Bucket_I(Cons, Index, S, StartColumn, Columns, 1, []),
		io:format(S, "~s ~n", [lists:flatten(Buckete_I)]);
	_->
		continue
	end,
	print_Cons_Buckets(Cons, S, StartColumn, Columns, PageIndex, Index+1, BucketSize).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
hTMLTable(S, StartColumn, Columns, Cons, Index, BucketSize) when (Index*50) >= BucketSize ->
	ok;
hTMLTable(S, StartColumn, Columns, Cons, Index, BucketSize)->
	THIS:hTMLTableHeader(Cons, S, StartColumn, Columns, Index, 1, BucketSize),
	THIS:print_Cons_Buckets(Cons, S, StartColumn, Columns, Index, Index*50+1, BucketSize),
	case (Index+1)*50 > BucketSize of
	true->
%% 		End = ["</TABLE><br>"];
		io:format(S, "~s ~n", [lists:flatten(["</TABLE><br>"])]);
	_->
%% 		End = [""]
		continue
	end,	
	hTMLTable(S, StartColumn, Columns, Cons, Index+1, BucketSize).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
hTMLTable_Child(S, StartColumn, Columns, ColumnsLeft, Cons, BucketSize) when (ColumnsLeft =< 0) ->	
	ok;
hTMLTable_Child(S, StartColumn, Columns, ColumnsLeft, Cons, BucketSize)->	
	case ColumnsLeft < Columns of
		true->			
			THIS:hTMLTable(S, StartColumn, ColumnsLeft, Cons, 0, BucketSize),
			hTMLTable_Child(S, StartColumn + ColumnsLeft, ColumnsLeft, ColumnsLeft - Columns, Cons, BucketSize);
		_->
			THIS:hTMLTable(S, StartColumn, Columns, Cons, 0, BucketSize),
			hTMLTable_Child(S, StartColumn + Columns, Columns, ColumnsLeft - Columns, Cons, BucketSize)
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
hTMLTables(S, Cons, ConSize)->
	case ConSize > 0 of
		true->
			io:format(S, "~s ~n", [lists:flatten("<CENTER>")]),
			BucketSize = (lists:nth(1, Cons)):getBucketCount(),	
			hTMLTable_Child(S, 1, 5, ConSize, Cons, BucketSize),
			io:format(S, "~s ~n", [lists:flatten("</TABLE><br><br></CENTER>")]);
		_->
			ok
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
calculateUpTime(UpTime, WarningTime, ErrorTime, TotalTime, Precision) ->
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
	end,	
	{UpTimePercentage, WarningTimePercentage, ErrorTimePercentage, Precision}.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
print_uptimeSummary([], S, Id) ->
	ok;
print_uptimeSummary([H|T], S, Id) ->
	case (H:getMonitorFullID() =/= Id) or (Id == '') of
		true ->
%%			X1:getLastTime()
%% 			X1:getNaTime()
			{UpTimePercentage, WarningTimePercentage, ErrorTimePercentage, _} = THIS:calculateUpTime(H:getGoodTime(), H:getWarningTime(), H:getErrorTime(), H:getTotalTime(), precision),
			L = ["<tr BGCOLOR=#DDDDDD>", "<td>"] ++ to_list(H:getMonitorName()) ++ ["</td>", "<td>"] ++ to_list(UpTimePercentage) ++ ["</td>" ,"<td>"] ++ 
					to_list(ErrorTimePercentage) ++ ["</td>" , "<td>"] ++ to_list(WarningTimePercentage) ++ ["</td>" , "<td>"] ++ to_list(H:getLastCategory()) ++ ["</td>", "</tr>"],
			io:format(S, "~s ~n", [lists:flatten(L)]),
			print_uptimeSummary(T, S, H:getMonitorFullID());
		_ ->
			print_uptimeSummary(T, S, Id)
	end.	

to_list(X) when is_atom(X)->
	atom_to_list(X);
to_list(X) when is_float(X)->
	lists:flatten(io_lib:format("~.2f", [X]));
to_list(X) when is_integer(X)->	
    integer_to_list(X);
to_list(X) ->
  [X].


datetoString(Date)->
	case is_tuple(Date) of
		true ->			
			{{Y,M,D},{HH,MM,SS}}=Date,
			Ret=integer_to_list(Y)++"-"++integer_to_list(M)++"-"++integer_to_list(D)++" "++integer_to_list(HH)++":"
               ++integer_to_list(MM)++":"++ integer_to_list(SS);
		false ->
			Ret="no data"
	end,
	Ret.
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
hTMLSummaryTable(S , Map,Title)->	
%% 	io:format(S, "~s ~n", ["<P><CENTER><A NAME=uptimeSummary> </A>
%% 		<TABLE WIDTH=100% BORDER=1 CELLSPACING=0><CAPTION><B>uptimeSummary</B></CAPTION>
%% 		<TR BGCOLOR=#88AA99><TH>Name</TH><TH>Good Time %</TH><TH>Error Time %</TH><TH>Warning %</TH><TH>Last State</TH></TR>"]),
%% 	THIS:print_uptimeSummary(Map, S, ''),
%% 	
%% 	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]),
	io:format(S, "~s ~n", ["<P><CENTER><A NAME=readingsSummary> </A>
	<TABLE class='table historytable' WIDTH=100% CELLSPACING=0><CAPTION><B>"++Title++"</B></CAPTION>
	<TR BGCOLOR=#88AA99><TH>"++gettext:key2str("GroupName","zh-cn")++"</TH><TH>"++gettext:key2str("Measurement","zh-cn")++"</TH><TH>"++gettext:key2str("Max","zh-cn")++"</TH><TH>"++gettext:key2str("Average","zh-cn")++"</TH><TH>"++gettext:key2str("Last","zh-cn")++"</TH><TH>"++gettext:key2str("Max Time","zh-cn")++" </TH></TR>"]),	
	F4 = fun(X4)->
		L = ["<tr BGCOLOR=#DDDDDD>", "<td>"] ++ to_list(X4:get_parent_full_name())++":"++to_list(X4:getMonitorName()) ++ ["</td>", "<td>"] ++ to_list(X4:getPropertyLabel()) ++ ["</td>" ,"<td>"] ++ 
			to_list(X4:getMaximum()) ++ ["</td>" , "<td>"] ++ to_list(X4:getAverage()) ++ ["</td>" , "<td>"] ++ to_list(X4:getLastValue())++ ["</td>" , "<td>"]++datetoString(X4:getMaximumTime())  ++ ["</td>", "</tr>"],		
		io:format(S, "~s ~n", [lists:flatten(L)])
	end,
	lists:foreach(F4, Map),
	io:format(S, "~s ~n", ["</TABLE></CENTER><P>"]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
initializeFilePaths()->
%% 	Root = platform:getDirectoryPath(""admin"", ""),
	Root = platform:getDirectoryPath("", ""),	
%% 	UrlPath = platform:getURLPath("admin", ""),
	UrlPath = platform:getURLPath("", ""),
	{ok,{_, ID}} = THIS:get_property(id),
	io:format("~p ~n", [to_list(ID)]),	
	{ok,{_,TApp}}=THIS:get_property(app_),
	case is_atom(TApp) of
		true ->
			App=atom_to_list(TApp);
		_ -> 
			App=TApp
	end,
	ReportDirectory = filename:nativename(Root ++  "wwwroot/htdocs/TimeComparisonReports-" ++ to_list(ID)++"-"++App++"/") ++ "/",
    io:format("ReportDirectory !!!!!!!!!!!~n~p~n", [ReportDirectory]),
	case filelib:is_dir(ReportDirectory) of
		true->
			ok;
		_->
%% 			io:format("~p ~n", [ReportDirectory]),
			file:make_dir(ReportDirectory)			
	end,
	
%% 	io:format("~p ~n", [to_list(ReportDirectory ++ "logo.gif")]),
	case filelib:is_file(ReportDirectory ++ "logo.jpg") of
		true->
		    ok;
		_->
		   io:format("~p ~n", [to_list(Root ++ "wwwroot\\images\\logo.jpg")]),
		   file:copy(filename:nativename(Root ++ "wwwroot/images/logo.jpg"), ReportDirectory ++ "logo.jpg")
	end,
	{Year,Month,Day} = date(),
	{Time,Minute,Second} = time(),
%% 	io:format("~p ~n", [Year]),
%% 	io:format("~p ~n", [Month]),
%% 	io:format("~p ~n", [Day]),
	FileName = lists:flatten(io_lib:format("TimeComparisonReport_~p_~p_~p-~p_~p.html",[Year,Month,Day,Time,Minute])),	
%% 	io:format("~p ~n", [lists:flatten(FileName)]),
	ReportPath =  ReportDirectory ++ FileName,
		
	THIS:set_property(reportDirectory, ReportDirectory),
	THIS:set_property(reportPath, ReportPath),
	THIS:set_property(reportVirtual, UrlPath ++ "../htdocs/TimeComparisonReports-" ++ to_list(ID)++"-"++App),
	THIS:set_property(reportURL, UrlPath ++ "../htdocs/TimeComparisonReports-" ++ to_list(ID)++"-"++App ++ "/" ++ FileName),
	THIS:set_property(reportlistVirtual, UrlPath ++ "/TimeComparisonReports-htdocs/" ++ to_list(ID)++"-"++App),
	THIS:set_property(reportlistURL, UrlPath ++ "/TimeComparisonReports-" ++ to_list(ID)++"-"++App ++ "/" ++ FileName),

	
	FileName1 = lists:flatten(io_lib:format("TimeComparisonReport_~p_~p_~p-~p_~p",[Year,Month,Day,Time,Minute])),
	THIS:set_property(filename, FileName1),	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
addLogo(S) ->
	io:format(S, "~s ~n",["<center><IMG SRC=logo.jpg> <table class=fine border=0 cellspacing=0 width=500 align=center><tr><td><p class=fine align=center><br>
	<small>"++?OEM(gettext:key2str("SiteView"))++"</p><p class=fine align=center><a href=http://www.dragonflow.com/company/copyright.html target=web> Copyright &copy; 2009 </a> , All rights reserved.</small></p></td></tr></table></center>"]).
getDate(Date) ->
	L=string:tokens(Date, "-"),
	Y=list_to_integer(hd(L)),
	M=list_to_integer(lists:nth(2, L)),
	D=list_to_integer(lists:nth(3, L)),
	{Y,M,D}.
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
initializeTimeParameters()->
	{ok,{_, StartDayStr}} = THIS:get_property(startDay),
	{ok,{_, StartHourStr}} = THIS:get_property(startHour),
	{ok,{_, WindowStr}} = THIS:get_property(window),
	{ok,{_, PrecisionStr}} = THIS:get_property(precision),
	{_,{_,FirstTime}}=THIS:get_property(first),
	{_,{_,SecondTime}}=THIS:get_property(second),
	{_,{_,ComparisonType}}=THIS:get_property(comparisonType),
	{Y,M,D}=getDate(FirstTime),
	{Y1,M1,D1}=getDate(SecondTime),
	Freq = THIS:calculateMinimumFrequency(),
%% 	THIS:set_property(precision, 60),
	case ComparisonType of
		"day" ->
			Start=calendar:datetime_to_gregorian_seconds({{Y,M,D},{0,0,0}}),
		    End=calendar:datetime_to_gregorian_seconds({{Y,M,D},{23,59,59}}),
	        Start1=calendar:datetime_to_gregorian_seconds({{Y1,M1,D1},{0,0,0}}),
		    End1=calendar:datetime_to_gregorian_seconds({{Y1,M1,D1},{23,59,59}}),
			Now=calendar:datetime_to_gregorian_seconds(erlang:localtime()),
			case Start>Now of
				false ->
					THIS:set_property(startTime,Start);
				true ->
					THIS:set_property(startTime,Now)
			end,
			case Start1<Now of
				true ->
					THIS:set_property(startTime1,Start1);
				false ->
					THIS:set_property(startTime1,Now)
			end,
			case End<Now of
				true ->
					THIS:set_property(endTime, End);
				false ->
					THIS:set_property(endTime,Now)
			end,
			case End1<Now of
				true ->
					THIS:set_property(endTime1, End1);
				false ->
					THIS:set_property(endTime1,Now)
			end,
	        THIS:set_property(precision, 1800);
		"week"->
			WeekDay= calendar:day_of_the_week(Y,M,D),
			NDays=7-WeekDay-1,
			Date=calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({Y,M,D}) -WeekDay),
			Start=calendar:datetime_to_gregorian_seconds({Date,{0,0,0}}),
			EDate=calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({Y,M,D}) +NDays),
		    End=calendar:datetime_to_gregorian_seconds({EDate,{23,59,59}}),
			WeekDay1= calendar:day_of_the_week(Y1,M1,D1),
			Date1=calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({Y1,M1,D1}) -WeekDay1),
	        Start1=calendar:datetime_to_gregorian_seconds({Date1,{0,0,0}}),
			EDate1=calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({Y1,M1,D1}) +NDays),
		    End1=calendar:datetime_to_gregorian_seconds({EDate1,{23,59,59}}),
			Now=calendar:datetime_to_gregorian_seconds(erlang:localtime()),
			case Start>Now of
				false ->
					THIS:set_property(startTime,Start);
				true ->
					THIS:set_property(startTime,Now)
			end,
			case Start1<Now of
				true ->
					THIS:set_property(startTime1,Start1);
				false ->
					THIS:set_property(startTime1,Now)
			end,
			case End<Now of
				true ->
					THIS:set_property(endTime, End);
				false ->
					THIS:set_property(endTime,Now)
			end,
			case End1<Now of
				true ->
					THIS:set_property(endTime1, End1);
				false ->
					THIS:set_property(endTime1,Now)
			end,
			THIS:set_property(precision, 14400);
		"month"->
			Start=calendar:datetime_to_gregorian_seconds({{Y,M,1},{0,0,0}}),
			MaxDay=calendar:last_day_of_the_month(Y, M),
		    End=calendar:datetime_to_gregorian_seconds({{Y,M,MaxDay},{23,59,59}}),
	        Start1=calendar:datetime_to_gregorian_seconds({{Y1,M1,1},{0,0,0}}),
			MaxDay1=calendar:last_day_of_the_month(Y1, M1),
		    End1=calendar:datetime_to_gregorian_seconds({{Y1,M1,MaxDay1},{23,59,59}}),
			Now=calendar:datetime_to_gregorian_seconds(erlang:localtime()),
			case Start>Now of
				false ->
					THIS:set_property(startTime,Start);
				true ->
					THIS:set_property(startTime,Now)
			end,
			case Start1<Now of
				true ->
					THIS:set_property(startTime1,Start1);
				false ->
					THIS:set_property(startTime1,Now)
			end,
			case End<Now of
				true ->
					THIS:set_property(endTime, End);
				false ->
					THIS:set_property(endTime,Now)
			end,
			case End1<Now of
				true ->
					THIS:set_property(endTime1, End1);
				false ->
					THIS:set_property(endTime1,Now)
			end,
			THIS:set_property(precision, 43200)
	end,
%% 	case (string:len(WindowStr) == 0) of 
%% 		  true ->
%% 			Freq = THIS:calculateMinimumFrequency(),
%% %% 			io:format("~p ~n", [Freq]),
%% 			THIS:set_property(precision, Freq),
%% %% 			DefaultSamples = 48,
%% 			DefaultSamples = 20,
%%     		Window = Freq * DefaultSamples,
%% 			THIS:set_property(window, Window),
%% 			THIS:set_property(endTime, trunc(platform:timeMillis()/1000)),                                             
%% 			THIS:set_property(startTime, trunc(platform:timeMillis()/1000) - Window);
%% %%     		setUsingDefaultTimeParameters(true);
%% 		_->	
%% 			{Date, Time} = calendar:local_time(),
%% %% 			io:format("~p ~n", [calendar:datetime_to_gregorian_seconds({Date, Time})]),
%% 			case string:equal(WindowStr, "monthToDate") of
%% 				true ->
%% 					{_,_,Day} = Date,
%% 					Window1 = (Day - 1)*86400;
%% %% 					IsMonthToDate = true;
%% 				_ ->
%% 					{Window1, _} = string:to_integer(WindowStr)
%% %% 					IsMonthToDate = false
%% 			end,
%% 			io:format("~p ~n", [Window1]),
%% 			case (StartDayStr == today) of
%% 				true ->
%% 					T1 = calendar:datetime_to_gregorian_seconds({Date,{0,0,0}}),					
%% %% 					io:format("~p ~n", [T1]),
%% %% 					io:format("~p ~n", [calendar:gregorian_seconds_to_datetime(T1)]),
%% 					StartDay = T1;
%% 				_ ->
%% 					StartDay = calendar:datetime_to_gregorian_seconds(StartDayStr) 		
%% 			end,
%% 			case string:equal(StartHourStr, "now") of 
%% 				true ->
%% 					{Hour,Min,Sec} = Time, 
%% 					StartHour = Sec + Min * 60 + Hour * 3600; 
%% 				_->
%% %% 					StartHour = calendar:datetime_to_gregorian_seconds(StartHourStr)
%% 					{StartHour, _} = string:to_integer(StartHourStr)
%% 			end,
%% 			THIS:set_property(startTime, StartDay + StartHour),
%% %% 		 	boolean useAdvancedAlgorithm = ConfigManagerSession.getReadOnlyMasterConfig().getAsBoolean("_useReportAdvancedSamplingAlgorithm");
%% 			UseAdvancedAlgorithm = false,
%% 			case string:equal(PrecisionStr, "default") of 
%% 				true ->
%% 			 		 case useAdvancedAlgorithm of
%% 						true ->
%% 							Freq1 = THIS:calculateMinimumFrequency(),
%% 							THIS:set_property(precision, Freq1);
%% 						 _ -> 
%% 							case Window1 of
%% 								3600 ->
%% 										THIS:set_property(precision, 60);
%% 								86400 ->
%% 										THIS:set_property(precision, 1800);
%% 								604800 ->
%% 									THIS:set_property(precision, 14400);
%% 								2592000 ->
%% 										THIS:set_property(precision, 43200);
%% 								_->	
%% 									Precision = trunc((Window1 / 40) / 60) * 60,
%% 									case (Precision == 0) of
%% 										true ->
%% 										   THIS:set_property(precision, 60);
%% 										_->
%% 												ok
%% 									end,
%% 									THIS:set_property(precision, Precision)
%% 							end
%% 						end;
%% 				_->
%% 					case is_integer(PrecisionStr) of
%% 						true ->
%% 							Precision = PrecisionStr;
%% 						_ ->
%% 							{Precision, _} = string:to_integer(PrecisionStr)
%% 					end,
%% 					
%% 					io:format("~p ~n", [Precision]),
%% 					io:format("~p ~n", [Window1]),
%% 					MaxBuckets = 100,
%% 					case (Window1 / Precision > MaxBuckets) of
%% 						true ->
%% 							T_Precision = trunc(Window1 / MaxBuckets),
%% 							T_Precision1 = (T_Precision / 60) * 60,
%% 							case T_Precision1 == 0 of
%% 								true ->
%% 									THIS:set_property(precision, 60);
%% 								_ ->
%% 								  THIS:set_property(precision, T_Precision)
%% 							end;
%% 						_ ->
%% 							THIS:set_property(precision, Precision)
%% 					end
%% 			end,
%% 
%% 			{ok,{_, Relative}} = THIS:get_property(relative),
%% 			{ok,{_, StartTime}} = THIS:get_property(startTime),
%% %%  			io:format("startTime: ~p ~n", [StartTime]),
%% 			
%% 			case Relative == -1 of
%% 				true ->
%% 					THIS:set_property(endTime, StartTime),
%% 				    THIS:set_property(startTime, StartTime - Window1),	
%% 					THIS:setReportPeriod();
%% 				   _ ->
%% 					THIS:set_property(endTime, StartTime + Window1),
%% 					THIS:setReportPeriod()
%% 			end
%% 	end,	
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
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
%% @doc read report info from contentstore 
calculateMinimumFrequency() ->
	{ok,{_, Collectors}} = THIS:get_attribute(collectors),
	Frequency = calculateMinFreq_Child(Collectors, 6000),%%  zanshi 6000
	if 
		Frequency == 0 ->
			600;
	  	true ->
		 	Frequency
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
setReportPeriod() ->
	{ok,{_, StartTime}} = THIS:get_property(startTime),
	{ok,{_, EndTime}} = THIS:get_property(endTime),	

	L = "(from " ++ sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(StartTime))
	++ " to " ++ sv_datetime:tostr(calendar:gregorian_seconds_to_datetime(EndTime)) ++ ")",
 	THIS:set_property(reportPeriod, L).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
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
%% @doc read report info from contentstore 
getSummaryListByMap(Map, [], Cons) ->
	Cons;	
getSummaryListByMap(Map, [H|T], Cons) ->
	{ok, V} = dict:find(H, Map),
	T1 = lists:nth(1,  V),	
	getSummaryListByMap(Map, T, Cons ++ [{T1:getMonitorFullID(), T1:getMonitorName(), T1:getWorstCategory(), T1:getMaximum(), T1:getAverage()}]).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore 
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
			THIS:process_msg(Tmp)
	end.

process_msg([])->"";
process_msg("<" ++ T)->
	{T1, T2} = lists:splitwith(fun(A) -> A =/= $> end, T),
	case THIS:get_property(list_to_atom(T1)) of
		{error,{_, _}} ->
			THIS:process_msg(T2 -- [$>]);
		{ok,{_, Value}}->
			THIS:to_list(Value) ++ THIS:process_msg(T2 -- [$>])
	end;
process_msg([C|T])->
	[C] ++ THIS:process_msg(T).