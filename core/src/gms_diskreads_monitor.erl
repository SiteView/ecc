%% Author: Administrator
%% Created: 2011-10-10
%% Description: TODO: Add description to gms_totalbytessent_monitor
-module(gms_diskreads_monitor,[BASE]).
-extends(server_monitor).
-define(SNAME, "wmi").
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTER,20).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = server_monitor:new(),
	Base:set_attribute(lastMeasurementTime,0),
	Base:set_attribute(lastMeasurementTicks,0),
	Base:set_attribute(lastMeasurementWasNotAvailable,false),
	Base:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	Base:set_attribute(lastBaseMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	Base:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	Base:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	{?MODULE,Base}.
%% @spec getHostname(This) -> string()
%% where
%% This = instance()
%% @doc Get Machine.
getHostname(This)->
	{ok,{_,Machine}}=This:get_property(machine),
	Machine.
%% @spec getMaxCounters() -> integer()
%% @doc Get Max counters number.
getMaxCounter()->?MAX_COUNTER.
get_wmi_node()->
	case server_conf:getWmiNode() of
		undefined->
			{ok, Host} = inet:gethostname(),
			list_to_atom( ?SNAME ++ "@" ++ Host);
		Node->
			Node
	end.
getmhost(Host)->
	string:strip(Host, left, $\\).

getcounterids()->
	[{"DiskReadBytesPersec","PERF_COUNTER_BULK_COUNT"}
	].
%%
%% 
objects()->
	[{"LogicalDisk","Win32_PerfRawData_PerfDisk_LogicalDisk"}
	].

getCounters()->
	{ok,{_,Disk}}=THIS:get_property(disk),
	{ok,[#perf_counter{object="LogicalDisk",counterName="Disk Read Bytes/sec",instance=Disk}]}.
update()->
	THIS:set_attribute(status,"error"),
	{ok,{_,Machine}}=THIS:get_property(machine),
	
	{ok,{_,LastMeasurementTime}} = THIS:get_attribute(lastMeasurementTime),
	{ok,{_,LastMeasurementTicks}} = THIS:get_attribute(lastMeasurementTicks),
	{ok,{_,LastMeasurementWasNotAvailable}} = THIS:get_attribute(lastMeasurementWasNotAvailable),
	
	Templateids=THIS:getcounterids(),
	case THIS:getCounters() of
		{error,Err}->
			THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA),
			StatusString = lists:flatten(io_lib:format("error:~p~n",[Err])),
			THIS:set_attribute(?STATE_STRING,StatusString);
		{ok,Counters}->
			if
				length(Counters) > ?MAX_COUNTER ->
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute(?CATEGORY,?NO_DATA),
					THIS:set_attribute(contersInError,?MAX_COUNTER),
					THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MAX_COUNTER])));
				true ->
					R=case Machine of
						""->
							THIS:getCounterValues("127.0.0.1"," "," ",Counters);
						_->
							case machine:getNTMachine(Machine) of
								[]->
									THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
							        THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
							        THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
							        THIS:set_attribute(?NO_DATA,true),
							        THIS:set_attribute(?CATEGORY,?NO_DATA),
							        THIS:set_attribute(contersInError,?MAX_COUNTER),
							        THIS:set_attribute(?STATE_STRING,"connect target error!"),
									[];
							    [M|_]->
                                   THIS:getCounterValues(getmhost(M#machine.host),M#machine.login,M#machine.passwd,Counters)
							end
			        end,
		      	Ret=convert_result( R,[],Templateids),
%%  				io:format("Ret:~p~n", [Ret]),
			     case Ret of
						    []->
							    THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
							    THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
							    THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
							    THIS:set_attribute(?NO_DATA,true),
							    THIS:set_attribute(?CATEGORY,?NO_DATA),
							    THIS:set_attribute(contersInError,?MAX_COUNTER),
							    THIS:set_attribute(?STATE_STRING,"connect target error!");
						    _->
							    PerfFreq = proplists:get_value("PerfFreq",Ret),
							    PerfTime100nSec = proplists:get_value("PerfTime100nSec",Ret),
							    PerfTime = proplists:get_value("PerfTime",Ret),
							    TnSec = list_to_integer(PerfTime100nSec) - LastMeasurementTime,
							    TSec = list_to_integer(PerfTime) - LastMeasurementTicks,
							    Tf = TSec / list_to_integer(PerfFreq),
							    Flag = ((TnSec=<0) orelse (TSec=<0) orelse (LastMeasurementTime=<0) orelse (LastMeasurementWasNotAvailable=<0) orelse (LastMeasurementTicks=<0)),
							
							    FScale = 
							    case THIS:get_property(scale) of
								    {ok,{_,"1"}}->
									    1;
								    {ok,{_,Scale}}->
									    case string:to_float(Scale) of
										    {F1,[]}->
											    F1;
										    _->
											    case string:to_integer(Scale) of
												    {I1,[]}->
													    I1;
												    _->
													    1
											    end
									    end;
								    _->
									    1
							    end,

							    if
								    Flag ->
									    {ok,{_,M1}} = THIS:get_attribute(lastMeasurement),
									    {ok,{_,M2}} = THIS:get_attribute(lastBaseMeasurement),	
%% 										io:format("Ret :~p~n", [Ret]),
									    {Measure,MeasureBase} = THIS:set_measure_value(Ret,M1,M2,1),

									    timer:sleep(1001),
									    RR=case Machine of
											  ""->
												  THIS:getCounterValues("127.0.0.1"," "," ",Counters);
											  _->
												  case machine:getNTMachine(Machine) of
													  []->
														  THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
														  THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
														  THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
														  THIS:set_attribute(?NO_DATA,true),
							                              THIS:set_attribute(?CATEGORY,?NO_DATA),
							                              THIS:set_attribute(contersInError,?MAX_COUNTER),
							                              THIS:set_attribute(?STATE_STRING,"connect target error!"),
									                    [];
							                          [MM|_]->
                                                   THIS:getCounterValues(getmhost(MM#machine.host),MM#machine.login,MM#machine.passwd,Counters)
							                     end
										   end,
										
									    Ret2=convert_result(RR,[],Templateids),
									    PerfFreq2 = proplists:get_value("PerfFreq",Ret2),
									    PerfTime100nSec2 = proplists:get_value("PerfTime100nSec",Ret2),
									    PerfTime2 = proplists:get_value("PerfTime",Ret2),
									    TnSec2 = list_to_integer(PerfTime100nSec2) - list_to_integer(PerfTime100nSec),
									    put(measurementTime,PerfTime100nSec2),
									    put(perfFreq,PerfFreq2),
									    put(defaMeasureTime,TnSec2),
									    TSec2 = list_to_integer(PerfTime2) - list_to_integer(PerfTime),
									    Tf2 = TSec2/list_to_integer(PerfFreq2),
									    put(tf,Tf2),

									    put(values,list_to_tuple(lists:duplicate(length(Ret2),"n/a"))),
									    put(lastMeasurements,list_to_tuple(lists:duplicate(length(Ret2),"n/a"))),
									    put(lastBaseMeasurements,list_to_tuple(lists:duplicate(length(Ret2),"n/a"))),
									    put(measurements,list_to_tuple(lists:duplicate(length(Ret2),"n/a"))),

									    THIS:set_attribute(LastMeasurementTime,list_to_integer(PerfTime100nSec2)),
									    THIS:set_attribute(LastMeasurementTicks,list_to_integer(PerfTime2)),
									    S1=THIS:make_state_string(Ret2,Measure,MeasureBase,FScale,TSec2,0,1,?NTCounterSummaryMax),

									    THIS:set_attribute(values,get(values)),
									    THIS:set_attribute(lastMeasurement,get(lastMeasurements)),
									    THIS:set_attribute(lastBaseMeasurement,get(lastBaseMeasurements)),
									    THIS:set_attribute(measurement,get(measurements)),
									

									    THIS:set_attribute(?STATE_STRING,S1);

								    true->
									    {ok,{_,M3}} = THIS:get_attribute(lastMeasurement),
									    {ok,{_,M4}} = THIS:get_attribute(lastBaseMeasurement),
									    put(measurementTime,PerfTime100nSec),
									    put(perfFreq,PerfFreq),
									    put(defaMeasureTime,TnSec),
									    put(tf,Tf),

									    put(values,list_to_tuple(lists:duplicate(length(Ret),"n/a"))),
									    put(lastMeasurements,list_to_tuple(lists:duplicate(length(Ret),"n/a"))),
									    put(lastBaseMeasurements,list_to_tuple(lists:duplicate(length(Ret),"n/a"))),
									    put(measurements,list_to_tuple(lists:duplicate(length(Ret),"n/a"))),

									    THIS:set_attribute(LastMeasurementTime,list_to_integer(PerfTime100nSec)),
									    THIS:set_attribute(LastMeasurementTicks,list_to_integer(PerfTime)),
										
									    S2=THIS:make_state_string(Ret,M3,M4,FScale,TSec,0,1,?NTCounterSummaryMax),

								 	    THIS:set_attribute(values,get(values)),
									    THIS:set_attribute(lastMeasurement,get(lastMeasurements)),
									    THIS:set_attribute(lastBaseMeasurement,get(lastBaseMeasurements)),
									    THIS:set_attribute(measurement,get(measurements)),
									

									    THIS:set_attribute(?STATE_STRING,S2)
							    end
					    end
			end
	
	end,
	{ok,update_ok}.
%% Ids=getcounterids()
%%%% 	{"Memory_PagesPersec","Pages/sec","Win32_PerfRawData_PerfOS_Memory","PagesPersec","_total","PERF_COUNTER_COUNTER"} 
convert_result([],Acc,_)->
	Acc;
convert_result([H|E],Acc,Ids)->
	case H of
		{Sql, {_,empty}}->
			ACounter=lists:nth(2, string:tokens(Sql, " ")),
			AObj=lists:nth(4, string:tokens(Sql, " ")),
			Counters={{ list_to_atom(ACounter),""}};
		{Sql, {_,[ACounter]}}->
			AObj=lists:nth(4, string:tokens(Sql, " ")),
			Counters=ACounter
	end,
	Ssize=size(Counters),
	Obj= case lists:keyfind(AObj, 2, objects()) of
			 false ->
				 AObj;
			 {Bobj,_}->
				 Bobj
		 end,
	ListCounters=tuple_to_list(Counters),
	Tem=case Ssize of
		   1 ->
			   buildcountervalue(Counters,1,Acc,Ids,Obj);
			2->
			  buildcountervalue(Counters,2,Acc,Ids,Obj);
				
		    3->
			   {_,Value1}=lists:keyfind('Timestamp_PerfTime', 1, ListCounters),
			   {_,Value2}=lists:keyfind('Timestamp_Object', 1, ListCounters) ,
			   {_,Value3}=lists:keyfind('Frequency_PerfTime', 1, ListCounters) ,
			   [{"PerfTime",binary_to_list(Value1)},{"PerfFreq",binary_to_list(Value3)},{"PerfTime100nSec",binary_to_list(Value2)}];
			_ ->
			  [element( 1, Counters)]
		end,
%% 	io:format(" Tem:~p~n", [Tem]),
	Newtem=case Tem of
			   []->
				  Acc;
			   [{Newcounter,Newvalue}]->
				   case lists:keyfind(Newcounter, 1, Acc) of
					   false ->
						   Acc++Tem;
					   _ ->
						   lists:keyreplace(Newcounter, 1, Acc, {Newcounter,Newvalue})
				   end;
			   _ ->
				   Acc++Tem
		   end,	
%% 	io:format(" Newtem:~p~n", [Newtem]),
%% 	LTem=Acc++Tem,
	convert_result(E,Newtem,Ids).
buildcountervalue(Counters,N,Acc,Ids,AObj)->
%% 	          io:format("Counters result is ~p~n", [Counters]),
	       case N of 
			   1 ->
				   Instance="",
				    {Counter,Value}=element( 1, Counters);
			   2 ->
				   {C1,V1}=element( 2, Counters),
				   case C1 of 
					   'Name' ->
						    Instance=binary_to_list(V1),
						    {Counter,Value}=element( 1, Counters);
					    _ ->
							{CC1,VV1}=element(1, Counters),
							Instance=binary_to_list(VV1),
							{Counter,Value}={C1,V1}
				   end
		   end,
%% 	          {Counter,Value}=element( N, Counters),
               TempLCounter=atom_to_list(Counter),
		       Index=string:rstr(TempLCounter,"_Base"),
          IsBase= case Index of
					  0->
						  LCounter=TempLCounter,
						  false;
					  _ ->
						  LCounter=string:sub_string(TempLCounter,1,Index-1),
						  true
				  end,
		   
		   
			   Lvalue= case is_integer(Value) of
						   true ->
							   integer_to_list(Value);
						   _ ->
							   case is_float(Value) of
								   true->
									   float_to_list(Value);
							       _ ->
									   case is_binary(Value) of
										   true->
											   binary_to_list(Value);
										   _ ->
											   case is_list(Value) of
												   true ->
													   Value;
												   _->
													   [Value]
											   end
									   end
							   end
					   end,
			   case lists:keyfind(LCounter,1, Ids) of
				   false ->
					   [];
				   {TCountername1,Countertype}->
					  TCountername=get(TCountername1),
					   case N of
						   1 ->
							  Countername=AObj++":"++TCountername ;
						   2->
							   Countername=AObj++":"++TCountername++":"++Instance;
						   _ ->
							  Countername=AObj++":"++TCountername 
					   end,
					   case IsBase of
						   true ->
                               OldCounter=lists:keyfind(Countername, 1, Acc),
							   case OldCounter of 
								   false ->
									   [];
								   {Counternameold,{Lvalueold,_,Typeold}}->
									   [{Counternameold,{Lvalueold,Lvalue,Typeold}}]
							   end;
						   false ->
							    [{Countername,{Lvalue,"",Countertype}}]	
					  end
		   end.

%% 
getCounterValues(Host,User,Passwd,Counters)->

	{State,Counters} = THIS:getCounters(),
    Ssql=buildSql(Counters,[]),
	Ssql1=buildSql1(Counters,[]),
    case rpc:call(THIS:get_wmi_node(), wmic, wmic, [Host,User,Passwd,Ssql++Ssql1]) of
		{ok, Result} ->
%% 			    io:format("wmi result is ~p~n", [Result]),
				Result;		    
			Error ->
				THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
                THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
				THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
				THIS:set_attribute(?NO_DATA,true),
			    THIS:set_attribute(?CATEGORY,?NO_DATA),
				THIS:set_attribute(contersInError,?MAX_COUNTER),
				THIS:set_attribute(?STATE_STRING,"call wmi node error!"),
				[]
	end.
gettwocounter()->
	[
	"PERF_SAMPLE_FRACTION","PERF_RAW_FRACTION"
	].
%% Page Faults/sec PageFaultsPersec  Avg. Disk Queue Length AvgDiskQueueLength % Free Space PercentFreeSpace
%% "i/o"   "This Period mSec - Processor" "Connects LAN Manager 2.0" 
buildcounter(Counter1)->
%% 	" "
	LCounters=string:tokens(Counter1, " "),
	Ret=getRet([],LCounters),
%%  "/" 
	LCounters1=util:replace(Ret, "/", "Per"),
%%  "%" 	
	LCounters2=util:replace(LCounters1, "%", "Percent"),
%% "."
	LCounters3=util:replace(LCounters2, ".", ""),
	LCounters3.
buildSql([],Acc)->
	["select Timestamp_PerfTime,Timestamp_Object,Frequency_PerfTime from Win32_PerfRawData_PerfOS_System"]++lists:reverse(Acc);
buildSql([H|E],Acc)->
%% 	{Object,Counter,Instance,_}=H,
%% 	X#perf_counter.object++":"++X#perf_counter.counterName||X<-Counters,X#perf_counter.instance=:=""
	Object=case lists:keyfind(H#perf_counter.object, 1, objects()) of
			   false ->
				   "Win32_PerfRawData_PerfOS_"++H#perf_counter.object;
			   {_,Obj}->
				   Obj
		   end,
	Counter=buildcounter(H#perf_counter.counterName),
    put(Counter, H#perf_counter.counterName),
	Tem= case H#perf_counter.instance of
			 [] ->
				 "SELECT "++Counter++" FROM "++Object;
			 _ ->
                 "SELECT "++Counter++" FROM "++Object++" WHERE Name="++"'"++H#perf_counter.instance++"'"
		 end,
	buildSql(E,[Tem|Acc]).
%% two
buildSql1([],Acc)->
	lists:reverse(Acc);
buildSql1([H|E],Acc)->
	Object=case lists:keyfind(H#perf_counter.object, 1, objects()) of
			   false ->
				   "Win32_PerfRawData_PerfOS_"++H#perf_counter.object;
			   {_,Obj}->
				   Obj
		   end,
	Counter=buildcounter(H#perf_counter.counterName),
	Tem= case H#perf_counter.instance of
			 [] ->
					  "SELECT "++Counter++"_Base FROM "++Object;
				  _ ->
					  "SELECT "++Counter++"_Base FROM "++Object++" WHERE Name="++"'"++H#perf_counter.instance++"'"
		 end,
	buildSql1(E,[Tem|Acc]). 
string2val(Str)->
  case is_integer(Str) of
	  true ->
		  Str;
	  _ ->
		  case string:to_integer(Str) of
			  {I,[]}->
				  I;
			  _->
				  -1
		  end
  end.


set_measure_value([],Measure,MeasureBase,_)->{Measure,MeasureBase};
set_measure_value([{"PerfFreq",_}|T],Measure,MeasureBase,I)->THIS:set_measure_value(T,Measure,MeasureBase,I);
set_measure_value([{"PerfTime100nSec",_}|T],Measure,MeasureBase,I)->THIS:set_measure_value(T,Measure,MeasureBase,I);
set_measure_value([{"PerfTime",_}|T],Measure,MeasureBase,I)->THIS:set_measure_value(T,Measure,MeasureBase,I);
set_measure_value([{_,V}|T],Measure,MeasureBase,I)->
	case element(3,V) of
		"n/a"->
			set_measure_value(T,Measure,MeasureBase,I+1);
		_->
			M1 = string2val(element(1,V)),
			M2 = string2val(element(2,V)),
			set_measure_value(T,setelement(I,Measure,M1),setelement(I,MeasureBase,M2),I+1)
	end.

make_state_string([],_,_,_,_,_,_,_)->"";
make_state_string([{"PerfFreq",_}|T],Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max)->THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max);
make_state_string([{"PerfTime100nSec",_}|T],Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max)->THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max);
make_state_string([{"PerfTime",_}|T],Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max)->THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max);
make_state_string([{K,V}|T],Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max)->
    Val = string2val(element(1,V)),
	Base = string2val(element(2,V)),
	%%io:format("make_state_string key:~p, Cal:~p,BaCal:~p,V:~p~n",[K,element(Cur,Measure),element(Cur,MeasureBase),V]),
	
	Cal = case element(Cur,Measure) of "n/a"->Val;_->Val - element(Cur,Measure) end,
	BaCal = case element(Cur,MeasureBase) of "n/a"->0;_->element(Cur,MeasureBase) end,


	DefaBase = Base - BaCal, %%  - case element(Cur,MeasureBase) of "n/a"->0;_->element(Cur,MeasureBase)end,
	MeasureTime = string2val(get(measurementTime)),
	PerfFreq = string2val(get(perfFreq)),
	DefaMeasureTime = string2val(get(defaMeasureTime)),
	Tf = get(tf),
	%%io:format("make_state_string:~p,~p,~p,~p,~p,~p,~p,~p,~p~n",[Val,Base,Cal,BaCal,DefaBase,MeasureTime,PerfFreq,DefaMeasureTime,Tf]),
	if
		Val >= 0 ->
			S = element(3,V),
			{FResult,Flag2,Flag3} = 
			if 
				S=="n/a"->
					THIS:inc_attribute(contersInError),
					{"n/a",false,false};
				S=="PERF_COUNTER_COUNTER" ->
					if
						Cal >=0 ->
							case element(Cur,Measure) of
								0->
									{0,false,true};
								_->
									{Cal/Tf,false,true}
							end;
						true ->
							{0,false,false}
					end;
				S=="PERF_COUNTER_TIMER" ->
					if
						Cal >=0 ->
							{Cal/DefaLastMeasure,true,false};
						true ->
							{0,false,false}
					end;
				S=="PERF_COUNTER_TIMER_INV"->
					if 
						Cal >=0 ->
							{1 - Cal/Tf,true,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_COUNTER_BULK_COUNT" ->
					if
						Cal >= 0->
							{Cal/Tf,false,true};
						true ->
							{0,false,false}
					end;
				S == "PERF_COUNTER_RAWCOUNT" orelse S=="PERF_COUNTER_LARGE_RAWCOUNT" 
						orelse S=="PERF_COUNTER_RAWCOUNT_HEX" orelse S == "PERF_COUNTER_LARGE_RAWCOUNT_HEX" ->
					{Val,false,false};
				S== "PERF_ELAPSED_TIME" ->
					if
						Val > MeasureTime orelse Val < 0 ->
							{(MeasureTime - (Val bsr 32))/ 16#989680,false,false};
						true ->
							{(MeasureTime - Val)/ 16#989680,false,false}
					end;
				S == "PERF_RAW_FRACTION" -> 
					if 
						Val == 0 ->
							{0,false,false};
						true ->
							{Val/BaCal,true,false}
					end;
				S == "PERF_SAMPLE_FRACTION"->
					if
						Cal =< 0 orelse DefaBase =< 0 ->
							{0,true,false};
						true ->
							{Cal / DefaBase, true,false}
					end;
				S == "PERF_SAMPLE_COUNTER" ->
					if 
						Cal >= 0 andalso DefaBase > 0 ->
							{Cal / DefaBase, false,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_AVERAGE_TIME" ->
					{Val/BaCal,false,false};
				S == "PERF_AVERAGE_TIMER" ->
					if 
						Cal > 0 andalso DefaBase > 0->
							{Cal/DefaBase/PerfFreq,false,false};
						true ->
							{0,false,false}
					end;
				S ==  "PERF_AVERAGE_BULK" ->
					if
						Cal >0 andalso DefaBase > 0->
							{Cal/DefaBase,false,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_100NSEC_TIMER"->
					if
						Cal > 0 ->
							{Cal/DefaMeasureTime,false,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_100NSEC_TIMER_INV" ->
					if 
						Cal > 0 ->
							{1- Cal/DefaMeasureTime,true,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_COUNTER_MULTI_TIMER" ->
					if 
						Cal > 0 ->
							{Cal/Tf/BaCal,true,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_COUNTER_MULTI_TIMER_INV" ->
					if
						Cal > 0 ->
							{1-Cal/Tf/BaCal,true,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_100NSEC_MULTI_TIMER"->
					if
						Cal > 0 ->
							{Cal/BaCal/DefaMeasureTime,true,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_100NSEC_MULTI_TIMER_INV" ->
					if
						Cal > 0 ->
							{1-Cal/BaCal/DefaMeasureTime,true,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_COUNTER_QUEUELEN_TYPE" ->
					{(element(Cur,Measure) + MeasureTime * Val) / DefaMeasureTime,false,false};
				S == "PERF_COUNTER_LARGE_QUEUELEN_TYPE" ->
					{(element(Cur,Measure) + MeasureTime * Val) / DefaMeasureTime,false,false};
				S == "PERF_PRECISION_100NSEC_TIMER"->
					if
						Cal > 0 ->
							{Cal*1.0/DefaBase,true,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_PRECISION_100NSEC_QUEUELEN" ->
					if
						Cal > 0 ->
							{Cal/DefaMeasureTime,false,false};
						true ->
							{0,false,false}
					end;
				S == "PERF_COUNTER_100NS_QUEUELEN_TYPE" ->
					if
						Cal > 0 ->
							TTemp=Cal/DefaMeasureTime,
							if TTemp>0 ->
								   {TTemp,false,false};
							   true ->
								   {0,false,false}
							   end;
						true ->
							{0,false,false}
					end;
				true ->
					?ERROR_LOG2("NTCounterMonitor: no data for counter:~p,~p",[K,V]),
					{0,false,false}
			end,
			Result = 
			case FResult of
				"n/a"->
					0;
				_->
					if
						Flag2 ->
							if 
								FResult>1 ->
								  100;
                                true ->
									FResult*100
							end;
						true ->
							FResult
					end
				end,
            THIS:set_attribute(diskReadBytesPersec,Result), 
			THIS:set_attribute(status,"ok"),
			KK="Disk Read Bytes/sec",
			if
				is_number(FResult) ->
					put(values,setelement(Cur,get(values),lists:flatten(io_lib:format("~.2f",[(Result*Scale/1.0)])))),
					put(lastMeasurements,setelement(Cur,get(lastMeasurements),Val)),
					put(lastBaseMeasurements,setelement(Cur,get(lastBaseMeasurements),BaCal)),
					put(measurements,setelement(Cur,get(measurements),Val)),

			
					Unit1 = case THIS:get_property(units) of
							{ok,{_,U1}}->
								" " ++ U1;
							_->
								""
							end,
					Unit2 = if
								Flag2 == true ->
									" %";
								Flag3 == true ->
									" /sec";
								true ->
									""
							end,
					if
						Cur < Max ->
							KK ++ "=" ++ lists:flatten(io_lib:format("~.2f",[(Result*Scale/1.0)])) ++ Unit1 ++ Unit2 ++ "<br>" ++THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
						Cur == Max ->
							KK ++ "=" ++ lists:flatten(io_lib:format("~.2f",[(Result*Scale/1.0)])) ++ Unit1 ++ Unit2 ++ "..." ++ THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
						true ->
							THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max)
					end;
				true ->
					if
						Cur < Max ->                       
							KK ++ "= n/a<br>"++ THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
						Cur == Max ->
							KK ++ "= n/a ..."++ THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
						true ->
							THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max)
					end
					
			end;
		true ->
			THIS:inc_attribute(contersInError),
			if
				Cur <Max->
					K ++ "=" ++ element(1,V) ++ "<br>" ++  THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
				Cur == Max ->            
					K ++ "=" ++ element(1,V) ++ "..." ++  THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
				true->
					THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max)
			end
	end;

make_state_string([H|T],Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max)->
	THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur,Max).



getRet(Ret,[H|E])->
	TRet=Ret++H,
	getRet(TRet,E);
getRet(Ret,[])->
    Ret.
buildInstances([],R)->
	R;
buildInstances([H|E],R)->
	LINstance=tuple_to_list(H),
	{_,Binstance}=lists:keyfind('Name', 1, LINstance),
	Sinstance=binary_to_list(Binstance),
	Temp=case Sinstance of
			 "_Total" ->
				 R;
			 _ ->
				 R++[{Sinstance,Sinstance}]
		 end,
    buildInstances(E,Temp).
getInstances(Host,User,Passwd)->
	 Ssql=["select name from Win32_PerfRawData_PerfDisk_LogicalDisk"],
	case rpc:call(THIS:get_wmi_node(), wmic, wmic, [Host,User,Passwd,Ssql]) of
            {ok, Result} ->
				case Result of
					[{_SQL,{_,empty}}]->
			          [];
					[{_SQL,{_,Instances}}] ->
					  buildInstances(Instances,[])
				        
				end;			    
			_Error ->
				[]
	       end .
 getScalarValues(Prop,Params)->
%% 	{"4.x","4.x"},{"5.x","5.x"},
	case Prop of
		disk ->
			Machine = case lists:keysearch(machine,1,Params) of
							{value,{machine,Val}}->
								Val;
							_->
								""
						end,
			io:format("Machine Machine ~p~n",[Machine]),
			case Machine of
				  ""->
					  getInstances("127.0.0.1"," "," ");
				  _ ->
					  case machine:getNTMachine(Machine) of
						  []->
							  [];
						  [M|_] ->
							 getInstances(getmhost(M#machine.host),M#machine.login,M#machine.passwd)
					  end
			  end;
		_ ->
			BASE:getScalarValues(Prop, Params)
	end. 
%% @spec verify(Params) -> {ok, []} | {error, Reason}
%%  Params = [term()]
%%  Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params) ->
    Errs = 
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,   
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.
getLogProperties(This)->
	Temp = This:get_template_property(),
	[X#property.name|| X<-Temp,X#property.state=:=true].
getCostInLicensePoints()->
	1.
get_classifier(error)->
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{status,'!=',"ok"}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.
get_template_property()->
	BASE:get_template_property() ++  
	[
	#property{name=disk,title="Disk",type=scalar,editable=true,order=1},
	#property{name=diskReadBytesPersec,type=numeric,title="Disk Read Bytes/sec",configurable=false,state=true},
	#property{name=status,title="status",configurable=false,state=true}
    ].

