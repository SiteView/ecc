%% Author: Administrator
%% Created: 2011-4-15
%% Description: TODO: Add description to sqlcounter_base
-module(sqlcounter_base,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTER,20).

-export([new/0,increaseCounters/1,getHostname/1,remoteCommandLineAllowed/0,getMaxCounters/0,checkIdMap/2,getCounters/1,getActiveCounters/1,getRowCounters/1,getPerfCounters/3,getPerformanceData/2,getIDCacheForCounters/2,getPerfData/4,calcPerfResult/1]).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = server_monitor:new(),
	Base:set_attribute(contersInError,0),
	Base:set_attribute(lastMeasurementTime,0),
	Base:set_attribute(lastMeasurementTicks,0),
	Base:set_attribute(counters,[]),
	Base:set_attribute(instance,[]),
	Base:set_attribute(object,[]),
	Base:set_attribute(counterName,[]),
	Base:set_attribute(maxCounters,20),
	Base:set_attribute(flag1,false),
	Base:set_attribute(flag2,false),
	{?MODULE,Base}.
	
%% @spec increaseCounters(Number) -> ok
%% where
%% Number = integer()
%% @doc Set Max counters number.
increaseCounters(I)->
	THIS:set_attribute(maxCounters,I).
	
%% @spec getHostname(This) -> string()
%% where
%% This = instance()
%% @doc Get Machine.
getHostname(This)->
	{ok,{_,Machine}}=This:get_property(machine),
	Machine.
	
%% @spec remoteCommandLineAllowed() -> bool()
%% @doc allow remote command line.
remoteCommandLineAllowed()->false.

%% @spec getMaxCounters() -> integer()
%% @doc Get Max counters number.
getMaxCounters()->?MAX_COUNTER.

%% @spec getActiveCounters(Module) -> integer()
%% where
%% Module = instance()
%% @doc Get active counters number.
getActiveCounters(This)->
	Cnt = This:getCounters(This),
	Max = getMaxCounters(),
	if
		length(Cnt) > Max ->
			Max;
		true ->
			length(Cnt)
	end.
	
%% @spec getAvailableCounters(Module) -> Counters
%% where
%% Module = instance()
%% Counters = [{Counternumber,Countername}]
%% Counternumber = string()
%% Countername = string()
%% @doc Get specialized machine's counters.
%% Description: For page common module display the counters
%% ?PAGE_PARAMS is a new paramter which contain page paramters
getAvailableCounters(This)->
	Object=string:tokens(This:getCounterObjects(),","),
	PageParams = case This:get_property(?PAGE_PARAMS) of
		{ok,{_,PP}} ->
			PP;
		_->
			[]
	end,
	Machine = case proplists:get_value(machine,PageParams) of
		undefined ->
			"";
		Mach->
			Mach
	end,
	L = buildcounters(This:getPerfCounters(Machine,Object,[])),
	Ret = addkey(1,L),
	Ret.
	
%% @spec getRowCounters(Module) -> Counters
%% where
%% Module = instance()
%% Counters = [record()]
%% Counternumber = string()
%% Countername = string()
%% @doc Get specialized machine's counters,but result is different from getAvailableCounters().
%% Description: For getDefaultCounters/1
getRowCounters(This)->
	Object=string:tokens(This:getCounterObjects(),","),
	getPerfCounters(getHostname(This),Object,[]).
	
addkey(_,[]) ->[];
addkey(N,[F|R]) ->
	[{integer_to_list(N),F}]++addkey(N+1,R).
	

buildcounters([])->[];
buildcounters([F|R])->
	[F#perf_counter.object ++ " -- " ++ F#perf_counter.counterName ++ " -- " ++ F#perf_counter.instance]++buildcounters(R).
	
%% @spec checkIdMap(S,L) -> [tuple()]
%% where
%% S = string()
%% L = list()
%% @doc Get counters data.
checkIdMap(S,L)->
	getIDCacheForCounters(S,L).

%%此时已经创建了监测器实例，可以从里面取property了
getCounters(This)->
	L=This:getCountersContent(),
    {ok,{_,Machine}} = This:get_property(machine),
	forgetcounters(L,Machine).

forgetcounters([],_)->[];
forgetcounters([F1|R],Machine)->
    F = iconv:convert(httputils:pageEncode(),httputils:getMachineEncode(Machine),F1),
	Tuple = forgc(F,1,{"","","",""}),
	[{element(1,Tuple),#perf_counter{object=element(2,Tuple),counterName=element(3,Tuple),instance=element(4,Tuple)}}]++forgetcounters(R,Machine).
	
forgc(_,5,L)->L;
forgc(S,N,L) ->
    J = string:str(S,"@"),
	K = string:str(S," -- "),
    if
        J=/=0 ->
            forgc(string:sub_string(S,J+1,string:len(S)),N+1,setelement(1,L,string:sub_string(S,1,J-1)));
        true ->
            if
                K=/=0->
                    forgc(string:sub_string(S,K+string:len(" -- "),string:len(S)),N+1,setelement(N,L,string:sub_string(S,1,K-1)));
                true ->
                    forgc("",N+1,setelement(N,L,S))
            end
    end.
	
%% @spec getPerfCounters(Machine,Object,S) -> Counters
%% where
%% Machine = string()
%% Object = string()
%% Counters = record()
%% @doc Get specialized object's counters from machine.
getPerfCounters(Machine,Object,S)->
	THIS:set_attribute(flag1,false),
	THIS:set_attribute(flag2,false),
	Cmd1=make_params5(Object),
	Cmd2 = platform:perfexCommand1(Machine) ++ " " ++ Cmd1,
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd2),
	Array1 = perfCounters(string:tokens(iconv:convert(httputils:getMachineEncode(Machine),httputils:pageEncode(),Rets),"\r\n"),Object),
	case string:tokens(S,",") of
		[]->
			Array1;
		S1->
			case filtercounters(buildcounters(Array1),S1) of
				""->
					forgetcounters2(S1);
				String->
					forgetcounters(string:tokens(String,","),Machine)
			end
	end.
	
	
perfCounters([],_)->[];
perfCounters([F|R],Object)->
	case F of
		"object:"++_->
			THIS:set_attribute(flag1,false),
			THIS:set_attribute(flag2,false),
			Obj = string:sub_string(F,string:str(F,": ")+2,string:rstr(F," ")-1),
			THIS:set_attribute(object,Obj),
			search(Object,Obj),
			perfCounters(R,Object);
		"name:"++_->
			THIS:set_attribute(flag1,true),
			THIS:set_attribute(instance,string:strip(string:sub_string(F,string:str(F,": ")+2,string:len(F)))),
			perfCounters(R,Object);
		_->
			{ok,{_,F1}} = THIS:get_attribute(flag1),
			{ok,{_,F2}} = THIS:get_attribute(flag2),
			Index1 = string:str(F,":"),
			Index2 = string:rstr(F,"_BASE"),
			if
				(not F1) or (not F2) ->
					perfCounters(R,Object);
				(Index1 =:= 0) or (Index2 =/= 0) ->
					perfCounters(R,Object);
				true ->
					CounterName = string:strip(string:substr(F,1,Index1-1)),
					THIS:set_attribute(counterName,CounterName),
					{ok,{_,Object1}} = THIS:get_attribute(object),
					{ok,{_,Instance1}} = THIS:get_attribute(instance),
					[#perf_counter{object=Object1,counterName=CounterName,instance=Instance1}]++perfCounters(R,Object)
			end
	end.


search([],_)->ok;
search([F|R],Obj)->
	case F of
		Obj->
			THIS:set_attribute(flag2,true);
		_->
			search(R,Obj)
	end.
	
filtercounters([],_)->"";
filtercounters([F|R],L)->
	case for(F,L) of
		""->
			for(F,L)++filtercounters(R,L);
		_->
			for(F,L) ++ "," ++filtercounters(R,L)
	end.
	
for(_,[])->"";
for(L1,[F1|R1])->
	if
		(F1 =:= L1)->
			L1;
		true ->
			for(L1,R1)
	end.
	
forgetcounters2([])->[];
forgetcounters2([F|R])->
	L1 = [string:strip(X)||X<-string:tokens(F,"--")],
	[#perf_counter{object=lists:nth(1,L1),counterName=lists:nth(2,L1)++" (default not available)",instance=lists:nth(3,L1)}]++forgetcounters2(R).

%% @spec getPerformanceData(Machine,PageCounters) -> ok
%% where
%% Machine = string()
%% PageCounters = [record()]
%% @doc parse counters data and set result.
getPerformanceData(Machine,PageCounters)->
%%     io:format("get from page counters is:~p~n",[PageCounters]),
	THIS:set_attribute(contersInError,0),
    Counters = [V||{_,V}<-PageCounters],
	{ok,{_,LastMeasurementTime}} = THIS:get_attribute(lastMeasurementTime),
	{ok,{_,LastMeasurementTicks}} = THIS:get_attribute(lastMeasurementTicks),
	Ids = getIDCacheForCounters(Machine,Counters),
	PerfResult = #perf_result{
												counterType = "",
												measurement = 0,
												baseMeasurement = 0,
												lastMeasurement = 0,
												lastBaseMeasurement = 0,
												measurementTime = 0,
												measurementFreq = 1,
												measurementTicks = 0,
												lastMeasurementTime = LastMeasurementTime,
												lastMeasurementTicks = LastMeasurementTicks,
												percent = false,
												perSec = false,
												precision = 0,
												value = 0.0
												},
	{Flag,NewPerfResult,Array} = lableloop(0,true,Ids,Machine,Counters,PerfResult,[]),
	StateString = make_state_string(Array,Flag,NewPerfResult,1,PageCounters),
	THIS:set_attribute(?STATE_STRING,StateString).

%%orginal is n=0,flag=true
lableloop(2,Flag,_,_,_,PerfResult,Array)->{Flag,PerfResult,Array};
lableloop(_,false,_,_,_,PerfResult,Array)->{false,PerfResult,Array};
lableloop(N,_,Map,Machine,Counters,PerfResult,Array)->
	if
		N=/=0 ->
			platform:sleep(4000);
		true ->
			ok
	end,
	Ret = getPerfData(Machine,Counters,THIS,Map),
%% 	io:format("ret is:~p~n",[Ret]),
	PerfFreq = proplists:get_value("PerfFreq",Ret),
	PerfTime100nSec = proplists:get_value("PerfTime100nSec",Ret),
	PerfTime = proplists:get_value("PerfTime",Ret),
	PerfResult1 = PerfResult#perf_result{
																		measurementTime = string2val(PerfTime100nSec),
																		measurementTicks = string2val(PerfTime),
																		measurementFreq = string2val(PerfFreq)
																		},
	TnSec = PerfResult1#perf_result.measurementTime - PerfResult1#perf_result.lastMeasurementTime,
	TSec = PerfResult1#perf_result.measurementTicks - PerfResult1#perf_result.lastMeasurementTicks,
	Flag1 = (TnSec=<0) orelse (TSec=<0) orelse (PerfResult1#perf_result.lastMeasurementTime=<0) orelse (PerfResult1#perf_result.lastMeasurementTicks=<0),
	%%io:format("flag is:~p~n",[Flag1]),
	if
		N>0 ->
		%%jump out the loop
			lableloop(2,Flag1,Map,Machine,Counters,PerfResult1,Ret);
		true ->
			if
				(not Flag1) ->
					lableloop(N+1,Flag1,Map,Machine,Counters,PerfResult1,Ret);
				true ->
                    %%io:format("when change counters this step should get in~n"),
					PerfResult2 = PerfResult1#perf_result{
																						lastMeasurementTime = PerfResult1#perf_result.measurementTime,
																						lastMeasurementTicks = PerfResult1#perf_result.measurementTicks
																						},
					set_measure_value(Ret,1),
					lableloop(N+1,Flag1,Map,Machine,Counters,PerfResult2,Array++Ret)
			end
	end.

string2val(Str)->
	case string:to_integer(Str) of
		{I,[]}->
			I;
		_->
			0
	end.
	
getMeasurement(N,N1) ->
	case string:to_float(N) of
		{error,_} ->
			0;
		{Num,_}->
			NN = round((Num/N1)*10),
			if
				NN>10 ->
					10;
				NN<0 ->
					0;
				true ->
					NN
			end
	end.

set_measure_value([],_)->ok;
set_measure_value([{"PerfFreq",_}|T],I)->set_measure_value(T,I);
set_measure_value([{"PerfTime100nSec",_}|T],I)->set_measure_value(T,I);
set_measure_value([{"PerfTime",_}|T],I)->set_measure_value(T,I);
set_measure_value([{_,V}|T],I)->
	case element(3,V) of
		"n/a"->
			THIS:set_attribute(list_to_atom("lastMeasurement"++integer_to_list(I)),0),
			THIS:set_attribute(list_to_atom("lastBaseMeasurement"++integer_to_list(I)),0);
		_->
			THIS:set_attribute(list_to_atom("lastMeasurement"++integer_to_list(I)),string2val(element(1,V))),
			THIS:set_attribute(list_to_atom("lastBaseMeasurement"++integer_to_list(I)),string2val(element(2,V)))
	end,
	set_measure_value(T,I+1).


make_state_string([],_,PerfResult,_,_)->
	THIS:set_attribute(lastMeasurementTime,PerfResult#perf_result.measurementTime),
	THIS:set_attribute(lastMeasurementTicks,PerfResult#perf_result.measurementTicks),
	"";
make_state_string([{"PerfFreq",_}|T],Flag,PerfResult,N,Counters)->THIS:make_state_string(T,Flag,PerfResult,N,Counters);
make_state_string([{"PerfTime100nSec",_}|T],Flag,PerfResult,N,Counters)->THIS:make_state_string(T,Flag,PerfResult,N,Counters);
make_state_string([{"PerfTime",_}|T],Flag,PerfResult,N,Counters)->THIS:make_state_string(T,Flag,PerfResult,N,Counters);
make_state_string([{_,V}|T],Flag,PerfResult,N,Counters)->
	PerfCounter = element(2,lists:nth(N,Counters)),
    Key = element(1,lists:nth(N,Counters)),
	CounterType = element(3,V),
	Measurement = string2val(element(1,V)),
	BaseMeasurement = string2val(element(2,V)),
   LastMeasurement =  case THIS:get_attribute(list_to_atom("lastMeasurement"++integer_to_list(N))) of
        {ok,{_,LM}} ->
            LM;
        _ ->
            0
        end,
	LastBaseMeasurement = case THIS:get_attribute(list_to_atom("lastBaseMeasurement"++integer_to_list(N))) of
        {ok,{_,LBM}} ->
            LBM;
        _ ->
           0
       end,
	PerfResult1 = PerfResult#perf_result{
																		precision = 2,
																		percent = false,
																		perSec = false,
																		counterType = CounterType,
																		measurement = Measurement,
																		baseMeasurement = BaseMeasurement,	
																		lastMeasurement = LastMeasurement,
																		lastBaseMeasurement = LastBaseMeasurement,
                                                                        %%java中0.0/0.0表示一个非数字
																		value = "n/a"
																		},
	F1 = PerfResult1#perf_result.counterType=/="n/a",
	F2 = ((not Flag) and (PerfResult1#perf_result.measurement>=0)),
	PerfResult2 = if
		(F1 and F2) ->
			calcPerfResult(PerfResult1);
		true ->
			PerfResult1
	end,
	case is_number(PerfResult2#perf_result.value) of
		false->
            THIS:inc_attribute(contersInError),
			THIS:set_attribute(lastMeasurementTime,0),
			THIS:set_attribute(list_to_atom("value"++integer_to_list(N)),"n/a"),
			THIS:set_attribute(list_to_atom("measurement"++integer_to_list(N)),0),
			THIS:set_attribute(list_to_atom("lastMeasurement"++integer_to_list(N)),0),
            THIS:set_attribute(bc(PerfCounter),"n/a"),
			S4 = PerfCounter#perf_counter.object++":"++PerfCounter#perf_counter.counterName ++ "=" ++ "n/a" ++ "<br>";
		_->
			VV = PerfResult2#perf_result.value / 1,
            THIS:set_attribute(bc(PerfCounter),VV),
			PP = case PerfResult2#perf_result.precision of
				0 ->
					integer_to_list(round(VV));
				_->
                    httputils:floatToString(VV,PerfResult2#perf_result.precision)
			end,
            NewPP = if
                PerfResult2#perf_result.percent ->
                    PP++"%";
                PerfResult2#perf_result.perSec ->
                    PP++"/sec";
                true ->
                    PP
            end,
			THIS:set_attribute(list_to_atom("value"++integer_to_list(N)),PP),
			THIS:set_attribute(list_to_atom("lastMeasurement"++integer_to_list(N)),PerfResult2#perf_result.measurement),
			THIS:set_attribute(list_to_atom("lastBaseMeasurement"++integer_to_list(N)),PerfResult2#perf_result.baseMeasurement),
			THIS:set_attribute(list_to_atom("measurement"++integer_to_list(N)),getMeasurement(PP,10)),
			S4 = PerfCounter#perf_counter.object++":"++PerfCounter#perf_counter.counterName++":"++PerfCounter#perf_counter.instance++" = "++NewPP++"<br>"
	end,
	S4++make_state_string(T,Flag,PerfResult2,N+1,Counters).
    
bc(F = #perf_counter{}) ->
    F#perf_counter.object ++ " -- " ++ F#perf_counter.counterName ++ " -- " ++ F#perf_counter.instance;
bc(_) ->
    "".
	
%% @spec calcPerfResult(PerfResult) -> NewPerfResult
%% where
%% PerfResult = record()
%% NewPerfResult = record()
%% @doc parse content type for each counters' data,for different content type there are different arithmetic and required precision.
calcPerfResult(PerfResult) ->
	L = PerfResult#perf_result.measurementTime - PerfResult#perf_result.lastMeasurementTime,
	L1 = PerfResult#perf_result.measurementTicks - PerfResult#perf_result.lastMeasurementTicks,
	F = L1/PerfResult#perf_result.measurementFreq,
	L2 = PerfResult#perf_result.measurement - PerfResult#perf_result.lastMeasurement,
	L3 = PerfResult#perf_result.baseMeasurement - PerfResult#perf_result.lastBaseMeasurement,
%% 	io:format("L2:~p~n", [PerfResult#perf_result.lastMeasurement]),
%% 	io:format("L3:~p~n", [PerfResult#perf_result.lastBaseMeasurement]),
	%%{value,perSec,percent,precision}
	S = PerfResult#perf_result.counterType,
	Result = if
		S=:="PERF_COUNTER_COUNTER" ->
			if
				L2>=0 ->
					if
						PerfResult#perf_result.lastMeasurement =:= 0 ->
							{0.0,true,PerfResult#perf_result.percent,PerfResult#perf_result.precision};
						true ->
							{L2/F,true,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
					end;
				true ->
					{PerfResult#perf_result.value,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S=:="PERF_COUNTER_TIMER" ->
			if
				L2>=0 ->
					{L2/L1,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{PerfResult#perf_result.value,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S=:="PERF_COUNTER_TIMER_INV"->
			if 
				L2 >=0 ->
					{1 - L2/F,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S=:="PERF_COUNTER_BULK_COUNT" ->
			if
				L2 >= 0->
					{L2/F,true,PerfResult#perf_result.percent,PerfResult#perf_result.precision};
				true ->
					{PerfResult#perf_result.value,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S=="PERF_COUNTER_RAWCOUNT" orelse S=="PERF_COUNTER_LARGE_RAWCOUNT" orelse S=="PERF_COUNTER_RAWCOUNT_HEX" orelse S == "PERF_COUNTER_LARGE_RAWCOUNT_HEX" ->
					{PerfResult#perf_result.measurement,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,0};
		%%!!!!!!!!!!!!!!!!may be a mistake
		%%%
		S== "PERF_ELAPSED_TIME" ->
			L4 = if
				((PerfResult#perf_result.measurement > PerfResult#perf_result.measurementTicks) orelse PerfResult#perf_result.measurement < 0) ->
					(PerfResult#perf_result.measurement bsr 32);
				true ->
					PerfResult#perf_result.measurement
			end,
			{(PerfResult#perf_result.measurementTicks - L4)/ PerfResult#perf_result.measurementFreq,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,0};
		%% 分母为0时，直接把结果作为n/a
		%%%
		S == "PERF_RAW_FRACTION" -> 
			if 
				PerfResult#perf_result.measurement =:= 0 ->
					{0,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
                    if
                        PerfResult#perf_result.baseMeasurement =/= 0 ->
                            {PerfResult#perf_result.measurement / PerfResult#perf_result.baseMeasurement,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
                        true ->
                            {"n/a",PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision}
                    end
			end;
			S == "PERF_LARGE_RAW_FRACTION" -> 
			if 
				PerfResult#perf_result.measurement =:= 0 ->
					{0,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
                    if
                        PerfResult#perf_result.baseMeasurement =/= 0 ->
                            {PerfResult#perf_result.measurement / PerfResult#perf_result.baseMeasurement,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
                        true ->
                            {"n/a",PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision}
                    end
			end;
		%%%
		S == "PERF_SAMPLE_FRACTION"->
			if
				L2 =< 0 orelse L3 =< 0 ->
					{0.0,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{L2 / L3, PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision}
			end;
		S == "PERF_SAMPLE_COUNTER" ->
			if 
				L2 >= 0 andalso L3 > 0 ->
					{L2 / L3, PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_AVERAGE_TIME" ->
            if
                PerfResult#perf_result.baseMeasurement =:= 0->
                    {"n/a",PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision};
                true ->
                    {PerfResult#perf_result.measurement / PerfResult#perf_result.baseMeasurement,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
            end;
		S == "PERF_AVERAGE_BULK" ->
			if
				((L2>=0) and (L3>0)) ->
					{L2 / L3,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_100NSEC_TIMER"->
			if
				L2 >= 0 ->
					{L2/L,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{PerfResult#perf_result.value,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_100NSEC_TIMER_INV" ->
			if 
				L2 >= 0 ->
					{1- L2/L,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_COUNTER_MULTI_TIMER" ->
			if 
				L2 >= 0 andalso PerfResult#perf_result.baseMeasurement=/=0 ->
					{L2/F/PerfResult#perf_result.baseMeasurement,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
                L2 >= 0 ->
                    {0.0,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_COUNTER_MULTI_TIMER_INV" ->
			if 
				L2 >= 0 andalso PerfResult#perf_result.baseMeasurement=/=0 ->
					{1-L2/F/PerfResult#perf_result.baseMeasurement,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
                L2 >= 0 ->
                    {0.0,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_100NSEC_MULTI_TIMER" ->
			if 
				L2 >= 0 andalso PerfResult#perf_result.baseMeasurement=/=0 ->
					{L2/L/PerfResult#perf_result.baseMeasurement,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
                L2 >= 0 ->
                    {0.0,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_100NSEC_MULTI_TIMER_INV" ->
			if 
				L2 >= 0 andalso PerfResult#perf_result.baseMeasurement=/=0 ->
					{1-L2/L/PerfResult#perf_result.baseMeasurement,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
                L2 >= 0 ->
                    {0.0,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
				true ->
					{0.0,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
			end;
		S == "PERF_COUNTER_QUEUELEN_TYPE" ->
			{(PerfResult#perf_result.lastMeasurement + (PerfResult#perf_result.measurementTime * PerfResult#perf_result.measurement)) / L,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision};
		S == "PERF_COUNTER_LARGE_QUEUELEN_TYPE" ->
			{(PerfResult#perf_result.lastMeasurement + (PerfResult#perf_result.measurementTime * PerfResult#perf_result.measurement)) / L,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision};
		((S == "PERF_PRECISION_100NSEC_TIMER") and L2>=0) ->
            if
                L3=/=0 ->
                    {L2/L3,PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision};
                true ->
                    {"n/a",PerfResult#perf_result.perSec,true,PerfResult#perf_result.precision}
            end;
		true ->
			{PerfResult#perf_result.value,PerfResult#perf_result.perSec,PerfResult#perf_result.percent,PerfResult#perf_result.precision}
	end,
	
	NewResult = case element(3,Result) of
		true ->
			RR = setelement(4,Result,2),
            case element(1,Result) of
                "n/a" ->
                    Result;
                _->
                    setelement(1,RR,element(1,Result)*100)
            end;
		_->
			Result
	end,
	
	NewPerfResult = PerfResult#perf_result{
																			value = element(1,NewResult),
																			perSec = element(2,NewResult),
																			percent = element(3,NewResult),
																			precision = element(4,NewResult)
																			},
	NewPerfResult.

%% @spec getIDCacheForCounters(Host,PerfCounters) -> list()
%% where
%% Host = string()
%% PerfCounters = [record()]
%% @doc parse counters data and get the id for counters.
getIDCacheForCounters(Host,PerfCounters)->
	Hmap = [{string:to_lower(X#perf_counter.counterName),X#perf_counter.counterName}||X<-PerfCounters] ++
			[{string:to_lower(X#perf_counter.object),X#perf_counter.object}||X<-PerfCounters],
	
	Cmd = platform:perfexCommand1(Host) ++ " -map",
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd),
	Result = string:tokens(Rets,"\r\n"),
%% 	io:format("Result:~p~n", [Result]),
%% 	io:format("Hmap:~p~n", [Hmap]),
	IDS=loop1(Result,Hmap),
%% 	io:format("IDS:~p~n", [IDS]),
	IDS.

loop1([],_)->[];
loop1([F|R],Map) ->
	case string:strip(F) of
		"id:"++Rest ->
			S = string:sub_string(Rest,1,string:str(Rest," ")-1),
			S1 = string:sub_string(Rest,string:str(Rest,"name:")+5,string:len(Rest)),
			%%io:format("s,s1,map~p:~p~n",[S,S1]),
			New = case proplists:get_value(string:to_lower(S1),Map) of
				undefined ->
					[];
				S2->
					[{S,S2}]
			end;
		_->
			New = []
	end,
	New++loop1(R,Map).

%% @spec getPerfData(Host,Counters,Monitor,Map) -> term()
%% where
%% Host = string()
%% PerfCounters = [record()]
%% Monitor = instance()
%% @doc filter counters data and get data for chosen counters.
getPerfData(Host,Counters,Monitor,Map)->
	%%io:format("map is:~p~n",[Map]),
	Flag = Map=/=[],
	THIS:set_attribute(hashmap,[]),
	THIS:set_attribute(i43,1),
	THIS:set_attribute(string40,""),
	THIS:set_attribute(string41,""),
	THIS:set_attribute(string42,""),
	THIS:set_attribute(string44,list_to_tuple(lists:duplicate(?MAX_COUNTER,""))),
	THIS:set_attribute(string45,list_to_tuple(lists:duplicate(?MAX_COUNTER,""))),
	THIS:set_attribute(bool,list_to_tuple(lists:duplicate(?MAX_COUNTER,false))),
	THIS:set_attribute(string,list_to_tuple(lists:duplicate(?MAX_COUNTER,""))),
	I = case length(Counters)>?MAX_COUNTER of
		true ->
			?MAX_COUNTER;
		_->
			length(Counters)
	end,
	Objects = filter_counters(1,[],Counters,I),
	Params = if
		Flag ->
			FMaps = filter_map(1,[],Map,length(Map)),
			make_params(Map,Counters,Objects,FMaps);
		true ->
			make_params4(Objects)
	end,
	Cmds = platform:perfexCommand1(Host) ++" " ++  Params,
%% 	io:format("command line is:~p~n",[Cmds]),
	CmdLine = command_line:new(),
	Ret = CmdLine:exec(Cmds),
	Result = string:tokens(Ret,"\r\n"),
	%%io:format("Result:~p~nmap:~p~n",[Result,Map]),
	THIS:set_attribute(bool57,false),
	THIS:set_attribute(string58,""),
	THIS:set_attribute(string59,""),
	V = parse_line(Result,Map,Counters),
	%%io:format("getperfdate is:~p~n",[V]),
	V.
	
make_result(NewBools,NewStrings,NewString44,NewString45,N,Len,Result)->
	if
		N>Len ->
			Result;
		true->
			Bool = element(N,NewBools),
			String = element(N,NewStrings),
			String44 = element(N,NewString44),
			String45 = element(N,NewString45),
			if
				Bool ->
					make_result(NewBools,NewStrings,NewString44,NewString45,N+1,Len,Result++[{N,{String44,String45,String}}]);
				true ->
					make_result(NewBools,NewStrings,NewString44,NewString45,N+1,Len,Result++[{N,{"n/a","n/a","n/a"}}])
			end
	end.


parse_line([],_,Counters)->
	{ok,{_,NewBools}} = THIS:get_attribute(bool),
	{ok,{_,NewStrings}} = THIS:get_attribute(string),
	{ok,{_,NewString44}} = THIS:get_attribute(string44),
	{ok,{_,NewString45}} = THIS:get_attribute(string45),
	Len = length(Counters),
	{ok,{_,Freq}} = THIS:get_attribute(string41),
	{ok,{_,Time100}} = THIS:get_attribute(string40),
	{ok,{_,Time}} = THIS:get_attribute(string42),
	make_result(NewBools,NewStrings,NewString44,NewString45,1,Len,[])++[{"PerfFreq",Freq},{"PerfTime100nSec",Time100},{"PerfTime",Time}];
parse_line([F1|R],Map,Counters) ->
	Flag = Map=/=[],
	F = string:strip(F1),
	Flag1 = httputils:startsWith(F,"PerfFreq: "),
	Flag2 = string:str(F,"PerfFreq: ")=/=0,
	Flag3 = httputils:startsWith(F,"PerfTime100nSec: "),
	Flag4 = string:str(F,"PerfTime100nSec: ")=/=0,
	Flag5 = httputils:startsWith(F,"PerfTime: "),
	Flag6 = string:str(F,"PerfTime: ")=/=0,
	Flag7 = httputils:startsWith(F,"name: "),
	Flag8 = httputils:startsWith(F,"object: "),
	Flag9 = httputils:endsWith(F,"_BASE"),
	{ok,{_,Bool57}} = THIS:get_attribute(bool57),
	{ok,{_,Time100}} = THIS:get_attribute(string40),
	{ok,{_,Freq}} = THIS:get_attribute(string41),
	{ok,{_,Time}} = THIS:get_attribute(string42),
	if
		((Freq=:="") and (Flag1 or Flag2)) ->
			THIS:set_attribute(string41,string:strip(string:sub_string(F,string:str(F,":")+1,string:len(F))));
		true ->
			ok
	end,
	if
		((Time100=:="") and (Flag3 or Flag4)) ->
			THIS:set_attribute(string40,string:strip(string:sub_string(F,string:str(F,":")+1,string:len(F))));
		true->
			ok
	end,
	if
		((Time=:="") and (Flag5 or Flag6)) ->
			THIS:set_attribute(string42,string:strip(string:sub_string(F,string:str(F,":")+1,string:len(F))));
		true->
			ok
	end,
	if
		Flag7 ->
			Name1 = string:strip(string:sub_string(F,string:str(F,": ")+2,string:len(F))),
			THIS:set_attribute(string58,Name1),
			{ok,{_,HashMap}} = THIS:get_attribute(hashmap),
			case string:to_integer(Name1) of
				{error,_}->
					THIS:set_attribute(hashmap,HashMap++[{Name1,Name1}]);
				_->
					ok
			end;
		true->
			ok
	end,
	if
		Flag8 ->
			Obj = string:strip(string:sub_string(F,string:str(F,": ")+2,string:rstr(F," ")-1)),
			Object1 = if
				Flag ->
					proplists:get_value(Obj,Map);
				true ->
					Obj
			end,
			%%io:format("object :~p~n",[Object1]),
			THIS:set_attribute(string59,Object1),
			THIS:set_attribute(hashmap,[]);
		true ->
			ok
	end,
	if
		Bool57 ->
			if
				Flag9 ->
					S64 = string:tokens(F," "),
					Len1 = length(S64),
					if
						Len1>2 ->
							{ok,{_,String45}} = THIS:get_attribute(string45),
							{ok,{_,I43}} = THIS:get_attribute(i43),
							THIS:set_attribute(string45,setelement(I43,String45,lists:nth(Len1-2+1,S64)));
						true ->
							ok
					end;
				true ->
					ok
			end,
			THIS:set_attribute(bool57,false);
		true ->
			ok
	end,
	{ok,{_,Object}} = THIS:get_attribute(string59),
	parse_line_for_counters(Counters,Object,F,Map=/=[],Map,1),
	parse_line(R,Map,Counters).

parse_line_for_counters([],_,_,_,_,_)->ok;
parse_line_for_counters([F|R],Object,CurrentLine,Flag,Map,N) ->
	Flag1 = string:to_lower(F#perf_counter.object) =:= string:to_lower(Object),
	if
		Flag1 ->
			Flag2 = false,
			CounterName = F#perf_counter.counterName,
			Flag3 = httputils:endsWith(CounterName,"*"),
			I = string:str(CurrentLine,":"),
			String68 = if
				I=/=0 ->
					string:sub_string(CurrentLine,1,I-1);
				true ->
					CurrentLine
			end,
			Obj = if
				Flag ->
					proplists:get_value(String68,Map);
				true ->
					String68
			end,
			Bool66 = if
				Flag3 ->
					if
						Obj=/=undefined ->
							httputils:startsWith(Obj,string:sub_string(CounterName,1,string:len(CounterName)-1));
						true ->
							Flag2
					end;
				true ->
					CounterName =:= Obj
			end,
			{ok,{_,Bools}} = THIS:get_attribute(bool),
			BB = element(N,Bools),
			{ok,{_,String58}} = THIS:get_attribute(string58),
			BB1 = string:to_lower(String58) =:= string:to_lower(F#perf_counter.instance),
			if
				Bool66 ->
					if
						F#perf_counter.instance =:= "*total*" ->
							Bool71 = true,
							Bool70 = true;
						(not BB) ->
							if
								F#perf_counter.instance =:= "" ->
									Bool70 = true;
								BB1 ->
									Bool70 = true;
								true ->
									Bool70 = false
							end,
							Bool71 = false;
						true ->
							Bool71 = false,
							Bool70 = false
					end,
					{ok,{_,Strings}} = THIS:get_attribute(string),
					if
						Bool70 ->
							THIS:set_attribute(bool,setelement(N,Bools,true)),
							THIS:set_attribute(bool57,true),
							THIS:set_attribute(i43,N),
							if
								I=/=0 ->
									NewArray = string:tokens(string:sub_string(CurrentLine,I+1,string:len(CurrentLine))," "),
									NewLen = length(NewArray),
									if
										NewLen =:=2 ->
											THIS:set_attribute(string,setelement(N,Strings,lists:nth(2,NewArray)));
										NewLen =:=3 ->
											THIS:set_attribute(string,setelement(N,Strings,lists:nth(3,NewArray)));
										true->
											ok
									end,
									{ok,{_,String44}} = THIS:get_attribute(string44),
									{ok,{_,String45}} = THIS:get_attribute(string45),
									S44 = if
										Bool71 ->
											integer_to_list(string2val(lists:nth(1,NewArray))+string2val(element(N,String44)));
										true ->
											lists:nth(1,NewArray)
									end,
									THIS:set_attribute(string44,setelement(N,String44,S44)),
									THIS:set_attribute(string45,setelement(N,String45,""));
								true ->
									ok
							end;
						true->
							parse_line_for_counters(R,Object,CurrentLine,Flag,Map,N+1)
					end;
				true->
					parse_line_for_counters(R,Object,CurrentLine,Flag,Map,N+1)
			end;
		true->
			parse_line_for_counters(R,Object,CurrentLine,Flag,Map,N+1)
	end.
	

filter_counters(I,Ret,_,Max) when I > Max ->Ret;
filter_counters(I,Ret,Counters,Max)->
	C = lists:nth(I,Counters),
	case lists:keysearch(C#perf_counter.object,1,Ret) of
		{value,{_,Cs}} ->
			filter_counters(I+1,lists:keyreplace(C#perf_counter.object,1,Ret,{C#perf_counter.object,Cs++[C]}),Counters,Max);
		_->
			filter_counters(I+1,Ret++[{C#perf_counter.object,[C]}],Counters,Max)
	end.


filter_map(I,Ret,_,Max) when I > Max ->Ret;
filter_map(I,Ret,Maps,Max)->
	C = lists:nth(I,Maps),
	case lists:keysearch(element(2,C),1,Ret) of
		{value,{_,Mids}} when Mids=/="" ->
			filter_map(I+1,lists:keyreplace(element(2,C),1,Ret,{element(2,C),Mids ++ "," ++ element(1,C)}),Maps,Max);
        {value,{_,Mids}} ->
			filter_map(I+1,lists:keyreplace(element(2,C),1,Ret,{element(2,C),Mids ++ element(1,C)}),Maps,Max);
		_ ->
			filter_map(I+1,Ret++[{element(2,C),element(1,C)}],Maps,Max)
	end.

%%map counters,Object,Mapid
make_params([],_,_,_)->"";
make_params([{Id,Name}|R],Counters,Object,MapId)->
	case proplists:get_value(Name,Object) of
		undefined ->
			make_params(R,Counters,Object,MapId);
		List ->
			"-oic \"" ++ Id ++ "/" ++ clear(make_params2(List,[]),",") ++ "/" ++ clear(make_params3(List,MapId),",") ++ 
			"\" " ++ make_params(R,Counters,Object,MapId)
	end.

make_params2([],_)->"";
make_params2([C=#perf_counter{instance=undefined}|T],Hashset)->
	make_params2(T,Hashset);
make_params2([C=#perf_counter{instance=""}|T],Hashset)->
	make_params2(T,Hashset);
make_params2([C|T],Hashset)->
	Flag = lists:any(fun(X)->X=:=C#perf_counter.instance end,Hashset),
	if
		Flag ->
			make_params2(T,Hashset);
		true ->
			C#perf_counter.instance++","++make_params2(T,Hashset++[C#perf_counter.instance])
	end.


make_params3([],_)->"";
make_params3([C|T],Map)->
	proplists:get_value(C#perf_counter.counterName,Map,"")++","++make_params3(T,Map).


make_params4([])->"";
make_params4([{Obj,_}|T])->
	"-o " ++ "\"" ++ Obj ++ "\" "++ make_params4(T).
	
make_params5([])->"";
make_params5([Obj|T])->
	"-o \"" ++ Obj ++ "\" "++ make_params5(T).
	

clear(S,S1)->
	F1 = httputils:startsWith(S,S1),
	F2 = httputils:endsWith(S,S1),
	SS = if
		F1->
			string:sub_string(S,1+string:len(S1),string:len(S));
		true ->
			S
	end,
	SS1 = if
		F2->
			string:sub_string(SS,1,string:len(SS)-string:len(S1));
		true ->
			SS
	end,
	SS1.
	

