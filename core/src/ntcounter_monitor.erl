%%
%% diskspace_monitor
%%
%%
-module(ntcounter_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MAX_COUNTER,10).


new()->
	Base = server_monitor:new(),
	Base:set_attribute(lastMeasurementTime,0),
	Base:set_attribute(lastMeasurementTicks,0),
	Base:set_attribute(lastMeasurementWasNotAvailable,false),
	Base:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	Base:set_attribute(lastBaseMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	Base:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	Base:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
	Base:set_attribute(contersInError,0),
	{?MODULE,Base}.


getCounters()->
	getCounters(THIS:get_property(pmcfile)).

getCounters({ok,{_,"(CustomObject)"}})->
	{ok,{_,Object}} = THIS:get_property(object),
	{ok,{_,Counter}} = THIS:get_property(counter),
	{ok,{_,Instance}} = THIS:get_property(instance),
	{ok,[#perf_counter{object=Object,counterName=Counter,instance=Instance}]};
getCounters({ok,{_,File}})->
	perfchartfile:get_settings(File);
getCounters(_)->
	{error,get_counters_error}.

update()->
	THIS:set_attribute(contersInError,0),
	{ok,{_,Machine}}=THIS:get_property(machine),
	{ok,{_,LastMeasurementTime}} = THIS:get_attribute(lastMeasurementTime),
	{ok,{_,LastMeasurementTicks}} = THIS:get_attribute(lastMeasurementTicks),
	{ok,{_,LastMeasurementWasNotAvailable}} = THIS:get_attribute(lastMeasurementWasNotAvailable),
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
					Ids = ntcounter_util:getIDCacheForCounters(Machine,Counters),
                    Ret = ntcounter_util:getPerfData(Machine,Counters,THIS,Ids),
					if Ids /= [] ->                                            
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
									    {Measure,MeasureBase} = THIS:set_measure_value(Ret,M1,M2,1),

									    platform:sleep(1000),
									
									    Ret2 = ntcounter_util:getPerfData(Machine,Counters,THIS,Ids),
									
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
					    end;
                    true ->
					    THIS:set_attribute(lastMeasurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
					    THIS:set_attribute(measurement,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
					    THIS:set_attribute(values,list_to_tuple(lists:duplicate(?MAX_COUNTER,"n/a"))),
					    THIS:set_attribute(?NO_DATA,true),
					    THIS:set_attribute(?CATEGORY,?NO_DATA),
					    THIS:set_attribute(contersInError,?MAX_COUNTER),
                        THIS:set_attribute(?STATE_STRING,make_empty_string(Counters))                    
                    end 
			end
	end,
	{ok,update_ok}.

string2val(Str)->
	case string:to_integer(Str) of
		{I,[]}->
			I;
		_->
			-1
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
							FResult *100;
						true ->
							FResult
					end
				end,
            THIS:set_attribute(list_to_atom(K),Result),            
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
							K ++ "=" ++ lists:flatten(io_lib:format("~.2f",[(Result*Scale/1.0)])) ++ Unit1 ++ Unit2 ++ "<br>" ++THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
						Cur == Max ->
							K ++ "=" ++ lists:flatten(io_lib:format("~.2f",[(Result*Scale/1.0)])) ++ Unit1 ++ Unit2 ++ "..." ++ THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
						true ->
							THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max)
					end;
				true ->
					if
						Cur < Max ->                       
							K ++ "= n/a<br>"++ THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
						Cur == Max ->
							K ++ "= n/a ..."++ THIS:make_state_string(T,Measure,MeasureBase,Scale,DefaLastMeasure,Tab,Cur+1,Max);
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

getScalarValues(Prop,Params)->
	case Prop of
		pmcfile->             
			Files = filelib:wildcard(?TEMPLATES_PERFMON ++ "/*.pmc") ++
					filelib:wildcard(?TEMPLATES_PERFMON ++ "/*.PMC") ++
					filelib:wildcard(?TEMPLATES_PERFMON ++ "/*.pmw") ++
					filelib:wildcard(?TEMPLATES_PERFMON ++ "/*.PMW"),      
			[{filename:basename(X),filename:basename(X)}|| X<-Files] ++ [{"(Custom Object)","(CustomObject)"}];
		scale->
			[
			{"10000000","10000000"},
			{"1000000","1000000"},
			{"100000","100000"},
			{"10000","10000"},
			{"1000","1000"},
			{"100","100"},
			{"10","10"},
			{"1","1"},
			{"0.1","0.1"},
			{"0.01","0.01"},
			{"0.001","0.001"},
			{"0.0001","0.0001"},
			{"0.00001","0.00001"},
			{"0.000001","0.000001"},
			{"0.0000001","0.0000001"},
			{"0.00000001","0.00000001"},
			{"kilobytes","0.0009765625"},
			{"megabytes","9.536743E-007"}
			];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{contersInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{contersInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{contersInError,'==',0}]
	end.

verify(Params)->
	Errs=
	case proplists:get_value(object,Params) of
		""->
			case proplists:get_value(pmcfile,Params) of
				"(CustomObject)"->
					[{object,"Object missing"}];
				_->
					[]
			end;
		_->
			[]
	end ++
	case proplists:get_value(counter,Params) of
		""->
			case proplists:get_value(pmcfile,Params) of
				"(CustomObject)"->
					[{counter,"Counter missing"}];
				_->
					[]
			end;
		_->
			[]
	end ++
	case regexp:match(proplists:get_value(scale,Params),"^[0-9]+[\.]*[0-9]*$") of
		nomatch ->
			[{scale,"scale must be a number"}];
		_->
			[]
	end ++
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true ->
			{ok,""}
	end.

remoteCommandLineAllowed()->
	false.

get_template_property()->
	BASE:get_template_property() ++  
	[
	#property{name=contersInError,title="counters in error",type=numeric,state=true,configurable=false},    
	#property{name=pmcfile,title="PerfMon Chart File",type=scalar,default="System.pmc",description="a Performance Monitor setting file that specifies the counters (from templates.perfmon directory)"},
	#property{name=object,title="Object",type=text,advance=true,order=1,description="this is same as the name of the object in the NT Performance Monitor under \"Add to Chart\""},
	#property{name=counter,title="Counter",type=text,advance=true,order=1,description="this is same as the name of the counter in the NT Performance Monitor under \"Add to Chart\""},
	#property{name=instance,title="Instance",type=text,advance=true,order=1,description="this is same as the instance in the NT Performance Monitor under \"Add to Chart\". Leaving this blank will choose the first instance in the list, if there are multiple instances."},
	#property{name=units,title="Units",type=text,advance=true,order=1,description="optional units string to append when displaying the value of this counter."},
	#property{name=scale,title="Scale",type=scalar,advance=true,allowother=true,order=1,default="1",description="optional scale multiplier for this counter - for example, you may want to scale a disk usage counter by megabytes to make the output more readable."}    
    ].

getStateProperties(This,Params) ->
    Temp = This:get_template_property(),
	T = [X || X<-Temp,X#property.state=:=true],
	case proplists:get_value(pmcfile,Params) of
    undefined ->        
        case  This:get_property(pmcfile) of
        {ok,{_,Pmcfile}} ->
            case perfchartfile:get_settings(Pmcfile) of
            {ok,List} ->
                T ++ make_template(List) ;  
            _ ->
                T
            end;
        _ ->
            T 
        end; 
    File->
        case perfchartfile:get_settings(File) of
        {ok,List} ->
            T ++ make_template(List) ;  
        _ ->
            T
        end        
    end.        
 
make_template(List) ->
    make_template_t(List,length(List),[]).
make_template_t(_L,0,Template) -> Template; 
make_template_t([A|B],Num,Te) ->
    {_,_,N,In,_,_} = A,
    case In of
    [] ->
        Temp =  #property{name=list_to_atom(N),title=N,type=numeric,state=true,configurable=false},    
        make_template_t(B,Num-1,[Temp|Te]);
    _ ->
        Temp = #property{name=list_to_atom(N ++":" ++In),title=N++":"++In,type=numeric,state=true,configurable=false}, 
        make_template_t(B,Num-1,[Temp|Te]) 
    end.
   
make_empty_string([F|RecordList]) ->     
    make_empty_string_t(RecordList,length(RecordList),F#perf_counter.object++":"++F#perf_counter.counterName++"="++"n/a").
make_empty_string_t(_L,0,S) -> S;    
make_empty_string_t([A|B],Num,Str) ->
    make_empty_string_t(B,Num-1,Str ++ "<br>" ++A#perf_counter.object++":"++A#perf_counter.counterName++"="++"n/a").
