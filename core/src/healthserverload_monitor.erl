%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Health Server Load Monitor.
%%
%% Description: Monitor the health of server load
%% Versions supported: N/A
%% Platform: Windows
%%
-module(healthserverload_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MaxCounters,40).
-define(PROPERTY_NAME_COUNTER_VALUE,"browsableValue").
-define(PROPERTY_NAME_COUNTER_NAME,"_browseName").

%% @spec new() -> ok
%% @doc initialization health serverload monitor.
new() ->
    Base = browsable_base:new(),
    Base:set_property(pROPERTY_NAME_COUNTER_NAME,"_browseName"),
    Base:set_property(pROPERTY_NAME_COUNTER_ID,"_browseNameid"),
    Base:set_property(pROPERTY_NAME_COUNTER_VALUE,"browsableValue"),
    Base:set_property(pROPERTY_NAME_COUNTERS_IN_ERROR,"countersInError"),
    Base:set_property(pROPERTY_NAME_BROWSABLE,"_browse"),
    Base:set_property(pROPERTY_VALUE_BROWSABLE,"browseName"),
    Base:set_property(cOUNTER_PROPERTY_INDEX,100),
    Base:set_attribute(pLastMeasurementTime,0),
    Base:set_attribute(pLastMeasurementTicks,0),
    Base:set_attribute(pLastMeasurementWasNA,0),
    Base:set_attribute(pValues,lists:duplicate(?MaxCounters,"n/a")),
    Base:set_attribute(pMeasurements,lists:duplicate(?MaxCounters,0)),
    Base:set_attribute(pLastMeasurements,lists:duplicate(?MaxCounters,0)),
    Base:set_attribute(pLastBaseMeasurements,lists:duplicate(?MaxCounters,0)),
    {?MODULE,Base}.
    
%% @spec update() -> ok
%% @doc overloading function,monitor update data.
update() ->
    %{ok,{_,Host}} = THIS:get_property(machine),
    %{ok,{_,File}} = THIS:get_property(pFile),
    Host = "",
    File = "templates.health/health.xml",
    {ok,{_,LastMeasurementTime}} = THIS:get_attribute(pLastMeasurementTime),
    {ok,{_,LastMeasurementTicks}} = THIS:get_attribute(pLastMeasurementTicks),
    {ok,{_,LastMeasurementWasNA}} = THIS:get_attribute(pLastMeasurementWasNA),
    THIS:set_attribute(countersInError,0),
    Flag = (LastMeasurementWasNA == 1),
    {ok,XML} = file:read_file(File),
    {ok,D,_} = erlsom:simple_form(XML),
    List = findCounters(D),
    buildBrowseString(D,List),
    {ok,{_,Browse}} = THIS:get_property(browse),
    if length(Browse) > ?MaxCounters ->
		    THIS:set_attribute(?NO_DATA,true),
		    THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(countersInError,?MaxCounters),
		    THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MaxCounters])));
    true ->
        {I,Stringbuffer,Array,Vector,Vector1} = update_util_1(Browse), 
        if length(Array) == 0 ->
            THIS:set_attribute(pLastMeasurementTime,0),
            THIS:set_attribute(pValues,list_to_tuple(lists:duplicate(?MaxCounters,"n/a"))),
            THIS:set_attribute(pMeasurements,list_to_tuple(lists:duplicate(?MaxCounters,0))),
            THIS:set_attribute(pLastMeasurements,list_to_tuple(lists:duplicate(?MaxCounters,0)));
        true ->                        
            nothing
        end,
        %NtCounterBase = ntcounter_base:new(),      
        IDMap = ntcounter_util:getIDCacheForCounters(Host,Array),
        {F,L4,L2,L5,L6,L3,Flag1,Array1,Al,Al2} = update_util_2(Host,Array,IDMap,LastMeasurementTime,LastMeasurementTicks,Flag), 
        %io:format("Al:~p~nAl2:~p~n",[Al,Al2]),
        %io:format("111"),
        List1 = lists:sublist(Array1,4,100000),
        %io:format("111"),
        %%io:format("Vector:~p~nVector1:~p~nL2:~p~nL3:~p~nL5:~p~nL6:~p~nF:~p~nStringbuffer:~p~nArray:~p~nList1:~p~nAl:~p~nAl2:~p~nFlag1:~p~n",[Vector,Vector1,L2,L3,L5,L6,F,Stringbuffer,Array,List1,Al,Al2,Flag1]),
        _Stringbuffer = update_util_4(Vector,Vector1,L2,L3,L5,L6,F,Stringbuffer,Array,List1,Al,Al2,Flag1),        
        THIS:set_attribute(pLastMeasurementTime,L2),
        THIS:set_attribute(pLastMeasurementTicks,L4),
        if length(_Stringbuffer) > 0 ->
            THIS:set_attribute(?STATE_STRING,_Stringbuffer);
        true ->
            THIS:set_attribute(?STATE_STRING,"no data") 
        end,
        if Flag1 ->
            THIS:set_attribute(?NO_DATA,true);
        true ->
            nothing
        end    
    end.
    

update_util_1(BrowseList) ->
    update_util_1_t(BrowseList,length(BrowseList),0,"",[],[],[]).
update_util_1_t(_B,0,Peri,Stringbuffer,Array,Vector,Vector1) -> {Peri,Stringbuffer,Array,Vector,Vector1};
update_util_1_t(BL,Num,Per,Sbuf,TArray,V,V1) ->
    [A|B] = BL,
    {Browseid,CounterName} = A,
    {ok,{_,Perfcounter}} = THIS:get_property(Browseid),
    CounName = ?PROPERTY_NAME_COUNTER_NAME ++ integer_to_list(Per+1),
    THIS:set_attribute(CounName,"n/a"),
    update_util_1_t(B,Num-1,Per,Sbuf,lists:append(TArray,[Perfcounter]),lists:append(V,[CounterName]),lists:append(V1,[CounName])).   



%getPerfData(Host,Counters,Monitor,Map),只运行一次
update_util_2(Host,Array,IDMap,LastMeasurementTime,LastMeasurementTicks,Flag) ->
    update_util_2_t(Host,Array,IDMap,LastMeasurementTime,LastMeasurementTicks,true,Flag,0,false,0.0,0,0,0,0,0,[],[],[]).
update_util_2_t(_Host,_Array,_IDMap,_LastMeasurementTime,_LastMeasurementTicks,_Flag1,_Flag,_N,true,_F,L4,L2,L5,L6,L3,Array,_Al,_Al2) -> {_F,L4,L2,L5,L6,L3,_Flag1,Array,_Al,_Al2};
update_util_2_t(Host1,Array1,IDMap1,LastMeasurementTime1,LastMeasurementTicks1,TFlag1,TFlag,N,Fl,Float,TL4,TL2,TL5,TL6,TL3,Tarray,TAl,TAl2) ->    
    if TFlag1 == true,N < 2 ->
        if N /= 0 ->
            platform:sleep(4000);
        true ->
            nothing
        end,
        Array = ntcounter_util:getPerfData(Host1,Array1,THIS,IDMap1), 
        %io:format("!!!!!!!!!!Array:~p~n",[Array]),        
        case lists:keysearch("PerfTime100nSec",1, Array) of
        {value,{_,_TL2}} ->
            L2 = list_to_integer(_TL2);
        true ->
            L2 = 0
        end,
     
        case lists:keysearch("PerfTime",1, Array) of
        {value,{_,_TL4}} ->
            L4 = list_to_integer(_TL4);
        true ->
            L4 = 0
        end,
        case lists:keysearch("PerfFreq",1, Array) of
        {value,{_,_TL3}} ->
            L3 = list_to_integer(_TL3);
        true ->
            L3 = 1
        end,

        L5 = L2 - LastMeasurementTime1,

        L6 = L4 - LastMeasurementTicks1,

        F = (L6 / L3),

        Flag1 =  ((L5 =< 0) or (L6 =< 0) or (LastMeasurementTime1 =< 0) or TFlag or (LastMeasurementTicks1 =< 0)),
        if  N > 0 ->
            update_util_2_t(Host1,Array1,IDMap1,LastMeasurementTime1,LastMeasurementTicks1,Flag1,TFlag,N,true,F,L4,L2,L5,L6,L3,Array,TAl,TAl2);  
        true ->  
            if Flag1  == false ->               
                {ok,{_,Al}} = THIS:get_attribute(pLastMeasurements),
                {ok,{_,Al2}} = THIS:get_attribute(pLastBaseMeasurements),
                %io:format("!!!!!!!!!!Al:~p~n",[Al]),
                %io:format("!!!!!!!!!!Al2:~p~n",[Al2]),
                update_util_2_t(Host1,Array1,IDMap1,L2,L4,Flag1,TFlag,N+1,Fl,F,L4,L2,L5,L6,L3,Array,Al,Al2);
            true ->
                L = L2,
                L1 = L4,
                Flag = false,
                List1 = lists:sublist(Array,4,100000),
                {Al,Al2} = update_util_3(List1),           
                update_util_2_t(Host1,Array1,IDMap1,L2,L4,Flag1,Flag,N+1,Fl,F,L4,L2,L5,L6,L3,Array,Al,Al2)            
            end
        end;            
    true ->
        update_util_2_t(Host1,Array1,IDMap1,LastMeasurementTime1,LastMeasurementTicks1,TFlag1,TFlag,N,true,Float,TL4,TL2,TL5,TL6,TL3,Tarray,TAl,TAl2)
    end.

%
update_util_3(PerfData) ->
    update_util_3(PerfData,length(PerfData),[],[]).
update_util_3(_PerfD,0,Al,Al1) -> {Al,Al1};
update_util_3(PerfData1,Num,Tal,Tal1) ->
    [A|B] = PerfData1,   
    {_,{N1,N2,String1}} = A,
    if String1 == "n/a" ->   
        update_util_3(B,Num-1,lists:append(Tal,[0]),lists:append(Tal1,[0]));
    true ->
        if N1 == [] ->
            TN1 = 0;
        true ->
            TN1 = list_to_integer(N1)
        end,
        if N2 == [] ->
            TN2 = 0;
        true ->
            TN2  = list_to_integer(N2)
        end,       
        update_util_3(B,Num-1,lists:append(Tal,[TN1]),lists:append(Tal1,[TN2]))        
    end.   
        
%Arrry is  NtCounterBase1:getPerfData's value,but 3 

update_util_4(Vector,Vector1,L2,L3,L5,L6,F,Stringbuffer,PerfCounter,Array,Al,Al2,Flag1) ->  
    update_util_4_t(1,Vector,Vector1,L2,L3,L5,L6,F,Stringbuffer,PerfCounter,Array,Al,Al2,Flag1,length(Array),[]).
update_util_4_t(_J1,_Vector,_Vector1,_L2,_L3,_L5,_L6,_F,_Stringbuffer,_PerfCounter,_Array,_Al,_Al2,_Flag1,0,Al1) ->_Stringbuffer;
update_util_4_t(TJ1,TVector,TVector1,TL2,TL3,TL5,TL6,TF,TStringbuffer,TPerfCounter,TArray,TAl,TAl2,TFlag1,Num,TAl1) ->
    [A|B] = TArray,
    [Tper|Eper] = TPerfCounter,         
    [AlJ1|AlEnd] = TAl,
    [Al2J1|Al2End] = TAl2,   
    {String2,{TN1,TN2,String1}} = A,
    if (TN1 == []) or (TN1 == "n/a") ->
        N1 = "0";
    true ->
        N1 = TN1
    end,     
    if String1 == "n/a" ->
        if length(TStringbuffer) > 0 ->
            Stringbuffer = TStringbuffer ++ ", " ++ "<br>";
        true ->
            Stringbuffer = TStringbuffer
        end,
        L7 = 0,
        Flag2= false,
        Flag3= false,
        Byte0 = 2,
        TF1 = 0;
    true -> 

                
        L8 = (erlang:list_to_integer(N1) - AlJ1),
        if (TN2 == []) or (TN2 == "n/a") ->
            N2 = 0;
        true ->
            N2 = list_to_integer(TN2)
        end, 

        L7 = N2,         
        L9 = L7 - Al2J1,        

        if TFlag1 == false ->
            if length(TStringbuffer) > 0 ->
                Stringbuffer = TStringbuffer ++ ", " ++ "<br>";
            true ->
                Stringbuffer = TStringbuffer
            end,
            K = list_to_integer(N1),
            if  K >= 0 ->
                case String1 of
                "PERF_COUNTER_COUNTER" ->
                    if L8 >= 0 ->
                        if AlJ1 == 0 ->
                            F1 = 0;
                        true ->
                            F1 =  L8 / TF
                        end,
                        Flag3 = true;
                    true ->
                        F1 = 0,
                        Flag3 = false
                    end,
                    Flag2 = false,
                    Byte0 = 2;                    
                "PERF_COUNTER_TIMER" ->
                    if L8 >= 0 ->
                        F1 = L8 / TL6,
                        Flag2 = true;
                    true ->
                        F1 = 0,
                        Flag2 = false
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_COUNTER_TIMER_INV" ->
                    if L8 >= 0 ->
                        F1 = 1 - (L8 / TF),
                        Flag2 = true;
                    true ->
                        F1 = 0,
                        Flag2 = false 
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_COUNTER_BULK_COUNT" ->
                    if L8 >= 0 ->
                        F1 = L8 / TF,
                        Flag3 = true;
                    true ->
                        F1 = 0,
                        Flag3 = false 
                    end,
                    Flag2 = false,
                    Byte0 = 2;                    
                "PERF_COUNTER_RAWCOUNT" ->
                    Flag3 = false,
                    Flag2 = false,                
                    F1 = list_to_integer(N1),
                    Byte0 = 0;
                "PERF_COUNTER_LARGE_RAWCOUNT" ->
                    Flag3 = false,
                    Flag2 = false,                
                    F1 = list_to_integer(N1),
                    Byte0 = 0;
                "PERF_COUNTER_RAWCOUNT_HEX" ->
                    Flag3 = false,
                    Flag2 = false,
                    F1 = list_to_integer(N1),
                    Byte0 = 0;
                "PERF_COUNTER_LARGE_RAWCOUNT_HEX" ->
                    Flag3 = false,
                    Flag2 = false,                
                    F1 = list_to_integer(N1),
                    Byte0 = 0;
                "PERF_ELAPSED_TIME" ->
                    TL10 = list_to_integer(N1),
                    if (TL10 > TL2) or (TL10 < 0) ->
                        L10 = 0;
                    true ->
                        L10 = TL10
                    end,                   
                    F1 = (TL2 - L10) /  16#989680,
                    Byte0 = 0,
                    Flag3 = false,
                    Flag2 = false;                    
                "PERF_RAW_FRACTION" ->
                    K1 = list_to_integer(N1),
                    if  K1 == 0 -> 
                        F1 = 0.0;
                    true ->
                        F1 = list_to_integer(N1) / L7
                    end,
                    Flag2 = true,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_SAMPLE_FRACTION" ->
                    Flag2 = true,
                    if (L8 =< 0) or (L9 =< 0) ->
                        F1 = 0.0;
                    true ->
                        F1  = L8 / L9                         
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_SAMPLE_COUNTER"  ->
                    if (L8 >= 0) and (L9 > 0) ->
                        F1 = L8 / L9;
                    true ->
                        F1 = 0.0
                    end,
                    Flag3 = false,
                    Flag2 = false,
                    Byte0 = 2;                    
                "PERF_AVERAGE_TIME" ->
                    Flag3 = false,
                    Flag2 = false,
                    Byte0 = 2,                
                    F1 = list_to_integer(N1) / L7;
                "PERF_AVERAGE_TIMER" ->
                    if (L8 >= 0) and (L9 >0) ->
                        F1 = L8 / TL3 / L9;
                    true ->
                        F1 = 0.0
                    end,
                    Flag3 = false,
                    Flag2 = false,
                    Byte0 = 2;                    
                "PERF_AVERAGE_BULK" ->  
                    if (L8 >= 0) and (L9 > 0) ->
                        F1 = L8 / L9;
                    true ->
                        F1 = 0.0
                    end,
                    Flag3 = false,
                    Flag2 = false,
                    Byte0 = 2;                     
                "PERF_100NSEC_TIMER" -> 
                    if L8 >= 0 ->
                        F1 = L8 / TL5,
                        Flag2 = true;
                    true ->
                        F1 = 0.0,
                        Flag2 = false                        
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_100NSEC_TIMER_INV" ->
                    if L8 >= 0 ->
                        F1 = 1.0 - (L8 / TL5),
                        Flag2 = true;
                    true ->
                        F1 = 0.0,
                        Flag2 = false                        
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_COUNTER_MULTI_TIMER" ->
                    if L8 >= 0 ->
                        F1 = L8 / TF / L7,
                        Flag2 = true;
                    true ->
                        F1 = 0.0,
                        Flag2 = false 
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_COUNTER_MULTI_TIMER_INV" -> 
                    if L8 >= 0 ->
                        F1 = 1 - (L8 / TF / L7),
                        Flag2 = true;
                    true ->
                        F1 = 0.0,
                        Flag2 = false                        
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_100NSEC_MULTI_TIMER" ->
                    if L8 >= 0 ->
                        F1 = L8 / TL5 / L7,
                        Flag2 = true;
                    true ->
                        F1 = 0.0,
                        Flag2 = false 
                    end,
                    Flag3 = false,
                    Byte0 = 2;                    
                "PERF_100NSEC_MULTI_TIMER_INV" ->
                    if L8 >= 0 ->
                        F1 = 1 - (L8 / TL5 / L7), 
                        Flag2 = true;
                    true ->    
                        F1 = 0.0,
                        Flag2 = false
                    end,
                    Flag3 = false,
                    Byte0 = 2;
                "PERF_COUNTER_QUEUELEN_TYPE" -> 
                    Flag2 = false,
                    Flag3 = false,
                    Byte0 = 2,                 
                    F1 =  (AlJ1 + (TL2 * list_to_integer(N1))) / TL5;
                "PERF_COUNTER_LARGE_QUEUELEN_TYPE" ->
                    Flag2 = false,
                    Flag3 = false,
                    Byte0 = 2,                     
                    F1 =  (AlJ1 + (TL2 * list_to_integer(N1))) / TL5;
                "PERF_PRECISION_100NSEC_TIMER" ->
                    if L8 >= 0 ->
                        F1 = L8 / L9,
                        Flag2 = true;
                    true ->
                        F1 = 0.0,
                        Flag2 = false                         
                    end,
                    Flag3 = false,
                    Byte0 = 2;
                "PERF_PRECISION_100NSEC_QUEUELEN" ->
                    if L8 >= 0 ->
                        F1 = L8 / TL5;
                    true ->                         
                        F1 = 0.0 
                    end,
                    Flag2 = false,
                    Flag3 = false,
                    Byte0 = 2;                    
                _ ->
                    F1 = 0.0,
                    Flag2 = false,
                    Flag3 = false,
                    Byte0 = 2                                          
                end,
                if Flag2 == true ->
                    TByte0 = 2,
                    TF1 = F1 * 100;
                true ->
                    TByte0 = Byte0,
                    TF1 = F1                     
                end;                 
            true ->
                TF1 = 0.0,
                Flag2 = false,
                Flag3 = false,
                TByte0 = 2                    
            end;
        true ->
            Stringbuffer = TStringbuffer,
            TF1 = 0.0,
            Flag2 = false,
            Flag3 = false,
            TByte0 = 2 
        end        
    end,
    [S9|EVector] = TVector,
    [S10|EVector1] = TVector1,
    %io:format("&&&&&S9:~p~n",[S9]),
    %io:format("&&&&&10:~p~n",[S10]),
    if erlang:is_float(TF1) or erlang:is_integer(TF1) ->
        S11 = integer_to_list(round(TF1)),
        THIS:set_attribute(S10,TF1),
        THIS:set_attribute(pLastMeasurementWasNA,0),
        
        {ok,{_,Values}} = THIS:get_attribute(pValues),
        {ok,{_,Measurements}} = THIS:get_attribute(pMeasurements),
        {ok,{_,LastMeasurements}} = THIS:get_attribute(pLastMeasurements),
        {ok,{_,LastBaseMeasurements}} = THIS:get_attribute(pLastBaseMeasurements),
        
        Len1 = length(Values),
        Len2 = length(Measurements),
        Len3 = length(LastMeasurements),
        Len4 = length(LastBaseMeasurements),
        
        V1 = lists:sublist(Values,1,TJ1-1),
        V2 = lists:sublist(Values,TJ1+1,Len1),
        V = lists:append(lists:append(V1,[S11]),V2),
        
        M1 = lists:sublist(Measurements,1,TJ1-1),
        M2 = lists:sublist(Measurements,TJ1+1,Len2),
        M = lists:append(lists:append(M1,[S11]),M2),
        
        La1 = lists:sublist(LastMeasurements,1,TJ1-1),
        La2 = lists:sublist(LastMeasurements,TJ1+1,Len3),
        La = lists:append(lists:append(La1,[list_to_integer(N1)]),La2),
        
        
        Lb1 = lists:sublist(LastBaseMeasurements,1,TJ1-1),
        Lb2 = lists:sublist(LastBaseMeasurements,TJ1+1,Len4),
        Lb = lists:append(lists:append(Lb1,[L7]),Lb2), 
        
        THIS:set_attribute(createBrowseId(Tper),round(TF1)),
        THIS:set_attribute(pValues,V),
        THIS:set_attribute(pMeasurements,M),
        THIS:set_attribute(pLastMeasurements,La),        
        THIS:set_attribute(pLastBaseMeasurements,Lb), 

        if Flag2 ->
            EStringbuffer1 = Stringbuffer  ++ S9 ++ "=" ++ S11 ++ "%";
        true ->
            EStringbuffer1 = Stringbuffer  ++ S9 ++ "=" ++ S11 
        end,
        if Flag3 ->
            EStringbuffer = EStringbuffer1 ++ "/sec";
        true ->
            EStringbuffer = EStringbuffer1
        end;
    true ->
        THIS:set_attribute(pLastMeasurementTime,0),
        THIS:set_attribute(pLastMeasurementWasNA,1),
        {ok,{_,Values}} = THIS:get_attribute(pValues),
        {ok,{_,Measurements}} = THIS:get_attribute(pMeasurements),
        {ok,{_,LastMeasurements}} = THIS:get_attribute(pLastMeasurements),
        Len1 = length(Values),
        Len2 = length(Measurements),
        Len3 = length(LastMeasurements),
        V1 = lists:sublist(Values,1,TJ1-1),
        V2 = lists:sublist(Values,TJ1+1,Len1),
        V = lists:append(lists:append(V1,["n/a"]),V2),
        M1 = lists:sublist(Measurements,1,TJ1-1),
        M2 = lists:sublist(Measurements,TJ1+1,Len2),
        M = lists:append(lists:append(M1,[0]),M2),
        La1 = lists:sublist(LastMeasurements,1,TJ1-1),
        La2 = lists:sublist(LastMeasurements,TJ1+1,Len3),
        La = lists:append(lists:append(La1,[0]),La2),
        THIS:set_attribute(pValues,V),
        THIS:set_attribute(pMeasurements,M),
        THIS:set_attribute(pLastMeasurements,La),
        THIS:set_attribute(S10,"n/a"),
        THIS:set_attribute(createBrowseId(Tper),"n/a"),
        EStringbuffer = Stringbuffer  ++ S9 ++ " " ++ "n/a"
    end,
    update_util_4_t(TJ1+1,EVector,EVector1,TL2,TL3,TL5,TL6,TF,EStringbuffer,Eper,B,AlEnd,Al2End,TFlag1,Num-1,TAl1).               

update_for(PROPERTY_NAME_COUNTER_ID,PROPERTY_NAME_COUNTER_VALUE,PROPERTY_NAME_COUNTER_NAME,MaxCounter) ->
    update_for(PROPERTY_NAME_COUNTER_ID,PROPERTY_NAME_COUNTER_VALUE,PROPERTY_NAME_COUNTER_NAME,0,MaxCounter,0).
update_for(_Pnci,_Pncv,_Pncn,_N,0,I) -> I;
update_for(Pnci,Pncv,Pncn,Num,Max,Int) ->
    {ok,{_,S2}} = THIS:get_property(Pnci ++ integer_to_list(Num+1)), %S2 格式 "33 LogicalDiskx_TotalxFreexMegabytes"
    {ok,{_,S6}} = THIS:get_property(Pncn ++ integer_to_list(Num+1)),
    S5 = (Pncv ++ integer_to_list(Num+1)),
    if length(S2) =< 0 ->
        update_for(Pnci,Pncv,Pncn,Num+1,Max-(Num+1),Int);
    true ->
        [Int,StringTokenizer] = string:tokens(S2," "),
        S3 = string:to_lower(StringTokenizer)
        %if  Perfcounter /= null
                     
    end.


%% @spec getBrowseData(Params) -> List
%% @doc overloading function,return browse data.
getBrowseData(Params) ->
    case file:read_file("templates.health/health.xml") of
    {ok,XML} -> 
        {ok,D,_} = erlsom:simple_form(XML),
        List = findCounters(D),
        Browse = buildBrowseString(D,List),
        Tree = builtTrees(Browse),
        Tree;
    _ ->
        [] 
    end.
            
builtTrees(Browse)->
    builtTrees(Browse,[],[],[]).
    
builtTrees([],Object,Instance,Tree) ->Tree;
builtTrees([{K,V}|R],Object,Instance,Tree) ->
    I = string:str(V,"/"),
    J = httputils:indexOf(V,"/",I+1),
    Obj = string:sub_string(V,1,I-1),
    Ins = string:sub_string(V,I+1,J-1),
    Name = string:sub_string(V,J+1,length(V)),
    IsObjectNameExist = lists:member(Obj,Object),
    IsInstanceNameExist = lists:member(Obj++"/"++Ins,Instance),
    ObjCounter = if
        (not IsObjectNameExist) ->
            AddObj = Obj,
            [{Obj,Obj}];
        true ->
            AddObj = "",
            []
    end,
    InsCounter = if
        (not IsInstanceNameExist) ->
            AddIns = Obj++"/"++Ins,
            [{Obj++"/"++Ins,Obj++"/"++Ins}];
        true ->
            AddIns = "",
            []
    end,
    NewCounter = [{K,V}],
    %%io:format("new counter :~p~n",[ObjCounter++InsCounter++NewCounter]),
    NTree = Tree++ObjCounter++InsCounter,
    NNTree = lists:keystore(K,1,NTree,{K,V}),
    builtTrees(R,Object++[AddObj],Instance++[AddIns],NNTree).
    
    
getObject(List) ->
    getObject_t(List,length(List),[]).
getObject_t(_L,0,E) -> E;
getObject_t(Li,Num,El) ->
    [A|B] = Li,
    {"object",C,D} = A,
    {value,{"name",Name}} = lists:keysearch("name",1,C),
    getObject_t(B,Num-1,lists:append(El,[{Name,Name ++ "test"}])).      
  

%% @spec getScalarValues(Prop,Params) -> List
%% @doc overloading function,return scalar values.
getScalarValues(Prop,Params)->
	case Prop of
		pFile ->
            OsNum = platform:getOs(),
            SepStr = platform:pathSeparator(OsNum),
            Path = platform:getRoot() ++ SepStr ++ "templates.perfmon" ++ SepStr ++ "browsable",
            {ok,FileList} = file:list_dir(Path),                        
			makeScalar(FileList,Path,SepStr);            
		_->
			BASE:getScalarValues(Prop,Params)
	end.


%% @spec remoteCommandLineAllowed() -> Bool
%% @doc overloading function.
remoteCommandLineAllowed()->
	true.


makeScalar(FileList,Path,SepStr) ->
    makeScalar_t(FileList,length(FileList),Path,SepStr,0,[]).
makeScalar_t(_FL,0,_P,_SepSt,_N,List) ->  List;
makeScalar_t(FL,Num,P,Sep,N,Li) ->
    [A|B] = FL,
    makeScalar_t(B,Num-1,P,Sep,N+1,lists:append(Li,[{A,iconv:convert("gbk","utf-8",P++Sep++A)}])).    


%% @spec get_template_property() -> List
%% @doc overloading function.
get_template_property() ->
    BASE:get_template_property() ++ 
    [   
        %#property{name=machine,title="Server",type=server,editable=true,order=1},
        %#property{name=pHost,title="Server", description="the IP address or host name of the agent",type=text,editable=true,order=1},
        %#property{name=pFile,title="Counter File",type=scalar,order=2,description="a Performance Monitor setting file that specifies the counters"}        
    ].
    


   

%set_property  {createBrowseId,perfcounter} 
install_conuter(List) ->
    install_conuter_t(List,length(List)).
install_conuter_t(_Li,0) -> ok; 
install_conuter_t(Li,Num) ->
    [A|B] = Li,
    THIS:set_property(createBrowseId(A),A),
    install_conuter_t(B,Num-1).


%Document is erlsom:simple_form(XML)'s return
findCounters(Document) ->
    {"browse_data",_,Element} = Document,
    Object = findCounters_util_1(Element), %Array is list
    Hostname = "",  
    getPerfCounters(Hostname,Object,"").
    
    
    
        
findCounters_util_1(List) ->
   findCounters_util_1_t(List,length(List),[]).
findCounters_util_1_t(_L,0,EL) ->  EL;
findCounters_util_1_t(Li,Num,EndL) ->
    [A|B] = Li,
    {"object",C,D} = A,
    case lists:keysearch("type",1,C) of
    {value,{"type",Type}} ->
        if Type == "perf" ->
            {value,{"name",Name}} = lists:keysearch("name",1,C),
            findCounters_util_1_t(B,Num-1,lists:append(EndL,[Name])); 
        false ->
            findCounters_util_1_t(B,Num-1,EndL)
        end;            
    false ->
        findCounters_util_1_t(B,Num-1,EndL)
    end.   

        
    

%{perf_counter,"Memory","Cache Bytes Peak","SINGLE",undefined,undefined},\
%% @spec createBrowseId(Perfcounter) -> BrowseId
%% @doc create browseId string.
createBrowseId(Perfcounter) ->
    {perf_counter,Object,CounterName,Instance,_,_} = Perfcounter,
    createBrowseId_util(string:to_lower(Object) ++ "x" ++ string:to_lower(Instance) ++ "x" ++ string:to_lower(CounterName)).
         
createBrowseId_util(String) ->
    createBrowseId_util_t(String,1,length(String),"").
createBrowseId_util_t(_St,_K,0,E) -> E;
createBrowseId_util_t(Str,K,Num,EL) ->
    Char =  string:substr(Str,K,1),
    if Char == " " ->
        createBrowseId_util_t(Str,K+1,Num-1,EL ++ "x");
    true ->
        createBrowseId_util_t(Str,K+1,Num-1,EL ++ Char)
    end.


getCounterFilename() ->
    {ok,{_,File}} = THIS:get_property(pFile),
    File.

%Document is erlsom:simple_form(XML)'s return, List is findCounters's return.
buildBrowseString(Document,List) ->
    %CounterFilename = getCounterFilename(),
    {"browse_data",_,Nodelist} = Document,
    Linkedlist = buildBrowseString_util(Nodelist), % type == "perf" 's node list
    buildBrowseString_for(Linkedlist,List).
    


buildBrowseString_for(Linkedlist,List) ->
    buildBrowseString_for_t(Linkedlist,List,length(Linkedlist),[]).
buildBrowseString_for_t(_Ll,_List,0,E) -> E;
buildBrowseString_for_t(Llist,Map,Num,El) ->
    [Node|B] = Llist,
    {"object",C,D} = Node,
    case lists:keysearch("name",1,C) of 
    {value,{"name",N}} ->
        Name = N;
    _ ->
        Name = ""
    end,
    case lists:keysearch("instances",1,C) of 
    {value,{"instances",Ins}} ->
        Instances = Ins;
    _ ->
        Instances = ""
    end,
    if length(Instances) > 0 , Instances /= "*" ->
        Hashset = string:tokens(string:to_lower(Instances),","); %Hastset is list
    true ->
        Hashset = null     
    end,
    Hashset1 = buildBrowseString_for_for(D),  %["% Free Space","Free Megabytes"],
    %{ok,{_,Perfcounterlist}} = THIS:get_property(perfcounterlist),
    Map1 = get_element(Map,Name), % Map1 is Objectname == Name's list  of Perfcounterlist
    if length(Map1) > 0 -> 
        L = buildBrowseString_for_if_while_while(Map1,Hashset1);
    true ->
        L = []
    end,
    buildBrowseString_for_t(B,Map,Num-1,lists:append(El,L)). 
         
         

%List is Map1
%buildBrowseString_for_if_while(List,Name,Hashset,Node,Hashset1) ->
%    buildBrowseString_for_if_while_t(List,Name,Hashset,Node,Hashset1,length(List),[]).
%buildBrowseString_for_if_while_t(_Li,_Na,_Hashset,_Node,_Hashs1,0,E) -> E;
%buildBrowseString_for_if_while_t(Li,NName,Hashs,Element1Node,Hashs1,Num,El) ->
%    [A|B] = Li,
%    {perf_counter,Obj,Counter,In,_,_} = A,
%    if (Hashs == null) or lists:member(In,Hashs)  or (In == "SINGLE") ->
        
        
         
                   
%Iterator3 is Map1返回键值对的list                   
buildBrowseString_for_if_while_while(Map1,Hashset1) -> %
    buildBrowseString_for_if_while_while_t(Map1,Hashset1,length(Map1),[]).
buildBrowseString_for_if_while_while_t(_Map1,_Hashs1,0,E) -> E;  
buildBrowseString_for_if_while_while_t(Map,Hashs1,Num,El) ->
    [A|B] = Map, 
    {perf_counter,Obj,Counter,Ins,_,_} = A,
    case lists:member(Counter,Hashs1) of
    true ->
        String = createBrowseId(A),
        THIS:set_property(String,A),
        buildBrowseString_for_if_while_while_t(B,Hashs1,Num-1,lists:append(El,[{String,Obj ++ "/" ++ Ins ++ "/" ++Counter}]));
    _ ->
        buildBrowseString_for_if_while_while_t(B,Hashs1,Num-1,El)
    end.
    



get_element(Perfcounterlist,ObjectName) ->
    get_element_t(Perfcounterlist,ObjectName,length(Perfcounterlist),[]).
get_element_t(_P,_O,0,E) -> E;
get_element_t(PerList,Obj,Num,El) ->
    [A|B] = PerList, %{perf_counter,"Memory","Free System Page Table Entries","SINGLE",undefined,undefined}
    {perf_counter,Object,_,_,_,_} = A,
    if Object == Obj ->
        get_element_t(B,Obj,Num-1,lists:append(El,[A]));
    true ->
        get_element_t(B,Obj,Num-1,El)
    end.

% return namelist of CounterInfo List
buildBrowseString_for_for(CounterInfoList) ->
    buildBrowseString_for_for_t(CounterInfoList,length(CounterInfoList),[]).
buildBrowseString_for_for_t(_Cou,0,E) -> E;
buildBrowseString_for_for_t(CouList,Num,El) ->
    [A|B] = CouList,
    {"counterInfo",Value,_} = A,
    case lists:keysearch("name",1,Value) of   %{"counterInfo",[{"name","% DPC Time"}],[]}    
    {value,{"name",N}} ->
        Name = N;
    true ->
        Name = ""
    end,    
    if length(Name) > 0 ->
        buildBrowseString_for_for_t(B,Num-1,lists:append(El,[Name]));
    true ->
        buildBrowseString_for_for_t(B,Num-1,El)
    end.   
    
    
    
buildBrowseString_util(List) ->
    buildBrowseString_util_t(List,length(List),[]).
buildBrowseString_util_t(_Li,0,E) -> E;
buildBrowseString_util_t(Li,Num,El) ->
    [A|B] = Li,
    {"object",C,D} = A,
    case lists:keysearch("type",1,C) of
    {value,{"type",Type}} ->
        if Type == "perf" ->
            buildBrowseString_util_t(B,Num-1,lists:append(El,[A]));
        true ->
            buildBrowseString_util_t(B,Num-1,El) 
        end;
    _ ->
        buildBrowseString_util_t(B,Num-1,El)
    end.        


%% @spec get_classifier(Parameter) -> List
%% @doc overloading function.
get_classifier(error)->
	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{countersInError,'>',0}]
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
			[{countersInError,'>',0}]
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
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.

buildcounters([])->"";
buildcounters([F|R])->
	iconv:convert("gb2312","utf-8",F#perf_counter.object) ++ " -- " ++ iconv:convert("gb2312","utf-8",F#perf_counter.counterName) ++ " -- " ++ iconv:convert("gb2312","utf-8",F#perf_counter.instance) ++ "," ++ buildcounters(R).


forgetcounters([])->[];
forgetcounters([F|R])->
	L1 = [string:strip(X)||X<-string:tokens(F,"--")],
	[#perf_counter{object=lists:nth(1,L1),counterName=lists:nth(2,L1),instance=lists:nth(3,L1)}]++forgetcounters(R).
 
getPerfCounters(Machine,Object,S)->
	THIS:set_attribute(flag1,false),
	THIS:set_attribute(flag2,false),
	Cmd1=make_params5(Object),
	Cmd2 = platform:perfexCommand(Machine) ++ " " ++ Cmd1,
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd2),
	Array1 = perfCounters(string:tokens(Rets,"\r\n"),Object),
	case string:tokens(S,",") of
		[]->
			Array1;
		S1->
			case filtercounters(string:tokens(buildcounters(Array1),","),S1) of
				""->
					forgetcounters2(S1);
				String->
					forgetcounters(string:tokens(String,","))
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
		"name:"++Name->
%%改动部分
			THIS:set_attribute(flag1,true),
			Instance1 = string:strip(string:sub_string(F,string:str(F,": ")+2,string:len(F))),
			Instance2 = httputils:replace(Instance1,":",""),
			THIS:set_attribute(instance,Instance2),
%%
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
					[#perf_counter{object=iconv:convert(platform:getLocalCode(),httputils:pageEncode(),Object1),counterName=iconv:convert(platform:getLocalCode(),httputils:pageEncode(),CounterName),instance=iconv:convert(platform:getLocalCode(),httputils:pageEncode(),Instance1)}]++perfCounters(R,Object)
			end
	end.


make_params5([])->"";
make_params5([Obj|T])->
	"-o \"" ++ Obj ++ "\" "++ make_params5(T).

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