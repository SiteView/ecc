-module(network_flow).
-export([get_formulas_data/3, get_formula_value/2, get_formula_value/3,
         max/1, min/1, sum/1, avg/1,
         get_ipmac_abnormal_data/0, get_ipmac_data_desc/1,
         get_interface_state_data/1, get_interface_state_desc/1,
         get_bandwidth_rate_data/1, get_bandwidth_rate_data_desc/1,
         get_interface_use_data/1, get_interface_use_rate_desc/1]).

-include("monitor.hrl").

get_interface_use_data(Counters)->
    %%io:format("Counters:~p~n",[Counters]),
    [ { Id, Device , get_device_interface_use_rate(list_to_atom(Id)) } || {Id,Device} <- Counters].

get_device_interface_use_rate(DeviceID)->    
    Value = case nnm_monitor_util:get_snmp_params(DeviceID) of
        [] -> 
            [];            
        SnmpParams ->
            IndexInfo = nnm_snmp_api:readTreeValue(SnmpParams,[1,3,6,1,2,1,2,2,1,1]),
            %io:format("IndexInfo:~p~n",[IndexInfo]),
            TypeInfo = nnm_snmp_api:readTreeValue(SnmpParams,[1,3,6,1,2,1,2,2,1,3]),
            %io:format("TypeInfo:~p~n",[TypeInfo]),
            StateInfo = nnm_snmp_api:readTreeValue(SnmpParams,[1,3,6,1,2,1,2,2,1,8]),
            %io:format("StateInfo:~p~n",[StateInfo]),
            IndexList = [proplists:get_value(Key,IndexInfo) || {Key,_} <- IndexInfo],
            TypeList = [proplists:get_value(Key,TypeInfo,[]) || {Key,_} <- IndexInfo],
            StateList = [proplists:get_value(Key,StateInfo,[]) || {Key,_} <- IndexInfo],
            Data = lists:zip3(IndexList,TypeList,StateList),
            %io:format("Data:~p~n",[Data]),
            Interfaces = lists:filter(fun({_Index,Type,_State})-> Type == 6 end,Data),
            %io:format("Interfaces:~p~n",[length(Interfaces)]),
            UseedInterfaces = lists:filter(fun({_Index,_Type,State})-> State == 1 end,Interfaces),
            %io:format("UseedInterfaces:~p~n",[length(UseedInterfaces)]),
            get_div_value(length(UseedInterfaces), length(Interfaces)) * 100
    end,
    Value.    

get_interface_use_rate_desc([]) ->
    [];
get_interface_use_rate_desc(Data)->
    string:join([ Device ++ " = " ++ nnm_monitor_util:to_list(nnm_monitor_util:transform_unit(Value,perc))  || {_Id, Device , Value} <- Data ],",").    

get_formulas_data(FormulaTypes,Counters,Type)->
    FirstData = [{FormulaType, get_formula_data(Counters, FormulaType, Type, get_calc_fun(Type))} || FormulaType <- FormulaTypes],
    timer:sleep(1000),
    SecondData= [{FormulaType, get_formula_data(Counters, FormulaType, Type, get_calc_fun(Type))} || FormulaType <- FormulaTypes],
    %io:format("First:~p ; Second:~p ~n",[FirstData,SecondData]),
    Data = diff(FirstData,SecondData),
    Data.

%% @spec : get_formula_value(Data,Formulas) -> [{Formula,Value}]
get_formula_value(Data,Formulas)->
     get_formula_value(Data,Formulas,kb).  

get_ipmac_abnormal_data()->
    Data = [ getAbnormalItem(Item) || Item <-api_nnm:db_read("nnm_changeIpMac",[id,newMac,newIp],[],"")],
    %Standard = format(api_nnm:db_read("nnm_baseIpMac", [id,info], "", "")),
    %Data = try api_nnm:monitor_get_device_data(arpTable,[]) of
    %       Real->
    %           get_abnormal_data(Standard,Real)
    %   catch 
    %       _:_ -> error
    %   end,
    Data. 

getAbnormalItem(Item) ->
    {proplists:get_value(id,Item),proplists:get_value(newMac,Item),proplists:get_value(newIp,Item)}.    


get_ipmac_data_desc([])->
    "None";
get_ipmac_data_desc(AbnormalData)->
    string:join([ NewMAC ++ " " ++ NewIP || {_Id,NewMAC,NewIP}<- AbnormalData ],",").

%get_ipmac_data_desc({_,[]})->
%    [];
%get_ipmac_data_desc({_,[{_Mac,_Ip}|_T] = AbnormalData}) ->
%    "AbnormalData :" ++ get_ipmac_desc(AbnormalData).  

get_interface_state_data(Counters) ->
    Data = [{Id,get_counter_desc(Counter),get_device_info(Counter,state)} || {Id,_IfIndex} = Counter <- Counters],
    Data.    

get_interface_state_desc(Data) ->
    string:join([ Text ++ "(state) = " ++ nnm_monitor_util:to_list(get_state_desc(get_or_default(State,unknow))) || {_Id,Text,State} <- Data], ","). 
    
get_state_desc(1) ->
    up; 
get_state_desc(2) ->
    down;  
get_state_desc(_State) ->
    unknow.     

get_bandwidth_rate_data([])->
    [];
get_bandwidth_rate_data(Counters)->
    First = get_bandwidth_data(Counters),
    timer:sleep(1000),
    Second = get_bandwidth_data(Counters),
    Speed = [snmp_get(Counter,get_snmp_oid(Counter,speed)) || Counter <- Counters],
    lists:zip([{Id,get_counter_desc(Counter)} || {Id,_} = Counter <- Counters],calc_bandwidth_use_rate(First,Second,Speed)).

get_bandwidth_rate_data_desc([])->
    [];
get_bandwidth_rate_data_desc(Data)->
    string:join([Text ++ " = " ++ nnm_monitor_util:to_list(nnm_monitor_util:transform_unit(Value,perc)) || {{_Id,Text},Value} <- Data], ",").   

%% private


get_formula_value(Data,Formulas,Unit) ->   
    [{Formula,nnm_monitor_util:transform_unit(calc_formula_value(Data,Formula),Unit),Unit} || Formula <- Formulas].

get_bandwidth_data(Counters)->
    [{snmp_get(Counter,get_snmp_oid(Counter,sended,flow)),snmp_get(Counter,get_snmp_oid(Counter,received,flow))} || Counter <- Counters].

calc_bandwidth_use_rate(First,Second,Speed)->
    Data = lists:zip3(First,Second,Speed),
    [ get_div_value( get_or_default(sub( add(SS,SR) , add(FS,FR) ),0) * 8 * 100 , P )  || {{FS,FR},{SS,SR},P} <- Data].        

get_device_info({DeviceID,_IfIndex} = Counter,state)->
    nnm_snmp_api:readOneValue(nnm_monitor_util:get_snmp_params(DeviceID),get_snmp_oid(Counter,state)).  
    
%get_ipmac_desc([])->
%    [];
%get_ipmac_desc(Data) ->
%    string:join([ Ip ++ " = " ++ Mac || {Mac,Ip} <- Data], ",").       

%get_abnormal_data([],_)->
%    [];
%get_abnormal_data(_,[])->
%    [];
%%[[normal],[abmormal]]
%get_abnormal_data(Standard,Data)->
%    lists:partition(
%        fun({Mac,Ip}) -> proplists:get_value(Mac,Standard) =:= Ip end,
%        Data).    

%format([])->
%    [];
%format([{_ID,_Host,List} | _T]) ->
%    [{Mac,Ip} || {_,Mac,Ip,_} <- List].    
    
%% Formula : [max_total | min_total | ...]
calc_formula_value(Data,Formula)->
    %io:format("Data:~p ,Formula:~p ~n",[Data,Formula]),
    [Fun,Type] = string:tokens(atom_to_list(Formula),"_"),
    Values = [Value || {_Desc,Value} <- proplists:get_value(list_to_atom(Type),Data)],
    case Values of
        [] ->
            [];
        Params ->
            apply(network_flow,list_to_atom(Fun),[Params])
    end.  
    
max(Data)->
    lists:max(Data).
min(Data)->
    lists:min(Data).
sum(Data)->
    lists:sum(Data).
avg(Data)->
    lists:sum(Data)/length(Data).         

%% 
get_formula_data(Counters,FormulaType,Type,rate)->
    %io:format("get_formula_data:~p ~p ~p",[Counters,FormulaType,Type]),
    Data = case FormulaType of
    total ->
        [{get_counter_desc(Counter),{add(snmp_get(Counter,get_snmp_oid(received,Type,Counter)),snmp_get(Counter,get_snmp_oid(sended,Type,Counter))),add(add(snmp_get(Counter,get_snmp_oid(sended,noncast,Counter)),snmp_get(Counter,get_snmp_oid(sended,cast,Counter))),add(snmp_get(Counter,get_snmp_oid(received,noncast,Counter)),snmp_get(Counter,get_snmp_oid(received,cast,Counter))))}} || Counter <- Counters];
    sended ->
        [{get_counter_desc(Counter),{snmp_get(Counter,get_snmp_oid(sended,Type,Counter)),add(snmp_get(Counter,get_snmp_oid(sended,noncast,Counter)),snmp_get(Counter,get_snmp_oid(sended,cast,Counter)))}} || Counter <- Counters];
    received ->
        [{get_counter_desc(Counter),{snmp_get(Counter,get_snmp_oid(received,Type,Counter)),add(snmp_get(Counter,get_snmp_oid(received,noncast,Counter)),snmp_get(Counter,get_snmp_oid(received,cast,Counter)))}} || Counter <- Counters]
    end,
    %io:format("Result:~p~n",[R]),
    Data;
get_formula_data(Counters,FormulaType,Type,Fun)->
    case FormulaType of
        total ->
            [{get_counter_desc(Counter), add(Fun(Counter,sended,Type),Fun(Counter,received,Type))} || Counter <- Counters];
        _Other ->
            [{get_counter_desc(Counter), Fun(Counter,FormulaType,Type)} || Counter <- Counters]
    end. 
    
get_counter_desc({DeviceId,IfIndex})->
    %io:format("DeviceId:~p~n",[DeviceId]),
    Desc =case api_machine:get_machine(DeviceId) of
       #machine{id=undefined} ->
           "not exist ";       
       #machine{id=_Id,name=Name,host=_Host,other = DeviceProp } ->        
           %io:format("Other:~p~n",[DeviceProp]),
           Infs = proplists:get_value(infs, DeviceProp,[]),
           Name ++ "/" ++  get_one_desc(IfIndex,Infs)
       end,
    %io:format("Desc:~p~n",[R]),
    Desc.  

get_one_desc(_,[])->
    "";
get_one_desc(FindIndex,[{Index,_,Desc,_,_,_,_}|T] = _Infs) ->
    %io:format("Infs:~p~n",[_Infs]),
    case list_to_integer(Index) == FindIndex of
        true ->
            Desc;
        false ->
            get_one_desc(FindIndex,T)
    end.  
    
get_snmp_oid({_,IfIndex},state) ->
    [1,3,6,1,2,1,2,2,1,8] ++ [IfIndex];
get_snmp_oid({_,IfIndex},speed) ->
    [1,3,6,1,2,1,2,2,1,5] ++ [IfIndex].         

get_snmp_oid(received,discard_pkts_rate,{_,IfIndex} = _Counter)->
    [1,3,6,1,2,1,2,2,1,13] ++ [IfIndex];
get_snmp_oid(sended,discard_pkts_rate,{_,IfIndex} = _Counter) ->
    [1,3,6,1,2,1,2,2,1,19] ++ [IfIndex];
get_snmp_oid(received,error_pkts_rate,{_,IfIndex} = _Counter) ->
    [1,3,6,1,2,1,2,2,1,14] ++ [IfIndex];
get_snmp_oid(sended,error_pkts_rate,{_,IfIndex} = _Counter) ->
    [1,3,6,1,2,1,2,2,1,20] ++ [IfIndex];
get_snmp_oid(received,noncast,{_,IfIndex} = _Counter) ->
    [1,3,6,1,2,1,2,2,1,11] ++ [IfIndex];
get_snmp_oid(sended,noncast,{_,IfIndex} = _Counter) ->
    [1,3,6,1,2,1,2,2,1,17] ++ [IfIndex];
get_snmp_oid(received,cast,{_,IfIndex} = _Counter) ->
    [1,3,6,1,2,1,2,2,1,12] ++ [IfIndex];
get_snmp_oid(sended,cast,{_,IfIndex} = _Counter) ->
    [1,3,6,1,2,1,2,2,1,18] ++ [IfIndex];
get_snmp_oid({_,IfIndex},sended,Type) when is_integer(IfIndex) ->
    Oid = case Type of 
        flow ->
            [1,3,6,1,2,1,2,2,1,16];
        broadcast_pkts -> 
            [1,3,6,1,2,1,2,2,1,18];
        frame ->
            [1,3,6,1,2,1,2,2,1,17]
    end,
    Oid ++ [IfIndex];
get_snmp_oid({_,IfIndex},received,Type) when is_integer(IfIndex) ->
    Oid = case Type of 
        flow ->
            [1,3,6,1,2,1,2,2,1,10];
        broadcast_pkts -> 
            [1,3,6,1,2,1,2,2,1,12];
        frame ->
            [1,3,6,1,2,1,2,2,1,11]
    end,
    Oid ++ [IfIndex].        
    
snmp_get({DeviceID,_},Oid)->
    %io:format("~p ~p ~n",[Counter,Oid]),
    Value = 
    case nnm_monitor_util:get_snmp_params(DeviceID) of
        [] -> 
            [];            
        Params ->
            nnm_snmp_api:readOneValue(Params,Oid)
    end,
    Value.  
    
add([],Other) ->
    Other;
add(Other,[]) ->
    Other;
add(V1,V2) ->
    V1 + V2.       

diff([],_) ->
    [];
diff(_,[])->
    [];
diff([ {FormulaType,F} | TF] = _FirstData,[ {FormulaType,S} | TS ] = _SecondData)->
    [{FormulaType,sub(F,S)}] ++ diff(TF,TS). 
    
sub([],_)->
    [];
sub(_,[])->
    [];
sub([{Desc,[]}|FT] = _FirstData,[{Desc,[]}|ST] = _SecondData)->
    [{Desc,[]}] ++ sub(FT,ST);
sub([{Desc,{[],Other1}}|FT] = _FirstData,[{Desc,{[],Other2}}|ST] = _SecondData)->
    [{Desc,sub(Other2,Other1)}] ++ sub(FT,ST);
sub([{Desc,{Value1,Value2}}|FT] = _FirstData,[{Desc,{Value3,Value4}}|ST] = _SecondData) ->
    [{Desc,get_div_value(get_or_default(sub(Value3,Value1),[]),get_or_default(sub(Value4,Value2),[]))}] ++ sub(FT,ST);
sub([{Desc,FValue}|FT] = _FirstData,[{Desc,SValue}|ST] = _SecondData)->
    [{Desc,SValue - FValue}] ++ sub(FT,ST);
sub(Value2,Value1) ->
    Value2 - Value1.  
    
get_div_value([],_)->
    [];
get_div_value(_,[]) ->
    [];
get_div_value(_,0) ->
    [];
get_div_value(V1,V2) ->
    V1 / V2.

get_or_default([],Default)->
    Default;
get_or_default(Value,_) ->
    Value.             

get_calc_fun(Type)->
    Fun = case Type of
        flow ->
            fun single/3;
        frame ->
            fun single/3;
        broadcast_pkts ->
            fun single/3;
        discard_pkts_rate ->
            rate;
        error_pkts_rate ->
            rate
    end,
    Fun.  

single({DeviceID,_} = Counter,sended, Type)->
    Value = nnm_snmp_api:readOneValue(nnm_monitor_util:get_snmp_params(DeviceID),get_snmp_oid(Counter,sended,Type)),
    %io:format("SNMP Params:~p~n",[nnm_monitor_util:get_snmp_params(DeviceID)]),
    %io:format("SNMP OID:~p",[get_snmp_oid(Counter,sended,Type)]),
    %io:format("Value:~p sent ~p",[Type,Value]),
    Value;
single({DeviceID,_} = Counter, received, Type) ->
    Value = nnm_snmp_api:readOneValue(nnm_monitor_util:get_snmp_params(DeviceID),get_snmp_oid(Counter,received,Type)),
    %io:format("Value:~p received ~p",[Type,Value]),
    Value.        

