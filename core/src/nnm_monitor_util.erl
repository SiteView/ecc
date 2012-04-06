%% @author lianbing.wang
%% @doc the nnm's utility

-module (nnm_monitor_util).
-export([get_counters/0 , filter_device_ports/1 , build_classifier/2 ,
         get_selected_formulas/1 , get_formula_type/1 , unique/1 ,
         get_data_desc/1 , get_data_desc/2 , transform_unit/2,
         to_list/1 , get_snmp_params/1,
         get_devices/0]).

-include("monitor.hrl").

get_snmp_params(DeviceID) ->
    case api_machine:get_machine(DeviceID) of
        {error,_Reason} -> 
            [];
        #machine{id = undefined,other = _DeviceProps} ->
            [];
        #machine{host = Ip,other = DeviceProps} -> 
            KeyValues = [{ip,Ip} | proplists:get_value(snmpParam,DeviceProps)],
            Param = [ {Key,Value} || {Key,Value} <- KeyValues,Key =/= bindPort] ++ [{bindPort,162}],
            %io:format("Param:~p~n",[Param])
            %KeyValues  
            Param       
    end.

get_devices() ->
    Devices = get_devices_data(),
    build_counters(Devices,list).    

get_counters() ->   
    Devices = get_devices_data(),
    build_counters(Devices,parent_childs).

get_devices_data()->
    Type = ["ROUTE","SWITCH","ROUTE_SWITCH","FIREWALL"],    
    Devices = [ Machine || Machine <- api_machine:get_DeviceByType(Type,0,0,"",""),is_record(Machine,machine)],
    Devices.

build_counters([],_) ->
    [];
build_counters([#machine{id = Id,name = Name,host = Host } | T ] = _Devices,list)->
    Current = { atom_to_list(Id) , Host ++ "(" ++ Name ++ ")" },
    [ Current ] ++ build_counters(T,list);
build_counters([#machine{id = Id,name = Name,host = Host,other = DeviceProp } | T ] = _Devices,parent_childs)->
    Parent = { atom_to_list(Id) , Host ++ "(" ++ Name ++ ")" },
    [ Parent ] ++ parent_child( Parent , child( DeviceProp )) ++ build_tree(T).    

build_tree([])->
    [];
build_tree([#machine{id = Id,name = Name,host = Host,other = DeviceProp } | T ] = _Devices) ->
    Parent = { atom_to_list(Id) , Host ++ "(" ++ Name ++ ")" },
    [ Parent ] ++ parent_child( Parent , child( DeviceProp )) ++ build_tree(T).

parent_child(_,[]) ->
    [];
parent_child({ParentId,ParentText},[{Index,Desc} | T ] = _Ifs) ->
    CurrentText = ParentText ++ "/" ++ string:join(string:tokens(Desc,"/"),[]),
    CurrentId = ParentId ++ "-" ++ Index,
    [{CurrentId, CurrentText}]  ++ parent_child({ParentId,ParentText},T) .

child(DeviceProp) ->
    [{Index,Desc} || {Index,_,Desc,_,_,_,_} <-  proplists:get_value(infs, DeviceProp)].

%% @doc get the device's id and interface index
get_deviceId_and_ifIndex(CounterStr)->
    [DeviceId,IfIndex] = string:tokens(CounterStr,"-"),
    {list_to_atom(DeviceId),list_to_integer(IfIndex)}.

filter_device_ports(Counters) ->
	[get_deviceId_and_ifIndex(Id) || {Id,_Text} <- Counters,length(Id) > 18].

%% build_classifier(Classifiers,LimitCount) ->
%% 	if length(Classifiers) < LimitCount ->
%% 	    Classifiers ++ lists:map(fun(_X)->{'N/A','',''} end,lists:seq(0,LimitCount - length(Classifiers)));
%% 	true ->
%% 	    Classifiers
%%     end.
%% 
%% get_selected_formulas([])->
%%     [];
%% get_selected_formulas(Classifier) ->
%%     [F || {F,_,_} <- Classifier,F =/= 'N/A'].

build_classifier(Classifiers,LimitCount) ->
	if length(Classifiers) < LimitCount ->
	    Classifiers ++ lists:map(fun(_X)->{'N/A','','',''} end,lists:seq(0,LimitCount - length(Classifiers)));
	true ->
	    Classifiers
    end.

get_selected_formulas([])->
    [];
get_selected_formulas(Classifier) ->
    [F || {F,_,_,_} <- Classifier,F =/= 'N/A'].

get_formula_type(Formula) ->
    [_Fun,Type] = string:tokens(atom_to_list(Formula),"_"),
    list_to_atom(Type).  

transform_unit([],_) ->
    %~ "data is empty";
	"0.00";
transform_unit(DataValue,Unit) ->  
    Result = case get_unit_value(DataValue,Unit) of
         FloatValue when is_float(FloatValue) ->
             lists:flatten(io_lib:format("~.2f",[FloatValue]));
         Value ->
            Value
    end,
    to_list(Result) ++ get_unit_desc(Unit).

get_unit_value(DataValue,perc) ->
    DataValue;
get_unit_value(DataValue,no_unit)->
    DataValue;
get_unit_value(DataValue,kb)->
    DataValue/1024.     

unique([]) ->
	[];
unique(List) ->
	unique([],List).
unique(R,[])->
    R;
unique(R,[H|T])->
    case lists:member(H,R) of
		true->
	    	unique(R,T);
		false ->
	    	unique(lists:append(R,[H]),T)
    end.

get_data_desc([])->
    [];
get_data_desc(Data)->
    get_data_desc(Data,kb).

get_data_desc(Data,Unit)-> 
    string:join([build_desc(FormulaType,Dict,Unit) || {FormulaType,Dict} <- Data ], ";").

build_desc(_,[],_)->
    [];
build_desc(FormulaType,Dict , Unit) ->
   string:join([ Desc ++ "(" ++ atom_to_list(FormulaType) ++ ") = " ++ to_list(transform_unit(Value,Unit))  || {Desc,Value} <- Dict], ",").

get_unit_desc(perc) ->
    "%";
get_unit_desc(no_unit) ->
    [];  
get_unit_desc(Unit)->
    string:to_upper(to_list(Unit)).

to_list([])->
    [];
to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_float(Value) ->
    float_to_list(Value);
to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_list(Value) ->
    Value.    