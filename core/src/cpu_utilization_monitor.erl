%%
%% cpu utilization monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc cpu utilization monitor
-module(cpu_utilization_monitor,[BASE]).
-compile(export_all).
-extends(server_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MaxCpus,16).

-export([new/0,update/0,get_classifier/1,get_template_property/0,get_cpu_for_linux/1,get_cpu_for_win32/1]).


%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for directory monitor
new()->
	Base = server_monitor:new(),
	%Base:set_attribute(utilization,"n/a"),
    case preferences:get(master_config,cpuMaxProcessors) of
    {ok,[{_,Cpus}]} ->
             Base:set_attribute(pLastMeasurements,lists:duplicate(Cpus,"n/a"));
     _ ->        
            Base:set_attribute(pLastMeasurements,lists:duplicate(?MaxCpus,"n/a"))
    end,
    Base:set_attribute(pLastMeasurement,0), 
    Base:set_attribute(pLastMeasurementTime,0),
    Base:set_attribute(pCpusNum,0),    
	{?MODULE,Base}.


%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->    
	io:format("-------------------------update cpu monitor ~n",[]),
	THIS:set_attribute(utilization,"n/a"),
    %Base:set_attribute(pLastMeasurements,lists:duplicate(?MaxCpus,0)),    
	{ok,{_,Machine}}=THIS:get_property(machine), 
    %[Machi] = machine:getMachine(Machine),
    case machine:getMachine(Machine) of
    [_Machine|_] ->
	    case machine:getOS(Machine) of
        1 ->
		    get_cpu_for_win32(Machine);
	    _ ->		
            get_cpu_for_linux(Machine)
        end;
    [] ->
        if (Machine == "") or (Machine == "\\\\localhost") ->
	        case machine:getOS("") of
            1 ->
		        get_cpu_for_win32("");
	        _ ->		
                get_cpu_for_linux("")
            end;
        true ->            
            THIS:set_attribute(utilization,"n/a"),
            THIS:set_attribute(?NO_DATA,true),
            THIS:set_attribute(?CATEGORY,?NO_DATA),
	        THIS:set_attribute(?STATE_STRING,"timeout")
        end             
    end. 
%% @spec get_template_property() -> List
%% List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++
    template_property_util() ++     
    [
         #property{name=utilization,title="utilization%",type=numeric,order=1,configurable=false,state=true,baselinable=true}
	] .
    

template_property_util() ->
    case preferences:get(master_config,cpuMaxProcessors) of
    {ok,[{_,Cpus}]} ->
             MaxCpus = Cpus;
     _ ->        
            MaxCpus = ?MaxCpus
    end,
     template_property_util_t(MaxCpus,[]).
template_property_util_t(0,List) -> List;
template_property_util_t(Num,L) ->
    Recode = #property{name=list_to_atom("cpu" ++ integer_to_list(Num)),title="Cpu" ++ "#"++integer_to_list(Num),type=numeric,order=Num+1,configurable=false,state=true,baselinable=true},
    template_property_util_t(Num-1,[Recode|L]).

%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->  
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{utilization,'==',100}, {utilization,'==',"n/a"}]
			end;
get_classifier(warning)->
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{utilization,'>=',80}]
	end;	
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{utilization,'<',80}]
	end.
	

%% @spec get_cpu_for_win32(Host) -> ok
%% Host = string()
%% @doc  Get windows machine cpu information
get_cpu_for_win32(Host) ->
    {ok,{pLastMeasurements,LastMeasurements}} = THIS:get_attribute(pLastMeasurements),
    {ok,{pLastMeasurement,LastMeasurement}} = THIS:get_attribute(pLastMeasurement),
    {ok,{pLastMeasurementTime,LastMeasurementTime}} = THIS:get_attribute(pLastMeasurementTime),
    List = platform:cpuUsed(Host,LastMeasurementTime,LastMeasurement,LastMeasurements,THIS,[]),    
    [TUtilization] = lists:sublist(List,1,1),
    [TLastMeasurementTime] = lists:sublist(List,2,1),
    [TLastMeasurement] = lists:sublist(List,3,1),    
    if TUtilization == -1 ->
        THIS:set_attribute(pLastMeasurementTime,0),
        THIS:set_attribute(pLastMeasurement,0),
        THIS:set_attribute(pMeasurement,0),
        THIS:set_attribute(utilization,"n/a"),
		THIS:set_attribute(?NO_DATA,true),     
        THIS:set_attribute(?CATEGORY,?NO_DATA),
	    THIS:set_attribute(?STATE_STRING,"timeout");	
    true ->
        THIS:set_attribute(utilization,TUtilization),
        THIS:set_attribute(pLastMeasurementTime,TLastMeasurementTime),
        THIS:set_attribute(pLastMeasurement,TLastMeasurement),
        THIS:set_attribute(pMeasurement,TUtilization),
        [Cpu] = lists:sublist(List,4,1),
        THIS:set_attribute(pCpusNum,Cpu),
        if Cpu > 1 ->
            CpusMeasurementList = string:substr(List,5),
            String = make_string(CpusMeasurementList,length(CpusMeasurementList)),
            Avg = round(TUtilization),
            THIS:set_attribute(?STATE_STRING,integer_to_list(Avg)++ "% avg"++String),
            THIS:set_attribute(pLastMeasurements,CpusMeasurementList);
        true ->
            THIS:set_attribute(?STATE_STRING,integer_to_list(round(TUtilization)) ++ "% used")
        end       
    end.    

make_string(CpusMeasurementList,CpusMeasurementListNumber) ->
    make_string_t(1,CpusMeasurementList,CpusMeasurementListNumber,"").
make_string_t(_T,_L,0,Str)-> Str;
make_string_t(T,List,Nu,S) ->
    [A|B] = List,
    if A  /= "n/a" ->    
        THIS:set_attribute(list_to_atom("cpu" ++ integer_to_list(T)),round(A)),
        make_string_t(T+1,B,Nu-1,S ++ ", cpu#" ++ integer_to_list(T) ++" "++ integer_to_list(round(A)) ++ "%");
    true ->
        THIS:set_attribute(list_to_atom("cpu" ++ integer_to_list(T)),"n/a"),
        make_string_t(T+1,B,Nu-1,S)
    end.    



%% @spec get_cpu_for_linux(Host) -> ok
%% Host = string()
%% @doc  Get linux machine cpu information
get_cpu_for_linux(Host) ->
    List = platform:cpuUsed(Host,0,0,[]),  
    [UtilizationT] = lists:sublist(List,1,1),    
    if UtilizationT < 0 ->
        THIS:set_attribute(utilization,"n/a"),
        THIS:set_attribute(?NO_DATA,true),
        THIS:set_attribute(?CATEGORY,?NO_DATA),
	    THIS:set_attribute(?STATE_STRING,"timeout");     
    true ->
        case List of        
        {error,_} ->
            THIS:set_attribute(utilization,"n/a"),
            THIS:set_attribute(?NO_DATA,true),
            THIS:set_attribute(?CATEGORY,?NO_DATA),
	        THIS:set_attribute(?STATE_STRING,"timeout");     
        [] ->
            THIS:set_attribute(utilization,"n/a"),
            THIS:set_attribute(?NO_DATA,true),
            THIS:set_attribute(?CATEGORY,?NO_DATA),
	        THIS:set_attribute(?STATE_STRING,"timeout");
        _ ->        
            case is_integer(UtilizationT) of
            true ->
	            if UtilizationT > 100 ->
	                Utilization = 100;
                true ->
                    Utilization = UtilizationT
                end;                      
            false ->
	            To_Integer = round(UtilizationT),
	            if To_Integer > 100 ->
	                Utilization = 100;
                true ->
                    Utilization = To_Integer
                end
            end,            
	       THIS:set_attribute(utilization,Utilization),
	       THIS:set_attribute(?STATE_STRING,integer_to_list(Utilization) ++ "% Utilization")
        end   
    end.    


make_integer(F) ->
    String = float_to_list(F),
    List = string:tokens(String,"."),	
	[Big] = lists:sublist(List,1,1),
	[Li] =  lists:sublist(List,2,1),
	L = string:substr(Li,1,1),
	Num = list_to_integer(L),
	if  Num > 5 ->
	   list_to_integer(Big) + 1;
    true ->
       list_to_integer(Big)
    end.	   
	
	
get_time(Data) ->
	[First] = lists:sublist(Data,1,1),
    [Judge] = lists:sublist(string:tokens(First," "),1,1),	
	if Judge == "ERROR:" ->
	    [Reason] = lists:sublist(string:tokens(First," "),2,1),
	    THIS:set_attribute(?STATE_STRING,Reason);
	true ->
        [TimeList] = lists:sublist(Data,4,1),	
	    [Time] = lists:sublist(string:tokens(TimeList," "),2,1),
	    list_to_integer(Time)
	end.	


get_cpu_num(Data) ->
	get_cpu_num_t(Data,length(Data),0).
get_cpu_num_t(_L,0,CPU_Num) -> CPU_Num-1;	
get_cpu_num_t(L,Num,E) ->	
    [A|B] = L,
	K = string:str(A,"name:"),
    if K > 0 ->
	    get_cpu_num_t(B,Num-1,E+1);
	true ->
	    get_cpu_num_t(B,Num-1,E)
    end. 	

get_cpu_inv(Data) ->
	get_cpu_inv_t(Data,length(Data),1,[]).
get_cpu_inv_t(_L,0,_N,_List) -> _List;    
get_cpu_inv_t(L,Num,N,Li) ->
    [A|B] = L,
	K = string:str(A,"PERF_100NSEC_TIMER_INV"),
	if K > 0 ->
	    List = string:tokens(A," "),
        [Inv] = lists:sublist(List,2,1), 		
        get_cpu_inv_t(B,Num-1,N+1,lists:append(Li,[{N,list_to_integer(Inv)}]));
	true ->
       	get_cpu_inv_t(B,Num-1,N,Li)
	end.	


get_inv_add(List) ->
    get_inv_add_t(List,length(List),0).
get_inv_add_t(_L,0,Cost) ->Cost;
get_inv_add_t(L,Num,C) ->	
    [A|B] =  L,
	{N,Co} = A,
	get_inv_add_t(B,Num-1,C+Co).


get_evcpu_used(L1,L2,T1,T2,CpuN) ->
    get_evcpu_used_t(L1,L2,length(L2),T1,T2,CpuN,"").
get_evcpu_used_t(Li1,Li2,0,Time1,Time2,Cpu,Cost) -> Cost; 	
get_evcpu_used_t(List1,List2,Num,Ti1,Ti2,CpuNum,Co) ->    
	[A|B] = List1,
	[C|D] = List2,
	{A1,A2} = A,	
    {C1,C2}	= C,	
	Used1 = "cpu" ++ integer_to_list(A1) ++ integer_to_list(make_integer(100 - (100 * (C2 - A2))/((Ti2 - Ti1)/CpuNum))),
	get_evcpu_used_t(B,D,Num-1,Ti1,Ti2,CpuNum,Co ++ Used1 ).

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% Params = [term()]
%% Reason = string()
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



get_cpu_cmd(Machine) ->
    {Osfamily, Osname} = os:type(),
	case Osfamily of       		
	    win32 ->
		    if Machine == [] ->
			    {ok,"tools\\perfex.exe" ++ " =238"};
			true ->	
		        case Machine#machine.method of
			    "NetBIOS" ->
		            case Machine#machine.os of
			        "nt" ->
		                {ok,"tools\\perfex.exe" ++ " =238" ++  " -connect " ++ Machine#machine.host ++ " -u " ++ Machine#machine.login ++ " -p "  ++ Machine#machine.passwd};
			        _ ->
                        {error,"can't support this method"} 
				    end;	
			    "SSH" ->
			        {ok,sshcommandline:getsshcommand(Machine)}		
			    end
			end;	 
        unix ->
            case Machine#machine.os of
			"nt" ->
			    {error,"can't support this method"};
			_ ->
                {ok,sshcommandline:getsshcommand(Machine)}
        end				
    end. 			
	


