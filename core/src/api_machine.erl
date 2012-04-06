%% 
%% @doc api of group operation
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_machine).
-extends(api_siteview).
-include("monitor.hrl").
-include("remoteMachine.hrl").
-compile(export_all).
-define(MONITOR_SETS_DIR,"templates.machine/server_monitorset").

-export([get_ntmachine/0,get_unixmachine/0,get_machine/1,create_machine/1,update_machine/1,delete_machine/1,
          save_tagToMachine/2,removeFromLabel/2,update_Machines/1, get_Machine_ByTag/5,get_Machine_ByTagTree/6, get_MachineTemplateNames/0,
          delete_machines/1,getDisks/1,getDynamicData/1,configure_template/2,crate_monitors/2,
          get_refreshes/1, getAllMachine/4, get_machinename/1, getMachineByNotOs/5, getMachineByOs/5,get_DeviceByType/5,create_machines/3,
          getMachineByMethod/5, getMachineByHost/5]).

%% @spec get_ntmachine()-> ({error,Reason}|Result)
%% where
%%		Reason = atom()
%%		Result=[#machine{}]
%% @doc get nt machine list,return a list of machine record
%% release
get_ntmachine()->
	remoteMachineTag:getMachineByOs("nt", 0, 0, "", "").

%% @spec get_unixmachine()-> ({error,Reason}|Result)
%% where
%%		Reason = atom()
%%		Result=[#machine{}]
%% @doc get unix machine list,return a list of machine record
%% release
get_unixmachine()->
	Machine = remoteMachineTag:getMachineByNotOs("nt", 0, 0, "", ""),
    Machine.

%% @spec get_DeviceByType(Type,Index,Count,Sort,SortType)-> ({error,Reason}|Result)
%% where
%%      Type = [Device]
%%  @doc get Device by Device TypeList. Device = ("SWITCH" | "ROUTE_SWITCH" | "ROUTE" | "FIREWALL" | "SERVER")
%% release
get_DeviceByType(Type,Index,Count,Sort,SortType)->
    remoteMachineTag:getDeviceByType(Type,Index,Count,Sort,SortType).
    

%% @spec create_machine(Machine)-> ({error,Reason}|{ok,Id})
%% where
%%		Id = atom()
%%		Reason = atom() | string()
%%		Machine=#machine{}
%% @doc insert a new machine to database
%% release
create_machine(Machine=#machine{})->
	remoteMachineTag:create_machine(Machine);
create_machine(_)->{error,parameter_error}.

%% @spec create_machines(IPPre, Begin, End)-> ok
%% where
%%		IPPre = string()
%%		Begin = integer()
%%		End = integer
%% @doc insert machines to database
%% release
create_machines(IPPre, Begin, End) ->
    if
        Begin > 255 ->
            ok;
        Begin =< End ->
            IP = string:join([IPPre, erlang:integer_to_list(Begin)], "."),
            Result = create_machine(#machine{host=IP,os="linux"}),
            io:format("IP: ~p~n", [IP]),
            io:format("Result: ~p~n", [Result]),
            create_machines(IPPre, Begin+1, End);
        true ->
            ok
    end.
    
    
	
%% @spec update_machine(Machine)-> ({error,Reason}|{ok,update_ok})
%% where
%%		Reason = atom() | string()
%%		Machine=#machine{}
%% @doc update a machine's setting data
%% release
update_machine(Machine=#machine{})->
	case remoteMachineTag:update_machine(Machine) of
		{ok,_}->
			{ok,"update_ok"};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end;
update_machine(_)->{error,parameter_error}.

%% @spec get_machine(Id)-> ({error,Reason}|Result)
%% where
%%		Reason = atom()
%%		Result=#machine{}
%% @doc get a machine's information
%% release
get_machine(Id) when is_atom(Id)->
	remoteMachineTag:get_machine(Id).
    

	
%% @spec delete_machine(Id)-> ({error,Reason}|{ok,Result})
%% where
%%		Id = (atom() | string())
%%		Reason = atom()
%%		Result= atom()
%% @doc delete a machine from database
%% release
delete_machine(Id) when is_atom(Id)->
	delete_machine(atom_to_list(Id));
delete_machine(Id)->
	remoteMachineTag:deleteMachine(Id).
    

%% @spec delete_machines(Ids)-> ({error,Reason}|{ok,Result})
%% where
%%		Ids = [string()]
%%		Reason = atom()
%%		Result= {ok, [string()]}
%% @doc delete machines by some machine id
%% release
delete_machines(Ids) ->
    {ok,delete_machines_t(Ids)}.
delete_machines_t([]) ->
    [];
delete_machines_t([Id|T]) ->
    case delete_machine(Id) of
        {ok, _} ->
            [Id];
        Other ->
            []
    end ++
    delete_machines_t(T).
    

%% @spec save_tagToMachine(Machs, Tag)-> ({error,Reason}|{ok,Result})
%% Machs = [#machine{}|T]
%% Tag = string()
%% Reason = string()
%% Result= string()
%% @doc save tag to machines
%% release
save_tagToMachine([], Tag) ->
    {ok, "saveTagOk"};
save_tagToMachine([Machs = #machine{}|T], Tag) ->
    remoteMachineTag:addMachineToTag(Tag, Machs),
    save_tagToMachine(T, Tag);
save_tagToMachine([H|T], Tag) ->
    save_tagToMachine(T, Tag).
    

%% @spec update_Machines(Machs)-> ({error,Reason}|{ok,Result})
%% Machs = [#machine{}|T]
%% Reason = string()
%% Result= string()
%% @doc update machines
%% release
update_Machines(Maches) ->
    {ok, update_Machines_t(Maches)}.
update_Machines_t([]) ->
    [];
update_Machines_t([Mach = #machine{}|T]) ->
    case remoteMachineTag:update_machine(Mach) of
        {ok, Id} ->
            [Id];
        _ ->
            []
    end ++
    update_Machines_t(T);
update_Machines_t([H|T]) ->
    update_Machines_t(T).

%% @spec getMachineByNotOs(OsType, Index, Count, Sort, SortType)-> (Result)
%% OsType = string()
%% Index = integer()
%% Count = integer()
%% Sort = string()
%% SortType = string()
%% Result= ([#machine{}|T])
%% @doc get machine by not ostype 
%% release
getMachineByNotOs(OsType, Index, Count, Sort, SortType) ->
    remoteMachineTag:getMachineByNotOs(OsType, Index, Count, Sort, SortType).

%% @spec getMachineByMethod(OsType, Index, Count, Sort, SortType)-> (Result)
%% OsType = string()
%% Index = integer()
%% Count = integer()
%% Sort = string()
%% SortType = string()
%% Result= ([#machine{}|T])
%% @doc get machine by method
%% release
getMachineByMethod(Method, Index, Count, Sort, SortType) ->
    remoteMachineTag:getMachineByMethod(Method, Index, Count, Sort, SortType).

%% @spec getMachineByHost(OsType, Index, Count, Sort, SortType)-> (Result)
%% OsType = string()
%% Index = integer()
%% Count = integer()
%% Sort = string()
%% SortType = string()
%% Result= ([#machine{}|T])
%% @doc get machine by host
%% release
getMachineByHost(Host, Index, Count, Sort, SortType) ->
    remoteMachineTag:getMachineByHost(Host, Index, Count, Sort, SortType).
    
%% @spec getMachineByOs(OsType, Index, Count, Sort, SortType)-> (Result)
%% OsType = string()
%% Index = integer()
%% Count = integer()
%% Sort = string()
%% SortType = string()
%% Result= ([#machine{}|T])
%% @doc get machine by not ostype 
%% release
getMachineByOs(OsType, Index, Count, Sort, SortType) ->
    remoteMachineTag:getMachineByOs(OsType, Index, Count, Sort, SortType).

%% @spec removeFromLabel(TagId, MacId)-> ({error,Reason}|{ok,Result})
%% TagId = string()
%% MacId = string()
%% Reason = string()
%% Result= string()
%% @doc remove machine from label
%% release
removeFromLabel(TagId, MacId) ->
    remoteMachineTag:removeFromLabel(TagId, MacId).
    
    
%% @spec removeMachinesFromLabel(TagId, [MacId|T]) -> ({error,Reason}|{ok,Result})
%% TagId = string()
%% MacId = atom()
%% T = list()
%% Reason = string()
%% Result= string()
%% @doc remove many machine from label
%% release
removeMachinesFromLabel(TagId, []) ->
    {ok, "delete machine from label ok"};
removeMachinesFromLabel(TagId, [MacId|T]) when erlang:is_atom(MacId) ->
    remoteMachineTag:removeFromLabel(TagId, MacId),
    removeMachinesFromLabel(TagId, T);
removeMachinesFromLabel(TagId, [H|T]) ->
    removeMachinesFromLabel(TagId, T).
    

    

%% @spec get_Machine_ByTag(TagId, Index, Count, Sort, SortType)-> (Result)
%% TagId = string()
%% Index = integer()
%% Count = integer()
%% Sort = string()
%% SortType = string()
%% Result= ([#machine{}|T])
%% @doc get machine by tag
%% release
get_Machine_ByTag(TagId, Index, Count, Sort, SortType) ->
    remoteMachineTag:get_Machine_ByTag(TagId, Index, Count, Sort, SortType).

%% @spec get_Machine_ByTagTree(TagId, Index, Count, Sort, SortType)-> (Result)
%% TagId = string()
%% Index = integer()
%% Count = integer()
%% Sort = string()
%% SortType = string()
%% Result= ([#machine{}|T])
%% @doc get machine by tag
%% release
get_Machine_ByTagTree(Ids,TagId,Index, Count, Sort, SortType) ->
    remoteLabelTree:get_allMachinesbylabel(Ids,TagId,Index, Count, Sort, SortType).
    
%% @spec get_machinename(Id)-> (Result)
%% Id = list()
%% Result= #machine{}
%% @doc get Machine by id
%% release
get_machinename(Id) ->
    dbcs_machine:get_machinename(Id).


%% @spec getAllMachine(Index, Count, Sort, SortType)-> (Result)
%% Index = integer()
%% Count = integer()
%% Sort = string()
%% SortType = string()
%% Result= ([#machine{}|T])
%% @doc get All Machine
%% release
getAllMachine(Index, Count, Sort, SortType) ->
    remoteMachineTag:getAllMachine(Index, Count, Sort, SortType).



%% @spec get_MachineTemplateNames()-> (Result)
%% Result= ([{string(),string()}|T])
%% @doc get machine os template names
%% release
get_MachineTemplateNames() ->
    Temps =
    case file:list_dir("template.os") of
        {ok,Filenames} ->
            get_MachineTemplateNames_t(Filenames);  
        _ ->
            [] 
    end,
    lists:keysort(1, Temps).
get_MachineTemplateNames_t([]) -> 
    [];
get_MachineTemplateNames_t([A|B]) ->
    SL= string:tokens(A,"."),
    if 
        length(SL) == 2 ->
            [Name,Pre] = SL,
            case re:run(Pre,"config") of
                {match,_} ->
                    [{gettemplatename("template.os"++"/"++A), Name}];
                _ ->
                    []
            end ++
            get_MachineTemplateNames_t(B);
        true ->
            get_MachineTemplateNames_t(B)   
    end;
get_MachineTemplateNames_t(_) ->
    [].
gettemplatename(Filename) ->
     {ok,Data} =  file:consult(Filename),
     case lists:keysearch(name,1,Data) of
          {value,{name,Name}} -> Name;
	  _ -> nomatch
     end.

%%%%%%%%%%%%%%%%%%%%% Detector set remote server api %%%%%%%%%%%%%%%%%%%%%

%% @spec get_template(File)-> ({ok,TemplateData} | {error,Err})
%% where
%%	File = string()
%%	TemplateData = #monitor_set{}
%%	Err = atom()
%% @doc get detail information of a remote server monitor set. File is the file name of monitor set,TemplateData is the Data of the monitor set,is a monitor_set record release
get_template(File)->
	case file:consult(?MONITOR_SETS_DIR ++ "/" ++ File) of
		{ok,[Ms|_]}->
			{ok,Ms};
		{error,Err}->
			{error,Err};
		Else->
			{error,Else}
	end.
    
%% @spec configure_template(File,Params)->({ok,MonitorSet} | {error,Err})
%% where
%%	File = string()
%%	Params = [{Key,Value}]
%%	Key = string()
%%	Value = string()
%%	MonitorSet = #monitor_set{}
%% @doc configure monitor set with input value
%% release
configure_template(File,Params)->
    io:format("Params: ~p~n", [Params]),
    io:format("File: ~p~n", [File]),
	case get_template(File) of
		{ok,Ms}->
            io:format("Ms: ~p~n", [Ms]),
			{ok,Ms#monitor_set{monitors=monitor_set_template:configure_monitors(Params,Ms#monitor_set.monitors)}};
		{error,Err}->
			{error,Err};
		Else->
			{error,Else}
	end.

%% @spec crate_monitors(Parent, Params)->Result
%% Params = [{Parent,Items}]
%% Parent = term()
%% Items = list()
%% @doc create monitor by parent and monitor's param
%% release
crate_monitors(_, []) ->
    [];
crate_monitors(Parent, [Items|T]) ->
    case api_monitor_set:create_monitor_from_monitorset(Parent,Items) of
        {ok, Monitor} ->
            [Monitor];
        _ ->
            []
    end ++
    crate_monitors(Parent, T).


%% @spec get_refreshes(Ids)->Result
%% Ids = list()
%% Result = list()
%% @doc refresh monitor some monitor id.
%% release
get_refreshes(Ids) when erlang:is_list(Ids) ->
    {ok, get_refreshes_t(Ids)}.
get_refreshes_t([]) ->
    [];
get_refreshes_t([Id|T]) when erlang:is_list(Id) ->
    AtomId = erlang:list_to_atom(Id),
    case api_monitor:get_run_info(AtomId) of
        [] ->
            [];
        Value when erlang:is_list(Value) ->
            [Value];
        _ ->
            []
    end ++
    get_refreshes_t(T);
get_refreshes_t([Id|T])->
	get_refreshes_t(T).

%% Key value list converting the position of Key and Value
revisionKeyValueList([]) ->
    [];
revisionKeyValueList([{Key, Value}|T]) ->
    [{Value, Key}] ++
    revisionKeyValueList(T);
revisionKeyValueList([H|T]) ->
    revisionKeyValueList(T).
    

%% @spec getDisks(Machine)-> (Result)
%% Machine= string()
%% Result= ([{string(),string()}|T])
%% @doc get disks of this ip
%% release
getDisks(Machine) ->
    lists:map(fun(X)->[Y|_]=string:tokens(X," "),{X,Y} end,platform:getDisks(Machine)).
 

%% The following is to obtain dynamic data monitors
    
%% @spec getDynamicData(Params)-> (Result)
%% Params= ([{atom(),atom()}])
%% Result= ([{string(),string()}|T])
%% @doc get dynamic data
%% release
getDynamicData(Params) when erlang:is_list(Params) ->
    case lists:keysearch(class, 1, Params) of
        {value, {class, Value}} ->
            Monitor = Value:new(),
            try getDynamicData_t({Value, Params, Monitor}) of
                DValue ->
                    DValue
            catch
                _:_ ->
                    []
            after
                Monitor:delete()
            end;
        _ ->
            []
    end;
getDynamicData(Params) ->
    [].








getDynamicData(Node,Params) when erlang:is_list(Params) ->
	rpc:call(Node, api_machine, getDynamicData, [Params]).
%%     case lists:keysearch(class, 1, Params) of
%%         {value, {class, Value}} ->
%%             Monitor = Value:new(),
%%             try getDynamicData_t({Value, Params, Monitor}) of
%%                 DValue ->
%% 					io:format("*****************~p~n",DValue),
%%                     DValue
%%             catch
%%                 _:_ ->
%%                     []
%%             after
%%                 Monitor:delete()
%%             end;
%%         _ ->
%%             []
%%     end;
%% getDynamicData(Params) ->
%%     [].













%% The following is a dynamic data processing logic detector
getDynamicData_t({diskspace_monitor,Params,Monitor}) ->             %%Hard detector
    Monitor:getScalarValues(disk,Params);
    
getDynamicData_t({service_monitor,Params,Monitor}) ->             %%Services detector
    Monitor:getScalarValues(service,Params);
    
getDynamicData_t({snmp_monitor,Params,Monitor}) ->             %%Snmp oid List
    Monitor:getScalarValues(oid,Params);

getDynamicData_t({network_bandwidth_monitor,Params,Monitor}) ->             %%Snmp oid List
    io:format("~nParams:~p~n",[Params]),
    Monitor:getBrowseData(Params);

getDynamicData_t({interface_monitor,Params,Monitor}) ->             %%Snmp oid List
    io:format("~nParams:~p~n",[Params]),
    V = Monitor:getBrowseData(Params),
    io:format("~nInterfaceMonitor V:~p~n",[V]),
    revisionKeyValueList(V);
    
getDynamicData_t({browsa_cpu_utilization,Params,Monitor}) ->             %%Snmp oid List
    io:format("~nParams:~p~n",[Params]),
    V = Monitor:getBrowseData(Params),
    io:format("~ncpu monitor V:~p~n",[V]),
    revisionKeyValueList(V);

getDynamicData_t(_) ->
    [].
    
   

%% Remove the monitor through the device.

deleteMachineAndMonitor(Machine)->
    Host = lists:nth(1,string:tokens(Machine#machine.host,"\\")),
    io:format("Host : ~p~n",[Host]),
    MonitorList = api_monitor:browse('all_type',[{"match_machine",Host}]),
    io:format("MonitorList : ~p~n",[MonitorList]),
    Result = [ api_monitor:delete(proplists:get_value('id',Monitor)) || Monitor<- MonitorList],
    io:format("Result : ~p~n",[Result]),
    case proplists:get_value(error,Result,[]) of
        []->
            case delete_machine(Machine#machine.id)  of
                {error,Err}->{error,Err};
                _->
                    Monitorids = [ proplists:get_value('id',Monitor) || Monitor<- MonitorList],
                     io:format("Monitorids : ~p~n",[Monitorids]),
                    {ok,Monitorids}
            end;
        _->{error,"delete failed"}
    end.
    
deleteMachinesAndMonitors(HostNameList)->
    Result = [deleteMachineAndMonitor(Machine) || Machine<-HostNameList],
    case proplists:get_value(error,Result,[]) of
        []->
            Monitorids = get_ids(Result,[]),
            io:format("Monitorids : ~p~n",[Monitorids]),
            {ok,Monitorids};
        _->{error,"delete failed"}
    end.
get_ids([],EndIds)->EndIds;
get_ids([{ok,Ids}| Last],EndIds)->
      Ids++get_ids(Last,EndIds).
    
    
    
    
	