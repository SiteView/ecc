-module(remoteMachineTag).
-behaviour(gen_server).
-define(SERVER,'ecc_remoteMachineTag').
-compile(export_all).
-include("monitor.hrl").
-include("remoteMachine.hrl").

-record(state, {parent,files=[]}).

-export([start_link/0, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         
-export([remove_AllLabel/0, addMachineToTag/2, create_label/1, getTagById/1, getMachineByOs/5,getMachineByStatus/5,
            getMachineByMethod/5, getAllMachine/4, getMachineByUserDefineTag/5, get_Machine_ByTag/5,getUserDefineTag/4,
            deleteMachine/1, removeFromLabel/2, remove_label/1, get_machine/1, update_machine/1, get_all_label/0,translate_OS/1,
            get_label_type/1, update_label/2, getMachineByNotOs/5, create_machine/1,getDeviceByType/5,get_root_label/0,getMachineByUserDefineTags/5]).
         

%% gen_server callbacks

init(Opts) ->
    writeDefaultTag(),
    %%
	{ok,#state{files=Opts}}.

handle_call({create_machine, Machine, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    Id = dbcs_base:get_id(),
    Exist = exist_hostmachine(Machine#machine.host),
    IsIP = is_machineIp(Machine#machine.host),
    Reply =
    if 
        IsIP =:= false ->
            {error, 'ip format error'};
        Exist =:= true ->
            {error, 'existed_this_host'};
        true ->
            case dbcs_machine:create_machine(Machine#machine{id=Id}) of
                {ok,_}->
					% create index
					%index_store:update(machine,Machine),
					
                    {ok,Id};
                {error,Err}->
                    {error,Err};
                Err2->
                    {error,Err2}
            end
    end,
    {reply, Reply, State};
handle_call({update_label, OriId, Label, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    Reply =
    if
        OriId =:= Label#machine_label.id ->
            case dbcs_machine:update_label(Label) of
                {ok, _} ->
                    {ok, OriId};
                Err ->
                    {error, "Update Label Fail"}
            end;
        true ->
            case dbcs_machine:remove_label(OriId) of
                {ok, _} ->
                    case dbcs_machine:create_label(Label) of
                        {ok, _} ->
                            NTagId = Label#machine_label.id,
                            OldMacTag = 
                            case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, OriId) of
                                {ok, [{OriId, Val1}]} ->
                                    Val1;
                                _ ->
                                    []
                            end,
                            remoteMachineConfig:remove(?REMOTECONF_RELTAGANDMA, OriId),
                            remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, NTagId, OldMacTag),
                            Fun = 
                                fun(X) ->
                                    MacId = X,
                                    OldTagMac =
                                        case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, MacId) of
                                            {ok, [{MacId, Val}]} ->
                                                Val;
                                            _ ->    
                                                []
                                        end,
                                    NMacTag = 
                                        case lists:member(OriId, OldTagMac) of
                                            true ->
                                                lists:delete(OriId, OldTagMac);
                                            _ ->
                                                OldTagMac
                                                
                                        end,    
                                    NMacTag1 =     
                                        case lists:member(NTagId, NMacTag) of
                                            true ->
                                                NMacTag;
                                            _ ->
                                                NMacTag ++ [NTagId]
                                        end,
                                    remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, MacId, NMacTag1)
                                end,
                            lists:map(Fun, OldMacTag),
                            {ok, "update label ok"};
                        _ ->
                            {error, "Update Label Fail"}
                    end;
                _ ->
                    {error, "Update Label Fail"}
            end
    end,
    {reply, Reply, State};
handle_call({update_machine,Mach, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    IsIP = is_machineIp(Mach#machine.host),
    Reply = 
    if 
        IsIP =:= false ->
            {error, 'ip format error'};
        true ->
            case dbcs_machine:update_machine(Mach) of
                {ok, _} ->
                    % create index
                   % index_store:update(machine,Mach),
                    {ok, Mach#machine.id};
                _ ->
                    {error, "update machine fail"}
            end
    end,
    {reply, Reply, State};
handle_call({remove_label, Id, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    TagId = Id,
    Reply =
    case dbcs_machine:remove_label(TagId) of
        {ok, _} ->
            OldTagMac = 
                case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, TagId) of
                    {ok, [{TagId, Val1}]} ->
                        Val1;
                    _ ->
                        []
                end,
            remoteMachineConfig:remove(?REMOTECONF_RELTAGANDMA, TagId),
            Fun = 
                fun(X) ->
                    MacId = X,
                    OldMacTag =
                        case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, MacId) of
                            {ok, [{MacId, Val}]} ->
                                Val;
                            _ ->
                                []
                        end,
                    NMacTag = 
                        case lists:member(TagId, OldMacTag) of
                            true ->
                                lists:delete(TagId, OldMacTag);
                            _ ->
                                OldMacTag
                        end,    
                    remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, MacId, NMacTag)
                end,
            lists:map(Fun, OldTagMac),
            {ok, "delete tag ok"};
        _ ->
            {error, "delete tag fail"}
    end,
    {reply, Reply, State};
handle_call({removeFromLabel, TagId, MacId, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    OldMacTag = 
        case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, MacId) of
            {ok, [{MacId, Val1}]} ->
                Val1;
            _ ->
                []
        end,
    OldTagMac =
    case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, TagId) of
        {ok, [{TagId, Val}]} ->
            Val;
        _ ->
            []
    end,
    NMacTag = 
        case lists:member(TagId, OldMacTag) of
            true ->
                lists:delete(TagId, OldMacTag);
            _ ->
                OldMacTag
        end,    
    remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, MacId, NMacTag),
    NTagMac = 
        case lists:member(MacId, OldTagMac) of
            true ->
                lists:delete(MacId, OldTagMac);
            _ ->
                OldTagMac
        end,    
    remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, TagId, NTagMac),
    Reply = {ok, "delete machine form label ok"},
    {reply, Reply, State};
handle_call({delete_machine, Id, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    MacId = Id,
    Reply =
    case dbcs_machine:delete(MacId) of
        {ok, _} ->
			% remove index
			index_store:remove(machine, MacId),
			
            OldMacTag = 
                case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, MacId) of
                    {ok, [{MacId, Val1}]} ->
                        Val1;
                    _ ->
                        []
                end,
            remoteMachineConfig:remove(?REMOTECONF_RELTAGANDMA, MacId),
            Fun = 
                fun(X) ->
                    TagId = X,
                    OldTagMac =
                        case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, TagId) of
                            {ok, [{TagId, Val}]} ->
                                Val;
                            _ ->
                                []
                        end,
                    NMacTag = 
                        case lists:member(MacId, OldTagMac) of
                            true ->
                                lists:delete(MacId, OldTagMac);
                            _ ->
                                OldTagMac
                        end,    
                    remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, TagId, NMacTag)
                end,
            lists:map(Fun, OldMacTag),
            {ok, "delete machine ok"};
        _ ->
            {error, "delete machine fail"}
    end,
    {reply, Reply, State};
handle_call({create_label, Label, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    Id = Label#machine_label.id,
    Reply =
	case dbcs_machine:create_label(Label) of
        {ok,_}->
            {ok, Id};
        {error, Err} ->
            {error,Err};
        Err1 ->
            {error, Err1}
    end,
    {reply, Reply, State};
handle_call({addMachineToTag, Tag, Machine, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    MacId = Machine#machine.id,
    TagId = Tag,
    %% 此标签组下面目前有的服务器
    OldTagMac =
    case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, TagId) of
        {ok, [{TagId, Val}]} ->
            Val;
        _ ->
            []
    end,
    OldMacTag = 
    case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, MacId) of
        {ok, [{MacId, Val1}]} ->
            Val1;
        _ ->
            []
    end,
    %% 此标签组下面现在有的服务器
    NTagMac =
    case lists:member(MacId, OldTagMac) of
        true ->
            OldTagMac;
        _ ->
            OldTagMac ++ [MacId]
    end,
    %% 
    NMacTag = 
    case lists:member(TagId, OldMacTag) of
        true ->
            OldMacTag;
        _ ->
            OldMacTag++[TagId]
    end,    
    remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, TagId, NTagMac),
    remoteMachineConfig:set(?REMOTECONF_RELTAGANDMA, MacId, NMacTag),
    Reply = {ok, "Add Machine To Tag Ok"},
    {reply, Reply, State};
handle_call({remove_AllLabel, HostName}, _From, State) ->
    erlang:put(hostname, HostName),
    Reply = remove_AllLabel_tool(),
    {reply, Reply, State};
handle_call(Other, _From, State) ->
    {reply, {error, notsupport},State}.
    
handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

start_link() ->
    start_link([]).
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).
stop() ->
    cast(stop).
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
    
cast(Req) ->
    gen_server:cast(?SERVER, Req).


%% 工具方法

%% 删除所有标签组的工具方法，初始化
remove_AllLabel_tool() ->
    case dbcs_machine:remove_AllLabel() of
        {ok, _} ->
            case remoteMachineConfig:remove(?REMOTECONF_SET, ?ISCREATEDDEFAULTTAG) of
                {ok, _} ->
                    remoteMachineConfig:removeAll(?REMOTECONF_RELTAGANDMA);
                _ ->
                    {error, "delete fail"}
            end;
        _ ->
            {error, "delete fail"}
    end.

get_Machine_ByTagTool(Ids,TagId, Index, Count, Sort, SortType) ->
    Tag = getTagByIdTool(TagId),
    Result = handleMachineByTagType(Ids,{Tag#machine_label.type, Tag, Index, Count, Sort, SortType}).

%% @spec exist_hostmachine(Host)-> Result
%%  Host = string()
%%	Result= true|false
%% @doc get unix machine list,return a list of machine record
%%
exist_hostmachine(Host) ->
    case dbcs_machine:get_machine_by_host(Host) of
        [Mach = #machine{}|T] ->
            io:format("ContentMahcineHost = ~p~n", [Mach#machine.host]),
            if
                Mach#machine.host =:= Host ->
                    true;
                true ->
                    false
            end;
        _ ->
            false
    end.

getTagByIdTool(TagId) ->
    Tg =
    case textutils:contains(TagId, " ") of
        true ->
            WhereCondition=
                #query_condition_where{
                        where=
                        [
                            {"my.id","=","'"++TagId++"'","&"}
                        ]
                    },
            Index = 0,
            Count = 0,
            Sort = "",
            SortType = "",
            Condition=
                #query_condition1{
                    where=WhereCondition,
                    index=Index,
                    count=Count,
                    sort=Sort,
                    sortType=SortType
                },
            case api_machine_adapter:get_tag(Condition) of
                [Label = #machine_label{}] ->
                    Label;
                _ ->
                    {error,"search tag error"}
            end;
        _ ->    
            case dbcs_machine:get_LabelById(TagId) of
                [Label = #machine_label{}] ->
                    Label;
                Other ->
                    io:format("Other: ~p~n", [Other]),
                     {error,"search tag error"}
            end
    end,
    Tg.


%% 为用户自定义标签组获取统计
labelCount([]) ->
    [];
labelCount([Label=#machine_label{}|T]) ->
    Labels =
    case Label#machine_label.type of
        ?SYSTAG_USERDEFINE ->
            Id = Label#machine_label.id,
%% io:format("++++~w++++++++++>~w~n",[Id,totalMachine(Id)]),	    
            case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, Id) of
                {ok, [{Id, Val}]} ->
                    case Val of
                        Vlist when erlang:is_list(Vlist) ->
                      %%    io:format("===================UserTag====================: ~p~n", [length(get_Machine_ByTag("",Id,0,0,"",""))]),
                           %% Label#machine_label{index=string:len(Vlist)};
			   Label#machine_label{index=length(get_Machine_ByTag("",Id,0,0,"",""))};
                        _ ->
                           Label#machine_label{index=0}
                    end;
                _ ->
		
                       %%   io:format("===================UserTag====================: ~p~n", [Label#machine_label.name]),
                    Label#machine_label{index=length(get_Machine_ByTag("",Id,0,0,"",""))}
            end;
        _ ->
            Result = get_Machine_ByTagTool([],Label#machine_label.id, 0, 1, "", ""),
	 %%   io:format("=====================>~w~n",[Result]),
            case Result of
                [M=#machine{}] ->
                    Label#machine_label{index=M#machine.total};
                _ ->
                    Label#machine_label{index=0}
            end
    end,
    [Labels|labelCount(T)];
labelCount([H|T]) ->
    labelCount(T).


%% totalMachine(Id)->
%%   case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, Id) of
%%	{ok, [{Id, Val}]} ->
%%	    case Val of
%%		Vlist when erlang:is_list(Vlist) ->
%% io:format("++++++++++List++++>~w~n",[Vlist]),	    
%%		    string:len(Vlist);
%%		_ ->
%%		    0
%%	    end;
%%	_ ->
%%	    0
%%    end.
	


%% 对标签进行排序, 按照以下进行排序: OS类型, 链接状态不成功的, 链接方法（SSH，Telnet，WMI）, All, 下面接着显示用户定义的
sortTag(Labels) ->
    Dict = dict:new(),
    NDict = sortTag_t(Labels, Dict),
 %%   Fun = 
 %%       fun(E1, E2) ->
  %%          if 
   %%             E1#machine_label.id =< E2#machine_label.id ->
   %%                 true;
    %%            true ->
     %%               false
      %%      end
      %%  end,
    SYSTAG_OS =
    case dict:find(?SYSTAG_OS, NDict) of
        {ok, Value1} ->Value1;
      %%      lists:sort(Fun, );
        _ ->
            []
    end,
    SYSTAG_STATUS_ERROR =
    case dict:find(?SYSTAG_STATUS_ERROR, NDict) of
        {ok, Value2} ->Value2;
          %%  lists:sort(Fun, );
        _ ->
            []
    end,
    Device_type =
    case dict:find(?Device_type, NDict) of
        {ok, Value3} ->Value3;
        %%    lists:sort(Fun, );
        _ ->
            []
    end,
    SYSTAG_METHOD =
    case dict:find(?SYSTAG_METHOD, NDict) of
        {ok, Value4} ->Value4;
      %%      lists:sort(Fun, );
        _ ->
            []
    end,
    SYSTAG_ALL =
    case dict:find(?SYSTAG_ALL, NDict) of
        {ok, Value5} ->Value5;
     %%       lists:sort(Fun, );
        _ ->
            []
    end,
    SYSTAG_USERDEFINE =
    case dict:find(?SYSTAG_USERDEFINE, NDict) of
        {ok, Value6} ->Value6;
         %%   lists:sort(Fun, );
        _ ->
            []
    end,
    SYSTAG_OS ++ SYSTAG_STATUS_ERROR ++Device_type++ SYSTAG_METHOD ++ SYSTAG_ALL ++ SYSTAG_USERDEFINE.
sortTag_t([], Dict) ->
    Dict;
sortTag_t([Label=#machine_label{}|T], Dict) ->
    Labels =
    case dict:find(Label#machine_label.type, Dict) of
        {ok, Value} ->
            Value ++ [Label];
        _ ->
            [Label]
    end,
    NDict = dict:store(Label#machine_label.type, Labels, Dict),
    sortTag_t(T, NDict);
sortTag_t([H|T], Dict) ->
    sortTag_t(T, Dict).


writeDefaultTag() ->
    case remoteMachineConfig:get(?REMOTECONF_SET, ?ISCREATEDDEFAULTTAG) of
        {ok, [{?ISCREATEDDEFAULTTAG, Val}]} ->
            case Val =:= true of
                false ->
                    writeDefaultTag_T(),
                    remoteMachineConfig:set(?REMOTECONF_SET, ?ISCREATEDDEFAULTTAG, true);
               _->
                    ok
            end;
        _ ->
            writeDefaultTag_T(),
            remoteMachineConfig:set(?REMOTECONF_SET, ?ISCREATEDDEFAULTTAG, true)
    end.
writeDefaultTag_T() ->
    %% 为防止不兼容问题, 删除以前旧标签
    %%dbcs_machine:remove_AllLabel(),     
    dbcs_machine:remove_AllSysLabel(),
    %% 添加默认系统标签
    deefultTag_Os(),
    deefultTag_StatusError(),
    deefultTag_Method([{"SSH", "SSH"}, {"Telnet", "Telnet"}, {"rlogin", "rlogin"},{"WMI", "WMI"},{"Snmp", "Snmp"}]),
    deefultTag_DeviceType(?AllDeviceType), 
    %%  "SWITCH" or "ROUTER_SWITCH" or "ROUTER" or "FIREWALL" or "SERVER" 
    deefultTag_All(),
    deefultTag_UserRoot(),
    checkAllUserLabel().

%% 默认标签 操作系统类型
deefultTag_Os() ->
    OsTag = parseAllOsType(?ALLOSTYPE),
    deefultTag_Os_t(OsTag).
deefultTag_Os_t([]) ->
    [];
deefultTag_Os_t([{Name, Value}|T]) ->
    Label = #machine_label{id=Name, name=Name, type=?SYSTAG_OS, syslabel="true", index = "0", hide="false", value=Value,treeindex="0:1"++":"++integer_to_list(random:uniform(99999999))},
    Result = dbcs_machine:create_label(Label),
    deefultTag_Os_t(T);
deefultTag_Os_t([H|T]) ->
    deefultTag_Os_t(T).
    
%% 默认标签 错误连接状态类型
deefultTag_StatusError() ->
    Label = #machine_label{id="Error Status", name="Error Status", type=?SYSTAG_STATUS_ERROR, syslabel="true", index = "0", hide="false", value="Error Status",treeindex="0:2"},
    Result = dbcs_machine:create_label(Label).

%% 默认标签 连接方法类型
deefultTag_Method([]) ->[];
deefultTag_Method([{Name, Value}|T]) ->
    Label =  #machine_label{id=Name, name=Name, type=?SYSTAG_METHOD, syslabel="true", index = "0", hide="false", value=Value,treeindex="0:3"++":"++integer_to_list(random:uniform(999999999))},
    Result = dbcs_machine:create_label(Label),
    deefultTag_Method(T);
deefultTag_Method([H|T]) ->
    deefultTag_Method(T).

%% 默认标签 设备类型
deefultTag_DeviceType([])->[];
deefultTag_DeviceType([{Name,Value}|T]) ->
    Label =  #machine_label{id=Name, name=Name, type=?Device_type, syslabel="true", index = "0", hide="false", value=Value,treeindex="0:4"++":"++integer_to_list(random:uniform(999999999))},
    Result = dbcs_machine:create_label(Label),
    deefultTag_DeviceType(T);
deefultTag_DeviceType([H|T]) ->
    deefultTag_DeviceType(T).

%% 默认标签 所有类型
deefultTag_All() ->
    Label = #machine_label{id="All", name="All", type=?SYSTAG_ALL, syslabel="true", index = "0", hide="false", value="All",treeindex="0:5"},
    Result = dbcs_machine:create_label(Label).

deefultTag_UserRoot() ->
    MaxchildIndex = get_max_user_labelTreeIndex(),
    Label = #machine_label{id=?TreeRootId, name="root", type=?SYSTAG_ROOR, syslabel="true", index = "0", hide="false", value="root",treeindex=?TreeRootIndex,maxchild=MaxchildIndex},
    Result = dbcs_machine:create_label(Label),
    io:format(" ~n Result:~p ",[Result]).

checkAllUserLabel()->
    AllUserLabel = getUserDefineTagRoot(),
    [PLabel] = dbcs_machine:get_LabelById(?TreeRootId),
    checkAllUserLabelroot_t(PLabel,AllUserLabel).

get_max_user_labelTreeIndex()->
        Labels=getLabel_likeindex(?TreeRootIndex++":"),
        FunFilter = 
        fun(Label)->
            TreeIndex = Label#machine_label.treeindex,
            case (string:str(TreeIndex,":") =:= string:rstr(TreeIndex,":")) of
            true->true;
            _->false
            end
        end,
        FilterLabel = lists:filter(FunFilter,Labels),
        Fun_Sort =
         fun(Label1,Label2)->
            TreeIndex1 = Label1#machine_label.treeindex,
            TreeIndex2 = Label2#machine_label.treeindex,
            Index1 = string:substr(TreeIndex1,string:rstr(TreeIndex1,":")+1,string:len(TreeIndex1)),
            Index2 = string:substr(TreeIndex2,string:rstr(TreeIndex2,":")+1,string:len(TreeIndex2)),
            case (list_to_integer(Index1)>list_to_integer(Index2)) of
            true->
                true;
            _->
                false
            end
       end,
       Sort_Label = lists:usort(Fun_Sort,FilterLabel),
       case length(Sort_Label)>0 of
       true->
           MaxLabel = lists:nth(1,Sort_Label),
           MaxTreeIndex = MaxLabel#machine_label.treeindex,
           MaxIndex = string:substr(MaxTreeIndex,string:rstr(MaxTreeIndex,":")+1,string:len(MaxTreeIndex)),
           case list_to_integer(MaxIndex)>100 of
               true->MaxTreeIndex;
               _-> "0:100"
           end;
       _->  "0:100"
       end.

checkAllUserLabelroot_t(_Plabel,[])->ok;
checkAllUserLabelroot_t(PLabel,[Label|Next])->
    Maxchild = PLabel#machine_label.maxchild,
    TreeIndex = 
    case string:substr(Maxchild,string:rstr(Maxchild,":")+1,string:len(Maxchild)) of
        []-> PLabel#machine_label.treeindex ++ ":1";
        M-> PLabel#machine_label.treeindex++":"++ integer_to_list(list_to_integer(M)+1)
    end,
    NLabel = Label#machine_label{treeindex=TreeIndex},
    NPLabel = PLabel#machine_label{maxchild=TreeIndex},
    case dbcs_machine:update_label(NPLabel) of
        {error,Err}-> {error,Err};
         _-> dbcs_machine:update_label(NLabel)
    end,
    case Label#machine_label.childrenid of
     []->[];
     "[]"->[];
    Childrenids->
        Childrenlabels = getLabel_byIds(Childrenids),
        checkAllUserLabelroot_t(NLabel,Childrenlabels)
    end,
    checkAllUserLabelroot_t(NPLabel,Next).


%% 解析标签操作系统类型  ^^^^^^^^

% 解析所有标签类型类型
parseAllOsType([]) ->
    [];
parseAllOsType([{Name, _}|T]) ->
    [{Name, Name}] ++
    parseAllOsType(T);
parseAllOsType([H|T]) ->
    parseAllOsType(T).


%% 为查询到的服务器列表加上标签字段
statTagForMachines([])->
    [];
statTagForMachines([Machine=#machine{}|T]) ->
    Id = Machine#machine.id,
	% io:format("***********remoteMachineConfig******machine host****~p~n", [Machine#machine.host]),
    case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, Id) of
        {ok, [{Id, Val}]} ->
            Tags = textutils:listEveryToString(Val),
            [Machine#machine{label=Tags}];
        _ ->
            [Machine#machine{label=[]}]
    end++
    statTagForMachines(T);
statTagForMachines([H|T]) ->
    statTagForMachines(T).
   

%% 工具方法，根据标签类型获取服务器
handleMachineByTagType(Ids,{?Device_type, Tag, Index, Count, Sort, SortType}) ->               %% 根据 设备类型 标签类型
    DeviceType = Tag#machine_label.value,
    DY = proplists:get_value(DeviceType,?AllDeviceType),
    getDeviceByType([DY], Index, Count, Sort, SortType);
handleMachineByTagType(Ids,{?SYSTAG_OS, Tag, Index, Count, Sort, SortType}) ->                   %% 根据 操作系统 标签类型
    OsType = Tag#machine_label.value,
    case lists:keysearch(OsType, 1, ?ALLOSTYPE) of
        {value, {OsType, Value}} ->
            Fun =
                fun({K, V}) ->
                    V
                end,
            OsList = lists:map(Fun, Value),
            getMachineByOsTypeList_t(OsList, Index, Count, Sort, SortType);
        _ ->
            []
    end;
    %%getMachineByOs(OsType, Index, Count, Sort, SortType);
handleMachineByTagType(Ids,{?SYSTAG_STATUS_ERROR, Tag, Index, Count, Sort, SortType}) ->        %% 根据 状态错误 标签类型
    Status = "Connection Failure",
    getMachineByStatus(Status, Index, Count, Sort, SortType);
handleMachineByTagType(Ids,{?SYSTAG_METHOD, Tag, Index, Count, Sort, SortType}) ->               %% 根据 连接方法 标签类型
    Method = Tag#machine_label.value,
    getMachineByMethod(Method, Index, Count, Sort, SortType);

handleMachineByTagType(Ids,{?SYSTAG_ALL, Tag, Index, Count, Sort, SortType}) ->                  %% 根据 所有 标签类型
    io:format(" ~n Ids: ~p ~n",[Ids]),
    case Ids of
    [[]]->getAllMachine(Index, Count, Sort, SortType);
    []-> getAllMachine(Index, Count, Sort, SortType);
    "[]"->getAllMachine(Index, Count, Sort, SortType);
    _-> getMachinesByIds(Ids,Index,Count,Sort,SortType)
    end;
handleMachineByTagType(Ids,{?SYSTAG_ROOR, Tag, Index, Count, Sort, SortType}) ->           %% 根据 用户定义 标签类型
    TagId = Tag#machine_label.id,
    getAllMachine(Index, Count, Sort, SortType);
handleMachineByTagType(Ids,{?SYSTAG_USERDEFINE, Tag, Index, Count, Sort, SortType}) ->           %% 根据 用户定义 
    Tags = getLabel_likeindex(Tag#machine_label.treeindex),
    Tagids = [ Label1#machine_label.id ||Label1<-Tags,string:str(Label1#machine_label.treeindex,Tag#machine_label.treeindex)=:=1],
    getMachineByUserDefineTags(Tagids, Index, Count, Sort, SortType).
    

%% 根据Os 类型的List获取到这个Os列表中的所有服务器
getMachineByOsTypeList_t(OsList, Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
                    where=[{"my.os","in",textutils:listToStr(OsList),"&"}]
               },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    Machines = api_machine_adapter:get_machine(Condition).





%% *************************************************************************
%% ********************************* api ***********************************
%% *************************************************************************

%% --------- 标签操作 ---------------

%% *** 读操作 ***

%% 根据标签id获取到标签实体
%% release
getTagById(TagId) ->
    Tg = getTagByIdTool(TagId),
    [Tag] = labelCount([Tg]),
    Tag.
    %%Tg.

%% 获取用户自定义标签
getUserDefineTag(Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
            where=
            [
                {"my.type","=","'"++?SYSTAG_USERDEFINE++"'","&"}
            ]
        },
    Condition=
    #query_condition1{
        where=WhereCondition,
        index=Index,
        count=Count,
        sort=Sort,
        sortType=SortType
    },
    Labels = api_machine_adapter:get_tag(Condition),
    Tags = sortTag(Labels),
    labelCount(Tags).
    %%Tags.

getUserDefineTagRoot() ->
    WhereCondition=
        #query_condition_where{
            where=
            [
                {"my.type","=","'"++?SYSTAG_USERDEFINE++"'","&"},
                {"my.parentid","=",""++"[]"++"","&"}
            ]
        },
    Condition=
    #query_condition1{
        where=WhereCondition,
        index=0,
        count=0,
        sort="",
        sortType=""
    },
    Labels = api_machine_adapter:get_tag(Condition),
    Tags = sortTag(Labels),
    labelCount(Tags).

%% 获取所有的标签
%%
get_all_label() ->
    Labels = dbcs_machine:get_all_label(),
    Tags = sortTag(Labels),
    labelCount(Tags).
    %%Tags.
    
 %%获得根节点的标签组
 get_root_label()->
    WhereCondition=
        #query_condition_where{
            where=
            [
                {"my.parentid","=","[]","&"}
            ]
        },
    Condition=
    #query_condition1{
        where=WhereCondition,
        index=0,
        count=0,
        sort="",
        sortType=""
    },
    Labels = api_machine_adapter:get_tag(Condition),
    Tags = sortTag(Labels),
    labelCount(Tags).

%% 按照类型获取标签
get_label_type(Type) ->
    Labels = dbcs_machine:get_label_type(Type),
    Tags = sortTag(Labels),
    labelCount(Tags).
    %%Tags.

%% *** 写操作 ***


%% 删除标签
%% release
remove_label(Id) ->
    HostName = get(hostname),
    call({remove_label, Id, HostName}).

%% 更新标签，OriId为原始的标签Id, Label里面包含新的标签Id
%% release
update_label(OriId, Label) ->
    HostName = get(hostname),
    call({update_label, OriId, Label, HostName}).
    


%% ------- 服务器操作 -------------

%% ** 写操作 **

%% 创建服务器
create_machine(Machine=#machine{})->
    HostName = get(hostname),
	call({create_machine, Machine, HostName}).

%% 编辑服务器
%% release
update_machine(Mach) ->
    HostName = get(hostname),
    call({update_machine,Mach, HostName}).
    

%% 从标签里面删除服务器
%% release
removeFromLabel(TagId, MacId) ->
    HostName = get(hostname),
    call({removeFromLabel, TagId, MacId, HostName}).
    

%% 添加服务器到标签组
%% release
addMachineToTag(Tag, Machine) ->
    HostName = get(hostname),
    call({addMachineToTag, Tag, Machine, HostName}).

%% 删除设备，根据Id
%% release
deleteMachine(Id) ->
    HostName = get(hostname),
    call({delete_machine, Id, HostName}).

%% ** 读操作 **

%% 根据标签id获取到服务器
%% release
get_Machine_ByTag(TagId, Index, Count, Sort, SortType) ->
    %% release
    
    io:format("TagId TagId TagId  TagId TagId TagId~p~n",[TagId]),
    
    io:format("Count Count Count  Count Count Count~p~n",[Count]),
    Result = get_Machine_ByTagTool([],TagId, Index, Count, Sort, SortType),
    statTagForMachines(Result).
get_Machine_ByTag(Ids,TagId, Index, Count, Sort, SortType) ->
    %% release
    Result = get_Machine_ByTagTool(Ids,TagId, Index, Count, Sort, SortType),
    statTagForMachines(Result).
    

%% 根据操作系统查询服务器
%% release
getMachineByOs(OsType, Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
                where=
                [ 
                    {"my.os","=","'"++OsType++"'","&"}
                ]
            },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    Result = api_machine_adapter:get_machine(Condition),
    statTagForMachines(Result).
    
%% 根据操作系统查询服务器
%% release
getMachineByNotOs(OsType, Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
                where=
                [
                    {"my.os","!=","'"++OsType++"'","&"}
                ]
            },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    Result = api_machine_adapter:get_machine(Condition),
    statTagForMachines(Result).

%% 根据连接状态信息查询服务器
%% release
getMachineByStatus(Status, Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
                where=
                [
                    {"my.status","=","'"++Status++"'","&"}
                ]
            },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    statTagForMachines(api_machine_adapter:get_machine(Condition)).

%% 根据连接方法获取服务器
%% release
getMachineByMethod(Method, Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
                where=
                [
                    {"my.method","=","'"++Method++"'","&"}
                ]
            },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    statTagForMachines(api_machine_adapter:get_machine(Condition)).

%% 根据主机ip获取设备
%% release
getMachineByHost(Host, Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
                where=
                [
                    {"my.host","=","'"++Host++"'","&"}
                ]
            },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    statTagForMachines(api_machine_adapter:get_machine(Condition)).

%% 获取所有服务器
%% release
getAllMachine(Index, Count, Sort, SortType) ->
    WhereCondition=
        #query_condition_where{
                where=[]
            },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    statTagForMachines(api_machine_adapter:get_machine(Condition)).

%% 获取用户自定义的标签下面的服务器
%% release
getMachineByUserDefineTag(TagId, Index, Count, Sort, SortType) ->
    Machines =
    case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, TagId) of
        {ok, [{TagId, Val}]} ->
            WhereCondition=
                #query_condition_where{
                        where=[{"id","in",textutils:listToStr(Val),"&"}]
                    },
            Condition=
                #query_condition1{
                    where=WhereCondition,
                    index=Index,
                    count=Count,
                    sort=Sort,
                    sortType=SortType
                },
            api_machine_adapter:get_machine(Condition);
        _ ->
            []
    end,
    statTagForMachines(Machines).

%% 获取一组用户自定义的标签下面的服务器
%% release
getMachineByUserDefineTags(TagIds, Index, Count, Sort, SortType) ->
        Val = getMachineByUserDefineTags_t(TagIds),
        WhereCondition=
            #query_condition_where{
                    where=[{"id","in",textutils:listToStr(Val),"&"}]
                },
        Condition=
            #query_condition1{
                where=WhereCondition,
                index=Index,
                count=Count,
                sort=Sort,
                sortType=SortType
            },
statTagForMachines(api_machine_adapter:get_machine(Condition)).

getMachineByUserDefineTags_t([])->[];
getMachineByUserDefineTags_t([Tagid|Next])->
  case remoteMachineConfig:get(?REMOTECONF_RELTAGANDMA, Tagid) of
  {ok, K} ->
    case proplists:get_value(Tagid,K) of
    undefined->[];
    V->V
    end
    ++getMachineByUserDefineTags_t(Next);
  Err->
  []
  end.
  
%%@doc通过设备类型，获取设备。
%% TypeList = [Device]
%% Device= "SWITCH" | "ROUTER_SWITCH" | "ROUTE" | "FIREWALL" | "SERVER" 
getDeviceByType(TypeList,Index,Count,Sort,SortType)->
    WhereCondition=
        #query_condition_where{
                    where=[{"my.type","in",textutils:listToStr(TypeList),"&"}]
               },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    Result = api_machine_adapter:get_machine(Condition),
    statTagForMachines(Result).
    

%%@doc通过设备类型，获取设备。
%% TypeList = [Device]
%% Device= "SWITCH" | "ROUTER_SWITCH" | "ROUTE" | "FIREWALL" | "SERVER" 
getMachinesByIds(Ids,Index,Count,Sort,SortType)->
    WhereCondition=
        #query_condition_where{
                    where=[{"id","in",textutils:listToStr(Ids),"&"}]
               },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
    Result = api_machine_adapter:get_machine(Condition),
    statTagForMachines(Result).

getLabel_likeindex(Index)->
    WhereCondition=
        #query_condition_where{
                    where=[{"my.treeindex","like",""++Index++"","&"}]
               },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=0,
            count=0,
            sort="",
            sortType=""
        },
    api_machine_adapter:get_tag(Condition).
    
getLabel_byIds(Ids)->
    WhereCondition=
        #query_condition_where{
                     where=[{"id","in",textutils:listToStr(Ids),"&"}]
               },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=0,
            count=0,
            sort="",
            sortType=""
        },
    api_machine_adapter:get_tag(Condition).

%% 通过id获取服务器
get_machine(Id) when is_atom(Id)->
	get_machine(atom_to_list(Id));
get_machine(Id)->
	dbcs_machine:get_machineById(Id).


%%通过IP获得设备
get_machine_ByIP(IP)when is_list(IP)->
    IPList = ["\\\\"++IP,"\\"++IP,IP],
    WhereCondition=
        #query_condition_where{
                    where=[{"my.host","in",textutils:listToStr(IPList),"&"}]
               },
    Condition=
        #query_condition1{
            where=WhereCondition,
            index=0,
            count=0,
            sort="",
            sortType=""
        },
    Result = api_machine_adapter:get_machine(Condition),
    statTagForMachines(Result);
get_machine_ByIP(Other)->
{error,"error ip format"}.

translate_OS(Key)->
   case proplists:get_value(Key,?ALLOSTYPE) of
   undefined->Key;
   Value->
       {_Name1,Name2} = lists:nth(1,Value),
       Name2
   end.

%% ------ 标签组操作 ---------

%% 删除所有标签组, 
remove_AllLabel() ->
    HostName = get(hostname),
    call({remove_AllLabel, HostName}).

%% 创建标签组
create_label(Label)->
    HostName = get(hostname),
    call({create_label, Label, HostName}).


    

    
    
is_machineIp(IP) ->
    case IP of
        "\\\\" ++ Ip ->
            textutils:isIpaddr(Ip);
        _ ->
            textutils:isIpaddr(IP)
    end.
    
