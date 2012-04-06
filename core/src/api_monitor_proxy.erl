%% 
%% @doc api function of monitor proxy
%% @version{1.0}
%% @copyright 2010 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_monitor_proxy).
%%-extends(api_siteview).
-compile(export_all).
-include("monitor.hrl").

-export([get_proxy_list/0,
		get_proxy_stat/0,
		get_master_stat/0,
		get_proxy_mapping_list/0,
		add_proxy_mapping/2,
		get_mapping_by_monitor/1,
		get_mapping_by_proxy/1,
		remove_proxy_mapping/1,
		get_master_node/0,
		get_all_types/0,
		get_monitor_by_proxy/1,
		add_monitor/2,
		get_monitor_stat/1,
		update_mapping/2,
		delete_monitor/2,
		add_proxy/1,
		remove_proxy/1,
		update_proxy_name/2
		]).

%% @spec get_proxy_list()-> ({ok,[{Proxy,Type,Name}]} | {error,Reason})
%% where
%%	Proxy = atom()
%%	Type = atom()
%%	Name = string()
%%	Reason = atom()
%% @doc get list of monitor proxy
get_proxy_list()->
	case monitor_proxy_server:get_proxy_list() of
		{ok,List}->
			{ok,[{Proxy,Type,get_proxy_name(Proxy)}||{Proxy,Type}<-List]};
		Else->
			Else
	end.
	
get_proxy_name(Proxy)->
	case api_preferences:get_prefs('proxy_name_config',Proxy) of
		{ok,[{_,Name}|_]}->
			Name;
		_->
			lists:flatten(io_lib:format("~s",[Proxy]))
	end.
	
%% @spec update_proxy_name(Proxy,Name)-> (ok | error)
%% where
%%	Proxy = atom()
%%	Name = string()
%% @doc update name of monitor proxy
update_proxy_name(Proxy,Name)->
	case api_preferences:set_prefs('proxy_name_config',Proxy,Name) of
		{ok,_}->
			ok;
		_->
			error
	end.
	
%% @spec get_proxy_stat()-> ({ok,[StatInfo]} | {error,Reason})
%% where
%%	StatInfo = {Name,StartTime,TotalRunCount,CurRunCount,CurRunMonitor,HisRunMonitor,CurMinRunCount,MaxMinRunCount,MaxMinRunTime}
%%	Reason = atom()
%% @doc get stat infomation of monitor proxy
%%
get_proxy_stat()->
	monitor_proxy_server:get_stat().

%% @spec get_master_stat()-> ({ok,[StatInfo]} | {error,Reason})
%% where
%%	StatInfo = {Name,StartTime,TotalRunCount,CurRunCount,CurRunMonitor,HisRunMonitor,CurMinRunCount,MaxMinRunCount,MaxMinRunTime}
%%	Reason = atom()
%% @doc get stat infomation of monitor proxy
%%	
get_master_stat()->
	monitor_local_stat:get_stat().

%% @spec get_proxy_mapping_list()-> ({ok,[MappingInfo]} | {error,Reason})
%% where
%%	MappingInfo = {MonitorId,Proxy}
%%	MonitorId = atom()
%%	Proxy = atom()
%%	Reason = atom()
%% @doc get infomation of monitor-proxy mapping
%%
get_proxy_mapping_list()->
	proxy_mapping:all(dbcs_base:get_app()).
	
%% @spec add_proxy_mapping(MId,Proxy)-> ({ok,proxy_mapping} | {error,Reason})
%% where
%%	MId = atom()
%%	Proxy = atom()
%%	Reason = atom()
%% @doc add monitor-proxy mapping
%%
add_proxy_mapping(MId,Proxy)->
	proxy_mapping:add(dbcs_base:get_app(),MId,Proxy).
	
%% @spec get_mapping_by_monitor(MId)-> ({ok,[MappingInfo]} | {error,Reason})
%% where
%%	MappingInfo = {MId,Proxy}
%%	MId = atom()
%%	Proxy = atom()
%%	Reason = atom()
%% @doc find monitor-proxy mapping by monitor id
%%
get_mapping_by_monitor(MId)->
	proxy_mapping:get(dbcs_base:get_app(),MId).
	
%% @spec get_mapping_by_proxy(Proxy)-> ({ok,[MappingInfo]} | {error,Reason})
%% where
%%	MappingInfo = {MonitorId,Proxy}
%%	MonitorId = atom()
%%	Proxy = atom()
%%	Reason = atom()
%% @doc find monitor-proxy mapping by monitor proxy
%%	
get_mapping_by_proxy(Proxy)->
	proxy_mapping:get_by_proxy(dbcs_base:get_app(),Proxy).

%% @spec remove_proxy_mapping({MId,Proxy})-> ({ok,deleted} | {error,Reason})
%% where
%%	MId = atom()
%%	Proxy = atom()
%%	Reason = atom()
%% @doc remove monitor-proxy mapping
%%		
remove_proxy_mapping({MId,Proxy})->
	proxy_mapping:remove({dbcs_base:get_app(),MId,Proxy});
remove_proxy_mapping(_)->{error,error_parameter}.

%% @spec get_master_node()-> Node
%% where
%%	Node = atom()
%% @doc get master node
%%		
get_master_node() ->
    erlang:node().

%% @spec get_all_types()-> Result
%% where
%%	Result = list()
%% @doc get all proxy type
%%	
get_all_types() ->
    Proxy = case get_proxy_list() of
        {ok,P} ->
            P;
        _ ->
            []
    end,
    get_type(Proxy,[]).
    
get_type([],Result) ->lists:reverse(Result);
get_type([{_,Type}|R],Result) ->
    case lists:member(Type, Result) of
        true ->
            get_type(R,Result);
        _ ->
            get_type(R,[Type|Result])
    end;
get_type([_|R],Result) ->
    get_type(R,Result).
    
%% @spec get_proxy_by_type(Type)-> Result
%% where
%%	Result = list()
%% @doc get all proxy by type
%%	
get_proxy_by_type(Type) ->
    Proxy = case get_proxy_list() of
        {ok,P} ->
            P;
        _ ->
            []
    end,
    [Id||{Id,T}<- Proxy,T==Type].
    
%% @spec get_monitor_by_proxy(Proxy)-> Result
%% where
%%	Result = list()
%% @doc get all monitor by proxy
%%	
get_monitor_by_proxy(Proxy) ->
    Monitors = case get_mapping_by_proxy(Proxy) of
        {ok,P} ->
            P;
        _ ->
            []
    end,
    MonitorTemp = api_monitor_template:get_templates(),
    create_monitor_list(Monitors,MonitorTemp,[]).
    
%% @spec get_server_monitor()-> Result
%% where
%%	Result = list()
%% @doc get all server monitor
%%	
get_server_monitor() ->
    Monitors = [{list_to_atom(Id),Info}||{Info,Id}<- api_monitor:get_all_monitors()],
    MonitorTemp = api_monitor_template:get_templates(),
    create_monitor_list(Monitors,MonitorTemp,[]).
    
%% @spec get_monitor_stat(Ids)-> Result
%% where
%% Ids = list()
%%	Result = list()
%% @doc get monitor info
%%	
get_monitor_stat(Ids) ->
    Monitors = get_monitor_stat(Ids,[]),
    MonitorTemp = api_monitor_template:get_templates(),
    create_monitor_list(Monitors,MonitorTemp,[]).
    
get_monitor_stat([],Result) ->lists:reverse(Result);
get_monitor_stat([F|R],Result) ->
    case proplists:get_value(F,Result) of
        undefined ->
            get_monitor_stat(R,[{F,""}|Result]);
        _ ->
            get_monitor_stat(R,Result)
    end.
    
%% @spec update_mapping(Proxy,Monitors)-> Result
%% where
%% Monitors = list()
%% Proxy = atom()
%%	Result = atom()
%% @doc update proxy monitor mapping
%%	
update_mapping(Proxy,Monitors) ->
    OM = case get_mapping_by_proxy(Proxy) of
        {ok,L} ->
            [Mid||{Mid,P}<- L, P==Proxy];
        _ ->
            []
    end,
    add_monitor(get_oper_monitor(Monitors,OM,[]),Proxy),
    delete_monitor(get_oper_monitor(OM,Monitors,[]),Proxy).

get_oper_monitor([],_,Result) ->Result;
get_oper_monitor([F|R],Monitor,Result) ->
    NR = case lists:member(F,Monitor) of
        true ->
            Result;
        _ ->
            [F|Result]
    end,
    io:format("nr:~p~n",[NR]),
    get_oper_monitor(R,Monitor,NR).
    
%% @spec add_monitor(Monitors,Proxy)-> Result
%% where
%% Monitors = list()
%% Proxy = atom()
%%	Result = atom()
%% @doc add monitor list
%%	
add_monitor([],_)->ok;
add_monitor([F|R],Proxy) ->
    add_proxy_mapping(F,Proxy),
    add_monitor(R,Proxy).
    
%% @spec delete_monitor(Monitors,Proxy)-> Result
%% where
%% Monitors = list()
%% Proxy = atom()
%%	Result = list()
%% @doc delete monitor list
%%	
delete_monitor([],_)->ok;
delete_monitor([F|R],Proxy) ->
    remove_proxy_mapping({F,Proxy}),
    delete_monitor(R,Proxy).

create_monitor_list([],_,Result) ->Result;
create_monitor_list([{Id,_}|R],MonitorTemp,Result) ->
    case api_monitor:info(Id) of
        {error,_} -> %The monitor node may have been deleted
            create_monitor_list(R,MonitorTemp,Result);
        StaticInfo -> %Static information
            Class = proplists:get_value(class,StaticInfo),
            Name = proplists:get_value(name,StaticInfo),
			Disabled = proplists:get_value(disabled,StaticInfo),
            PlatForm = get_platform(MonitorTemp, Class),
            RunInfo = case api_monitor:get_run_info(Id) of %Some running monitor dynamic information not available
                {error,_} ->
                    [];
                Ri ->
                    Ri
            end,
            Last = proplists:get_value(last,RunInfo,""),
            Cate = proplists:get_value(category,RunInfo,""),
            State = proplists:get_value(state_string,RunInfo,""),
            %Is already running in the proxy
            InProxy = case get_mapping_by_monitor(Id) of
                {ok, P} when length(P)>0 ->
                    [PP||{_,PP}<- P];
                _ ->
                    []
            end,
            create_monitor_list(R,MonitorTemp,[{Id,Class,Name,PlatForm,Last,Cate,State,InProxy,Disabled}|Result])
    end;
create_monitor_list([_|R],MonitorTemp,Result) ->
    create_monitor_list(R,MonitorTemp,Result).
    
get_platform([],_)->[];
get_platform([{Class,_,V}|_R],Class) ->
    case proplists:get_value(platform,V) of
        undefined ->
            [];
        O ->
            O
    end;
get_platform([_|R],Class) ->
    get_platform(R,Class).
	

%% @spec add_proxy(Node)-> ({ok,Pid} | {error,Reason})
%% where
%%	Proxy = atom()
%%	Reason = atom()
%% @doc add monitor proxy to this master node
%%	
add_proxy(Node) when is_list(Node)->
	monitor_proxy_server:add_proxy(list_to_atom(Node));
add_proxy(Node) when is_atom(Node)->
	monitor_proxy_server:add_proxy(Node);
add_proxy(_)->{error,error_parameter}.

%% @spec remove_proxy(Node)-> ({ok,Pid} | {error,Reason})
%% where
%%	Proxy = atom()
%%	Reason = atom()
%% @doc remove monitor proxy from this master node
%%	
remove_proxy(Node)when is_list(Node)->
	monitor_proxy_server:remove_proxy(list_to_atom(Node));
remove_proxy(Node)when is_atom(Node)->
	monitor_proxy_server:remove_proxy(Node);
remove_proxy(_)->{error,error_parameter}.