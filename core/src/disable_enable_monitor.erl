-module(disable_enable_monitor,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").

-define(PRODUCT_NAME,"elecc").
-define(TIMEOUT,5).
-define(LOG_TYPE,"Disable or Enable Monitor(s)").

new(Monitor,Rule)->
	Obj = action:new(Monitor,Rule),
	% Obj:set_monitor(Monitor),
	% Obj:set_rule(Rule),
	% Obj:set_attribute(runType,2),
	{?MODULE,Obj,Monitor,Rule}.


execute()->
	Msg = ?PRODUCT_NAME ++ " Alert, " ++ 
	case Monitor:get_attribute(?CATEGORY) of 
		{ok,{_,Category}} -> 
			atom_to_list(Category);
		_->
			""
	end ++ "," ++
	case Monitor:get_property(?NAME) of
		{ok,{_,Name}}->
			Name;
		_->
			""
	end ++ "," ++
	case Monitor:get_attribute(?STATE_STRING) of
		{ok,{_,State}}->
			State;
		_->
			""
	end,
	{ok,{_,Params}} = Rule:get_property(action_param), 
	{ok,{_,Enabled}} = Rule:get_property(enabled),
	case Rule:get_property(disabled) of
		{ok,{_,true}}->
			{error,"disabled"};
		_->
			case THIS:check_enabled(Enabled) of
				false ->
					{error,"time_disabled"};
				_->
					disable_enable_monitor(Params,Msg)
			end
	end.

verify(Params)->
	{ok,""}.
    
%% Prohibit or start monitoring interface
disable_enable_monitor(Params,Content)->
    Oper = Params#disable_alert.oper,
    T1 =
    case lists:keysearch("groupWithMonitorName", 2, Params#disable_alert.target) of
        {value, Tuple} ->
            Params#disable_alert.target;
        false ->
            Params#disable_alert.target;
        _ ->
            Params#disable_alert.target
    end,
    T2 =
    case lists:keysearch("groupContainingMonitor", 2, Params#disable_alert.target) of
        {value, Tuple1} ->
            case Monitor:get_property(?PARENT) of
                {ok,{_,GId}}->
                    Tuple2 = lists:keydelete("groupContainingMonitor", 2, T1),
                    Tuple2 ++ [{erlang:element(1, Tuple1),erlang:atom_to_list(GId), erlang:element(3, Tuple1)}];
                _->
                    T1
            end;
        false ->
            T1;
        _ ->
            T1
    end,    
    T3 = lists:keysort(2, T2),
    %%io:format("T3 = ~p~n", [T3]),
    Target = process_op_groups_monitors([], T3),
    %%io:format("Target = ~p~n", [Target]),
    Re =
    try exe_disoren_monitor(Oper, Target, []) of
        {ok, {monitornames, MoNames}} ->
            Con = Oper ++ " monitor(s) " ++ MoNames,
            THIS:logAlert(?LOG_TYPE, "localhost",Con,"","ok"),
            {ok,Con, disable_enable_monitor};
        {error, {monitornames, MoNames}} ->
            Con = Oper ++ " monitor(s) " ++ MoNames,
            THIS:logAlert(?LOG_TYPE, "localhost", Con,"","fail"),
            {error, Con, disable_enable_monitor}
    catch
        _:Reason when erlang:is_list(Reason) ->
            THIS:logAlert(?LOG_TYPE, "localhost", Reason,"","fail"),
            {error, Reason, disable_enable_monitor};
        _:Rs ->
            %%io:format("Rs = ~p~n", [Rs]),
            R = "Fail because of other reason",
            THIS:logAlert(?LOG_TYPE, "localhost", R, "","fail"),
            {error, R, disable_enable_monitor}
    end,
    {ok,[Re]}.

%% monitoring of the specified group to perform or initiate action against
exe_disoren_monitor(Oper, [], MoNames) ->
    {ok, {monitornames, MoNames}};
exe_disoren_monitor(Oper, [H|T], MoNames) ->
    %%io:format("TH = ~p~n", [H]),
    case erlang:is_tuple(H) of
        true when erlang:size(H) =:= 3 ->
            Id = erlang:element(2, H),
            Type = erlang:element(3, H),
            Name = erlang:element(1, H),
            case disable_enable_monitor_by_oper(Oper, Id, Type) of
                {error, {monitornames, MonitorNames}} ->
                    %%io:format("MonitorNames = ~p~n", [MonitorNames]),
                    {error, {monitornames, MonitorNames}};
                {error,Rs} ->
                    %%io:format("Rs = ~p~n", [Rs]),
                    {error, {monitornames, Name}};
                {ok, {monitornames,MonitorNames}} ->
                    %%io:format("MonitorNames = ~p~n", [MonitorNames]),
                    MName = 
                    case MoNames of
                        [] ->
                            MonitorNames;
                        _ ->
                            MoNames ++ " , " ++ MonitorNames
                    end,
                    %%io:format("Id = ~p~n", [Id]),
                    %%io:format("OkName = ~p~n", [MName]),
                    exe_disoren_monitor(Oper, T, MName);
                {ok,Result} ->
                    %%io:format("Name = ~p~n", [Name]),
                    MName = 
                    case MoNames of
                        [] ->
                            Name;
                        _ ->
                            MoNames ++ " , " ++ Name
                    end,
                    exe_disoren_monitor(Oper, T, MName)
            end;
        _ ->
            exe_disoren_monitor(Oper, T, MoNames)
    end.
    
%% According to the type of action, id, type to determine the start or prohibit or monitor group
disable_enable_monitor_by_oper(Oper, Id, Type) ->
    AtomId = erlang:list_to_atom(Id),
    DisableDesc = "Disable or Enable Monitor(s) Alert",
    case Oper of
        "disable" ->
            case Type of
                "monitor_group" ->
                    disable_monitors(AtomId,DisableDesc);
                "monitor" ->        
                    disable(AtomId,DisableDesc)
            end;
        "enable" ->
            case Type of
                "monitor_group" ->
                    %%io:format("enable_monitors~n"),
                    enable_monitors(AtomId, DisableDesc);
                "monitor" ->      
                    %%io:format("enable_monitor~n"),
                    enable(AtomId, DisableDesc)
            end
    end.

%%---------------------Start inhibit monitoring interface---------------------------------------

update_monitors(F, [], DisableDesc, MoNames)->{ok,{monitornames, MoNames}};
update_monitors(F, [Obj|T], DisableDesc, MoNames)->
    %%io:format("CLASS = ~p~n", [?CLASS]),
	case Obj:get_property(?CLASS) of
		{ok,{?CLASS,group}}->
            %%io:format("Group~n"),
			update_monitors(F, Obj:get_childs() ++ T, DisableDesc, MoNames);
		O->
            %%io:format("O = ~p~n", [O]),
            case Obj:get_property(id) of
                {ok,{_,AId}} ->
                    %%Id = erlang:atom_to_list(AId),
                    Id = AId,
                    %%io:format("Id = ~p~n", [Id]),
                    case F(Id, DisableDesc) of
                        {ok,Result} ->
                            %%io:format("bs~n"),
                            MName = 
                            case MoNames of
                                [] ->
                                    Obj:get_full_name();
                                _ ->
                                    Obj:get_full_name() ++ " , " ++ MoNames
                            end,
                            update_monitors(F, T, DisableDesc, MName);
                        {error,Reason} ->
                            %%io:format("ErrReason = ~p~n", [Reason]),
                            {error, {monitornames, Obj:get_full_name()}};
                        ErrR ->    
                            %%io:format("ErrR = ~p~n", [ErrR]),
                            {error, {monitornames, Obj:get_full_name()}}
                    end;
                Err ->
                    %%io:format("Err = ~p~n", [Err]),
                    {error, {monitornames, Obj:get_full_name()}}
            end
	end.

%% @spec disable_monitors(GroupId,DisableDesc)-> ({ok,Result}| {error,Reason})
%% where
%%	GroupId = atom()
%%	DisableDesc = string()
%%	Result = atom()
%%	Reason = atom()
%% @doc disable group's monitors
%% DisableDesc is the descripition of disable
disable_monitors(GroupId,DisableDesc) when is_atom(GroupId)->
    Groups = api_group:find_group(GroupId),
    %%io:format("Groups = ~p~n", [Groups]),
    update_monitors(fun disable/2, Groups, DisableDesc, []);
disable_monitors(_,_)->{error,parameter_error}.

%% @spec disable(Monitors,DisableDesc)->({error,Reason} | {ok,Result})
%%Bulk prohibited monitor ()
batch_disable_monitors([], MonitorNames, DisableDesc) ->
    {ok, {monitornames,MonitorNames}};
batch_disable_monitors([H|T], MonitorNames, DisableDesc) ->
    %%io:format("H = ~p~n", [H:get_full_name()]),
    H:set_property(?DISABLED,true),
    H:set_property(?DISABLED_DESCRIPTION,DisableDesc),
    case H:save_monitor() of
        {ok,Result} ->
            batch_disable_monitors(T, H:get_full_name() ++ "," ++ MonitorNames, DisableDesc);
        {error,Reason} ->
            {error, {monitornames, H:get_full_name()}};
        _ ->
            {error, {monitornames, H:get_full_name()}}
    end.

%% @spec disable(MonitorId,DisableDesc)->({error,Reason} | {ok,Result})
%% where
%%		MonitorId = atom()
%%		DisableDesc = string()
%%		Result = atom()
%%		Reason = (not_found_monitor | save_monitor_error | parameter_error)
%% @doc disable monitor
disable(MonitorId,DisableDesc) when is_atom(MonitorId)->
    %%io:format("disableok~n"),
	[M] = api_siteview:find_object(MonitorId),
    %%io:format("disableok1~n"),
    %%io:format("M = ~p~n", [M]),
    M:set_property(?DISABLED,true),
    %%io:format("disableok2~n"),
    M:set_property(?DISABLED_DESCRIPTION,DisableDesc),
    %%io:format("disableok3~n"),
    M:save_monitor();
disable(_,_)->{error,parameter_error}.
    
%% @spec enable_monitors(GroupId) -> (ok|{error,Reason})
%% where
%%	GroupId = atom()
%%	Reason = atom()
%% @doc enable monitors in a group
enable_monitors(GroupId, DisableDesc) when is_atom(GroupId)->
    Groups = api_group:find_group(GroupId),
    %%io:format("Groups = ~p~n", [Groups]),
    update_monitors(fun enable/2, Groups, DisableDesc, []);
enable_monitors(_, DisableDesc)->{error,parameter_error}.

%% @spec batch_enable_monitors(Monitors,DisableDesc)->({error,Reason} | {ok,Result})
%%Batch Start Monitor ()
batch_enable_monitors([], MonitorNames, DisableDesc) ->
    {ok, {monitornames,MonitorNames}};
batch_enable_monitors([H|T], MonitorNames, DisableDesc) ->
    H:set_property(?DISABLED_DESCRIPTION,""),
    H:set_property(?TIMED_DISABLE,undefined),
	H:set_property(?DISABLED,false),
    case H:save_monitor() of
        {ok,Result} ->
            batch_enable_monitors(T, H:get_full_name() ++ "," ++ MonitorNames, DisableDesc);
        {error,Reason} ->
            {error, {monitornames, H:get_full_name()}};
        _ ->
            {error, {monitornames, H:get_full_name()}}
    end.

%% @spec enable(MonitorId)->{error,Reason} | {ok,Result}
%% where
%%	MonitorId = atom()
%%	Reason = (parameter_error | not_found_monitor | save_monitor_error)
%%	Result = atom()
%% @doc enable a monitor
enable(MonitorId, DisableDesc) when is_atom(MonitorId)->
    [M] = api_siteview:find_object(MonitorId),
    M:set_property(?DISABLED_DESCRIPTION,""),
    M:set_property(?TIMED_DISABLE,undefined),
	M:set_property(?DISABLED,false),
    M:save_monitor();
enable(_, DisableDesc) -> {error,parameter_error}.


%%---------------------------------------------------------------------------------
    
getScalarValues(Prop,Params)->
	case Prop of
		action->
			[{"Disable", "disable"}, {"Enable", "enable"}];
        targets ->
            getAllGroupsMonitorsEx();
        buildtargets ->
            build_op_groups_monitors(Params);
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%% Construct non-overlapping groups and monitor list
build_op_groups_monitors(Params) ->
    Alert_target = 
    case proplists:get_value(alert_target, Params) of
        undefined ->
            [];
        Value ->
            Value
    end,
    %%io:format("Alert_target = ~p~n",[Alert_target]),
    All_group_monitors = 
    case proplists:get_value(all_group_monitors, Params) of
        undefined ->
            [];
        Value1 ->
            Value1
    end,
    TList = get_fullname_targets(Alert_target, All_group_monitors),
    SortList = lists:keysort(2, TList),
    %%io:format("SortList = ~p~n", [SortList]),
    ReList = process_op_groups_monitors([], SortList),
    %%io:format("ReList = ~p~n", [ReList]),
    ReList.
    
getAllGroupsMonitorsEx()->
    Groups = siteview:get_object_by_class(monitor_group),
	F = fun(X)->
		{ok,{_,Id}} = X:get_property(id),
		{X:get_full_name(),atom_to_list(Id),"monitor_group"}
	end,
	List = [F(X)||X<-Groups],
	Monitors = siteview:get_object_by_type(monitor),
	F1 = fun(X)->
		{ok,{_,Id}} = X:get_property(id),
		{X:get_full_name(),atom_to_list(Id), "monitor"}
	end,
	List1 = [F1(X)||X<-Monitors],
    List ++ List1.

%% The final list of non-coincidence
process_op_groups_monitors(ReList, []) ->
    ReList;
process_op_groups_monitors(ReList, [H|T]) ->
    PeekId = 
    case ReList of
        [] ->
            [];
        L when erlang:is_list(L) ->
            erlang:element(2, lists:last(L)) ++ ".";
        _ ->
            []
    end,
    HId = erlang:element(2, H),
    %%case Monitor of
    %%    undefined ->
    %%        erlang:element(2, H);
    %%    _ ->
    %%        case erlang:element(2, H) of
    %%            "groupContainingMonitor" ->
    %%                io:format("Monitor = ~p~n", [Monitor]),
    %%                case Monitor:get_property(?PARENT) of
    %%                    {ok,{_,GId}}->
    %%                        io:format("GId = ~p~n", [GId]),
    %%                        erlang:atom_to_list(GId);
    %%                    _->
    %%                        []
    %%                end;
    %%            "groupWithMonitorName" ->
    %%                [];
    %%            Value ->
    %%                Value
    %%        end
    %%end,
    HHId = (HId ++ "."),
    NReList = 
    case lists:prefix(PeekId, HId) of
        true when PeekId =:= [] ->
            ReList ++ [H];
        true when PeekId =/= HId ->
            ReList;
        _ ->
            if
                HHId =:= PeekId ->
                    ReList;
                true ->
                    ReList ++ [H]
            end
    end,
    process_op_groups_monitors(NReList, T).
    
    
%% All groups and monitors from the list to find the corresponding list of the specified id
get_fullname_targets([], All_group_monitors) ->
    [];
get_fullname_targets([H|T], All_group_monitors) ->
    TList =
    case lists:keysearch(H, 2, All_group_monitors) of
        {value, Tuple} ->
            [Tuple];
        false ->
            [];
        _ ->
            []
    end,
    TList ++ get_fullname_targets(T, All_group_monitors).
    

get_template_property()->
	%BASE:get_template_property() ++ 
	[
%%	#property{name=to,title="To",type=scalar,multiple = true,allowother = true,listSize=5,description="either choose one or more e-mail setting(s), or enter the e-mail address of the person to send the mail to, separate multiple addresses with commas (example: " ++ ?SUPPORT_MAIL ++ "," ++ ?SALES_MAIL ++ ")"},
%%	#property{name=template,title="Template",type=scalar,description="choose which template to use for formatting the contents of the message.  If you are sending mail to a pager, choose one of the \"short\" templates."}
	].