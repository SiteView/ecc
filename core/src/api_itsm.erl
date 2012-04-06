-module(api_itsm).
-compile(export_all).
-include("itsm_structure.hrl").
-include("monitor.hrl").
-define(ModuleWriteLock, "sysmodule_write_").

%% *************Important general-purpose processing***********

%% **********General treatment, (Where a database and assembled into Order)
build_condition(Condition=#query_condition{}) ->
    Index = Condition#query_condition.index,
    Count = Condition#query_condition.count,
    Where = Condition#query_condition.where,
    IndexStr = 
        try erlang:integer_to_list(Index) of
            V when erlang:is_list(V) ->
                V;
            _ ->
                "0"
        catch
            _:_ ->
                "0"
        end,
    CountStr = 
        try erlang:integer_to_list(Index+Count) of
            V1 when erlang:is_list(V1) ->
                V1;
            _ ->
                "0"
        catch
            _:_ ->
                "0"
        end,
    Order = "from="++IndexStr++"& to="++CountStr,%%++"&order=my."++Order,
    #query_beam_condition{
        where=Where,
        order=Order
    }.
    
    
    


%% ***************************************************
%% ********************** interface************************
%% ***************************************************
%% ---- sys_choices interface

%%getParameterValue(DeviceId,ParametersList) ->
%%getParameterValue(Host,DeviceId,ParametersList) ->
%% <<Interface Operation>> choice_lists Table operation - for all
get_all_choices(Index, Count) ->
    dbcs_itsm:get_all_choices(Index, Count).

%%<<Interface operation>> choice_lists table operation - for the specified Table, Element of Choice Form
get_choices(Table, Element) ->
    Where = "my.table="++Table++"&my.element="++Element,
    dbcs_itsm:get_choices_where(Where).

%% <<Interface operation>> choice_lists table operation - for the specified Table, Element of Choice Table [{Id, Value} | T]
get_choices_t(Table, Element) ->
    Where = "my.table="++Table++"&my.element="++Element,
    Vs = 
        case dbcs_itsm:get_choices_where(Where) of
            {ok, V} ->
                V;
            _ ->
                []
        end,
    [{Choice#choice_lists.label, Choice#choice_lists.value} || Choice=#choice_lists{} <- Vs].
    
%%<<Interface operation>> choice_lists table operation - for the specified Table, Element, Value of Choice Table [{Id, Value} | T]
get_choices_label(Table, Element, Value) ->
    Where = "my.table="++Table++"&my.element="++Element++"&my.value="++Value,
    Vs = 
        case dbcs_itsm:get_choices_where(Where) of
            {ok, V} ->
                V;
            _ ->
                []
        end,
    Clists = [{Choice#choice_lists.label, Choice#choice_lists.value} || Choice=#choice_lists{} <- Vs],
    case Clists of
        VClists when erlang:is_list(VClists) ->
            case VClists of
                [] ->
                    [];
                _ ->
                    Tu = lists:nth(1,VClists),
                    case Tu of
                        {Label, Value} ->
                            Label;
                        _ ->
                            []
                    end
            end;
        _ ->
            []
    end.
    
    
%% <<Interface operation>> choice_lists table operation - the choice to delete data in the specified id
delete_choice(Id) ->
    dbcs_itsm:delete_choice(Id).

%%<<Interface operation>> choice_lists table options - to add a choice of data
save_choice(Table, Element, Language, Label, Value, Dependent_value, Hint, Sequence, Inactive) ->
    Id = textutils:guid(),
    Choices = #choice_lists{
                                id=Id,          %%id
                                table=Table,       %%Belongs to table Table
                                element=Element,      %%Element element belongs
                                language=Language,          %%Language
                                label=Label,     %%Respective display tags Label
                                value=Value,       %%所属值Value
                                dependent_value=Dependent_value,     %%依赖值Dependent value
                                hint=Hint,            %%Hint
                                sequence=Sequence,        %%排序Sequence
                                inactive=Inactive    %%是否无效
    },
    dbcs_itsm:save_choice(Choices).
    
%% <<界面操作>> choice_lists 表操作 -- 保存一个choice数据,根据id
save_choice(Id,Table, Element, Language, Label, Value, Dependent_value, Hint, Sequence, Inactive) ->
    Choices = #choice_lists{
                                id=Id,          %%id
                                table=Table,       %%所属表Table
                                element=Element,      %%所属元素Element
                                language=Language,          %%语言Language
                                label=Label,     %%所属显示标记Label
                                value=Value,       %%所属值Value
                                dependent_value=Dependent_value,     %%依赖值Dependent value
                                hint=Hint,            %%Hint
                                sequence=Sequence,        %%排序Sequence
                                inactive=Inactive    %%是否无效
    },
    dbcs_itsm:save_choice(Choices).
    
%% <<界面操作>> choice_lists 表操作 -- 获取指定id的choice
get_choice(Id) ->
    dbcs_itsm:get_choice(Id).
    

%% ---- incident接口

%% <<界面操作>> incident 表操作 -- 获取所有
get_all_incident("", "") ->
    dbcs_itsm:get_all_incident("", "");
get_all_incident(Index, Count) ->
    Condition=
        #query_condition{
            where="",
            index=Index,
            count=Count
        },
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition.where,
    Order = BeamCondition#query_beam_condition.order,
    dbcs_itsm:get_incident_where(Where, Order).


get_all_incident(Hostname, Index, Count) ->
    Condition=
        #query_condition{
            where="",
            index=Index,
            count=Count
        },
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition.where,
    Order = BeamCondition#query_beam_condition.order,
    dbcs_itsm:get_incident_where(Hostname, Where, Order).


%%Depending on the application name to delete all the state table
delete_all_incident(Hostname, Index, Count) ->
    case get_all_incident(Hostname, Index, Count) of
		[]->
			{error,not_found};
		{ok, R}->
			Ret = [dbcs_itsm:delete_incident(Hostname,X#itsm_incident.id) || X<-R],
			case lists:keymember(error,1,Ret) of
				true->
					{error,Ret};
				_->
					{ok,removed}
			end
	end.
    
%% <<Interface operation>> incident table operation - get the specified id incident
get_incident(Id) ->
    dbcs_itsm:get_incident(Id).
    
%% <<Interface operation>> incident table operations - remove the specified id incident data
delete_incident(Id, AppId) ->
    %%dbcs_itsm:delete_incident(Id).
    dbcs_itsm:delete_incident_appId(Id, AppId).
    
%% <<Interface operation>> incident table operation - to add or save the incident
save_incident(Number, Caller, Location, Configuration_item, Impact, Urgency, Priority, Knowledge, Short_description, Opened, Opened_by, 
            Incident_state, Category, Escalation, Assignment_group, Assigned_to, Additional_comments, Work_notes, Test, Testhoitest, Htmltest, 
            Follow_up, Created, Parent, Child, Status, Project, Step) ->
    Id = textutils:guid(),
    Incident = #itsm_incident{
                                project= Project,
                                id=Id,          %%id
                                number=Number,       %%Work Order Number%% This is not the only mark, but still give way to generate a work order number
                                caller=Caller,      %%Element element belongs
                                location=Location,          %%Language
                                configuration_item=Configuration_item,     %%Respective display tags Label
                                impact=Impact,       %%Respective values 
                                urgency=Urgency,     %%Dependent value
                                priority=Priority,            %%Hint
                                knowledge=Knowledge,        %%Sequence
                                short_description=Short_description,    %%Is invalid
                                opened=Opened,    %%Is invalid
                                opened_by=Opened_by,    %%Is invalid
                                incident_state=Incident_state,    %%Is invalid
                                category=Category,    %%Is invalid
                                escalation=Escalation,    %%Is invalid
                                assignment_group=Assignment_group,    %%Is invalid
                                assigned_to=Assigned_to,    %%Is invalid
                                additional_comments=Additional_comments,    %%Is invalid
                                work_notes=Work_notes,    %%Is invalid
                                test=Test,    %%Is invalid
                                testhoitest=Testhoitest,    %%Is invalid
                                htmltest=Htmltest,    %%Is invalid
                                follow_up=Follow_up,    %%Is invalid
                                created=Created,    %%Is invalid
                                parent=Parent,    %%Is invalid
                                child=Child,    %%Is invalid
                                status=Status,
                                step = Step
    },
    dbcs_itsm:save_incident(Incident).
    

save_incident(Id,Number, Caller, Location, Configuration_item, Impact, Urgency, Priority, Knowledge, Short_description, Opened, Opened_by, 
            Incident_state, Category, Escalation, Assignment_group, Assigned_to, Additional_comments, Work_notes, Test, Testhoitest, Htmltest, 
            Follow_up, Created, Parent, Child, Status,Project,Step) ->
    Incident = #itsm_incident{
                                project= Project,
                                id=Id,         
                                number=Number,       
                                caller=Caller,      
                                location=Location,          
                                configuration_item=Configuration_item,    
                                impact=Impact,       
                                urgency=Urgency,     
                                priority=Priority,           
                                knowledge=Knowledge,        
                                short_description=Short_description,   
                                opened=Opened,   
                                opened_by=Opened_by,   
                                incident_state=Incident_state,    
                                category=Category,    
                                escalation=Escalation,    
                                assignment_group=Assignment_group,    
                                assigned_to=Assigned_to,    
                                additional_comments=Additional_comments,    
                                work_notes=Work_notes,    
                                test=Test,    
                                testhoitest=Testhoitest,    
                                htmltest=Htmltest,    
                                follow_up=Follow_up,    
                                created=Created,    
                                parent=Parent,    
                                child=Child,    
                                status=Status,
                                step = Step
    },
    dbcs_itsm:save_incident(Incident).


save_incident(Incident=#itsm_incident{}) ->
    dbcs_itsm:save_incident(Incident).
    

save_incident_ex(AppId, Caller, Location, Configuration_item, Impact, Urgency, Priority, Knowledge, Short_description, Opened, Opened_by, 
            Incident_state, Category, Escalation, Assignment_group, Assigned_to, Additional_comments, Work_notes, Test, Testhoitest, Htmltest, 
            Follow_up, Created, Parent, Child, Status, Project) ->
    Id = textutils:guid(),
    Number = get_new_incident_number(AppId),
    Incident = #itsm_incident{
                                project= Project,
                                id=Id,         
                                number=Number,       
                                caller=Caller,      
                                location=Location,          
                                configuration_item=Configuration_item,     
                                impact=Impact,       
                                urgency=Urgency,     
                                priority=Priority,            
                                knowledge=Knowledge,        
                                short_description=Short_description,    
                                opened=Opened,    
                                opened_by=Opened_by,  
                                incident_state=Incident_state,   
                                category=Category,    
                                escalation=Escalation,    
                                assignment_group=Assignment_group,    
                                assigned_to=Assigned_to,   
                                additional_comments=Additional_comments,    
                                work_notes=Work_notes,    
                                test=Test,    
                                testhoitest=Testhoitest,   
                                htmltest=Htmltest,    
                                follow_up=Follow_up,    
                                created=Created,    
                                parent=Parent,    
                                child=Child,    
                                status=Status
    },
    case Number of
        {error, Vs} ->
            %%io:format("error number = ~p~n", [Number]),
            {error, Vs};
        _ ->
            %%io:format("right number = ~p~n", [Number]),
            dbcs_itsm:save_incident(Incident)
    end.


copy_incident(Id, ShortDes, Reporter, AppId) ->
    case get_incident(Id) of
        {ok, Incident=#itsm_incident{}} ->
            NId = textutils:guid(),
            Number = get_new_incident_number(AppId),
            Createdatetime = sv_datetime:now2str(sv_datetime:now()),
            NIncident=
                Incident#itsm_incident{
                        id=NId,
                        number=Number,
                        created=Createdatetime,
                        short_description=ShortDes,
                        caller=Reporter
                },
            dbcs_itsm:save_incident(NIncident);
        _ ->
            {error, "empty"}
    end.


test_incident(AppId, Count) ->
    test_save_incident(AppId, Count, 1).

test_save_incident(AppId, Count, CurrCount) ->
    if 
        CurrCount =< Count ->
            save_incident_ex(AppId, "", "", "", "", "", "", "", "test-incident", "", "", 
                            "Open", "", "", "", "", "", "", "", "", "", 
                            "", sv_datetime:now2str(sv_datetime:now()), "", "", ?OpenStatus, AppId),
            test_save_incident(AppId, Count, CurrCount+1);
        true ->
            {ok, completed}
    end.
    
    
    
update_incident(Id, Status, Step) ->
    case get_incident(Id) of
        {ok,V=#itsm_incident{}} ->
            dbcs_itsm:save_incident(V#itsm_incident{status=Status, step=Step});
        _ ->
            {error, "incident_empty"}
    end.


create_incident_alert(Monitor,THIS) ->
    Re = 
    case THIS:get_property(incident) of
        {ok,{incident,AppId}} ->
            {ok,{_,Category}} = THIS:get_property(category),
            {ok,{_,StateStr}} = Monitor:get_attribute(?STATE_STRING),
            {ok,{_,Id}} = Monitor:get_property(id),
            {ok,{_,Name}} = Monitor:get_property(name),
            IdStr = erlang:atom_to_list(Id),
            Classify = Monitor:get_classifier(Category),
            ClassStr = trans_classifier(Classify),
            CategoryStr = erlang:atom_to_list(Category),
            Des = "Monitor ID: "++IdStr ++ "\n" ++
                "Monitor Name: " ++ Name ++ "\n" ++
                "Category: " ++ CategoryStr ++ "\n"
                ++ "Classifier: " ++ ClassStr 
                ++ "Value: " ++ StateStr,
            %%io:format("Des = ~p~n", [Des]),
            save_incident_ex(AppId, "", "", "", "", "", "", "", "Alert-incident", "", "", 
                            "Open", "", "", "", "", "", Des, "", "", "", 
                            "", sv_datetime:now2str(sv_datetime:now()), "", "", ?OpenStatus, AppId);
        _ ->
            []
    end,
    ok.


count_incident(ModuleId) ->
    case get_incident_module(ModuleId, 0, 0) of
        {ok, V} ->
            string:len(V);
        _ ->
            0
    end.
    
trans_classifier([]) ->
    [];
trans_classifier([{Status,Oper,Value}|T]) ->
    StatusStr = erlang:atom_to_list(Status),
    OperStr = erlang:atom_to_list(Oper),
    Vstr = lists:flatten(Value),
    Str = lists:flatten(io_lib:format("~p~p~p",[StatusStr, OperStr,Vstr])),
    Str ++ "\n" ++
    trans_classifier(T);
trans_classifier([H|T]) ->
    trans_classifier(T).

get_all_application(Index, Count) ->
    dbcs_itsm:get_all_application(Index, Count).
    

get_application(Id) ->
    dbcs_itsm:get_application(Id).
    

delete_application(Id) ->
    dbcs_itsm:delete_application(Id).
    

save_application(Title,Name,Hint,Active,Order,Category,Roles,Device_type, Workflow) ->
    Id = textutils:guid(),
    Sys_Application = #itsm_sys_app_application{
                        id=Id,
                        title=Title,
                        name=Name,
                        hint=Hint,
                        active=Active,
                        order=Order,
                        category=Category,
                        roles=Roles,
                        device_type=Device_type,
                        workflow=Workflow
    },
    dbcs_itsm:save_application(Sys_Application).

save_application_withdefaultmodule(Title,Name,Hint,Active,Order,Category,Roles,Device_type, Workflow) ->
    Id = textutils:guid(),
    Sys_Application = #itsm_sys_app_application{
                        id=Id,
                        title=Title,
                        name=Name,
                        hint=Hint,
                        active=Active,
                        order=Order,
                        category=Category,
                        roles=Roles,
                        device_type=Device_type,
                        workflow=Workflow
    },
    case dbcs_itsm:save_application(Sys_Application) of
        {ok, _} ->
            create_default_module(Id);
        _ ->
            {error, "create error"}
    end.
    
save_application_withdefaultmodule(Id, Title,Name,Hint,Active,Order,Category,Roles,Device_type, Workflow) ->
    Sys_Application = #itsm_sys_app_application{
                        id=Id,
                        title=Title,
                        name=Name,
                        hint=Hint,
                        active=Active,
                        order=Order,
                        category=Category,
                        roles=Roles,
                        device_type=Device_type,
                        workflow=Workflow
    },
    case dbcs_itsm:save_application(Sys_Application) of
        {ok, _} ->
            create_default_module(Id);
        _ ->
            {error, "create error"}
    end.


save_application(Id,Title,Name,Hint,Active,Order,Category,Roles,Device_type, Workflow) ->
    Sys_Application = #itsm_sys_app_application{
                        id=Id,
                        title=Title,
                        name=Name,
                        hint=Hint,
                        active=Active,
                        order=Order,
                        category=Category,
                        roles=Roles,
                        device_type=Device_type,
                        workflow=Workflow
    },
    dbcs_itsm:save_application(Sys_Application).
    


get_all_sysmodule(Index, Count) ->
    dbcs_itsm:get_all_sysmodule(Index, Count).
    

get_sysmodule(Id) ->
    dbcs_itsm:get_sysmodule(Id).
    

get_sysmodule_byapp(AppId) ->
    dbcs_itsm:get_sysmodule_byapp(AppId).
    

delete_sysmodule(Id) ->
    dbcs_itsm:delete_sysmodule(Id).


delete_sysmodule_byapp(AppId) ->
    dbcs_itsm:delete_sysmodule_byapp(AppId).
    

save_sysmodule(Title,Table,Order,Application,Hint,Active,Image,Link_type,View_name,Roles,Filter=#itsm_filters{},Arguments) ->
             
    FId = Filter#itsm_filters.id,
    Host = get(hostname),
    Id = textutils:guid(),
    LockId = ?ModuleWriteLock++Id,
    is_lock(LockId),
    Sys_Module = #itsm_sys_app_module{
                id=Id,
                title=Title,
                table=Table,
                order=Order,
                application=Application,
                hint=Hint,
                active=Active,
                image=Image,
                link_type=Link_type,
                view_name=View_name,
                roles=Roles,
                filter=FId,
                arguments=Arguments
    },
    Result = dbcs_itsm:save_sysmodule(Sys_Module),
    case Result of
        {ok, _} ->
            save_filters(Host,Filter);
        _ ->
            {error, "failed"}
    end,
    release_lock(LockId),
    Result.
    

save_sysmodule(Id,Title,Table,Order,Application,Hint,Active,Image,Link_type,View_name,Roles,Filter=#itsm_filters{},Arguments) ->
                  
    FId = Filter#itsm_filters.id,
    Host = get(hostname),
    LockId = ?ModuleWriteLock++Id,
    is_lock(LockId),
    Sys_Module = #itsm_sys_app_module{
                id=Id,
                title=Title,
                table=Table,
                order=Order,
                application=Application,
                hint=Hint,
                active=Active,
                image=Image,
                link_type=Link_type,
                view_name=View_name,
                roles=Roles,
                filter=FId,
                arguments=Arguments
    },
    Result = dbcs_itsm:save_sysmodule(Sys_Module),
    case Result of
        {ok, _} ->
            api_itsm:save_filters(Host,Filter);
        _ ->
            {error, "failed"}
    end,
    release_lock(LockId),
    Result.


create_default_module(AppId) ->
    RootPath = platform:getRoot(),
    DefultFilterFileName = filename:nativename(RootPath ++ "/templates.incident/default_filters.incident"),
    %%io:format("DefultFilterFileName = ~p~n", [DefultFilterFileName]),
    case file:consult(DefultFilterFileName) of
        {ok, Fils} ->
            %%io:format("Fils = ~p~n", [Fils]),
            exe_create_module(Fils, AppId);
        Ots ->
            {error, "create fail"}
    end.

exe_create_module([], AppId) ->
    {ok, "create ok"};
exe_create_module([Module=#itsm_sys_app_module{}|T], AppId) ->
    Fils = 
        case Module#itsm_sys_app_module.filter of
            Fs=#itsm_filters{} ->
                Condition = build_def_condition(Fs#itsm_filters.condition),
                Fs#itsm_filters{id=textutils:guid(), condition=Condition};
            _ ->
                #itsm_filters{id=textutils:guid(),
                                title="All",
                                table="incident"
                                }
        end,
    {Id, Title, Table, Order, Application, Hint, Active, Image, Link_type, View_name, Roles, Filter, Arguments}    =
    case string:strip(Module#itsm_sys_app_module.id) of
        [] ->
            {textutils:guid(),
             Module#itsm_sys_app_module.title,
             Module#itsm_sys_app_module.table,
             Module#itsm_sys_app_module.order,
             AppId,
             Module#itsm_sys_app_module.hint,
             Module#itsm_sys_app_module.active,
             Module#itsm_sys_app_module.image,
             Module#itsm_sys_app_module.link_type,
             Module#itsm_sys_app_module.view_name,
             Module#itsm_sys_app_module.roles,
             Fils,
             Module#itsm_sys_app_module.arguments};
        _ ->
            {Module#itsm_sys_app_module.id,
             Module#itsm_sys_app_module.title,
             Module#itsm_sys_app_module.table,
             Module#itsm_sys_app_module.order,
             AppId,
             Module#itsm_sys_app_module.hint,
             Module#itsm_sys_app_module.active,
             Module#itsm_sys_app_module.image,
             Module#itsm_sys_app_module.link_type,
             Module#itsm_sys_app_module.view_name,
             Module#itsm_sys_app_module.roles,
             Fils,
             Module#itsm_sys_app_module.arguments}
    end,
    save_sysmodule(Id,Title,Table,Order,Application,Hint,Active,Image,Link_type,View_name,Roles,Filter,Arguments),
    exe_create_module(T, AppId);
exe_create_module([H|T], AppId) ->
    exe_create_module(T, AppId).
    
%% 根据默认条件文件构造默认的
build_def_condition([]) ->
    [];   
build_def_condition([{_,Condition=#fielter_condition{}}|T]) ->
    [{textutils:guid(), Condition}] ++
    build_def_condition(T);
build_def_condition([H|T]) ->
    build_def_condition(T).



get_incident_module(ModuleId) ->
    Moudle =
    case get_sysmodule(ModuleId) of
        {ok, V} ->
            V;
        _ ->
            []
    end,
    %%io:format("Moudle = ~p~n", [Moudle]),
    Filters =
    case Moudle of
        VV=#itsm_sys_app_module{} ->
            VV#itsm_sys_app_module.filter;
        _ ->
            []
    end,
    Apps =
    case Moudle of
        VV1=#itsm_sys_app_module{} ->
            VV1#itsm_sys_app_module.application;
        _ ->
            []
    end,
    %%io:format("Filters = ~p~n", [Filters]),
    Where1 = itsm_incident:parse_filters_to_contenstore_where(Filters),
    Len = string:len(Where1),
    Where =
    if
        Len > 0 ->
            Where1 ++ "&my.project="++ Apps;
        true ->
            "my.project="++ Apps
    end,
    dbcs_itsm:get_incident_where(Where).
    

get_incident_module(ModuleId, Index, Count) ->
    Moudle =
    case get_sysmodule(ModuleId) of
        {ok, V} ->
            V;
        _ ->
            []
    end,
    %%io:format("Moudle = ~p~n", [Moudle]),
    Filters =
    case Moudle of
        VV=#itsm_sys_app_module{} ->
            VV#itsm_sys_app_module.filter;
        _ ->
            []
    end,
    Apps =
    case Moudle of
        VV1=#itsm_sys_app_module{} ->
            VV1#itsm_sys_app_module.application;
        _ ->
            []
    end,
    %%io:format("Filters = ~p~n", [Filters]),
    Where1 = itsm_incident:get_contentstore_where(Filters, Apps),
    %%io:format("Where1 = ~p~n", [Where1]),
    Len = string:len(Where1),
    Where2 =
    if
        Len > 0 ->
            Where1;
        true ->
            []
    end,
    Condition=
        #query_condition{
            where=Where2,
            index=Index,
            count=Count
        },
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition.where,
    Order = BeamCondition#query_beam_condition.order,
    dbcs_itsm:get_incident_where(Where, Order).
    

get_incident_filters(Filss=#itsm_filters{}, Index, Count) ->
    %%io:format("filter~n"),
    Apps = [],
    Where1 = itsm_incident:get_contentstore_where_ex(Filss, Apps),
    %%io:format("Where1 = ~p~n", [Where1]),
    Len = string:len(Where1),
    Where2 =
    if
        Len > 0 ->
            Where1;
        true ->
            []
    end,
    Condition=
        #query_condition{
            where=Where2,
            index=Index,
            count=Count
        },
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition.where,
    Order = BeamCondition#query_beam_condition.order,
    dbcs_itsm:get_incident_where(Where, Order).



get_all_sys_sign(Index, Count) ->
    dbcs_itsm:get_all_sys_sign(Index, Count).
    

get_sys_sign(Id) ->
    dbcs_itsm:get_sys_sign(Id).
    

delete_sys_sign(Id) ->
    dbcs_itsm:delete_sys_sign(Id).
    

save_sys_sign(Id,Title,Value) ->
    Sys_sign = #itsm_sys_sign{
            id=Id,
            title=Title,
            value=Value
    },
    dbcs_itsm:save_sys_sign(Sys_sign).


get_max_incident_number(AppId) ->
    case get_sys_sign(AppId) of
        {ok, V=#itsm_sys_sign{}} ->
            V#itsm_sys_sign.value;
        _ ->
            "1"
    end.
    

get_new_incident_number(AppId) ->
    %%
    MaxNumId = "itsm-"++AppId,
    Number = get_max_incident_number(MaxNumId),
    itsm_incident:get_new_incident_number(Number, AppId, MaxNumId).


save_max_incident_number(AppId, Number) ->
    save_sys_sign(AppId,?ITSMField_SysSign_CurrNumber_Title,Number).


resume_factory_number() ->
    save_sys_sign(?ITSMField_SysSign_CurrNumber,?ITSMField_SysSign_CurrNumber_Title,"INC0010100").
    



get_all_comments(Index, Count) ->
    dbcs_itsm:get_all_comments(Index, Count).
    

get_syscomments(Id) ->
    dbcs_itsm:get_syscomments(Id).
    

delete_syscomments(Id) ->
    dbcs_itsm:delete_syscomments(Id).
    

save_syscomments(User, IsEdit, Createdatetime, Content, Incident_id) ->
    Id = textutils:guid(),
    Sys_Comments =
    #itsm_sys_comments{
            id=Id,
            user=User,
            is_edit=IsEdit,
            createdatetime=Createdatetime,
            content=Content,
            incident_id=Incident_id
    },
    dbcs_itsm:save_syscomments(Sys_Comments).
    

save_syscomments(Id, User, IsEdit, Createdatetime, Content, Incident_id) ->
    Sys_Comments =
    #itsm_sys_comments{
            id=Id,
            user=User,
            is_edit=IsEdit,
            createdatetime=Createdatetime,
            content=Content,
            incident_id=Incident_id
    },
    dbcs_itsm:save_syscomments(Sys_Comments).


get_syscomments_byincident(IncidentId) ->
    Where = "my.incident_id="++IncidentId,
    dbcs_itsm:get_syscomments_where(Where).
    



get_all_attachments(Index, Count) ->
    dbcs_itsm:get_all_attachments(Index, Count).
    

get_attachments(Id) ->
    dbcs_itsm:get_attachments(Id).
    

delete_attachments(Id) ->
    FilePath = 
        case get_attachments(Id) of
                {ok, Attach=#itsm_attachment{}} ->
                    %%io:format("FilePath = ~p~n", [Attach#itsm_attachment.filepath]),
                    Attach#itsm_attachment.filepath;
                _ ->
                    ok
        end,
    case dbcs_itsm:delete_attachments(Id) of
        {ok, R} ->
            incident_attachment_ifc:delete_attachments(FilePath),
            {ok, R};
        Oth ->
            Oth 
    end.
    

save_attachments(User, Incident_id, RealPath, FileName, AttachType) ->
    FileExt = filename:extension(FileName),
    case incident_attachment_ifc:save_attachments(RealPath, FileExt) of
        {ok, FilePath, Url} ->
            Id = textutils:guid(),
            Sys_Attachment =
                #itsm_attachment{
                        id=Id,        
                        filename=FileName,    
                        filesize=[],    
                        filetype=FileExt,    
                        updatetime=sv_datetime:now(), 
                        upuser=User,      
                        incident=Incident_id,     
                        filepath=FilePath,     
                        url=Url,
                        type=AttachType
                },
            dbcs_itsm:save_attachments(Sys_Attachment);
        Others ->
            Others
    end.
            
    

save_attachments(Id, User, Incident_id, RealPath, FileName, AttachType) ->
    FileExt = filename:extension(FileName),
    case incident_attachment_ifc:save_attachments(RealPath, FileExt) of
        {ok, FilePath, Url} ->
            Sys_Attachment =
                #itsm_attachment{
                        id=Id,          
                        filename=FileName,    
                        filesize=[],    
                        filetype=FileExt,    
                        updatetime=0,   
                        upuser=User,      
                        incident=Incident_id,    
                        filepath=FilePath,     
                        url=Url,
                        type=AttachType
                },
            dbcs_itsm:save_attachments(Sys_Attachment);
        Others ->
            Others
    end.


get_attachments_byincident(IncidentId) ->
    Where = "my.incident="++IncidentId,
    dbcs_itsm:get_attachments_where(Where).


get_menu(Id) ->
    case (catch get_sysmodule_byapp(Id)) of
            {ok, V} ->
                %%io:format("V = ~p~n", [V]),
                {ok,V};
            _ ->
                case get_sysmodule(Id) of
                    {ok, V1=#itsm_sys_app_module{}} ->
                        %%io:format("V1 = ~p~n", [V1]),
                         AId = V1#itsm_sys_app_module.application,
                        %%io:format("application = ~p~n", [AId]),
                         case (catch get_sysmodule_byapp(AId)) of
                            {ok, V2} ->
                                {ok,V2};
                            _ ->
                                {error, empty}
                         end;
                    _ ->
                        %%io:format("empty~n"),
                        {error, empty}
                end
        end.
        

get_users() ->
    Users = api_user_spl:browse_users(),
    itsm_incident:get_users(Users).


get_steps(Workflow) ->
    StepId = 
        case workflow_state_machine:get_all_step(Workflow) of
            {error, _} ->
                [];
            V ->
                V
        end,
    %%io:format("StepId = ~p~n", [StepId]),
    build_steps(StepId, Workflow).

build_steps([], Workflow) ->
    [];
build_steps([H|T], Workflow) ->
    Name = workflow_state_machine:get_step_name(Workflow,H),
    [{Name, H}] ++ build_steps(T, Workflow).


incident_total([]) ->
    0;
incident_total([Incident=#itsm_incident{}|T]) ->
    %%io:format("nTotal = ~p~n", [Incident#itsm_incident.total]),
    Incident#itsm_incident.total;
incident_total([H|T]) ->
    incident_total(T).
    


save_filters(Host, Filters) ->
    incident_filter:save_filters(Host, Filters).


read_filters(Host, FId) ->
    incident_filter:read_filters(Host, FId).
    

read_filters(FId) ->
    Host = get(hostname),
    %%io:format("Host = ~p~n", [Host]),
    incident_filter:read_filters(Host, FId).


read_default_filters() ->
    incident_filter:read_filters(?DefaultFielters).

delete_filters(Host, FId) ->
    incident_filter:delete_filters(Host, FId).







is_lock(EngineID) ->
	global:set_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	


release_lock(EngineID) ->	
	global:del_lock({EngineID, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).