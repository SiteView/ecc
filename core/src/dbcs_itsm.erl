-module(dbcs_itsm).
-compile(export_all).
-include("itsm_structure.hrl").
-include("config.hrl"). 


%% ********table record
-define(ChoiceListsTable,"choice_lists").   %%Drop-down list
-define(Author,<<"ning.cai@dragonflow.com">>).
-define(DBName,server_conf:get_db_node()).
-define(IncidentTable,"incidents").   %%Work Order Form
-define(ITSMApplication,"itsm_sys_app_application").   %%Application table
-define(ITSMAppModule,"itsm_sys_app_module").   %%Application module procedure table
-define(ITSMSysSignTable,"itsm_sys_sign").   %%System label table
-define(ITSMComments,"itsm_sys_comments").   %%Notes Table 
-define(ITSMAttachments,"itsm_attachment").   %%Note Table


%%

%% **************************************************
%% contenstore Operator Interface*******************************
%% **************************************************
db2term(K,T,V) when not is_binary(V)->{K,V};
db2term(K,T,V) when T=:= number ->
	NV = binary_to_list(V),
	case string:to_float(NV) of
		{error,_}->
            if NV /= [] -> 
			    {K,list_to_integer(NV)};
            true ->
                {K,0} 
            end; 
		_->
			{K,list_to_float(NV)}
	end;
db2term(K,T,V) when T=:= string ->  {K,binary_to_list(V)};
db2term(K,_,V)->{K,binary_to_term(V)}. 


domain(undefined) -> "localhost";
domain("localhost") -> "localhost";
domain(Host) when is_atom(Host) -> atom_to_list(Host);
domain(Host) ->
  case string:str(Host,".") of
      0 -> "localhost";
	  Pos -> 
          case lists:sublist(Host,1,Pos-1) of
               "127" -> "localhost";
               "192" -> "localhost";
               "222" -> "localhost";
               NewHost -> NewHost
          end
  end.
    
insert_data(_,_,_,{})->{error,parameter_error};
insert_data(Host,DbName,Table,Data)->
    AppName = domain(Host),
    %%io:format("itsm !!!!hostname:~p~n",[{AppName,DbName,Table,Data}]),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    rpc:call(DbName,content,create,[[{application,AppName},{version,2}],Newdata]).  
    
    
update_data(_,_,_,_,{})->{error,parameter_error};
update_data(Host,DbName,Table,Where,Data)->
    AppName = domain(Host),
    %%io:format("itsm !!!!  hostname:~p~n",[AppName]),
    Newdata = Data#content{xn_type = list_to_binary(Table),application=list_to_atom(AppName)},
    %io:format("_____1____:~p~n",[Newdata]),
	Result = rpc:call(DbName,content,update,[[{application,AppName},{content,Where}],Newdata]),
    %io:format("_____1____:~p~n",[Result]),
    Result.

delete_data(Host,DbName,Table,Where)->
    AppName = domain(Host),
    %%io:format("hostname:~p~n",[AppName]),
	rpc:call(DbName,content,delete,[[{application,AppName},{content,Where}]]).

get_data(Host,DbName, Table, [])->
    AppName = domain(Host),
    %%io:format("hostname:~p~n",[AppName]),
	%case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
    case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'"},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;

get_data(Host,DbName, Table, Where)->
    AppName = domain(Host),
    %%io:format("hostname:~p~n",[AppName]),
	%case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
    case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end.
get_data(Host,DbName, Table, [],Order) when is_list(Order),length(Order)>0->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'"},"from=0&to=100000" ++ "&" ++ Order]]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, Where,Order) when is_list(Order),length(Order)>0->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},"from=0&to=100000" ++ "&" ++ Order]]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000" ++ "&" ++ Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'"},"from=0&to=100000"]]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(Host,DbName, Table, Where,Order) when is_list(Order)->
    AppName = domain(Host),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},"from=0&to=100000"]]) of
    %case rpc:call(DbName, content, get, [[{application,Table},{content,Where},"from=0&to=100000"]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data(_,_, _, _,_)->{error,parameter_error}.

get_data2(Host,DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(Host),
    %%io:format("hostname:~p~n",[AppName]),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table ++ "'"},Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data2(Host,DbName, Table, Where,Order) when is_list(Order) andalso is_list(Where)->
    AppName = domain(Host),
    %%io:format("hostname:~p~n",[AppName]),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},Order]]) of
		{ok,{_,_,_,R}}->
			R;
		Else->
			Else
	end;
get_data2(_,_, _, _,_)->{error,parameter_error}.


%% **************************************************
get_data2(DbName, Table, [],Order) when is_list(Order)->
    AppName = domain(get(hostname)),
    % io:format("hostname:~p~n",[AppName]),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table ++ "'"},Order]]) of
		{ok,R}->
			R;
		Else->
			Else
	end;
get_data2(DbName, Table, Where,Order) when is_list(Order) andalso is_list(Where)->
    AppName = domain(get(hostname)),
    % io:format("hostname:~p~n",[AppName]),
	case rpc:call(DbName, content, get, [[{application,AppName},{content,"type = '"++Table++"'&"++Where},Order]]) of
		{ok,R}->
			R;
		Else->
			Else
	end;
get_data2(_, _, _,_)->{error,parameter_error}.

%% **************************************************


%% **********************************************************
%% Database structure and data format conversion**********************************
%% **********************************************************
db_to_choice(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #choice_lists{
            id = proplists:get_value(id, Data),
            table=proplists:get_value(table, Data),      
            element=proplists:get_value(element, Data),      
            language=proplists:get_value(language, Data),          
            label=proplists:get_value(label, Data),     
            value=proplists:get_value(value, Data),       
            dependent_value=proplists:get_value(dependent_value, Data),    
            hint=proplists:get_value(hint, Data),           
            sequence=proplists:get_value(sequence, Data),        
            inactive=proplists:get_value(inactive, Data)    
    }.    

choice_to_db(Mach) ->
	[   
        {id,string,list_to_binary(Mach#choice_lists.id)}, 
        {table,string,list_to_binary(Mach#choice_lists.table)}, 
        {element,string,list_to_binary(Mach#choice_lists.element)},
        {language,string,list_to_binary(Mach#choice_lists.language)},
        {label,string,list_to_binary(Mach#choice_lists.label)},
        {value,string,list_to_binary(Mach#choice_lists.value)},        
        {dependent_value,string,list_to_binary(Mach#choice_lists.dependent_value)}, 
        {hint,string,list_to_binary(Mach#choice_lists.hint)},
        {sequence,string,list_to_binary(Mach#choice_lists.sequence)}, 
        {inactive,string,list_to_binary(Mach#choice_lists.inactive)}       
    ].    
    
    
db_to_incident(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #itsm_incident{
            id = proplists:get_value(id, Data),
            number=proplists:get_value(number, Data),
            caller=proplists:get_value(caller, Data),
            location=proplists:get_value(location, Data),
            configuration_item=proplists:get_value(configuration_item, Data),
            impact=proplists:get_value(impact, Data),
            urgency=proplists:get_value(urgency, Data),
            priority=proplists:get_value(priority, Data),
            knowledge=proplists:get_value(knowledge, Data),
            short_description=proplists:get_value(short_description, Data),
            opened=proplists:get_value(opened, Data),
            opened_by=proplists:get_value(opened_by, Data),
            incident_state=proplists:get_value(incident_state, Data),
            category=proplists:get_value(category, Data),
            escalation=proplists:get_value(escalation, Data),
            assignment_group=proplists:get_value(assignment_group, Data),
            assigned_to=proplists:get_value(assigned_to, Data),
            additional_comments=proplists:get_value(additional_comments, Data),
            work_notes=proplists:get_value(work_notes, Data),
            test=proplists:get_value(test, Data),
            testhoitest=proplists:get_value(testhoitest, Data),
            htmltest=proplists:get_value(htmltest, Data),
            follow_up=proplists:get_value(follow_up, Data),
            created=proplists:get_value(created, Data),
            parent=proplists:get_value(parent, Data),
            child=proplists:get_value(child, Data),
            status=proplists:get_value(status, Data),
            project=proplists:get_value(project, Data),
            step=proplists:get_value(step, Data),
            total = 
                case proplists:get_value(total, Data) of
                    V when erlang:is_integer(V) ->
                        V;
                    _ ->
                        0
                end
    }.    

incident_to_db(Mach) ->
	[   
        {project,string,list_to_binary(Mach#itsm_incident.project)},
        {id,string,list_to_binary(Mach#itsm_incident.id)}, 
        {number,string,list_to_binary(Mach#itsm_incident.number)}, 
        {caller,string,list_to_binary(Mach#itsm_incident.caller)},
        {location,string,list_to_binary(Mach#itsm_incident.location)},
        {configuration_item,string,list_to_binary(Mach#itsm_incident.configuration_item)},
        {impact,string,list_to_binary(Mach#itsm_incident.impact)},        
        {urgency,string,list_to_binary(Mach#itsm_incident.urgency)}, 
        {priority,string,list_to_binary(Mach#itsm_incident.priority)},
        {knowledge,string,list_to_binary(Mach#itsm_incident.knowledge)}, 
        {short_description,string,list_to_binary(Mach#itsm_incident.short_description)},       
        {opened,string,list_to_binary(Mach#itsm_incident.opened)},
        {opened_by,string,list_to_binary(Mach#itsm_incident.opened_by)},
        {incident_state,string,list_to_binary(Mach#itsm_incident.incident_state)},
        {category,string,list_to_binary(Mach#itsm_incident.category)},
        {escalation,string,list_to_binary(Mach#itsm_incident.escalation)},
        {assignment_group,string,list_to_binary(Mach#itsm_incident.assignment_group)},
        {assigned_to,string,list_to_binary(Mach#itsm_incident.assigned_to)},
        {additional_comments,string,list_to_binary(Mach#itsm_incident.additional_comments)},
        {work_notes,string,list_to_binary(Mach#itsm_incident.work_notes)},
        {test,string,list_to_binary(Mach#itsm_incident.test)},
        {testhoitest,string,list_to_binary(Mach#itsm_incident.testhoitest)},
        {htmltest,string,list_to_binary(Mach#itsm_incident.htmltest)},
        {follow_up,string,list_to_binary(Mach#itsm_incident.follow_up)},
        {created,string,list_to_binary(Mach#itsm_incident.created)},
        {parent,string,list_to_binary(Mach#itsm_incident.parent)},
        {child,string,list_to_binary(Mach#itsm_incident.child)},
        {status,string,list_to_binary(Mach#itsm_incident.status)},
        {step,string,list_to_binary(Mach#itsm_incident.step)}
    ].    
    
    
db_to_application(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #itsm_sys_app_application{
            id = proplists:get_value(id, Data),
            title=proplists:get_value(title, Data),
            name=proplists:get_value(name, Data),
            hint=proplists:get_value(hint, Data),
            active=proplists:get_value(active, Data),
            order=proplists:get_value(order, Data),
            category=proplists:get_value(category, Data),
            roles=proplists:get_value(roles, Data),
            device_type=proplists:get_value(device_type, Data),
            workflow=proplists:get_value(workflow, Data)
    }.    

application_to_db(Mach) ->
	[   
        {id,string,list_to_binary(Mach#itsm_sys_app_application.id)}, 
        {title,string,list_to_binary(Mach#itsm_sys_app_application.title)}, 
        {name,string,list_to_binary(Mach#itsm_sys_app_application.name)},
        {hint,string,list_to_binary(Mach#itsm_sys_app_application.hint)},
        {active,string,list_to_binary(Mach#itsm_sys_app_application.active)},
        {order,string,list_to_binary(Mach#itsm_sys_app_application.order)},        
        {category,string,list_to_binary(Mach#itsm_sys_app_application.category)}, 
        {roles,string,list_to_binary(Mach#itsm_sys_app_application.roles)},
        {device_type,string,list_to_binary(Mach#itsm_sys_app_application.device_type)},
        {workflow,string,list_to_binary(Mach#itsm_sys_app_application.workflow)}
    ].    
    
    
db_to_appmodule(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #itsm_sys_app_module{
            id = proplists:get_value(id, Data),
            title=proplists:get_value(title, Data),
            table=proplists:get_value(table, Data),
            order=proplists:get_value(order, Data),
            application=proplists:get_value(application, Data),
            hint=proplists:get_value(hint, Data),
            active=proplists:get_value(active, Data),
            image=proplists:get_value(image, Data),
            link_type=proplists:get_value(link_type, Data),
            view_name=proplists:get_value(view_name, Data),
            roles=proplists:get_value(roles, Data),
            filter=proplists:get_value(filter, Data),
            arguments=proplists:get_value(arguments, Data)
    }.    

appmodule_to_db(Mach) ->
	[   
        {id,string,list_to_binary(Mach#itsm_sys_app_module.id)}, 
        {title,string,list_to_binary(Mach#itsm_sys_app_module.title)}, 
        {table,string,list_to_binary(Mach#itsm_sys_app_module.table)},
        {order,string,list_to_binary(Mach#itsm_sys_app_module.order)},
        {application,string,list_to_binary(Mach#itsm_sys_app_module.application)},
        {hint,string,list_to_binary(Mach#itsm_sys_app_module.hint)},        
        {active,string,list_to_binary(Mach#itsm_sys_app_module.active)}, 
        {image,string,list_to_binary(Mach#itsm_sys_app_module.image)},
        {link_type,string,list_to_binary(Mach#itsm_sys_app_module.link_type)},
        {view_name,string,list_to_binary(Mach#itsm_sys_app_module.view_name)},
        {roles,string,list_to_binary(Mach#itsm_sys_app_module.roles)},
        {filter,string,list_to_binary(Mach#itsm_sys_app_module.filter)},
        {arguments,string,list_to_binary(Mach#itsm_sys_app_module.arguments)}
    ].    
    
db_to_sign(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #itsm_sys_sign{
            id = proplists:get_value(id, Data),
            title=proplists:get_value(title, Data),
            value=proplists:get_value(value, Data)
    }.    

sign_to_db(Mach) ->
	[   
        {id,string,list_to_binary(Mach#itsm_sys_sign.id)}, 
        {title,string,list_to_binary(Mach#itsm_sys_sign.title)}, 
        {value,string,list_to_binary(Mach#itsm_sys_sign.value)}
    ].  
    
db_to_comments(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #itsm_sys_comments{
            id = proplists:get_value(id, Data),
            user=proplists:get_value(user, Data),
            is_edit=proplists:get_value(is_edit, Data),
            createdatetime=proplists:get_value(createdatetime, Data),
            content=proplists:get_value(content, Data),
            incident_id=proplists:get_value(incident_id, Data)
    }.    

comments_to_db(Mach) ->
	[   
        {id,string,list_to_binary(Mach#itsm_sys_comments.id)}, 
        {user,string,list_to_binary(Mach#itsm_sys_comments.user)}, 
        {is_edit,string,list_to_binary(Mach#itsm_sys_comments.is_edit)},
        {createdatetime,string,list_to_binary(Mach#itsm_sys_comments.createdatetime)},
        {content,string,list_to_binary(Mach#itsm_sys_comments.content)},
        {incident_id,string,list_to_binary(Mach#itsm_sys_comments.incident_id)}
    ].  
    
db_to_attachments(Advance) ->
    Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    #itsm_attachment{
            id=proplists:get_value(id, Data),
            filename=proplists:get_value(filename, Data),
            filesize=proplists:get_value(filesize, Data),
            filetype=proplists:get_value(filetype, Data),
            updatetime=proplists:get_value(updatetime, Data),
            upuser=proplists:get_value(upuser, Data),
            incident=proplists:get_value(incident, Data),
            filepath=proplists:get_value(filepath, Data),
            url=proplists:get_value(url, Data),
            type=proplists:get_value(type, Data)
    }.    

attachments_to_db(Mach) ->
	[   
        {id,string,list_to_binary(Mach#itsm_attachment.id)}, 
        {filename,string,list_to_binary(Mach#itsm_attachment.filename)}, 
        {filesize,string,list_to_binary(Mach#itsm_attachment.filesize)},
        {filetype,string,list_to_binary(Mach#itsm_attachment.filetype)},
        {updatetime,number,list_to_binary(integer_to_list(Mach#itsm_attachment.updatetime))},
        {upuser,string,list_to_binary(Mach#itsm_attachment.upuser)},
        {incident,string,list_to_binary(Mach#itsm_attachment.incident)},
        {filepath,string,list_to_binary(Mach#itsm_attachment.filepath)},
        {url,string,list_to_binary(Mach#itsm_attachment.url)},
        {type,string,list_to_binary(Mach#itsm_attachment.type)}
    ].  
%% **********************************************************


build_choices([]) ->
    [];
build_choices([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_choice(Advance)] ++
    build_choices(T);
build_choices([H|T]) ->
    build_choices(T).
    
build_incidents([]) ->
    [];
build_incidents([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_incident(Advance)] ++
    build_incidents(T);
build_incidents([H|T]) ->
    build_incidents(T);
build_incidents({R1, R2, Count,[]}) ->
    [];
build_incidents({R1, R2, Count,[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]}) ->
    VCount = erlang:list_to_binary(erlang:integer_to_list(Count)),
    NAdv =  [{total, number, VCount}],
    [db_to_incident(Advance++NAdv)] ++
    build_incidents({R1, R2, Count,T});
build_incidents({R1, R2, Count,[H|T]}) ->
    build_incidents({R1, R2, Count,T}).
    
build_application([]) ->
    [];
build_application([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_application(Advance)] ++
    build_application(T);
build_application([H|T]) ->
    build_application(T).
    
build_appmodule([]) ->
    [];
build_appmodule([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_appmodule(Advance)] ++
    build_appmodule(T);
build_appmodule([H|T]) ->
    build_appmodule(T).
    
build_sign([]) ->
    [];
build_sign([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_sign(Advance)] ++
    build_sign(T);
build_sign([H|T]) ->
    build_sign(T).
    
build_comments([]) ->
    [];
build_comments([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_comments(Advance)] ++
    build_comments(T);
build_comments([H|T]) ->
    build_comments(T).
    
build_attachments([]) ->
    [];
build_attachments([{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}|T]) ->
    [db_to_attachments(Advance)] ++
    build_attachments(T);
build_attachments([H|T]) ->
    build_attachments(T).
    

%% ************************************************************************
%% ***************************Database interface **************************
%% ************************************************************************

get_all_choices(Index, Count) ->
    Ret = db_ecc:get_data(?DBName, ?ChoiceListsTable, ""),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_choices(R)};
 		_ ->
			{error, empty}
	end.   


get_choice(Id) ->
    Where = "id="++Id,
    Ret = db_ecc:get_data(?DBName, ?ChoiceListsTable, Where),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_choice(Advance)};
 		_ ->
			{error, empty}
	end.   


get_choices_where(Where) ->
    Ret = db_ecc:get_data(?DBName, ?ChoiceListsTable, Where),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_choices(R)};
 		_ ->
			{error, empty}
	end.  


delete_choice(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?ChoiceListsTable,Where).
    

save_choice(Choices = #choice_lists{}) ->
    Id = Choices#choice_lists.id,
    Where = "id="++Id,   
    Adv = choice_to_db(Choices),
    Content = {content, erlang:list_to_atom(?ChoiceListsTable), erlang:list_to_atom(Id), <<"sys_choices">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?ChoiceListsTable,Content),
    case Re of
        {ok,_} ->
            {ok,Re};
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?ChoiceListsTable, Where, Content);
        R ->
            {error,R}
    end;
save_choice(_) ->
    {error, error_record}.
    

%% ---- incident�ӿ�


get_all_incident(Index, Count) ->
    Ret = db_ecc:get_data(?DBName, ?IncidentTable, ""),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_incidents(R)};
 		_ ->
			{error, empty}
	end.  
     
    

get_incident(Id) ->
    Where = "id="++Id,
    Ret = db_ecc:get_data(?DBName, ?IncidentTable, Where),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_incident(Advance)};
 		_ ->
			{error, empty}
	end. 


get_incident_where(Where) ->
    Ret = db_ecc:get_data(?DBName, ?IncidentTable, Where),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_incidents(R)};
 		_ ->
			{error, empty}
	end.  
    

get_incident_where(Where, Order) ->
    %%Ret = db_ecc:get_data(?DBName, ?IncidentTable, Where),
    %%io:format("Where = ~p~n", [Where]),
    %%io:format("Order = ~p~n", [Order]),
    %%io:format("real begin get incident.........~n"),
    Ret = get_data2(?DBName, ?IncidentTable, Where,Order),
    %%io:format("real end get incident.........~n"),
	case Ret of	
        {R1, R2, Count, R} when erlang:is_list(R) ->
			{ok,build_incidents({R1, R2, Count, R})};
 		_ ->
			{error, empty}
	end. 

get_incident_where(Hostname, Where, Order) ->
    %%Ret = db_ecc:get_data(?DBName, ?IncidentTable, Where),
    %%io:format("Hostname = ~p~nWhere = ~p~nOrder = ~p~n", [Hostname, Where, Order]),
    Ret = get_data2(Hostname, ?DBName, ?IncidentTable, Where,Order),
    io:format("Ret = ~p~n", [Ret]),
	case Ret of		
        [] ->
            {error, empty};
		R when erlang:is_list(Ret) ->
			{ok,build_incidents(R)};
 		_ ->
			{error, empty}
	end. 


delete_incident_appId(Id, AppId) ->
    Where = "id="++Id,
    Result = db_ecc:delete_data(?DBName,?IncidentTable,Where).
    %%save_incident_index(subtract, Result, AppId).
    
 
delete_incident(Hostname, Id) ->
    Where = "id="++Id,
    delete_data(Hostname, ?DBName,?IncidentTable,Where).


    

save_incident(Incident = #itsm_incident{}) ->
    Id = Incident#itsm_incident.id,
    Where = "id="++Id,   
    Adv = incident_to_db(Incident),
    Content = {content, erlang:list_to_atom(?IncidentTable), erlang:list_to_atom(Id), <<"incident">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?IncidentTable,Content),
    AppId = Incident#itsm_incident.project,
    case Re of
        {ok,_} ->
            %%save_incident_index(add, Re, AppId),        
            {ok,Re};
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?IncidentTable, Where, Content);
        R ->
            {error,R}
    end;
save_incident(_) ->
    {error, error_record}.



save_incident_index(subtract, Result, AppId) ->
    case Result of
        {ok, _} ->
            case AppId of
                [] ->
                    {ok, "svae incdient"};
                _ ->
                    case get_sys_sign("incident_count_all_"++AppId) of
                        {ok, Sign=#itsm_sys_sign{}} ->
                            Vint =
                            case string:to_integer(Sign#itsm_sys_sign.value) of
                                {error, _} ->
                                    0;
                                {Int, _} ->
                                    Int
                            end,
                            VVInt = 
                                if 
                                    Vint > 0 ->
                                        Vint - 1;
                                    true ->
                                        0
                                end,
                            save_sys_sign(Sign#itsm_sys_sign{value=erlang:integer_to_list(VVInt)});
                        _ ->
                            VVInt = 0,
                            Sign=#itsm_sys_sign
                            {
                                id="incident_count_all_"++AppId,
                                title="project total",
                                value=erlang:integer_to_list(VVInt)
                            },
                            save_sys_sign(Sign)
                    end
            end;
        Ots ->
            Ots 
    end;
save_incident_index(add, Result, AppId) ->
    case Result of
        {ok, _} ->
            case AppId of
                [] ->
                    {ok, "svae incdient"};
                _ ->
                    case get_sys_sign("incident_count_all_"++AppId) of
                        {ok, Sign=#itsm_sys_sign{}} ->
                            Vint =
                            case string:to_integer(Sign#itsm_sys_sign.value) of
                                {error, _} ->
                                    0;
                                {Int, _} ->
                                    Int
                            end,
                            VVInt = Vint + 1,
                            save_sys_sign(Sign#itsm_sys_sign{value=erlang:integer_to_list(VVInt)});
                        _ ->
                            VVInt = 1,
                            Sign=#itsm_sys_sign
                            {
                                id="incident_count_all_"++AppId,
                                title="project total",
                                value=erlang:integer_to_list(VVInt)
                            },
                            save_sys_sign(Sign)
                    end
            end;
        Ots ->
            Ots 
    end.
    



get_all_application(Index, Count) ->
    Ret = db_ecc:get_data(?DBName, ?ITSMApplication, ""),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_application(R)};
 		_ ->
			{error, empty}
	end.  
    

get_application(Id) ->
    Where = "id="++Id,
    Ret = db_ecc:get_data(?DBName, ?ITSMApplication, Where),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_application(Advance)};
 		_ ->
			{error, empty}
	end. 
    

delete_application(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?ITSMApplication,Where).
    

save_application(Sys_Application = #itsm_sys_app_application{}) ->
    Id = Sys_Application#itsm_sys_app_application.id,
    Where = "id="++Id,   
    Adv = application_to_db(Sys_Application),
    Content = {content, erlang:list_to_atom(?ITSMApplication), erlang:list_to_atom(Id), <<"itsm_sys_application">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?ITSMApplication,Content),
    case Re of
        {ok,_} ->
            {ok,Re};
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?ITSMApplication, Where, Content);
        R ->
            {error,R}
    end;
save_application(_) ->
    {error, error_record}.
    
    
%% ---- module interface


get_all_sysmodule(Index, Count) ->
    Ret = db_ecc:get_data(?DBName, ?ITSMAppModule, ""),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_appmodule(R)};
 		_ ->
			{error, empty}
	end.  
    
%% <<Interface operation>> itsm_sys_app_module table operation - get the specified id itsm_sys_app_module
get_sysmodule(Id) ->
    Where = "id="++Id,
    Ret = db_ecc:get_data(?DBName, ?ITSMAppModule, Where),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_appmodule(Advance)};
 		_ ->
			{error, empty}
	end. 
    
%% <<Interface operation>> itsm_sys_app_module table operation - for the specified application id of itsm_sys_app_module
get_sysmodule_byapp(AppId) ->
    Where = "my.application="++AppId,
    %%io:format("Where = ~p~n", [Where]),
    Ret = db_ecc:get_data(?DBName, ?ITSMAppModule, Where),
    %%io:format("Ret = ~p~n", [Ret]),
	case Ret of		
        [] ->
            {error, empty};
		R when erlang:is_list(Ret) ->
			{ok,build_appmodule(R)};
 		_ ->
			{error, empty}
	end.  
    
%% <<Interface operation>> itsm_sys_app_module table operations - remove the specified id itsm_sys_app_module data
delete_sysmodule(Id) ->
    Where = "id="++Id,
    %% ɾ��ģ���ͬʱ,ɾ�������
    case get_sysmodule(Id) of
        {ok, Mo=#itsm_sys_app_module{}} ->
            Host = get(hostname),
            FId = Mo#itsm_sys_app_module.filter,
            incident_filter:delete_filters(Host, FId);
        _ ->
            {error, "delete fail"}
    end,
    db_ecc:delete_data(?DBName,?ITSMAppModule,Where).

%% <<Interface operation>> itsm_sys_app_module table operation - delete the specified data applications itsm_sys_app_module
delete_sysmodule_byapp(AppId)->
    case get_sysmodule_byapp(AppId) of
        {ok,V} ->
            Ret = [delete_sysmodule(X#itsm_sys_app_module.id) || X<-V],
			case lists:keymember(error,1,Ret) of
				true->
					{error,Ret};
				_->
					{ok,removed}
			end;
        _ ->
            {error,"empty"}
    end.


save_sysmodule(Sys_Module = #itsm_sys_app_module{}) ->
    Id = Sys_Module#itsm_sys_app_module.id,
    Where = "id="++Id,   
    Adv = appmodule_to_db(Sys_Module),
    Content = {content, erlang:list_to_atom(?ITSMAppModule), erlang:list_to_atom(Id), <<"itsm_sys_app_module">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?ITSMAppModule,Content),
    case Re of
        {ok,_} ->
            {ok,Re};
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?ITSMAppModule, Where, Content);
        R ->
            {error,R}
    end;
save_sysmodule(_) ->
    {error, error_record}.
    



get_all_comments(Index, Count) ->
    Ret = db_ecc:get_data(?DBName, ?ITSMComments, ""),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_comments(R)};
 		_ ->
			{error, empty}
	end.  
    

get_syscomments(Id) ->
    Where = "id="++Id,
    Ret = db_ecc:get_data(?DBName, ?ITSMComments, Where),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_comments(Advance)};
 		_ ->
			{error, empty}
	end.  
    

get_syscomments_where(Where) ->
    Ret = db_ecc:get_data(?DBName, ?ITSMComments, Where),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_comments(R)};
 		_ ->
			{error, empty}
	end.  
    

delete_syscomments(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?ITSMComments,Where).


save_syscomments(Sys_Comments = #itsm_sys_comments{}) ->
    Id = Sys_Comments#itsm_sys_comments.id,
    Where = "id="++Id,   
    Adv = comments_to_db(Sys_Comments),
    Content = {content, erlang:list_to_atom(?ITSMComments), erlang:list_to_atom(Id), <<"itsm_sys_comments">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?ITSMComments,Content),
    case Re of
        {ok,_} ->
            {ok,Re};
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?ITSMComments, Where, Content);
        R ->
            {error,R}
    end;
save_syscomments(_) ->
    {error, error_record}.
    



get_all_attachments(Index, Count) ->
    Ret = db_ecc:get_data(?DBName, ?ITSMAttachments, ""),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_attachments(R)};
 		_ ->
			{error, empty}
	end.  
    

get_attachments(Id) ->
    Where = "id="++Id,
    Ret = db_ecc:get_data(?DBName, ?ITSMAttachments, Where),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_attachments(Advance)};
 		_ ->
			{error, empty}
	end.  
    

get_attachments_where(Where) ->
    Ret = db_ecc:get_data(?DBName, ?ITSMAttachments, Where),
	case Ret of		
		R when erlang:is_list(Ret) ->
            %%io:format("Ret = ~p~n", [Ret]),
			{ok,build_attachments(R)};
 		_ ->
			{error, empty}
	end.  
    

delete_attachments(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?ITSMAttachments,Where).


save_attachments(Sys_Comments = #itsm_attachment{}) ->
    Id = Sys_Comments#itsm_attachment.id,
    Where = "id="++Id,   
    Adv = attachments_to_db(Sys_Comments),
    Content = {content, erlang:list_to_atom(?ITSMAttachments), erlang:list_to_atom(Id), <<"itsm_attachment">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?ITSMAttachments,Content),
    case Re of
        {ok,_} ->
            {ok,Re};
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?ITSMAttachments, Where, Content);
        R ->
            {error,R}
    end;
save_attachments(_) ->
    {error, error_record}.


get_all_sys_sign(Index, Count) ->
    Ret = db_ecc:get_data(?DBName, ?ITSMSysSignTable, ""),
	case Ret of		
		R when erlang:is_list(Ret) ->
			{ok,build_sign(R)};
 		_ ->
			{error, empty}
	end.  
    

get_sys_sign(Id) ->
    Where = "id="++Id,
    Ret = db_ecc:get_data(?DBName, ?ITSMSysSignTable, Where),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			{ok,db_to_sign(Advance)};
 		_ ->
			{error, empty}
	end. 
    

delete_sys_sign(Id) ->
    Where = "id="++Id,
    db_ecc:delete_data(?DBName,?ITSMSysSignTable,Where).
    

save_sys_sign(Sys_sign = #itsm_sys_sign{}) ->
    Id = Sys_sign#itsm_sys_sign.id,
    Where = "id="++Id,   
    Adv = sign_to_db(Sys_sign),
    Content = {content, erlang:list_to_atom(?ITSMSysSignTable), erlang:list_to_atom(Id), <<"itsm_sys_sign">>,null,null,null,null,?Author,null,null,null,null,null,Adv},
    Re = db_ecc:insert_data(?DBName,?ITSMSysSignTable,Content),
    case Re of
        {ok,_} ->
            {ok,Re};
        {error,content_id_existed} ->
            db_ecc:update_data(?DBName, ?ITSMSysSignTable, Where, Content);
        R ->
            {error,R}
    end;
save_sys_sign(_) ->
    {error, error_record}.
