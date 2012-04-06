-module(dbcs_workflow).
-compile(export_all).

-define(StatusTable,"status").
-define(WorkflowTable,"workflow").
-define(StepTable,"step").
-define(WorkflowIdTable,"workflowid").
-define(IdTable,"id").
-define(TransitionTable,"transition").
-define(Author,<<"lei.lin@dragonflow.com">>).
-define(DBName,server_conf:get_db_node()).

-include("itsm_structure.hrl").

-record(application, {id, title, published, updated, author, description,
                        viewSourceUrl, privateSource, runOwnAds,
                        xn_premiumService,  xn_tag, xn_category, xn_domain,
                        xn_layoutVersion, xn_active, link, seqenceNumber, storeLimit, contentCount = 0}). 

%%%%%%%%%%%%%%%%%%%work flow function%%%%%%%%%%%%%%%%%%%
get_workflow_id() ->
    Host = get(hostname),
    is_lock(Host),
	Ret = db_ecc:get_data(?DBName, ?WorkflowIdTable, "id=id"),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			Record = db_to_workflowid(Advance);
 		_->
			Record = #id{}
	end,    
    case Record#id.number of     
    undefined ->
        Adv = #id{number="1"}, 
        db_ecc:insert_data(?DBName, ?WorkflowIdTable, {content, list_to_atom(?WorkflowIdTable), list_to_atom("id"), <<"id">>,null,null,null,null,?Author,null,null,null,null,null,workflowid_to_db(Adv)}),
        N = 1;        
    Num ->
        Adv = #id{number=integer_to_list(Num+1)},
        Where = "id=" ++"id",
        NewRecord = {content, list_to_atom(?WorkflowIdTable), list_to_atom("id"), <<"id">>, null, null, null, null, ?Author, null, null, null, null, null, workflowid_to_db(Adv)},
        db_ecc:update_data(?DBName, ?WorkflowIdTable, Where, NewRecord),
        N = Num+1
    end,
    release_lock(Host),
    N.  
    
workflowid_to_db(Record) ->
    [
        {number,number,list_to_binary(Record#id.number)} 
    ]. 

db_to_workflowid(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #id{ 
        number = proplists:get_value(number, Data)           
    }.  

create_workflow(WorkflowName,Description) ->
    Id = get_workflow_id(), 
    case  get_workflow_by_name(WorkflowName) of
    [] ->
        Adv = #workflow{id=integer_to_list(Id),name=WorkflowName,description = Description, schemes = "",steps = [1],index="0"},     
        case db_ecc:insert_data(?DBName, ?WorkflowTable, {content, list_to_atom(?WorkflowTable), list_to_atom( integer_to_list(Id)), <<"workflow">>,null,null,null,null,?Author,null,null,null,null,null,workflow_to_db(Adv)}) of
        {ok,_} ->
            Adv2 = #step{workflowid=integer_to_list(Id),step_id="1",name = "Open",linked_status = "Open", transitions=[],index="0"},     
            case db_ecc:insert_data(?DBName, ?StepTable, {content, list_to_atom(?StepTable), list_to_atom( integer_to_list(Id)++"_"++"1"), <<"step">>,null,null,null,null,?Author,null,null,null,null,null,step_to_db(Adv2)}) of
            {error,Reason} ->
                {error,Reason};
            _ ->
                ok 
            end;  
        {error,Reason} ->
            {error,Reason};
        _ ->
            {error,undefined} 
        end;  
    _ ->
        {error,existed} 
    end.

bind_workflow(WorkflowId) ->
    case  get_workflow(WorkflowId) of
    [] ->
        {error,""};
    [Old] ->
        Where = "id=" ++WorkflowId,   
        New = Old#workflow{index=integer_to_list(Old#workflow.index+1)},
        Record = {content, list_to_atom(?WorkflowTable), list_to_atom(WorkflowId), <<"workflow">>, null, null, null, null, ?Author, null, null, null, null, null, workflow_to_db(New)},
        db_ecc:update_data(?DBName, ?WorkflowTable, Where, Record)            
    end. 
    
release_workflow(WorkflowId) ->
    case  get_workflow(WorkflowId) of
    [] ->
        {error,""};
    [Old] ->
        Where = "id=" ++ WorkflowId,   
        New = Old#workflow{index=integer_to_list(Old#workflow.index-1)},
        Record = {content, list_to_atom(?WorkflowTable), list_to_atom(WorkflowId), <<"workflow">>, null, null, null, null, ?Author, null, null, null, null, null, workflow_to_db(New)},
        db_ecc:update_data(?DBName, ?WorkflowTable, Where, Record)            
    end.     
    
%StepId  is int  
update_workflow_step(WorkflowId,StepId) ->
    case  get_workflow(WorkflowId) of
    [] ->
        {error,""};
    [Old] ->
        OldStep = Old#workflow.steps,
        case lists:member(StepId,OldStep) of
        true ->
            {error,existed};
        _ ->
            Where = "id=" ++ WorkflowId, 
            Steps =[StepId|OldStep],
            New = Old#workflow{steps=Steps,index=integer_to_list(Old#workflow.index)},
            Record = {content, list_to_atom(?WorkflowTable), list_to_atom(WorkflowId), <<"workflow">>, null, null, null, null, ?Author, null, null, null, null, null, workflow_to_db(New)},
           db_ecc:update_data(?DBName, ?WorkflowTable, Where, Record)            
        end
    end.     

update_workflow(WorkflowId,NewWorkflowName,Description) ->  
    case  get_workflow(WorkflowId) of
    [] ->
        io:format("error~n"),
        {error,""};
    [Old] ->
         io:format("Old~p~n",[Old]),
        if NewWorkflowName == "" ->
            if  Description /= "" -> 
                Where = "id=" ++ WorkflowId, 
                New = Old#workflow{description=Description,index=integer_to_list(Old#workflow.index)},
                Record = {content, list_to_atom(?WorkflowTable), list_to_atom(WorkflowId), <<"workflow">>, null, null, null, null, ?Author, null, null, null, null, null, workflow_to_db(New)},
                db_ecc:update_data(?DBName, ?WorkflowTable, Where, Record);
            true ->
                ok 
            end; 
        true ->
            if Description /= "" ->          
                Where = "id=" ++ WorkflowId, 
                New = Old#workflow{name=NewWorkflowName,description=Description,index=integer_to_list(Old#workflow.index)},
                Record = {content, list_to_atom(?WorkflowTable), list_to_atom(WorkflowId), <<"workflow">>, null, null, null, null, ?Author, null, null, null, null, null, workflow_to_db(New)},
               db_ecc:update_data(?DBName, ?WorkflowTable, Where, Record);
            true ->
                Where = "id=" ++ WorkflowId, 
                New = Old#workflow{name=NewWorkflowName,index=integer_to_list(Old#workflow.index)},
                Record = {content, list_to_atom(?WorkflowTable), list_to_atom(WorkflowId), <<"workflow">>, null, null, null, null, ?Author, null, null, null, null, null, workflow_to_db(New)},
               db_ecc:update_data(?DBName, ?WorkflowTable, Where, Record)
            end 
        end
    end.        


delete_workflow_step(WorkflowId,StepId) ->  
    case  get_workflow(WorkflowId) of
    [] ->
        {error,""};
    [Old] ->
        OldStep = Old#workflow.steps,
        case lists:member(StepId,OldStep) of
        true ->
            {error,existed};
        _ ->
            Where = "id=" ++ WorkflowId, 
            Steps =lists:delete(list_to_integer(StepId),OldStep),
            New = Old#workflow{steps=Steps,index=integer_to_list(Old#workflow.index)},
            Record = {content, list_to_atom(?WorkflowTable), list_to_atom(WorkflowId), <<"workflow">>, null, null, null, null, ?Author, null, null, null, null, null, workflow_to_db(New)},
           db_ecc:update_data(?DBName, ?WorkflowTable, Where, Record)            
        end
    end.     

get_workflow_by_name(WorkflowName) ->
     Ret = db_ecc:get_data(?DBName, ?WorkflowTable, "my.name="++WorkflowName),
     io:format("Ret::::::::::::::::::::::::::::~p~n",[Ret]),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_workflow(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.    
   
get_workflow(WorkflowId) ->
     Ret = db_ecc:get_data(?DBName, ?WorkflowTable, "id="++WorkflowId),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_workflow(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.      
 
get_all_workflow() ->
    Ret = db_ecc:get_data(?DBName, ?WorkflowTable, ""),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			    [db_to_workflow(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.

delete_workflow(Workflowname) ->
    db_ecc:delete_data(?DBName, ?WorkflowTable, "id="++ Workflowname). 

delete_workflow(Workflowname,Host) ->
    dbcs_tr069:delete_data(Host,?DBName, ?WorkflowTable, "id="++ Workflowname). 

workflow_to_db(Record) -> 
	[
        {id,string,list_to_binary(Record#workflow.id)}, 
        {name,string,list_to_binary(Record#workflow.name)},       
        {description,string,list_to_binary(Record#workflow.description)},
        %{status,string,list_to_binary(Record#workflow.status)},
        {schemes,string,list_to_binary(Record#workflow.schemes)},
        {steps,term,term_to_binary(Record#workflow.steps)},
        {index,number,list_to_binary(Record#workflow.index)}         
	].
     
     
db_to_workflow(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #workflow{ 
        id = proplists:get_value(id, Data), 
        name = proplists:get_value(name, Data), 
        description = proplists:get_value(description, Data),  
        %status = proplists:get_value(status, Data),  
        schemes = proplists:get_value(schemes, Data),
        steps = proplists:get_value(steps, Data),
        index = proplists:get_value(index, Data)         
    }. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 


create_step(WorkflowId,Name,Linked_status) ->
    case  get_step_by_name(WorkflowId,Name) of
    [] ->
         Id = get_max_stepid(WorkflowId), 
        Adv = #step{workflowid=WorkflowId,step_id= integer_to_list(Id+1),name = Name,linked_status = Linked_status, transitions=[],index = "0"},     
        case db_ecc:insert_data(?DBName, ?StepTable, {content, list_to_atom(?StepTable), list_to_atom(WorkflowId++"_"++integer_to_list(Id+1)), <<"step">>,null,null,null,null,?Author,null,null,null,null,null,step_to_db(Adv)}) of
        {error,_} ->
            {error,""};
        _ ->
            update_workflow_step(WorkflowId,Id+1)  
        end; 
    _ ->
        {error,existed} 
    end.        

get_step_by_name(WorkflowId,Name) ->
     Ret = db_ecc:get_data(?DBName, ?StepTable, "my.workflowid="++WorkflowId++"&my.name="++Name),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_step(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.   

get_step(WorkflowId,StepId) ->
     Ret = db_ecc:get_data(?DBName, ?StepTable, "id="++WorkflowId++"_"++StepId),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_step(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.      


get_all_step(WorkflowId) ->
    Ret = db_ecc:get_data(?DBName, ?StepTable, "my.workflowid="++WorkflowId),
    io:format("Ret:~p~n",[Ret]),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_step(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.

get_line_status(WorkflowId,StepId) ->
    case  get_step(WorkflowId,StepId) of
    [] ->
        {error,""};
    [Step] ->
        Step#step.linked_status 
    end.
    
get_step_name(WorkflowId,StepId) ->
    case  get_step(WorkflowId,StepId) of
    [] ->
        {error,""};
    [Step] ->
        Step#step.name 
    end.

get_max_stepid(WorkflowId) ->
    case  get_all_step(WorkflowId) of
    [] ->
        1;
    Steps ->
        get_max_stepid_t(Steps,length(Steps),1) 
    end.
get_max_stepid_t(_,0,Number) ->Number;
get_max_stepid_t([A|B],Len,Num) ->
    Id = A#step.step_id, 
    if Id > Num ->
        get_max_stepid_t(B,Len-1,Id);
    true ->
        get_max_stepid_t(B,Len-1,Num) 
    end.
   
update_step_transitions(WorkflowId,StepId,TransitionId) ->
    case get_step(WorkflowId,StepId) of
    [] ->
        {error,""};
    [Step] ->
        Where = "id=" ++ WorkflowId++"_"++StepId, 
        NewTransitions = [TransitionId|Step#step.transitions],
        New =Step#step{step_id=integer_to_list(Step#step.step_id),transitions=NewTransitions,index =integer_to_list(Step#step.index)},
        Record = {content, list_to_atom(?StepTable), list_to_atom(WorkflowId++"_"++StepId), <<"step">>, null, null, null, null, ?Author, null, null, null, null, null, step_to_db(New)},
        db_ecc:update_data(?DBName, ?StepTable, Where, Record)                  
    end.       

delete_step(WorkflowId,StepId) ->
    case get_step(WorkflowId,StepId) of   
    [] ->
        {error,""};
    [Step] ->
        Transitions = Step#step.transitions, 
        release_steplist(WorkflowId,Transitions),
        case db_ecc:delete_data(?DBName, ?StepTable, "id="++ WorkflowId++"_"++StepId) of
        {error,_} ->
            {error,""};
        _ ->         
            delete_workflow_step(WorkflowId,StepId)  
        end
    end.    
 
release_steplist(WorkflowId,Transitions) ->
    release_steplist_t(WorkflowId,Transitions,length(Transitions)).
release_steplist_t(_,_,0) -> ok;
release_steplist_t(WorkflowId,[A|B],Len) ->
    case get_transition(list_to_integer(A)) of
    [] ->
        release_steplist_t(WorkflowId,B,Len-1);
    [Tran] ->
        io:format("Tran:~p~n",[Tran]), 
        release_step(WorkflowId,Tran#transition.destination_step),
        release_steplist_t(WorkflowId,B,Len-1)         
    end. 

delete_step(WorkflowId,StepId,Host) ->
    dbcs_tr069:delete_data(Host,?DBName, ?StepTable, "id="++ WorkflowId++"_"++StepId).  

bind_step(WorkflowId,StepId) ->
    case get_step(WorkflowId,StepId) of
    [] ->
        {error,""};
    [Old] ->       
        Where = "id=" ++ WorkflowId++"_"++StepId,   
        New = Old#step{step_id=integer_to_list(Old#step.step_id),index=integer_to_list(Old#step.index+1)},
        Record = {content, list_to_atom(?StepTable), list_to_atom(WorkflowId++"_"++StepId), <<"step">>, null, null, null, null, ?Author, null, null, null, null, null, step_to_db(New)},
        db_ecc:update_data(?DBName, ?StepTable, Where, Record)
    end. 
    
release_step(WorkflowId,StepId) ->
    case get_step(WorkflowId,StepId) of
    [] ->
        {error,""};
    [Old] ->        
        Where = "id=" ++ WorkflowId++"_"++StepId,   
        New = Old#step{step_id=integer_to_list(Old#step.step_id),index=integer_to_list(Old#step.index-1)},
        Record = {content, list_to_atom(?StepTable), list_to_atom(WorkflowId++"_"++StepId), <<"step">>, null, null, null, null, ?Author, null, null, null, null, null, step_to_db(New)},
        db_ecc:update_data(?DBName, ?StepTable, Where, Record)
    end.    

step_to_db(Record) ->
	[
        {workflowid,string,list_to_binary(Record#step.workflowid)},       
        {step_id,number,list_to_binary(Record#step.step_id)},
        {name,string,list_to_binary(Record#step.name)},
        {linked_status,string,list_to_binary(Record#step.linked_status)},
        {transitions,term,term_to_binary(Record#step.transitions)},
        {index,number,list_to_binary(Record#step.index)}        
	].     

db_to_step(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #step{ 
        workflowid = proplists:get_value(workflowid, Data), 
        step_id = proplists:get_value(step_id, Data),  
        %mode = proplists:get_value(mode, Data),  
        name = proplists:get_value(name, Data),
        linked_status = proplists:get_value(linked_status, Data),
        transitions = proplists:get_value(transitions, Data),
        index = proplists:get_value(index, Data)         
    }. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




init_status() ->
    Apps = all_app(?DBName),
    init_status_t(Apps,length(Apps)).
init_status_t(_,0) -> ok;
init_status_t([Host|B],Len) ->
    init_install_status(Host,"Open","The issue is open and ready for the assignee to start work on it.",""),
    init_install_status(Host,"InProgress","This issue is being actively worked on at the moment by the assignee.",""),
    init_install_status(Host,"Reopened","This issue was once resolved, but the resolution was deemed incorrect. From here issues are either marked assigned or resolved.",""),
    init_install_status(Host,"Resolved","A resolution has been taken, and it is awaiting verification by reporter. From here issues are either reopened, or are closed.",""),
    init_install_status(Host,"Closed","The issue is considered finished, the resolution is correct. Issues which are closed can be reopened.",""),
    init_install_status(Host,"Assigned","",""),
    init_install_status(Host,"Document","",""),
    init_install_status(Host,"Generic","",""),
    init_install_status(Host,"Visible","",""),
init_status_t(B,Len-1).    

init_install_status(Host,StatusName,Description,Icon_url) ->
    case  dbcs_tr069:get_data(Host,?DBName, ?StatusTable, "id="++StatusName) of
    [] ->
        Adv = #status{name=StatusName,description=Description,workflows=[],icon_url=Icon_url,index="1"},
        dbcs_tr069:insert_data(Host,?DBName, ?StatusTable, {content, list_to_atom(?StatusTable), list_to_atom(StatusName), <<"status">>,null,null,null,null,?Author,null,null,null,null,null,status_to_db(Adv)});         
    _ ->
        {error,existed}   
    end. 

all_app(DB) ->     
    case rpc:call(DB,ets,tab2list,[application]) of
    Apps when is_list(Apps) ->
        F = fun(X) -> X#application.id end,
        lists:map(F,Apps);
    _ ->
        []
    end.  


get_all_status() ->
    Ret = db_ecc:get_data(?DBName, ?StatusTable, ""),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end. 

get_all_status_localhost() ->
    Ret = dbcs_tr069:get_data("localhost",?DBName, ?StatusTable, ""),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_status(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end. 

install_status(StatusName,Description,Icon_url) ->
    case  db_ecc:get_data(?DBName, ?StatusTable, "id="++StatusName) of
    [] ->
        Adv = #status{name=StatusName,description=Description,workflows=[],icon_url=Icon_url,index="0"},
        db_ecc:insert_data(?DBName, ?StatusTable, {content, list_to_atom(?StatusTable), list_to_atom(StatusName), <<"status">>,null,null,null,null,?Author,null,null,null,null,null,status_to_db(Adv)});         
    _ ->
        {error,existed}   
    end. 

update_status(OldStatusName,NewStatusName,NewDescription,NewIcon_url) ->
    case  db_ecc:get_data(?DBName, ?StatusTable, "id="++OldStatusName) of 
    [] ->
        Adv = #status{name=NewStatusName,description=NewDescription,workflows=[],icon_url=NewIcon_url,index="0"},
        db_ecc:insert_data(?DBName, ?StatusTable, {content, list_to_atom(?StatusTable), list_to_atom(NewStatusName), <<"status">>,null,null,null,null,?Author,null,null,null,null,null,status_to_db(Adv)});
    Old ->
        Where = "id=" ++OldStatusName,
        Record = Old#status{name=NewStatusName,description=NewDescription,icon_url=NewIcon_url},
        NewRecord = {content, list_to_atom(?StatusTable), list_to_atom(NewStatusName), <<"status">>, null, null, null, null, ?Author, null, null, null, null, null, status_to_db(Record)},
        db_ecc:update_data(?DBName, ?StatusTable, Where, NewRecord)       
    end.


delete_status(StatusName) ->
    db_ecc:delete_data(?DBName, ?StatusTable, "id="++ StatusName).   

delete_status(W,Host) ->
    Where = "id="++W,
    dbcs_tr069:delete_data(Host,?DBName, ?StatusTable, Where). 

status_to_db(Record) -> 
	[
        {name,string,list_to_binary(Record#status.name)},       
        {description,string,list_to_binary(Record#status.description)},
        %{mode,string,list_to_binary(Record#status.mode)},
        {workflows,string,list_to_binary(Record#status.workflows)},
        {icon_url,string,list_to_binary(Record#status.icon_url)},
        {index,number,list_to_binary(Record#status.index)}         
	].
          
db_to_status(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #status{ 
        name = proplists:get_value(name, Data), 
        description = proplists:get_value(description, Data),  
        %mode = proplists:get_value(mode, Data),  
        workflows = proplists:get_value(workflows, Data),
        icon_url = proplists:get_value(icon_url, Data),
        index = proplists:get_value(index, Data)          
    }. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



get_transition_id() ->
    Host = get(hostname),
    is_lock(Host),
	Ret = db_ecc:get_data(?DBName, ?IdTable, "id=transition_id"),
	case Ret of		
		[{content, _,_MId, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			Record = db_to_transitionid(Advance);
 		_->
			Record = #transition_id{}
	end,    
    case Record#transition_id.number of     
    undefined ->
        Adv = #transition_id{number="1"}, 
        db_ecc:insert_data(?DBName, ?IdTable, {content, list_to_atom(?IdTable), list_to_atom("transition_id"), <<"transition_id">>,null,null,null,null,?Author,null,null,null,null,null,transitionid_to_db(Adv)}),
        N = 1;        
    Num ->
        Adv = #transition_id{number=integer_to_list(Num+1)},
        Where = "id=" ++"transition_id",
        NewRecord = {content, list_to_atom(?IdTable), list_to_atom("transition_id"), <<"transition_id">>, null, null, null, null, ?Author, null, null, null, null, null, transitionid_to_db(Adv)},
        db_ecc:update_data(?DBName, ?IdTable, Where, NewRecord),
        N = Num+1
    end,
    release_lock(Host),
    N.    

is_lock(Host) ->
	global:set_lock({Host, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock(Host) ->	
	global:del_lock({Host, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).


get_transition(Transition_id) ->
     Ret = db_ecc:get_data(?DBName, ?TransitionTable, "id="++integer_to_list(Transition_id)),
	case is_list(Ret) of
		    false ->
			    [];
		    true ->
			     [db_to_transition(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.      

get_transition_list(Transition_id_list) ->
    get_transition_list_t(Transition_id_list,length(Transition_id_list),[]).
get_transition_list_t(_T,0,R) -> R;
get_transition_list_t([A|B],Len,Res) ->
    Ret = db_ecc:get_data(?DBName, ?TransitionTable, "id="++A),    
	case is_list(Ret) of
		    false ->
			    Tran = [];
		    true ->
			    Tran = [db_to_transition(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end,
    get_transition_list_t(B,Len-1,lists:append(Res,Tran)).

get_transition_list_fo_interface(Transition_id_list) ->
    get_transition_list_fo_interface_t(Transition_id_list,length(Transition_id_list),[]).
get_transition_list_fo_interface_t(_T,0,R) -> R;
get_transition_list_fo_interface_t([A|B],Len,Res) ->
    Ret = db_ecc:get_data(?DBName, ?TransitionTable, "id="++A),    
	case is_list(Ret) of
		    false ->
			    Tran = [];
		    true ->
			    [Temp] = [db_to_transition(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret], 
                Tran = [{Temp#transition.transition_id,Temp#transition.name,Temp#transition.screens}]                 
	end,
    get_transition_list_fo_interface_t(B,Len-1,lists:append(Res,Tran)).

%����transition,����{ok,Transition_id} | {error,Result}  
create_transition(Originating_steps,TransitionName,Destination_step,Screens) ->            
    Transition_id = integer_to_list(get_transition_id()),
    Adv = #transition{transition_id=Transition_id,originating_steps=Originating_steps,name=TransitionName,destination_step=Destination_step,screens=Screens},
    Record = {content, list_to_atom(?TransitionTable), list_to_atom(Transition_id), <<"transition">>, null, null, null, null, ?Author, null, null, null, null, null, transition_to_db(Adv)},    
    case db_ecc:insert_data(?DBName, ?TransitionTable,Record) of 
    {ok,_} -> 
        {ok,Transition_id};
     _ ->
          {error,""}
     end.


update_transition_screens(Transition_id,Screens) ->
    Where = "id="++integer_to_list(Transition_id),
    case get_transition(Transition_id) of
    [] ->    
        {error,""};   
    [Old] ->
        Adv = Old#transition{transition_id=integer_to_list(Old#transition.transition_id),screens=lists:append(Old#transition.screens,Screens)},
        Record = {content, list_to_atom(?TransitionTable), list_to_atom(integer_to_list(Transition_id)), <<"transition">>, null, null, null, null, ?Author, null, null, null, null, null, transition_to_db(Adv)},    
        case db_ecc:update_data(?DBName, ?TransitionTable,Where, Record) of 
        {ok,_} ->
             {ok,Transition_id};
        _ ->
             {error,""}
        end
    end.    


delete_transition_screens(Transition_id,Screen) ->
    Where = "id="++integer_to_list(Transition_id),
    case get_transition(Transition_id) of
    [] ->    
        {error,""};   
    [Old] ->
        Adv = Old#transition{transition_id=integer_to_list(Old#transition.transition_id),screens=lists:delete(Screen,Old#transition.screens)},
        Record = {content, list_to_atom(?TransitionTable), list_to_atom(integer_to_list(Transition_id)), <<"transition">>, null, null, null, null, ?Author, null, null, null, null, null, transition_to_db(Adv)},    
        case db_ecc:update_data(?DBName, ?TransitionTable,Where, Record) of 
        {ok,_} ->
             {ok,Transition_id};
        _ ->
             {error,""}
        end
    end.    

delete_transition(Transition_id) ->
    db_ecc:delete_data(?DBName, ?TransitionTable, "id="++ Transition_id).   

transition_to_db(Record) ->
    [
        {transition_id,number,list_to_binary(Record#transition.transition_id)}, 
        {originating_steps,string,list_to_binary(Record#transition.originating_steps)},
        {name,string,list_to_binary(Record#transition.name)},
        {destination_step,string,list_to_binary(Record#transition.destination_step)},
        {screens,term,term_to_binary(Record#transition.screens)}      
    ].

db_to_transition(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #transition{ 
        transition_id = proplists:get_value(transition_id, Data),
        originating_steps = proplists:get_value(originating_steps, Data),
        name = proplists:get_value(name, Data),
        destination_step = proplists:get_value(destination_step, Data),
        screens = proplists:get_value(screens, Data)        
    }.  

transitionid_to_db(Record) ->
    [
        {number,number,list_to_binary(Record#transition_id.number)} 
    ]. 

db_to_transitionid(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #transition_id{ 
        number = proplists:get_value(number, Data)           
    }.     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


workflowlist_to_db(Record) -> 
	[
        {name,string,list_to_binary(Record#workflowlist.name)},       
        {description,string,list_to_binary(Record#workflowlist.description)},
        {status,string,list_to_binary(Record#workflowlist.status)},
        {schemes,string,list_to_binary(Record#workflowlist.schemes)},
        {number_of_steps,number,term_to_binary(Record#workflowlist.schemes)} 
	].
     
     
db_to_workflowlist(DB) ->
    Data = [db2term(K, T, V) || {K, T, V} <- DB], 
    #workflowlist{ 
        name = proplists:get_value(name, Data), 
        description = proplists:get_value(description, Data),  
        status = proplists:get_value(status, Data),  
        schemes = proplists:get_value(schemes, Data),
        number_of_steps = proplists:get_value(number_of_steps, Data)            
    }. 
     
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
db2term(K,T,V) when T=:= term ->  {K,binary_to_term(V)};
db2term(K,_,V)->{K,binary_to_term(V)}.      



     