-module(workflow_state_machine).
-include("monitor.hrl").
-compile(export_all).

-include("itsm_structure.hrl").

%获取workflow列表
get_all_workflow() ->
    case dbcs_workflow:get_all_workflow() of
    [] ->
        [];
    Workflow ->
        get_all_workflow_t(Workflow,length(Workflow),[]) 
    end.
get_all_workflow_t(_,0,E) -> E;
get_all_workflow_t([A|B],Len,Res) ->
    get_all_workflow_t(B,Len-1,[{A#workflow.id,A#workflow.name}|Res]). 

%当Transition_Id为空时返回CurrentStep对应的Transitions,当有Transition_Id时，返回下个状态和状态对应的Transitions
process_status_request(WorkflowId,CurrentStepId,Transition_Id) ->
    case Transition_Id of
    "" ->
        case dbcs_workflow:get_workflow(WorkflowId) of
        [] ->
            {error,"not exist"};
        [Workf] ->
            Steps = Workf#workflow.steps,
            case lists:member(list_to_integer(CurrentStepId),Steps) of
            true ->
                case dbcs_workflow:get_step(WorkflowId,CurrentStepId) of
                {error,_}->
                    {error,""};
                [Step] -> 
                    TransitionsId = Step#step.transitions,
                    dbcs_workflow:get_transition_list_fo_interface(TransitionsId)  %[{TransitionsId,TransitionsName,Screen}]                    
                end;   
            _ ->
                {error,"no_step"} 
            end             
        end;
    _ ->
        case dbcs_workflow:get_transition(Transition_Id) of
        [] ->
            {error,"Transition not exist"};
        [Transition]->
            Destination_step_Id  = Transition#transition.destination_step,
            [DesStep] = dbcs_workflow:get_step(WorkflowId,Destination_step_Id),
            case process_status_request(WorkflowId,Destination_step_Id,"") of   
            {error,_} ->
                {error,""};
            TList ->
                {{Destination_step_Id,DesStep#step.name},TList} 
            end                 
        end         
    end.   

get_all_step(WorkflowId) ->
    case dbcs_workflow:get_workflow(WorkflowId) of
    [] ->
        {error,""};
    [Workflow] ->
        to_list(Workflow#workflow.steps) 
    end.    

to_list(List) ->
    to_list_t(List,length(List),[]). 
to_list_t(_,0,L) -> L;
to_list_t([A|B],Len,Li) ->
    to_list_t(B,Len-1,[integer_to_list(A)|Li]). 

get_line_status(WorkflowId,Destination_step_Id) ->
    dbcs_workflow:get_line_status(WorkflowId,Destination_step_Id). 
    
    
get_step_name(WorkflowId,Destination_step_Id) ->
    dbcs_workflow:get_step_name(WorkflowId,Destination_step_Id).     

bind_workflow(WorkflowId) ->
    dbcs_workflow:bind_workflow(WorkflowId).    
  
release_workflow(WorkflowId) ->
    dbcs_workflow:release_workflow(WorkflowId). 







