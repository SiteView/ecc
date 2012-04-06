%% 
%% @doc api of machine label operation
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Ning Cai <ning.cai@dragonflow.com>

-module(api_machineLabel).
-include("monitor.hrl").
-include("remoteMachine.hrl").

-export([get_all_label/0, get_label_type/1, get_LabelById/1, create_label/2, update_label/1, remove_label/1, 
            remove_AllLabel/0, getUserDefineTag/4,get_root_label/0,get_children_label/1]).

%% ------------- label operation------------

%% @spec get_all_label()-> (Result
%% Result = [#machine_label{}|T]
%% @doc get all label of machine.
%% release
get_all_label() ->
    remoteMachineTag:get_all_label().

%% @spec get_root_label()-> (Result)
%% Result = [#machine_label{}|T]
%% @doc get all label which is root.
%% release
get_root_label()->
    remoteMachineTag:get_root_label().

%% @spec get_label_type(Type)-> (Result)
%% Type = string()
%% Result = [#machine_label{}|T]
%% @doc get label of machine by type.
%% release
get_label_type(Type) ->
    remoteMachineTag:get_label_type(Type).
    
%% @spec getUserDefineTag(Index, Count, Sort, SortType)-> (Result)
%% Index = integer()
%% Count = integer()
%% Sort = integer()
%% SortType = integer()
%% Result = [#machine_label{}|T]
%% @doc get user defined label
%% release
getUserDefineTag(Index, Count, Sort, SortType) ->
    remoteMachineTag:getUserDefineTag(Index, Count, Sort, SortType).


%%  get_Unix_label()-> (Result)
%% Result = [#machine_label{}|T]
%%  get label of unix machine.
%%
%%get_Unix_label() ->
%%    dbcs_machine:get_Unix_label().


%% get_NT_label()-> (Result)
%% Result = [#machine_label{}|T]
%%  get label of windowsNt machine.
%%
%%get_NT_label() ->
%%    dbcs_machine:get_NT_label().


%% @spec get_LabelById(Id)-> (Result)
%% Id = string()
%% Result = [#machine_label{}|T]
%% @doc get label by id
%% release
get_LabelById(Id) ->
    remoteMachineTag:getTagById(Id).
    

%% @spec create_label(Label)-> ({error,Reason}|{ok,Id})
%% where
%%		Id = atom()
%%		Reason = atom() | string()
%%		Machine=#machine{}
%% @doc insert a new machine to database.
%% release
create_label(Parentid,Label)->
    remoteLabelTree:create_Label(Parentid,Label).
    
    
%% @spec update_label(Label)-> ({error,Reason}|{ok,Id})
%% where
%%		Label = #machine_label{}
%%		Reason = atom() | string()
%%		Id = string()
%% @doc update a new machine to database
%% release
update_label(Label) ->
    io:format("~n Label: ~p",[Label]),
	remoteMachineTag:update_label(Label#machine_label.id,Label).
    
    
%% @spec remove_label(Id)-> ({error,Reason}|{ok,Result})
%% Id = string()
%% Reason = string()
%% Result= string()
%% @doc remove label and set machine's property label for empty
%% release
remove_label(Id) ->
    remoteLabelTree:removeLabelNode(Id).
 

%% @spec remove_AllLabel()-> ({error,Reason}|{ok,Result})
%% Reason = string()
%% Result= string()
%% @doc remove all labels
%% release
remove_AllLabel() ->
    remoteMachineTag:remove_AllLabel().

%% @spec get_children_label(Lid)-> ({error,Reason}|{ok,Result})
%% Reason = string()
%% Result= string()
%% @doc remove all labels
%% release
get_children_label(Lid)->
    remoteLabelTree:get_children_label(Lid).
%% %%%%%%%%%%%%%%%%%%% Utility methods%%%%%%%%%%%%%%%%%%%

    



