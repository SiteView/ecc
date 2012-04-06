%% 
%% @doc api of group operation
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_group).
-extends(api_siteview).

-export([create/2,update/1,refresh/2,find_group/1,delete/1,moveGroup/2,copyGroup/2,childs/1,info/1,get_run_info/1]).

-export([health_childs/0,health_group_id/0,health_add_default_monitors/1]).

-export([disable/4,enable/1,disable/2,enable/2]).

-export([disable_monitors/2,disable_monitors/4,enable_monitors/1,enable_monitors/2]).

-include("monitor.hrl").

%% @spec create(ParentId,GroupData)-> ({error,Reason}|{ok,Result})
%% where
%%		ParentId = atom()
%%		GroupData = [{Key,Value}]
%%		Key = atom()
%%		Value = term()
%%		Reason = id_generate_error | parent_not_exist | parameter_error | {verify_error,Err}
%%		Err = [{Key,string()}]
%% @doc create a group
%%
%%	<br>ParentId is atom like '0.1.1'</br>
%%	<br>GroupData is a key-value tuple list,it must contains group's property,name of property can get by api_monitor_template:get_template(monitor_group).</br>  
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>id_generate_error</dt><dd>can not get id of the group.</dd>
%%		<dt>parent_not_exist</dt><dd>parent group not exist </dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>{verify_error,Err}</dt><dd>verify data error, GroupData contains invalid data,Err is a list of tuple contains {key,error_string}</dd>
%%	</dl>
create('',_)->{error,parameter_error};
create(ParentId,GroupData) when is_atom(ParentId),is_list(GroupData)->
	TopId = siteview:getServerID(),
	case ParentId of
		TopId->
			create([siteview:get_current_siteview()],TopId,GroupData);
		_->
			create(find_group(ParentId),ParentId,GroupData)
	end;
create(_,_)->
	{error,parameter_error}.

create([],_,_)->{error,parent_not_exist};
create([G|_],ParentId,GroupData)->
	case dbcs_group:get_next_id(ParentId) of
		{ok,NextId}->
			NewData = case lists:keysearch(parent,1,GroupData) of
						{value,_}->
							GroupData;
						_->
							GroupData ++ [{parent,ParentId}]
					end,
			Mg = monitor_group:new(),
			case Mg:verify(NewData) of
				{ok,_}->
					Mg:delete(),
					case dbcs_group:create_group([{id,NextId}] ++ NewData) of
						{ok,RetData}->
							Obj = siteview:create_object(RetData),
							G:add_child(Obj),
							TopId = siteview:getServerID(),
							% if
							%	TopId == ParentId->
							%		pass;
							%	true ->
							%		Obj:set_parent(G)
							% end,
							Obj:set_parent(G),
							Obj:startGroup(),
							{ok,Obj:get_properties()};
						Err->
							Err
					end;
				{error,Err}->
					Mg:delete(),
					{error,{verify_error,Err}};
				Err2->
					Mg:delete(),
					{error,Err2}
			end;
		_->
			{error,id_generate_error}
	end.

%% @spec update(GroupData)-> ({ok,Result}|{error,Reason})
%% where 
%%	GroupData = [{Key,Value}]
%%	Key =atom()
%%	Value = term()
%%  Result = atom()
%%  Reason = groupdata_error | parameter_error | {verify_error,Err}
%%	Err = [{Key,string()}]
%% @doc update a group,GroupData is a key-value tuple list
%% 
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>groupdata_error</dt><dd>GroupData is invalid,maybe not contain a id</dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>{verify_error,Err}</dt><dd>data verify error, GroupData contains invalid data,Err is a list of tuple contains {key,error_string}</dd>
%%	</dl>
update(GroupData) when is_list(GroupData)->
	case lists:keysearch(id,1,GroupData) of
		{value,{id,Id}}->
			Mg = monitor_group:new(),
			case Mg:verify(GroupData) of
				{ok,_}->
					Mg:delete(),
					dbcs_group:update_group(GroupData),
					Group = find_group(Id),
					%[X:add_properties(GroupData)||X<-Group],
					[X:reload_self()||X<-Group],
					{ok,update_group_ok};
				{error,Err}->
					Mg:delete(),
					{error,{verify_error,Err}};
				Err2->
					Mg:delete(),
					{error,Err2}
			end;
		_->
			{error,groupdata_error}
	end;
update(_)->
	{error,parameter_error}.


%% @spec delete(GroupId)->({error,Reason} | {ok,Result})
%% where
%%	GroupId = atom()
%%	Reason = health_group_can_not_delete | parameter_error | group_have_childs
%%	Result = atom()
%% @doc delete a group by groupid
%%
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>health_group_can_not_delete</dt><dd>can not delete health group</dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>group_have_childs</dt><dd>the group have childs,can not delete a not empty group</dd>
%%	</dl>
delete(GroupId) when is_atom(GroupId)->
	IdStr = atom_to_list(GroupId),
	case re:run(IdStr,"^[0-9]+[\.]health$") of
		{match,_}->
			{error,health_group_can_not_delete};
		_->
			delete(GroupId,find_group(GroupId))
	end;
delete(_)->{error,parameter_error}.

delete(_,[])->{error,group_not_found};
delete(GroupId,[Group|_])->
	Childs = Group:get_childs(),
	if
		length(Childs) > 0 ->
			{error,group_have_childs};
		true->
			case Group:get_parent() of
				{ok,{parent,Parent}} ->
					Ret = dbcs_group:remove_group(GroupId),
					Parent:remove_child(Group),
					Group:stopGroup(),
					Group:delete(),
					Ret;
				_->
					Ret = dbcs_group:remove_group(GroupId),
					SV=siteview:get_current_siteview(),
					SV:remove_child(Group),
					Group:stopGroup(),
					Group:delete(),
					Ret
			end
	end.

%% @spec find_group(GID)->[{Key,Value}]
%% where
%%	GID = atom()
%%	Key = atom()
%%	Value = term()
%% @doc find a group by group id,return data is key-value tuple list
%%
find_group(GID)->
	% SV = siteview:get_current_siteview(),
	% {ok,{childs,Childs}} = SV:get_attribute(childs),
	api_siteview:find_object(GID).

%% @spec refresh(GId,Flag)->(true | false)
%% where
%%	Gid = atom()
%%	Flag = (true | false)
%% @doc refresh a group
%%
refresh(GId,Flag) when is_atom(GId)->
	refreshObj(find_group(GId),Flag);
refresh(_,_)->
	false.

refreshObj([MG|_],Flag)->
	Monitors = MG:getGroupsMonitors(Flag),
	%%io:format("refreshObj:~p~n",[Monitors]),
	[X:runUpdate(X,false,false)||X<-Monitors],
	true;
refreshObj(_,_)->
	false.

%% @spec moveGroup(GId,ParentId)->({ok,Result} | {error,Reason})
%% where
%%	GId = atom()
%%	ParentId = atom()
%%	Result = [{key,value}]
%%	Reason = health_group_can_not_move | parameter_error | group_not_found | parent_not_found
%% @doc move a group to another group
%%  
%%	<br>Result is GroupData,a list of key-value tuple</br>
%%
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>health_group_can_not_move</dt><dd>can not move health group</dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>parent_not_found</dt><dd>can not find parent group.</dd>
%%		<dt>group_not_found</dt><dd>can not find this group.</dd>
%%	</dl>
moveGroup(GId,ParentId) when is_atom(GId),is_atom(ParentId)->
	IdStr = atom_to_list(GId),
	case re:run(IdStr,"^[0-9]+[\.]health$") of
		{match,_}->
			{error,health_group_can_not_move};
		_->
			moveGroup(GId,ParentId,find_group(GId))
	end;
moveGroup(_,_)->{error,parameter_error}.

moveGroup(_,_,[])->{error,group_not_found};
moveGroup(GId,ParentId,[Group|_])->
	moveGroup(GId,ParentId,Group,find_group(ParentId)).
		
moveGroup(GId,ParentId,Group,[])->
	case siteview:getServerID() of
		ParentId->
			Group:set_property(parent, ParentId),
			Group:save(),
			SV = siteview:get_current_siteview(),
			SV:signalReload(),
			case dbcs_group:get_group(GId) of
				{error,Err}->
					{error,Err};
				Ret->
					{ok,Ret}
			end;
		_->
			{error,parent_not_found}
	end;
moveGroup(GId,ParentId,Group,[NewParent|_])->
	%%{ok,{parent,Parent}} = Group:get_owner(),
	case Group:get_owner() of
		{ok,{parent,Parent}}->
			% io:format("moveGroup....~n"),
			Parent:remove_child(Group),
			Group:set_owner(NewParent),
			NewParent:add_child(Group),
			Group:set_property(parent, ParentId),
			Group:save();
			% Parent:reload(),
			% NewParent:reload();
		_->
			case Group:get_property(parent) of
				{ok,{parent,PaID}}->
					case siteview:getServerID() of
						PaID->
							SV = siteview:get_current_siteview(),
							SV:remove_child(Group),
							Group:set_owner(NewParent),
							NewParent:add_child(Group);
						_->
							Group:set_owner(NewParent),
							NewParent:add_child(Group)
					end,
					Group:set_property(parent, ParentId),
					Group:save();
				_->
					% Group:set_property(parent, ParentId),
					% Group:save(),
					% NewParent:reload()
					error
			end
	end,
	case dbcs_group:get_group(GId) of
		{error,Err}->
			{error,Err};
		Ret->
			{ok,Ret}
	end.
	
%% @spec copyGroup(GId,ParentId)->({ok,Result} | {error, Reason})
%% where
%%	GId = atom()
%%	ParentId = atom()
%%	Reason = parameter_error | group_not_found | create_group_error | parent_not_found
%%	Result = [{key,value}]
%%	
%% @doc copy group to another group
%%
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>create_group_error</dt><dd>create group data error</dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>parent_not_found</dt><dd>can not find parent group.</dd>
%%		<dt>group_not_found</dt><dd>can not find this group.</dd>
%%	</dl>
copyGroup(GId,ParentId)when is_atom(GId),is_atom(ParentId)->
	copyGroup(GId,ParentId,find_group(GId));
copyGroup(_,_)->{error,parameter_error}.

copyGroup(_,_,[])->{error,group_not_found};
copyGroup(GId,ParentId,[Group|_])->
	copyGroup(GId,ParentId,Group,find_group(ParentId)).
	

copyGroup(GId,ParentId,Group,[])->
	case siteview:getServerID() of
		ParentId->
			Props = Group:get_properties(),
			N = lists:keydelete(parent, 1, Props),
			NewProps = lists:keydelete(id, 1, N),
			case create(ParentId,NewProps) of
				{ok,R}->
					NewParentId = proplists:get_value(id,R),
					Childs = Group:get_childs(),
					copyChilds(Childs,NewParentId),
					{ok,R};
				_->
					{error,create_group_error}
			end;
		_->
			{error,parent_not_found}
	end;
copyGroup(GId,ParentId,Group,[NewParent|_])->
	case Group:get_owner() of
		{ok,{parent,Parent}}->
			Props = Group:get_properties(),
			N = lists:keydelete(parent, 1, Props),
			NewProps = lists:keydelete(id, 1, N),
			case create(ParentId,NewProps) of
				{ok,R}->
					NewParentId = proplists:get_value(id,R),
					Childs = Group:get_childs(),
					copyChilds(Childs,NewParentId);
				_->
					error
			end;
		_->
			case Group:get_property(parent) of
				{ok,{parent,PaID}}->
					Props = Group:get_properties(),
					N = lists:keydelete(parent, 1, Props),
					NewProps = lists:keydelete(id, 1, N),
					case create(ParentId,NewProps) of
						{ok,R}->
							NewParentId = proplists:get_value(id,R),
							Childs = Group:get_childs(),
							copyChilds(Childs,NewParentId),
							{ok,R};
						_->
							{error,create_group_error}
					end;
				_->
					Props = Group:get_properties(),
					N = lists:keydelete(parent, 1, Props),
					NewProps = lists:keydelete(id, 1, N),
					case create(ParentId,NewProps) of
						{ok,R}->
							copyChilds(Group:get_childs(),ParentId),
							{ok,R};
						_->
							{error,create_group_error}
					end
			end
	end.

copyChilds(Childs,ParentId)->
	copyChilds(Childs,ParentId,find_group(ParentId)).
	
copyChilds(_,_,[])->{error,not_found_parent};
copyChilds([],_,_)->{ok,copy_childs_ok};
copyChilds([C|T],ParentId,Parent)->
	{ok,{_,CId}} = C:get_property(id),
	case C:get_property(?CLASS) of
		{ok,{_,group}}->
			case copyGroup(CId,ParentId) of
				{ok,Ret}->
					copyChilds(T,ParentId),
					{ok,Ret};
				{error,Err}->
					{error,Err};
				Else->
					Else
			end;
		_->
			case api_monitor:copy(CId,ParentId) of
				{ok,Ret}->
					copyChilds(T,ParentId),
					{ok,Ret};
				{error,Err}->
					{error,Err};
				Else->
					Else
			end
	end.


%% @spec childs(GId)->(Result | {error,Reason})
%% where
%%	GId = atom()
%%  Result = [{key,value}]
%%	Reason = parameter_error | group_not_found | is_not_group
%% @doc get the childs of a group
%%  
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>is_not_group</dt><dd>GId is not a group id</dd>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>group_not_found</dt><dd>can not find this group.</dd>
%%	</dl>
childs(GId) when is_atom(GId)->
	Nodes = api_siteview:get_nodes(),
	case lists:keysearch(GId,1,Nodes) of
		{value,_}->
			api_siteview:get_childs(GId);
		_->
			childs(GId,find_group(GId))
	end;
childs(_)->{error,parameter_error}.

childs(_,[])->{error,group_not_found};
childs(_,[G|_])->
	case G:get_property(class) of
		{ok,{_,group}}->
			Childs = G:get_childs(),
			[X:get_properties() ++ X:get_run_info() || X<- Childs];
		_->
			{error,is_not_group}
	end.

%% @spec health_childs()->[Monitor]
%% where
%%	Monitor = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%% @doc get childs of health group,return monitor data list,each item is a key-value tuple list
%%
health_childs()->
	Id = list_to_atom(atom_to_list(siteview:getServerID())++".health"),
	childs(Id,find_group(Id)).

%% @spec health_group_id()->atom()
%% @doc get health group id
%%
health_group_id()->
	list_to_atom(atom_to_list(siteview:getServerID())++".health").

%% @spec info(GId)->(Result | {error,Reason})
%% where
%%	GId = atom()
%%	Result = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%%	Reason = parameter_error | group_not_found
%% @doc get group's information
%%  
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>group_not_found</dt><dd>can not find this group.</dd>
%%	</dl>
info(GId) when is_atom(GId)->
	info(GId,find_group(GId));
info(_)->{error,parameter_error}.
	
info(_,[])->{error,group_not_found};
info(_,[G|_])->
	G:get_properties().

%% @spec get_run_info(GId)->(Result | {error,Reason})
%% where
%%	GId = atom()
%%	Result = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%%	Reason = parameter_error | group_not_found
%% @doc get group's runtime information
%%  
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>parameter_error</dt><dd>input parameter is error</dd>
%%		<dt>group_not_found</dt><dd>can not find this group.</dd>
%%	</dl>
get_run_info(GId)when is_atom(GId)->
	get_run_info(GId,find_group(GId));
get_run_info(_)->{error,parameter_error}.


get_run_info(_,[])->{error,group_not_found};
get_run_info(GId,[G|_])->
	[{id,GId}] ++ G:get_run_info().



%% @spec disable(GroupId,DisableDesc)->({error,Reason} | {ok,Result})
%% where
%%	GroupId = atom()
%%	DisableDesc = string()
%% 	Reason = parameter_error
%% @doc disable a group
disable(GroupId,DisableDesc) when is_atom(GroupId)->
	disable(GroupId,DisableDesc,api_siteview:find_object(GroupId));
disable(_,_)->{error,parameter_error}.

%% @spec disable(GroupId,DisableDesc,StartTime,EndTime)->({error,Reason} | {ok,Result})
%% where
%%		GroupId = atom()
%%		DisableDesc = string()
%%		StartTime = tuple()
%%		EndTime = tuple()
%%		Result = atom()
%%		Reason = parameter_error | not_found_group 
%% @doc disable a group within a periodic time
%%
%% <br>DisableDesc is the descripition of disable action,the format of StartTime and EndTime is {{Y,M,D},{HH,MM,SS}} </br>
%%	<br>(example:{{2009,11,9},{12,11,23}})</br>
disable(GroupId,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}})->
	disable(GroupId,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}},api_siteview:find_object(GroupId)).

disable(_,_,[])->{error,not_found_group};
disable(_,DisableDesc,[Group|_])->
	Group:set_property(?DISABLED,true),
	Group:set_property(?DISABLED_DESCRIPTION,DisableDesc),
	case Group:save() of
		{ok,_}->
			% Group:reload(),
			{ok,disable_group_ok};
		Err->
			Err
	end;
disable(_,_,_)->{error,parameter_error}.

disable(_,_,_,_,[])->{error,not_found_group};
disable(_,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}},[Group|_])->
	Group:set_property(?TIMED_DISABLE,{{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}}}),
	Group:set_property(?DISABLED_DESCRIPTION,DisableDesc),
	case Group:save() of
		{ok,_}->
			% Group:reload(),
			{ok,disable_group_ok};
		Err->
			Err
	end;
disable(_,_,_,_,_)->{error,parameter_error}.


%% @spec enable(GroupId)->{error,Reason} | {ok,Result}
%% where
%%	GroupId = atom()
%%	Reason = parameter_error | not_found_group
%%	Result = atom()
%% @doc enable a group
%%
enable(GroupId) when is_atom(GroupId)->
	enable(GroupId,api_siteview:find_object(GroupId),false);
enable(_) -> {error,parameter_error}.

%% @spec enable(MonitorId,Flag)->{error,Reason} | {ok,Result}
%% where
%%	MonitorId = atom()
%%	Flag=(ture|false)
%%	Reason = atom()
%%	Result = atom()
%% @doc enable a group ,when Flag is true only enable items that are temporarily disabled 
%%	
enable(GroupId,Flag) when is_atom(GroupId)->
	enable(GroupId,api_siteview:find_object(GroupId),Flag);
enable(_,_) -> {error,parameter_error}.

enable(_,[],_)->{error,not_found_group};
enable(_,[Group|_],false)->
	Group:set_property(?DISABLED_DESCRIPTION,""),
	Group:set_property(?TIMED_DISABLE,undefined),
	Group:set_property(?DISABLED,false),
	case Group:save() of
		{ok,_}->
			% Group:reload(),
			{ok,enable_group_ok};
		Err->
			Err
	end;
enable(_,[Group|_],true)->
	Group:set_property(?TIMED_DISABLE,undefined),
	case Group:save() of
		{ok,_}->
			% Group:reload(),
			{ok,enable_group_ok};
		Err->
			Err
	end.

%% @spec disable_monitors(GroupId,DisableDesc)-> ({ok,Result}| {error,Reason})
%% where
%%	GroupId = atom()
%%	DisableDesc = string()
%%	Result = atom()
%%	Reason = parameter_error 
%% @doc disable group's monitors
%% DisableDesc is the descripition of disable
disable_monitors(GroupId,DisableDesc) when is_atom(GroupId)->
	F = fun(X)->
		X:set_property(?DISABLED,true),
		X:set_property(?DISABLED_DESCRIPTION,DisableDesc)
		end,
	update_monitors(GroupId,F,find_group(GroupId),[]);
disable_monitors(_,_)->{error,parameter_error}.


update_monitors(_,_,[],Err)->
	case Err of
		[]->
			{ok,update_monitors};
		[FErr|_]->
			{error,FErr}
	end;
update_monitors(Id,F,[Obj|T],Err)->
	case Obj:get_property(?CLASS) of
		{ok,{?CLASS,group}}->
			Obj:set_attribute(?CATEGORY,?NO_DATA),
			case update_monitors(Id,F,Obj:get_childs(),Err) of
				{ok,_}->
					update_monitors(Id,F,T,Err);
				{error,Err2}->
					update_monitors(Id,F,T,Err ++ [Err2])
			end;
		_->
			F(Obj),
			case Obj:save_monitor() of
				{ok,_}->
					spawn(fun()->Obj:runUpdate(Obj,false,false) end),
					update_monitors(Id,F,T,Err);
				{error,NewErr}->
					update_monitors(Id,F,T,Err ++ [NewErr])
			end
	end.

%% @spec disable_monitors(GroupId,DisableDesc,StartTime,EndTime)->({error,Reason} | {ok,Result})
%% where
%%		GroupId = atom()
%%		DisableDesc = string()
%%		StartTime = tuple()
%%		EndTime = tuple()
%% @doc disable group's monitors within a periodic time,the format of StartTime and EndTime is {{Y,M,D},{HH,MM,SS}} (example:{{2009,11,9},{12,11,23}})
disable_monitors(GroupId,DisableDesc,{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}})->
	F = fun(X)->
		X:set_property(?DISABLED_DESCRIPTION,DisableDesc),
		X:set_property(?TIMED_DISABLE,{{{Y,M,D},{HH,MM,SS}},{{Y2,M2,D2},{HH2,MM2,SS2}}})
		end,
	update_monitors(GroupId,F,find_group(GroupId),[]);
disable_monitors(_,_,_,_)->{error,parameter_error}.

%% @spec enable_monitors(GroupId) -> (ok|{error,Reason})
%% where
%%	GroupId = atom()
%%	Reason = atom()
%% @doc enable monitors in a group
enable_monitors(GroupId) when is_atom(GroupId)->
	F=fun(X)->
		X:set_property(?DISABLED_DESCRIPTION,""),
		X:set_property(?TIMED_DISABLE,undefined),
		X:set_property(?DISABLED,false)
	end,
	update_monitors(GroupId,F,find_group(GroupId),[]);
enable_monitors(_)->{error,parameter_error}.


%% @spec enable_monitors(GroupId,Flag) -> (ok|{error,Reason})
%% where
%%	GroupId = atom()
%%	Flag = bool()
%%	Reason = parameter_error
%% @doc enable monitors in a group
enable_monitors(GroupId,Flag) when is_atom(GroupId)->
	F=fun(X)->
		case Flag of
			true->
				X:set_property(?TIMED_DISABLE,undefined);
			_->
				X:set_property(?DISABLED_DESCRIPTION,""),
				X:set_property(?TIMED_DISABLE,undefined),
				X:set_property(?DISABLED,false)
		end
	end,
	update_monitors(GroupId,F,find_group(GroupId),[]);
enable_monitors(_,_)->{error,parameter_error}.


%% @spec health_add_default_monitors(Params)-> (ok|{error,Reason})
%% where
%%	Params = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%% @doc add default monitors to health group
%%
health_add_default_monitors(Params)when is_list(Params)->
	health:update(Params,"");
health_add_default_monitors(_)->{error,parameter_error}.
	
	