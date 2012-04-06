%% ---
%% api_device
%%
%%---
-module(api_device).
-extends(api_siteview).
-compile(export_all).


%% create(ParentId,DeviceData)->({ok,Result}|{error,Reason})
%%
%%
create('',_)->{error,error_parameter};
create(ParentId,DeviceData) when is_atom(ParentId),is_list(DeviceData)->
	create(api_siteview:find_object(ParentId),ParentId,DeviceData);
create(_,_)->
	{error,parameter_error}.

create([],_,_)->{error,parent_not_exist};
create([G|_],ParentId,DeviceData)->
	case dbcs_group:get_next_id(ParentId) of
		{ok,NextId}->
			NewData = case lists:keysearch(parent,1,DeviceData) of
						{value,_}->
							DeviceData;
						_->
							DeviceData ++ [{parent,ParentId}]
					end,
			dbcs_device:create_device([{id,NextId}] ++ NewData),
			%%Obj = siteview:create_object([{id,NextId}] ++ NewData),
			%%Obj:set_parent(G),
			%%Obj:startDevice(),
			G:reload(),
			{ok,device_create_ok};
		_->
			{error,id_generate_error}
	end.


%% update(DeviceData)->({ok,Result}|{error,Reason})
%%
%%
update(DeviceData) when is_list(DeviceData)->
	case lists:keysearch(id,1,DeviceData) of
		{value,{id,Id}}->
			dbcs_device:update_device(DeviceData),
			Device = api_siteview:find_object(Id),
			[X:reload()||X<-Device],
			{ok,update_device_ok};
		_->
			{error,devicedata_error}
	end;
update(_)->
	{error,parameter_error}.


%% delete(DeviceId)->({error,Reason} | {ok,Result})
%%
%%
delete(DeviceId) when is_atom(DeviceId)->
	delete(DeviceId,api_siteview:find_object(DeviceId));
delete(_)->{error,parameter_error}.

delete(_,[])->{error,device_not_found};
delete(DeviceId,[Device|_])->
	Childs = Device:get_childs(),
	if
		length(Childs) > 0 ->
			{error,group_have_childs};
		true->
			{ok,{parent,Parent}} = Device:get_parent(),
			Ret = dbcs_device:remove_device(DeviceId),
			Parent:reload(),
			Ret
	end.

%%refresh(DId,Flag)->true | false
%%
%%
refresh(DId,Flag) when is_atom(DId)->
	refreshObj(api_siteview:find_object(DId),Flag);
refresh(_,_)->
	false.

refreshObj([MG|_],Flag)->
	Monitors = MG:getGroupsMonitors(Flag),
	[X:runUpdate(X,false)||X<-Monitors],
	true;
refreshObj(_,_)->
	false.

%% move(DId,ParentId)->({ok,Result} | {error,Reason})
%%
%%  
move(DId,ParentId) when is_atom(DId),is_atom(ParentId)->
	move(DId,ParentId,api_siteview:find_object(DId));
move(_,_)->{error,parameter_error}.

move(_,_,[])->{error,group_not_found};
move(DId,ParentId,[Device|_])->
	move(DId,ParentId,Device,api_siteview:find_object(ParentId)).
		
move(_,_,_,[])->{error,parent_not_found};
move(DId,ParentId,Device,[NewParent|_])->
	{ok,{parent,Parent}} = Device:get_owner(),
	Device:set_property(parent, ParentId),
	Device:save_monitor(),
	Parent:reload(),
	NewParent:reload(),
	case dbcs_device:get_device(DId) of
		{ok,Ret}->
			{ok,Ret};
		{error,Err}->
			{error,Err}
	end.

%% childs(DId)->(Result | {error,Reason})
%%
%%  
childs(DId) when is_atom(DId)->
	childs(DId,api_siteview:find_object(DId));
childs(_)->{error,parameter_error}.

childs(_,[])->{error,device_not_found};
childs(_,[G|_])->
	Childs = G:get_childs(),
	[X:get_properties() || X<- Childs].


%% get(DId)->(Result | {error,Reason})
%%
%%  
info(DId) when is_atom(DId)->
	info(DId,api_siteview:find_object(DId));
info(_)->{error,parameter_error}.
	
info(_,[])->{error,group_not_found};
info(_,[G|_])->
	G:get_properties().


get_run_info(DId)when is_atom(DId)->
	get_run_info(DId,api_siteview:find_object(DId));
get_run_info(_)->{error,parameter_error}.


get_run_info(_,[])->{error,group_not_found};
get_run_info(DId,[G|_])->
	[{id,DId}] ++ G:get_run_info().
	
get_device_type()->
	file:consult("conf/device_type.conf").