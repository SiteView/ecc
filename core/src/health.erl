%% @doc health
%%
%%
-module(health).
-compile(export_all).

getTemplateList()->
	case platform:platformName() of
		'nt'->
			file:consult("templates.health/WindowsHealth");
		_->
			file:consult("templates.health/UnixHealth")
	end.

update(Params,User)->
	case proplists:get_value(groupid,Params) of
		undefined->
			{error,not_found_group_id};
		GroupId->
			case getTemplateList() of
				{ok,Tl}->
					update_item(Tl,GroupId),
					ok;
				Err->
					Err
			end
	end.

update_item(Data,GroupId)->
	update_item(Data,GroupId,siteview:get_object(GroupId)).

update_item([],_,[])->{error,not_found_health_group};
update_item([],_,_)->ok;
update_item([{Key,Data}|T],GroupId,[G|_])->
	Nd = Data ++[{class,Key}],
	case dbcs_group:get_next_id(GroupId) of
		{ok,NextId}->
			case dbcs_monitor:create_monitor([{id,NextId},{parent,GroupId}] ++ Nd) of
				{error,_}->
					update_item(T,GroupId,[G]);
				_->
					case siteview:create_object_by_id(NextId) of
						{error,Err}->
							error;
						Obj->
							G:add_child(Obj),
							Obj:set_parent(G),
							Obj:startMonitor(Obj)
					end,
					update_item(T,GroupId,[G])
			end;
		_->
			{error,id_generate_error}
	end.
