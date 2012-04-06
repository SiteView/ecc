%% ---
%% dbcs_machine
%%
%%---
-module(dbcs_dynamic_update).
-compile(export_all).
-define(Table,"dynamic_update").
-include("monitor.hrl").
-include("dbcs_common.hrl").

get_field(Name,Data)->
	case lists:keysearch(Name,1,Data) of
		{value,{Name,Type,Val}}->
			
			{_K,V} = dbcs_base:db2term(Name,Type,Val),
			V;
		_->
			undefined
	end.

db_to_record(Data)->
	case Data of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			#dynamic_update_sets{
				id=Id,
				mset = get_field(mset,Adv),
				subgroup_name = get_field(subgroup_name,Adv),
				group_name = get_field(group_name,Adv),
				parent_group = get_field(parent_group,Adv),
				update_frequency = get_field(update_frequency,Adv),
				exclude_ip = get_field(exclude_ip,Adv),
				title = get_field(title,Adv),
				snmp_search = get_field(snmp_search,Adv),
				database_search = get_field(database_search,Adv)
			};
		_->
			#monitor_set{}
	end.
	
record_to_db(Dyn)->
	Id = Dyn#dynamic_update_sets.id,
	Advance = [dbcs_base:term2db(mset,Dyn#dynamic_update_sets.mset),
				dbcs_base:term2db(subgroup_name,Dyn#dynamic_update_sets.subgroup_name),
				dbcs_base:term2db(group_name,Dyn#dynamic_update_sets.group_name),
				dbcs_base:term2db(parent_group,Dyn#dynamic_update_sets.parent_group),
				dbcs_base:term2db(update_frequency,Dyn#dynamic_update_sets.update_frequency),
				dbcs_base:term2db(exclude_ip,Dyn#dynamic_update_sets.exclude_ip),
				dbcs_base:term2db(title,Dyn#dynamic_update_sets.title),
				dbcs_base:term2db(snmp_search,Dyn#dynamic_update_sets.snmp_search),
				dbcs_base:term2db(database_search,Dyn#dynamic_update_sets.database_search)
				],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"dynamic_update">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.



get_dynamic_update(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=group & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_exist};
				_->
					[Group|_] = Ret,
					db_to_record(Group)
			end
	end;
get_dynamic_update(_)->{error,parameter_error}.

remove_dynamic_update(Id) when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_dynamic_update(_)->{error,parameter_error}.


create_dynamic_update(Dyn)when is_record(Dyn,dynamic_update_sets) ->
	case db_ecc:insert_data(?DBName,?Table,record_to_db(Dyn)) of
		{ok,Ret}->
			{ok,db_to_record(Ret)};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end;
create_dynamic_update(_)->{error,parameter_error}.

update_dynamic_update(Dyn) when is_record(Dyn,dynamic_update_sets)->
	Id = Dyn#dynamic_update_sets.id,
	case db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),record_to_db(Dyn)) of
		{ok,Ret}->
			{ok,db_to_record(Ret)};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end;
update_dynamic_update(_)->{error,parameter_error}.
	

get_all()->
	Ret = db_ecc:get_data(?DBName, ?Table, ""),
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_record(X) || X <- Ret] 			
	end.
	
