%% 
%% @doc based content store的machine database module.
%%

-module(dbcs_ci).
-define(Table,"machine").
-include("dbcs_common.hrl").

%%-compile(export_all).
-export([create_machine/1,get_machine/1,update_machine/1,get_all/0,remove_machine/1,get_machine_match/1,get_machine_rule/1]).

-define(APP,app_).

base_property(id)->
	true;
base_property(?APP)->
	true;
base_property(_)->
	false.

machine_to_db(Machine)->
	{value,{id,Id}} = lists:keysearch(id,1,Machine),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<-Machine,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"machine">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_machine(MonitorData)->
	case MonitorData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP,App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

create_machine(Machine) when is_list(Machine) ->
	case db_ecc:insert_data(?DBName,?Table,machine_to_db(Machine)) of
		{ok,Ret} ->
			 {ok,db_to_machine(Ret)};
		Err ->
			Err
	end;
create_machine(_)-> {error,parameter_error}.

%% @doc Query machine information
%% 
%% @spec get_machine(Id)-> ({error,Reason} | Machine)
%%	where
%%			Id		= (atom() | list())
%%			Machine = list()
get_machine(Id) when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=machine & id="++atom_to_list(Id)),
	case Ret of
		{error,Reason}->
			{error,Reason};
		_->
			case length(Ret) of
				0->
					{error,not_found_machine};
				_->
					[Machine|_] = Ret,
					db_to_machine(Machine)
			end
	end;
get_machine(Id) when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=machine & id="++Id),
	case Ret of
		{error,Reason}->
			{error,Reason};
		_->
			case length(Ret) of
				0->
					{error,not_found_machine};
				_->
					[Machine|_] = Ret,
					db_to_machine(Machine)
			end
	end;
get_machine(_)->{error,parameter_error}.

%% @doc Update machine information
%% @spec update_machine(Machine)->({error,Reason}|{ok,Result})
%% where
%%			Machine = list()
%%			Result  = tuple()
update_machine(Machine) when is_list(Machine)->
	case lists:keysearch(id,1,Machine) of
		{value,{id,Id}}->
			Old = get_machine(Id),
			ND = lists:ukeymerge(1,lists:ukeysort(1,Machine),lists:ukeysort(1,Old)),
			db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),machine_to_db(ND));
		_->
			{error,not_found_id}
	end;
update_machine(_)-> {error,parameter_error}.
	

%% @doc Get all the information machines
%% @spec get_all()->Monitors
%% where
%%		Monitors = list()
get_all()->
	Apps = 
	case dbcs_base:get_app() of
		undefined->
			app:all();
		App->
			[App]
	end,
	F = fun(X)->
			dbcs_base:set_app(X,true),
			db_ecc:get_data(?DBName,?Table,"")
		end,
	Ret = lists:flatten(lists:map(F,Apps)),
	case Ret of
		{error,_}->
			[];
		_->
			[db_to_machine(X)||X <- Ret]
	end.

%% @doc Deleting a machine
%% @spec remove_machine(Id)->({ok,Result}|{error,Reason})
%% where
%%	Id = (atom() | string())
remove_machine(Id) when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_machine(Id) when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_machine(_)->{error,parameter_error}.

%% @doc Search machine combined conditions
%% @spec get_machine_match(Where)-> (Monitors | {error,Reason})
%% where
%%		Where = string()
%%		Monitors = list()
get_machine_match(Where) when is_list(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	case Ret of
		{error,_}->
			[];
		_->
			[db_to_machine(X)||X <- Ret]
	end;
get_machine_match(_)->{error,where_should_be_list}.


%% @doc Rules to obtain the machine
%% @spec get_machine_rule(Id)->({error,Reason} | Rules)
%% where
%%		Id		= (atom() | list())
%%		Rules	= list()
get_machine_rule(Id)when is_atom(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ atom_to_list(Id)++"'");
get_machine_rule(Id)when is_list(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ Id ++"'");
get_machine_rule(_)-> {error,parameter_error}.