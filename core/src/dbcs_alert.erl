%% 
%% @doc Based on the alert database module content store.
%%
%%
-module(dbcs_alert).
%%-compile(export_all).
-export([create_alert/1,get_alert/1,update_alert/1,get_alert_match/1,remove_alert/1,get_alert_rule/1,get_all/0]).
-define(Table,"alert").

-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(_)->
	false.

alert_to_db(Alert)->
	{value,{id,Id}} = lists:keysearch(id,1,Alert),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Alert,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"alert">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_alert(AlertData)->
	case AlertData of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

%% @doc Create a new alarm
%% @spec create_alert(alert)->({error,Reason} | {ok,Result})
%% where
%%		alert = list()
%%		Result = tuple()
create_alert(Alert)when is_list(Alert) ->
	case db_ecc:insert_data(?DBName,?Table,alert_to_db(Alert)) of
		{ok,Ret}->
			{ok,db_to_alert(Ret)};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end;
create_alert(_)->{error,parameter_error}.


%% @doc Query a data alarm
%% @spec get_alert(Id)->({error,Reason} | alert)
%% where
%%		Id = (atom() | string())
%%		alert = list()
get_alert(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=alert & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_alert};
				_->
					[Alert|_] = Ret,
					db_to_alert(Alert)
			end
	end;
get_alert(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=alert & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_alert};
				_->
					[Alert|_] = Ret,
					db_to_alert(Alert)
			end
	end;
get_alert(_)->
	{error,id_error}.


%% @doc Alarm information update
%% @spec update_alert(alert)->({error,Reason} | {ok,Result})
%% where
%%		alert = list()
%%		Result = tuple()
update_alert(Alert)->
	{value,{id,Id}} = lists:keysearch(id,1,Alert),
	db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),alert_to_db(Alert)).

%% @doc Search alarm combination of conditions
%% @spec  get_alert_match(Where)-> (alerts | {error,Reason})
%% where
%%		Where = string()
%%		alerts = list()
get_alert_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	[db_to_alert(X)||X <- Ret].


%% @doc Get all the alarm data
%% @spec get_all()->({error,Reason} | alerts)
%% where
%%		alerts = list()
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_alert(X)||X <- Ret].

%% @doc Remove a warm
%% @spec remove_alert(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple()
remove_alert(Id)when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_alert(Id)when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_alert(_)->
	{error,parameter_error}.

%% @doc Get alarm rules
%% @spec get_monitor_rule(Id)->({error,Reason} | Rules)
%% where
%%		Id		= (atom() | list())
%%		Rules	= list()
get_alert_rule(Id)when is_atom(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ atom_to_list(Id)++"'");
get_alert_rule(Id)when is_list(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ Id ++"'");
get_alert_rule(_)-> {error,parameter_error}.