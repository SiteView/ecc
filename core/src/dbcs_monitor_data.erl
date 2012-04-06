%% ---
%% dbcs_monitor_data
%%
%%---
-module(dbcs_monitor_data).
-define(Table,"monitor_data").

-include("dbcs_common.hrl").
-compile(export_all).

base_property(id)->
	true;
%%base_property(?CLASS)->
%%	true;
%%base_property(?NAME)->
%%	true;
%%base_property(?FREQUENCY)->
%%	true;
base_property(_)->
	false.

get_id()->
	case now() of
		{A,B,C}->
			list_to_atom(integer_to_list(A)++ ":" ++ integer_to_list(B) ++ ":" ++ integer_to_list(C));
		_->
			list_to_atom(integer_to_list(random:uniform(1000000)))
	end.

data_to_db(Monitor)->
	Id = get_id(),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<-Monitor,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"monitor_data">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_data(MonitorData)->
	case MonitorData of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

%% @doc Construction of a new monitoring records
%% @spec insert(Monitor) -> ({ok,Result}|{error,Reason})
%% where
%%		Monitor = list()
%%
%%
insert(Monitor) when is_list(Monitor) ->
	db_ecc:insert_data(?DBName,?Table,data_to_db(Monitor));
insert(_)-> {error,parameter_error}.
	
%% @doc All monitoring records obtained
%% @spec get_all()->Monitors
%% where
%%		Monitors = list()
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_data(X)||X <- Ret].


%% @doc Combination of conditions, monitoring records search
%% @spec query_data(Where)-> (Monitors | {error,Reason})
%% where
%%		Where = string()
%%		Monitors = list()
query_data(Where) when is_list(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	[db_to_data(X)||X <- Ret];
query_data(Where)->{error,where_should_be_list}.