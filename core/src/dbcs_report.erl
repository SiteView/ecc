%% 
%% dbcs_report
%%
%%
-module(dbcs_report).

-define(Table,"report").
-define(APP,app_).
-export([create_report/1,get_report/1,update_report/1,get_report_match/1,get_all/0,remove_report/1,get_monitor_report/1]).


-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(?APP)->
	true;
base_property(_)->
	false.
%% base_property(id)->
%% 	true;
%% base_property(_)->
%% 	false.

%% report_to_db(Report)->
%% 	{value,{id,Id}} = lists:keysearch(id,1,report),
%% 	Advance = [dbcs_base:term2db(K,V)||{K,V}<- report,base_property(K)=:=false],
%% 	case [is_atom(Id)] of
%% 		[true]->
%% 			{content,list_to_atom(?Table), Id, <<"report">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
%% 		_->
%% 			{}
%% 	end.

report_to_db(Report)->
	{value,{id,Id}}=lists:keysearch(id, 1, Report),
	Advance=[dbcs_base:term2db(K,V)||{K,V}<- Report,K=/=id],
	case is_atom(Id) of
		true ->
			{content,list_to_atom(?Table), Id, <<"report">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_ ->{}
	end.
db_to_report(ReportData)->
	case ReportData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP, App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

%% @doc create a rules
%% @spec create_report(report)->({error,Reason} | {ok,Result})
%% where
%%		report = list()
%%		Result = tuple()
create_report(Report)when is_list(Report) ->
	db_ecc:insert_data(?DBName,?Table,report_to_db(Report));
create_report(_)->{error,parameter_error}.


%% @doc Query a rule information
%% @spec get_report(Id)->({error,Reason} | report)
%% where
%%		Id = (atom() | string())
%%		report = list()
get_report(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=report & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_report};
				_->
					[Report|_] = Ret,
					db_to_report(Report)
			end
	end;
get_report(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=report & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_report};
				_->
					[Report|_] = Ret,
					db_to_report(Report)
			end
	end;
get_report(_)->
	{error,id_error}.


%% @doc Updated rules information
%% @spec update_report(report)->({error,Reason} | {ok,Result})
%% where
%%		report = list()
%%		Result = tuple()
update_report(Report)->
	{value,{id,Id}} = lists:keysearch(id,1,Report),
	db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),report_to_db(Report)).

%% @doc Search conditions combined rules
%% @spec  get_report_match(Where)-> (reports | {error,Reason})
%% where
%%		Where = string()
%%		reports = list()
get_report_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	[db_to_report(X)||X <- Ret].


%% @doc Made all the rules of data
%% @spec get_all()->({error,Reason} | reports)
%% where
%%		reports = list()
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_report(X)||X <- Ret].

%% @doc Delete a rule
%% @spec remove_report(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple()
remove_report(Id)when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_report(Id)when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_report(_)->
	{error,parameter_error}.

get_monitor_report(Id)when is_atom(Id)->
	get_report_match("my.target like " ++ "<" ++ atom_to_list(Id) ++ ">");
get_monitor_report(Id)when is_list(Id)->
	get_report_match("my.target like " ++ "<" ++ Id ++ ">");
get_monitor_report(_)-> [].