%% 
%% @doc based content store的monitor database module.
%%
%% 
%%

-module(dbcs_monitor).
%%-behaviour(db_monitor).

-define(Table,"monitor").

-include("dbcs_common.hrl").

%%-compile(export_all).
-export([create_monitor/1,get_monitor/1,update_monitor/1,get_all/0,remove_monitor/1,get_monitor_match/1,get_monitor_rule/1]).

-define(APP,app_).

base_property(id)->
	true;
base_property(?APP)->
	true;
%%base_property(?CLASS)->
%%	true;
%%base_property(?NAME)->
%%	true;
%%base_property(?FREQUENCY)->
%%	true;
base_property(_)->
	false.

%%monitor_to_db(Monitor)->
%%	{value,{id,Id}} = lists:keysearch(id,1,Monitor),
%%	{value,{?CLASS,Class}} =  lists:keysearch(?CLASS,1,Monitor),
%%	{value,{?NAME,Name}} =  lists:keysearch(?NAME,1,Monitor),
%%	{value,{?FREQUENCY,Frequency}} =  lists:keysearch(?FREQUENCY,1,Monitor),
%%	Advance = [term2db(K,V)||{K,V}<-Monitor,base_property(K)=:=false],
%%	case [is_atom(Id),is_atom(Class),is_atom(Name),is_integer(Frequency)] of
%%		[true,true,true,true]->
%%			{content,list_to_atom(?Table), Id, Class,Name,Frequency,null,null,?Author,null,null,null,null,null,Advance};
%%		_->
%%			{}
%%	end.

monitor_to_db(Monitor)->
	{value,{id,Id}} = lists:keysearch(id,1,Monitor),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<-Monitor,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"monitor">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_monitor(MonitorData)->
	case MonitorData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP,App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

%% @doc create the monitor
%% @spec create_monitor(Monitor) -> ({ok,Result}|{error,Reason})
%% where
%%		Monitor = list()
%%
%%
create_monitor(Monitor) when is_list(Monitor) ->
	%Data = [
	%	{"operationInfo", Monitor}
    % ],
    %Ret0 = ofbiz:call("operationCreate",[{"operationInfo", Monitor}]),
    % Ret = jav_node:send("OfbizService",operationCreate,[{"operationInfo", Monitor}],Timeout)->

   
    %case proplists:get_value('ret',Ret) of
	%	"OK"->
	%		io:format("creat monitor ret:~p~n",[proplists:get_value('result',Ret)]),
	%		{ok,proplists:get_value('result',Ret)};
	%	Err->
	%		{error,Err}
	% end;
	case db_ecc:insert_data(?DBName,?Table,monitor_to_db(Monitor)) of
		{ok,Ret} ->
			 {ok,db_to_monitor(Ret)};
		Err ->
			Err
	end;
create_monitor(_)-> {error,parameter_error}.

%% @doc Query monitor information
%% 
%% @spec get_monitor(Id)-> ({error,Reason} | Monitor)
%%	where
%%			Id		= (atom() | list())
%%			Monitor = list()
get_monitor(Id) when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=monitor & id="++atom_to_list(Id)),
	case Ret of
		{error,Reason}->
			{error,Reason};
		_->
			case length(Ret) of
				0->
					{error,not_found_monitor};
				_->
					[Monitor|_] = Ret,
					db_to_monitor(Monitor)
			end
	end;
get_monitor(Id) when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=monitor & id="++Id),
	case Ret of
		{error,Reason}->
			{error,Reason};
		_->
			case length(Ret) of
				0->
					{error,not_found_monitor};
				_->
					[Monitor|_] = Ret,
					db_to_monitor(Monitor)
			end
	end;
get_monitor(_)->{error,parameter_error}.

%% @doc Update monitor information
%% @spec update_monitor(Monitor)->({error,Reason}|{ok,Result})
%% where
%%			Monitor = list()
%%			Result  = tuple()
update_monitor(Monitor) when is_list(Monitor)->
	case lists:keysearch(id,1,Monitor) of
		{value,{id,Id}}->
			Old = get_monitor(Id),
			ND = lists:ukeymerge(1,lists:ukeysort(1,Monitor),lists:ukeysort(1,Old)),
			db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),monitor_to_db(ND));
		_->
			{error,not_found_id}
	end;
update_monitor(_)-> {error,parameter_error}.
	
%%db_to_monitor(MonitorData)->
%%	case MonitorData of
%%		{_,_,Id,Class,Name,Frequency,_,_,_,_,_,_,_,_,Adv}->
%%			[{id,Id},{?CLASS,Class},{?NAME,Name},{?FREQUENCY,Frequency}] ++ [db2term(K,T,V)||{K,T,V}<-Adv];
%%		Else->
%%			[]
%%	end.

%% @doc Get all the information monitors
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
			[db_to_monitor(X)||X <- Ret]
	end.

%% @doc Deleting a monitor
%% @spec remove_monitor(Id)->({ok,Result}|{error,Reason})
%% where
%%	Id = (atom() | string())
remove_monitor(Id) when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_monitor(Id) when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_monitor(_)->{error,parameter_error}.

%% @doc Search monitor combined conditions
%% @spec get_monitor_match(Where)-> (Monitors | {error,Reason})
%% where
%%		Where = string()
%%		Monitors = list()
get_monitor_match(Where) when is_list(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	case Ret of
		{error,_}->
			[];
		_->
			[db_to_monitor(X)||X <- Ret]
	end;
get_monitor_match(_)->{error,where_should_be_list}.


%% @doc Rules to obtain the monitor
%% @spec get_monitor_rule(Id)->({error,Reason} | Rules)
%% where
%%		Id		= (atom() | list())
%%		Rules	= list()
get_monitor_rule(Id)when is_atom(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ atom_to_list(Id)++"'");
get_monitor_rule(Id)when is_list(Id)->
	dbcs_rule:get_rule_match("my.parent='" ++ Id ++"'");
get_monitor_rule(_)-> {error,parameter_error}.