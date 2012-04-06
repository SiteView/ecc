%% 
%% dbcs_rule
%%
%%
-module(dbcs_rule).

-define(Table,"rule").

-export([create_rule/1,get_rule/1,update_rule/1,get_rule_match/1,get_all/0,remove_rule/1,get_monitor_rule/1]).

-define(APP,app_).
-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(?APP)->
	true;
base_property(_)->
	false.

rule_to_db(Rule)->
	{value,{id,Id}} = lists:keysearch(id,1,Rule),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Rule,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"rule">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_rule(RuleData)->
	case RuleData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP,App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

%% @doc Create a new rule
%% @spec create_rule(Rule)->({error,Reason} | {ok,Result})
%% where
%%		Rule = list()
%%		Result = tuple()
create_rule(Rule)when is_list(Rule) ->
	case db_ecc:insert_data(?DBName,?Table,rule_to_db(Rule)) of
		{ok,Ret}->
			{ok,db_to_rule(Ret)};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end;
create_rule(_)->{error,parameter_error}.


%% @doc Query a rule information
%% @spec get_rule(Id)->({error,Reason} | Rule)
%% where
%%		Id = (atom() | string())
%%		Rule = list()
get_rule(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=rule & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_rule};
				_->
					[Rule|_] = Ret,
					db_to_rule(Rule)
			end
	end;
get_rule(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=rule & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_rule};
				_->
					[Rule|_] = Ret,
					db_to_rule(Rule)
			end
	end;
get_rule(_)->
	{error,id_error}.


%% @doc Updated rules information
%% @spec update_rule(Rule)->({error,Reason} | {ok,Result})
%% where
%%		Rule = list()
%%		Result = tuple()
update_rule(Rule)->
	{value,{id,Id}} = lists:keysearch(id,1,Rule),
	Old = get_rule(Id),
	ND = lists:ukeymerge(1,lists:ukeysort(1,Rule),lists:ukeysort(1,Old)),
	case db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),rule_to_db(ND)) of
		{ok,Ret}->
			{ok,db_to_rule(Ret)};
		Else->
			Else
	end.

%% @doc Search conditions combined rules
%% @spec  get_rule_match(Where)-> (Rules | {error,Reason})
%% where
%%		Where = string()
%%		Rules = list()
get_rule_match(Where)->
	erlcache:start_link(),
	Key = erlang:phash2({rule,Where}),
	case erlcache:get(Key) of
		{ok,R}->
			R;
		_->
			Ret = db_ecc:get_data(?DBName,?Table,Where),
			Rules = [db_to_rule(X)||X <- Ret],
			erlcache:set(Key,Rules,10),
			Rules
	end.


%% @doc Made all the rules of data
%% @spec get_all()->({error,Reason} | Rules)
%% where
%%		Rules = list()
get_all()->
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_rule(X)||X <- Ret].
%	Apps = 
%	case dbcs_base:get_app() of
%		undefined->
%			app:all();
%		App->
%			[App]
%	end,
%	get_all(Apps).
	
get_all([])->[];
get_all([App|T])->
	dbcs_base:set_app(App,true),
	Ret = db_ecc:get_data(?DBName,?Table,""),
	[db_to_rule(X)||X <- Ret]++get_all(T).

%% @doc Delete a rule
%% @spec remove_rule(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple()
remove_rule(Id)when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_rule(Id)when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_rule(_)->
	{error,parameter_error}.
	
get_monitor_rule(Id)when is_atom(Id)->
	get_monitor_rule(atom_to_list(Id));
get_monitor_rule(Id)when is_list(Id)->
	Ret = 
	get_rule_match("my.target like <" ++ Id ++ ">") ++
	get_rule_match("my.target like <all>"),
	lists:foldl(fun(X,R)->
		case lists:member(X,R) of
			true->
				R;
			_->
				R ++ [X]
		end 
	end,[],Ret);
get_monitor_rule(_)-> [].

% get_monitor_rule(Id)when is_atom(Id)->
	% get_monitor_rule(atom_to_list(Id));
% get_monitor_rule(Id)when is_list(Id)->
	% Rules = get_all(),
	% F = fun(X,Sum)->
		% Target = proplists:get_value(target,X,""),
		% case string:str(Target,"<" ++ Id ++ ">") of
			% 0->
				% case string:str(Target,"<all>") of
					% 0->
						% Sum;
					% _->
						% Sum ++ [X]
				% end;
			% _->
				% Sum ++ [X]
		% end
	% end,
	% lists:foldl(F,[],Rules);
% get_monitor_rule(_)-> [].