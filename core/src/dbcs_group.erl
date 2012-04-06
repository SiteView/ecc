-module(dbcs_group).
-compile(export_all).
-export([create_group/1,get_group/1,update_group/1,get_group_match/1,remove_group/1,get_childs/1,get_all/0,get_allgroups/0,get_next_id/1]).
-define(Table,"group").

-define(APP,app_).

-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(?APP)->
	true;
base_property(_)->
	false.

group_to_db(Group)->
	{value,{id,Id}} = lists:keysearch(id,1,Group),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Group,base_property(K)=:=false],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"group">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_group(GroupData)->
	case GroupData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP,App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.
	
db_to_group2(GroupData)->
	case GroupData of
		{_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id},{?APP,App}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv,K=/=nextid];
		_->
			[]
	end.

%% @doc Create a new group
%% @spec create_group(Group)->({error,Reason} | {ok,Result})
%% where
%%		Group = list()
%%		Result = tuple()
create_group(Group)when is_list(Group) ->
	case db_ecc:insert_data(?DBName,?Table,group_to_db(Group)) of
		{ok,Ret}->
			{ok,db_to_group(Ret)};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end;
create_group(_)->{error,parameter_error}.


%% @doc query a group the data
%% @spec get_group_raw(Id)->({error,Reason} | Group)
%% where
%%		Id = (atom() | string())
%%		Group = list()
get_group_raw(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=group & id="++atom_to_list(Id)),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_group};
				_->
					[Group|_] = Ret,
					db_to_group(Group)
			end
	end;
get_group_raw(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=group & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_group};
				_->
					[Group|_] = Ret,
					db_to_group(Group)
			end
	end;
get_group_raw(_)->
	{error,id_error}.
	
	
get_group(Id)when is_atom(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=group & id="++atom_to_list(Id)),
    
    % io:format("~p~n",[{Ret,?DBName,?Table,"type=group & id="++atom_to_list(Id)}]),
    
    
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_group};
				_->
					[Group|_] = Ret,
					db_to_group2(Group)
			end
	end;
get_group(Id)when is_list(Id)->
	Ret = db_ecc:get_data(?DBName,?Table,"type=group & id="++Id),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_group};
				_->
					[Group|_] = Ret,
					db_to_group2(Group)
			end
	end;
get_group(_)->
	{error,id_error}.


%% @doc update group information
%% @spec update_group(Group)->({error,Reason} | {ok,Result})
%% where
%%		Group = list()
%%		Result = tuple()
update_group(Group)->
	{value,{id,Id}} = lists:keysearch(id,1,Group),
	Old = get_group_raw(Id),
	ND = lists:ukeymerge(1,lists:ukeysort(1,proplists:delete(nextid,Group)),lists:ukeysort(1,proplists:delete(id,Old))),
	% io:format("dbcs_group:update_group:~p,~p~n",[ND,dbcs_base:get_app()]),
	case db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),group_to_db(ND)) of
		{ok,Ret}->
			{ok,db_to_group(Ret)};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end.
	
update_group_raw(Group)->
	{value,{id,Id}} = lists:keysearch(id,1,Group),
	Old = get_group_raw(Id),
	ND = lists:ukeymerge(1,lists:ukeysort(1,Group),lists:ukeysort(1,Old)),
	db_ecc:update_data(?DBName,?Table,"id=" ++ atom_to_list(Id),group_to_db(ND)).

%% @doc Search groups combined conditions
%% @spec  get_group_match(Where)-> (Groups | {error,Reason})
%% where
%%		Where = string()
%%		Groups = list()
get_group_match(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	case Ret of
		{error,_}->
			[];
		_->
			[db_to_group2(X)||X <- Ret]
	end.


%% @doc Get All groups of data
%% @spec get_all()->({error,Reason} | Groups)
%% where
%%		Groups = list()
get_all()->
	Apps = 
	case dbcs_base:get_app() of
		undefined->
			app:all();
		App->
			[App]
	end,
	get_all(Apps).
	
%% @doc  Get All groups of data
%% @spec get_all()->({error,Reason} | Groups)
%% where
%%		Groups = list()
get_all_raw()->
	Apps = 
	case dbcs_base:get_app() of
		undefined->
			app:all();
		App->
			[App]
	end,
	get_all_raw(Apps).
	
get_all_raw([])->[];
get_all_raw([App|T])->
	dbcs_base:set_app(App),	
	Ret = db_ecc:get_data(?DBName,?Table,""),
	case Ret of
		{error,_}->
			[] ++ get_all_raw(T);
		_->
			[db_to_group(X)||X <- Ret] ++ get_all_raw(T)
	end.

get_all([])->[];
get_all([App|T])->
	dbcs_base:set_app(App),	
	Ret = db_ecc:get_data(?DBName,?Table,""),
	case Ret of
		{error,_}->
			[] ++ get_all(T);
		_->
			[db_to_group2(X)||X <- Ret] ++ get_all(T)
	end.

get_allgroups() ->
  GroupList=get_all(),
  getgroups(GroupList,[]).
getgroups([],Ret)->
	Ret;
getgroups([H|E],Ret)->
	Tmp= lists:keysearch(name, 1, H),
	case Tmp of
		false ->
			Name=[];
		_ ->{_,{_,Name}}=Tmp
	end,
	{_,{_,Id}}=lists:keysearch(id, 1, H),
	case length(Name) of
		0 -> Temp=Ret;
		_ ->Temp=Ret++[{Name,atom_to_list(Id) }]
	end,
	getgroups(E,Temp).
%% @doc Delete a group
%% @spec remove_group(Id)->({error,Reason} | {ok,Result})
%% where
%%		Id = (atom() | string())
%%		Result = tuple()
remove_group(Id)when is_atom(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++atom_to_list(Id));
remove_group(Id)when is_list(Id)->
	db_ecc:delete_data(?DBName,?Table,"id="++Id);
remove_group(Id)when is_number(Id)->
	db_ecc:delete_data(?DBName,?Table,lists:flatten(io_lib:format("id=~p",[Id])));
remove_group(_)->
	{error,parameter_error}.

%% @doc Return groups of children, including group and monitor
%% @spec  get_childs(Id)->({error,Reason} | Childs)
%% where
%%		Id = (atom() | string())
%%		Childs = list()
get_childs(Id) when is_atom(Id)->
	get_group_match("my.parent='" ++ atom_to_list(Id)++"'") 
	++ dbcs_monitor:get_monitor_match("my.parent='" ++ atom_to_list(Id)++"'")
	++ dbcs_device:get_device_match("my.parent='" ++ atom_to_list(Id) ++ "'");
get_childs(Id) when is_list(Id)->
	get_group_match("my.parent='" ++ Id ++ "'") 
	++ dbcs_monitor:get_monitor_match("my.parent='" ++ Id ++"'")
	++ dbcs_device:get_device_match("my.parent='" ++ Id ++ "'");
get_childs(_)->
	{error,parameter_error}.


%% @doc Get the next object uniquely identifies（ID)
%% @spec get_next_id(GID)->({error,Reason} | {ok,Id})
%% where
%%		GID = (atom() | string())
%%		Id = atom()
get_next_id(GID) when is_atom(GID);is_list(GID)->
	case get_group_raw(GID) of
		{error,_}->
			{error,not_found_group};
		Ret->
			% dbcs_base:set_app(proplists:get_value(?APP,Ret)),
			case lists:keysearch(nextid,1,Ret) of
				{value,{nextid,NextId}}->
					New = lists:map(fun({Key,Val})-> case Key of nextid-> {nextid,Val+1};_->{Key,Val} end end,Ret),
					update_group_raw(New),
					{ok,list_to_atom(atom_to_list(GID)++ "." ++ integer_to_list(NextId))};
				_->
					update_group_raw(Ret++[{nextid,2}]),
					{ok,list_to_atom(atom_to_list(GID)++ "." ++ integer_to_list(1))}
			end
	end;
get_next_id(_)->{error,parameter_error}.

reset()->
	% M=dbcs_monitor:get_all(),
	% lists:foreach(fun(X)->case lists:keysearch(id,1,X) of
	%						{value,{id,Id}}->
	%							dbcs_monitor:remove_monitor(Id);
	%						_->
	%							error
	%						end
	%				end, M),
	% D = dbcs_device:get_all(),
	% lists:foreach(fun(X)->case lists:keysearch(id,1,X) of
	%						{value,{id,Id}}->
	%							dbcs_device:remove_device(Id);
	%						_->
	%							error
	%						end
	%				end, D),
	% G = get_all(),
	% lists:foreach(fun(X)->case lists:keysearch(id,1,X) of
	%						{value,{id,Id}}->
	%							remove_group(Id);
	%						_->
	%							error
	%						end
	%				end, G),
	create_group([{id,server_conf:getServerID()},{class,group}]),
	create_group([{id,list_to_atom(atom_to_list(server_conf:getServerID())++".health")},{class,group},{name,"Health"},{parent,server_conf:getServerID()}]).
