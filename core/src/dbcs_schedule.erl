%%
%% @doc Based on content store的schedule database modules.
%%
%%
-module(dbcs_schedule).
-compile(export_all).
%%-export([insert_schedule/2, get_usergroup/1,update_usergroup/1,remove_usergroup/1,get_next_id/1]).
-define(Table,"schedule").
-define(MAXIDTable, "schedulemaxid").

-include("dbcs_common.hrl").


%% @doc create a new Schedule
%% @spec insert_Schedule(Id, Data) -> {error,Reason} | {badrpc,Reason} | {ok,Result}
%% where
%%		Id = string()
%%		Data = [{Key, Type, Value}]
insert_schedule(Id, Data) ->	
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"schedule">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, Data},
	db_ecc:insert_data(?DBName, ?Table, NewRecord).
	
	
%% @doc delete a Schedule
%% @spec delete_schedule(Id) -> {error,Reason} | {badrpc,Reason} | {ok,deleted} 
%% where
%%		Id = string()
delete_schedule(Id) ->
	Where = "id = " ++ Id,
	db_ecc:delete_data(?DBName, ?Table, Where).
	
	
%% @doc modify a Schedule
%% @spec update_schedule(Id, NewName) -> {error,Reason} | {badrpc,Reason} | {ok,Result}
%% where
%%		Id = string()
%%		NewData = [{Key, Type, Value}]
update_schedule(Id, NewData) ->	
	Where = "id = " ++ Id,	
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"schedule">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, NewData},
	db_ecc:update_data(?DBName, ?Table, Where, NewRecord).


%% @doc Schedule information for the specified
%% @spec query_schedule(Id) -> ScheduleInfo
%% where
%%		Id = string()
%%		ScheduleInfo = list(),eg:[{id, Id}, {name, Name}, {type, Type},...]
query_schedule(Id) ->	
	Where = "id = " ++ Id,		
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	case Ret of		
		[Schedule] ->
			db_to_group(Schedule);
 		_ ->
			[]
	end.
	
	
%% @doc Schedule information for all
%% @spec list_schedules() -> ScheduleInfoList
%% where
%%		ScheduleInfoList = [ScheduleInfo]
%%		ScheduleInfo = list(),eg:[{id, Id}, {name, Name}, {type, Type},...]
list_schedules() ->		
	Ret = db_ecc:get_data(?DBName, ?Table, ""),
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_group(Schedule) || Schedule <- Ret] 			
	end.	
	
	
%% @doc Get Schedule Name
%% @spec get_name(Id) -> Name | error | not_existed
%% where
%%		Name = string()
get_name(Id) ->
	Where = "id = " ++ Id,	
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, _, _, _, _, _, _, _, _, _, _, _, _, _, Ad}] ->
			case lists:keysearch(name, 1, Ad) of		
				{value, {name, string, _BinName}} ->
					try binary_to_list(_BinName)
					catch
						_ : _ -> ""
					end;		
				false ->
					""
			end;			
		_ ->
			error
	end.


%% @doc Get the maximum of the Schedule ID
%% @spec get_maxId() -> Maxid | error
%% where
%%		Maxid = string()
get_maxId() ->
	Ret = db_ecc:get_data(?DBName, ?MAXIDTable, "id = schedulemaxid"),
	
	case Ret of
		[] ->
			%init
			init_maxId(),
			"0";
		[{content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			case lists:keysearch(maxid, 1, Advance) of		
				{value, {maxid, _, _Bin}} ->
					try binary_to_list(_Bin)
					catch
						_ : _ -> "0"
					end;
				false ->
					"0"
			end;
		_ ->
			error
	end.


%% @doc Get the maximum of the Schedule ID
%% @spec get_maxIdByusergroup() -> Maxid | error
%% where
%%		Maxid = string()
get_maxIdByschedule() ->
	Ret = db_ecc:get_data(?DBName, ?Table, "type = schedule"),
	
	case is_list(Ret) of
		false ->
			error;
		true ->
			case length(Ret) of
				0 ->
					"0";
				_ ->
					CurIdList = [Id || {content, _, Id, _, _, _, _, _, _, _, _, _, _, _, _} <- Ret],
					MaxId = lists:max(CurIdList),
					atom_to_list(MaxId)
			end
	end.
	
	
%% @doc Update the maximum of the Schedule ID
%% @spec update_maxId(MaxId) -> {error,Reason} | {badrpc,Reason} | {ok,Result} 
%% where
%%		Maxid = string()
update_maxId(MaxId) ->	
	Advance = [{maxid, string, list_to_binary(MaxId)}],
	NewRecord = {content, list_to_atom(?MAXIDTable), list_to_atom(?MAXIDTable), list_to_binary(?MAXIDTable), null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:update_data(?DBName, ?MAXIDTable, "id = "++?MAXIDTable, NewRecord).
	
	
%% @doc 初始化最大用户组ID号
%% @spec init_maxId() -> {error,Reason} | {badrpc,Reason} | {ok,Result} 
%% where
%%		Maxid = string()	
init_maxId() ->	
	Advance = [{maxid, string, <<"0">>}],
	NewRecord = {content, list_to_atom(?MAXIDTable), list_to_atom(?MAXIDTable), list_to_binary(?MAXIDTable), null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:insert_data(?DBName, ?MAXIDTable, NewRecord).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generate a new ID
get_nextId() ->
	{MegaS, S, MicroS} = erlang:now(),
	"schedule_" ++ integer_to_list(MegaS) ++ ":" ++ integer_to_list(S) ++ ":" ++ integer_to_list(MicroS).
	

db_to_group({content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}) ->
	[{id, Id}] ++ [dbcs_base:db2term(K, T, V) || {K, T, V} <- Advance];
db_to_group(_) ->
	[].		

%% @spec query(Where)-> (Monitors | {error,Reason})
%% where
%%		Where = string()
%%		Monitors = list()
find(Where) when is_list(Where)->
	Ret = db_ecc:get_data(?DBName,?Table,Where),
	case Ret of
		{error,_}->
			[];
		_->
			[db_to_group(X)||X <- Ret]
	end;
find(_)->{error,error_parameter}.

name_existed(Name)when is_list(Name)->
	Ret = db_ecc:get_data(?DBName, ?Table, "my.name=" ++ Name),
	
	case Ret of
		[] ->
			not_existed;
		[{content, _, _, _, _, _, _, _, _, _, _, _, _, _, _}|_] ->
			ok;
		_ ->
			Ret
	end;
name_existed(_)->{error,error_parameter}.
	