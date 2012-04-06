%%
%% @doc Based on content store的user database modules.
%%
%%
-module(dbcs_user).
-compile(export_all).
%%-export([create_usergroup/1,get_usergroup/1,update_usergroup/1,remove_usergroup/1,get_next_id/1]).
-define(Table,"user").
-define(MAXIDTable,"usermaxid").

-include("dbcs_common.hrl").


%% @doc Generate a new ID
%% @spec get_maxId() -> Maxid | error
%% where
%%		Maxid = string()
get_maxId() ->
	Ret = db_ecc:get_data(?DBName, ?MAXIDTable, "id = usermaxid"),
	
	case Ret of
		[] ->
			"login0";
		[{content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			case lists:keysearch(maxid, 1, Advance) of		
				{value, {maxid, _, _Bin}} ->
					binary_to_list(_Bin);
				false ->
					"login0"
			end;
		_ ->
			error
	end.


%% @doc Get the current maximum number of user ID
%% @spec get_maxIdByuser() -> Maxid | error
%% where
%%		Maxid = string()
get_maxIdByuser() ->
	Ret = db_ecc:get_data(?DBName, ?Table, "type = user"),
	
	case is_list(Ret) of
		false ->
			error;
		true ->
			case length(Ret) of
				0 ->
					"login0";
				_ ->
					CurIdList = [Id || {content, user, Id, _, _, _, _, _, _, _, _, _, _, _, _} <- Ret, is_atom(Id)],
					MaxId = lists:max(CurIdList),
					atom_to_list(MaxId)
			end
	end.
	
	
%% @doc Update the current maximum number of user ID
%% @spec update_maxId(MaxId) -> {error,Reason} | {badrpc,Reason} | {ok,Result} 
%% where
%%		Maxid = string()
update_maxId(MaxId) ->	
	Advance = [{maxid, string, list_to_binary(MaxId)}],
	NewRecord = {content, list_to_atom(?MAXIDTable), list_to_atom(?MAXIDTable), list_to_binary(?MAXIDTable), null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:update_data(?DBName, ?MAXIDTable, "id = "++?MAXIDTable, NewRecord).
	
%% @doc Initialize the largest user ID
%% @spec init_maxId() -> {error,Reason} | {badrpc,Reason} | {ok,Result} 
%% where
%%		Maxid = string()	
init_maxId() ->	
	Advance = [{maxid, string, <<"login0">>}],
	NewRecord = {content, list_to_atom(?MAXIDTable), list_to_atom(?MAXIDTable), list_to_binary(?MAXIDTable), null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:insert_data(?DBName, ?MAXIDTable, NewRecord).
	
%% @doc Judgment whether the user exists
%% @spec get_user(Name) -> not_existed | existed | error
%% where
%%		Name = string()
get_user(Name) ->
	Where = "my.name = '" ++ Name ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[_] ->
			existed;
		_ ->
			error
	end.

%% @doc Judgment whether the user exists and if it is being disabled, returns the user Id and the respective user group
%% @spec get_user(Name, Password) -> not_existed | disabled | {existed, UId, GroupId} | error
%% where
%%		Name = string()
%%		Password = string()
%%		UId = string()
get_user(Name, Password) ->
	Where = "my.name = '" ++ Name ++ "' & my.password = '" ++ Password ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, user, Id, _, _, _, _, _, _, _, _, _, _, _, _Advance}] ->
			case lists:keysearch(disabled, 1, _Advance) of
				{value, {disabled, _, <<"1">>}} ->
					disabled;
				_ ->
					case lists:keysearch(group, 1, _Advance) of		
						{value, {group, string, GroupID}} ->
							{existed, atom_to_list(Id), binary_to_list(GroupID)};
						false ->
							{existed, atom_to_list(Id), ""}
					end
			end;
		_ ->
			error
	end.
	
%% @doc Judgment whether the user exists, the user ID is returned if there is
%% @spec get_userId(Name) -> not_existed | ID | error
%% where
%%		Name = string()
%%		ID = string()
get_userId(Name) ->
	Where = "my.name = '" ++ Name ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, user, Id, _, _, _, _, _, _, _, _, _, _, _, _}] ->
			try atom_to_list(Id)
			catch
				_ : _ -> error
			end;
		_ ->
			error
	end.
	
	
%% @doc Judgment whether the user existsif there is return the user ID, ​​group ID,Password
%% @spec get_userPassword(Name) -> not_existed | disabled | {existed, UId, GroupId, Password} | error
%% where
%%		Name = string()
get_userPassword(Name) ->
	Where = "my.name = '" ++ Name ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, user, Id, _, _, _, _, _, _, _, _, _, _, _, _Advance}|_] ->
			case lists:keysearch(disabled, 1, _Advance) of
				{value, {disabled, _, <<"1">>}} ->
					disabled;
				_ ->
					Pass = case lists:keysearch(password, 1, _Advance) of		
								{value, {password, string, P}} ->
									try binary_to_list(P)
									catch
										_ : _ -> ""
									end;
								false ->
									""
							end,
					case lists:keysearch(group, 1, _Advance) of		
						{value, {group, string, GroupID}} ->
							GId = try binary_to_list(GroupID)
								  catch
									_ : _ -> ""
								  end,
							{existed, atom_to_list(Id), GId, Pass};
						false ->
							{existed, atom_to_list(Id), "", Pass}
					end
			end;
		_ ->
			error
	end.


%% @doc create a new user
%% @spec create_user(Id, Name, Password, Desc, Email) -> {error,Reason} | {barpc,Reason} | {ok,Result}
%% where
%%		Id = string()
%%		Name = string()
%%		Password = string()	
%%		Desc = string()
%%		Email = string()	
create_user(Id, Name, Password, Desc, Email) ->
	Advance = [{name, string, list_to_binary(Name)},
				{password, string, list_to_binary(Password)},
				{desc, string, list_to_binary(Desc)},
				{email, string, list_to_binary(Email)}],  %初建用户时，my字段中的group不存在。待以后添加
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"user">>, null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:insert_data(?DBName, ?Table, NewRecord).


create_user(Id, MyData) ->
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"user">>, null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, MyData},
	db_ecc:insert_data(?DBName, ?Table, NewRecord).
	

	
	
%% @doc Return users log information (such as, user permissions, node)
%% @spec get_logininfo(Name, Password) -> not_existed | error | [Rights, Nodes] | raw_user
%% where
%%		Name = string()	
%%		Password = string()
%%		Rights = [{Did, Dright}]  Nodes = [{Nid, Nname, Url}]
get_logininfo(Name, Password) ->
	Where = "my.name = '" ++ Name ++ "' & my.password = '" ++ Password ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, user, _, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			case lists:keysearch(group, 1, Advance) of		
				{value, {group, string, GroupIDs}} ->
					GroupIDList = string:tokens(binary_to_list(GroupIDs), ","),
					%Queries each Groupid were the right, node, and summary
					RightNodeList = [dbcs_usergroup:get_rightandnodeinfo(Id) || Id <- GroupIDList],
					dbcs_usergroup:merge_rightandnode(RightNodeList);
				false ->
					raw_user
			end;
		_ ->
			error		
				
	end.
	
	
%% @doc For the (original) user-specified user group belongs
%% @spec add_group(GId, UId) -> not_existed | {ok,Result} | {error,Reason} | error
%% where
%%		GId = string()
%%		UId = string()
add_group(GId, UId) ->
	Where = "id = " ++ UId,	
	%需要先提取用户原有的信息
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	case Ret of
		[] ->
			not_existed;
		[{content, user, _, _, _, _, _, _, _, _, _, _, _, _, Ad}] ->
			NewAd = proplists:delete(group, Ad) ++ [{group, string, list_to_binary(GId)}],
			NewRecord = {content, list_to_atom(?Table), list_to_atom(UId), <<"user">>, null, null, null, null, 
							<<"zhangyan">>, null, null, null, null, null, NewAd},
			db_ecc:update_data(?DBName, ?Table, Where, NewRecord);
		_ ->
			error			
	end.

	
%% @doc Delete user group the user belongs
%% @spec delete_group(GId, UId) -> {ok,Result} | {error,Reason} | error
%% where
%%		GId = string()
%%		UId = string()
delete_group(_GId, UId) ->
	Where = "id = " ++ UId,		
	%Users need to extract the original information
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	case Ret of
		[] ->
			{ok, user_not_existed};
		[{content, user, _, _, _, _, _, _, _, _, _, _, _, _, Ad}] ->
			RestAd = proplists:delete(group, Ad) ++ [{group, string, <<>>}],
			NewRecord = {content, list_to_atom(?Table), list_to_atom(UId), <<"user">>, null, null, null, null, 
						<<"zhangyan">>, null, null, null, null, null, RestAd},
			db_ecc:update_data(?DBName, ?Table, Where, NewRecord);					
		_ ->
			error
	end.	

	
%% @doc Judgment whether there is a user under the user group
%% @spec isExistedUserOfGroup(GId) -> existed | not_existed | error
%% where
%%		GId = string()
isExistedUserOfGroup(GId) ->
	Where = "my.group like '" ++ GId ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case is_list(Ret) of
		false ->
			error;
		true ->
			case Ret of
				[] ->
					not_existed;
				_ ->
					existed
			end		
	end.
	
	
%% @doc Query with a user group under the user information, return to the list of user information
%% @spec query_group(GId) -> UserInfos | error
%% where
%%		GId = string()
%%		UserInfos = [] | [Infos]  %[{Id, Name, Title, Dis, Group}]
query_group(GId) ->
	Where = "my.group like '" ++ GId ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case is_list(Ret) of
		false ->
			error;
		true ->
			[retrieve_userInfo(Id, Advance) || {content, user, Id, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret, is_atom(Id)] 			
	end.
	

%% @doc View all users
%% @spec browse_users() -> UserInfos
%% where
%%		UserInfos = [] | [Infos]
%%		Infos = {Id, Name, Title, Dis, Group}
browse_users() ->
	Ret = db_ecc:get_data(?DBName, ?Table, ""),
	
	case is_list(Ret) of
		false ->
			[];
		true ->
			[retrieve_userInfo(Id, Advance) || {content, user, Id, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret, is_atom(Id)] 			
	end.
	
%% @doc View all of the original user (ie does not belong to any user group)
%% @spec browse_rawusers() -> UserInfos | error
%% where
%%		UserInfos = [] | [Infos]
%%		Infos =  {Id, Name, Title, Dis}
browse_rawusers() ->
	Ret = db_ecc:get_data(?DBName, ?Table, ""),
	
	case is_list(Ret) of
		false ->
			[];
		true ->
			lists:flatten([retrieve_rawuserInfo(Id, Advance) || {content, user, Id, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret, is_atom(Id)])
	end.
	
%% @doc Get a user name
%% @spec get_name(Id) -> UserName | error | not_existed
%% where
%%		UserName = string()
get_name(Id) ->
	Where = "id = " ++ Id,	
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, user, _, _, _, _, _, _, _, _, _, _, _, _, Ad}] ->
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
	

%% @doc Access not disable a user's user name, password, user group
%% @spec get_nameandpass(Id) -> {UserName, Password, GroupId} | error | not_existed | disabled
%% where
%%		UserName = string()
%%		Password = string()
get_nameandpass(Id) ->
	Where = "id = " ++ Id,	
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, user, _, _, _, _, _, _, _, _, _, _, _, _, Ad}] ->
			case lists:keysearch(disabled, 1, Ad) of
				{value, {disabled, _, <<"1">>}} ->
					disabled;
				_ ->					
					Name = case lists:keysearch(name, 1, Ad) of		
								{value, {name, string, _BinName}} ->
									try binary_to_list(_BinName)
									catch
										_ : _ -> ""
									end;		
								false ->
									""
							end,					
					Pass = case lists:keysearch(password, 1, Ad) of		
								{value, {password, string, _BinPass}} ->
									try binary_to_list(_BinPass)
									catch
										_ : _ -> ""
									end;		
								false ->
									""
							end,
					GId = case lists:keysearch(group, 1, Ad) of		
							{value, {group, string, _BinGroup}} ->
								try binary_to_list(_BinGroup)
									catch
										_ : _ -> ""
									end; 
							false ->
								""
						  end,
					{Name, Pass, GId}
			end;						
		_ ->
			error
	end.
	
	
%% @doc Get user information(name, password, Desc,...)
%% @spec get_userInfo(Id) -> {ok, UserInfos} | error | not_existed
%% where
%%		UserInfos = list(), eg:[{id, 'login1'},{name, "username"},...]
get_userInfo(Id) ->
	Where = "id = " ++ Id,	
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[User] ->
			{ok, db_to_group(User)};
		_ ->
			error
	end.
	

%% @doc Get user information(name, password, Desc,...)
%% @spec get_userInfoByName(Name) -> {ok, UserInfos} | error | not_existed
%% where
%%		UserInfos = list(), eg:[{id, 'login1'},{name, "username"},...]
get_userInfoByName(Name) ->
	Where = "my.name = '" ++ Name ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[User] ->
			{ok, db_to_group(User)};
		_ ->
			error
	end.
	
	
%% @doc Delete user
%% @spec delete_user(UId) -> {error,Reason} | {badrpc,Reason} | {ok,deleted} 
%% where
%%		UId = string()
delete_user(UId) ->
	Where = "id = " ++ UId,
	db_ecc:delete_data(?DBName, ?Table, Where).


%% @doc Change password
%% @spec change_password(Id, NewPass) -> {error,Reason} | {badrpc,Reason} | {ok,Result} | not_existed | error
%% where
%%		Id = string()
%%		NewPass = string()
change_password(Id, NewPass) ->
	Where = "id = " ++ Id,
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	case Ret of
		[] ->
			not_existed;
		[{content, user, _, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			NewAd = proplists:delete(password, Advance) ++ [{password, string, list_to_binary(NewPass)}],
			NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"user">>, null, null, null, null, 
						<<"zhangyan">>, null, null, null, null, null, NewAd},
			db_ecc:update_data(?DBName, ?Table, Where, NewRecord);
		_ ->
			error
	end.


%% @doc Update User
%% @spec update_user(UId, Info) -> {error,Reason} | {badrpc,Reason} | {ok,Result} 
%% where
%%		UId = string()
%%		Info = list()
update_user(UId, Info) ->
	Where = "id = " ++ UId,
	Advance = [{Key, string, list_to_binary(Value)} || {Key, Value} <- Info], %[{group, string, list_to_binary(Group)}],	
	%io:format("update user:~p ~n", [Advance]),
	NewRecord = {content, list_to_atom(?Table), list_to_atom(UId), <<"user">>, null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:update_data(?DBName, ?Table, Where, NewRecord).
	
retrieve_value(Key, Advance) ->
	case lists:keysearch(Key, 1, Advance) of
		{value, {_, _, Bin}} -> 
			try binary_to_list(Bin)
			catch
				_ : _ -> ""
			end;
		_ ->
			""
	end.
	
retrieve_rawuserInfo(Id, Advance) ->	
	case retrieve_value(group, Advance) of
		"" ->
			Dis = retrieve_value(disabled, Advance),
			Title = retrieve_value(title, Advance),
			Name = retrieve_value(name, Advance),
			{atom_to_list(Id), Name, Title, Dis};
		_ ->
			[]
	end.	

retrieve_userInfo(Id, Advance) ->
	Group = retrieve_value(group, Advance),	
	Dis = retrieve_value(disabled, Advance),
	Title = retrieve_value(title, Advance),
	Name = retrieve_value(name, Advance),
	{atom_to_list(Id), Name, Title, Dis, Group}.
	
% The user's database structure into a List, return the group id, and other structures List
db_to_group({content, user, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}) ->
	[{id,Id}] ++ [dbcs_base:db2term(K, T, V) || {K, T, V} <- Advance];
db_to_group(_) ->
	[].	