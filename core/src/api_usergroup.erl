%% @doc interface of usergroup operation
%% @version{1.0}
%% @copyright 2009 dragonflow.com

-module(api_usergroup).
-extends(api_siteview).

-export([user_create/9,user_validate/2,changePassword/2,delete_user/1]).
-export([create/4,update/3,delete/1]).

-compile(export_all).

-define(LDAPUseName,"cn=Manager,dc=quikview,dc=net").
-define(LDAPPassword,"siteview123").
-define(BaseDN,"dc=quikview,dc=net").


%% @spec user_create(Name, Password, LdapServer, LdapSecurity, Desc, Title, Email, Disabled, Group)->({ok,Result} | {error,Reason})
%% where
%%	Name = string()
%%	Password = string()
%%	LdapServer = string()
%%	Desc = string()
%%	Title = string()
%%	Email = string()
%%	Disabled = string()
%%	Group = string()
%%	Result = atom()
%%	Reason = atom()
%% @doc create a new user,return {ok,create_user_ok} is ok,else is error
%%
user_create(Name, Password, LdapServer, LdapSecurity, Desc, Title, Email, Disabled, Group) ->
	%Determine whether the user name to be created has been used
	case dbcs_user:get_user(Name) of
		existed ->
			{error, existed};
		not_existed ->			
			createUser(Name, Password, LdapServer, LdapSecurity, Desc, Title, Email, Disabled, Group);
		error ->
			{error, db_error}
	end.
	

%% @spec createUser(Id, Data) -> ({error,Reason} | {ok,Result})
%% where
%%	Id = string()
%%	Data = [{property,value}]
%%	Reason = atom()
%%	Result = atom()
%% @doc create user
createUser(Id, Data) ->
	case dbcs_user:create_user(Id, Data) of		
		{ok, _} ->
			createUser_update(Id);
		{error, existed} ->
			%The ID already exists, generate a new ID, re-inserted
			case dbcs_user:get_maxIdByuser() of
				"login"++_StrN ->
					_Num = list_to_integer(_StrN) + 1,
					_Id = "login" ++ integer_to_list(_Num),
					case dbcs_user:create_user(_Id, Data) of
						{ok, _} ->
							createUser_update(Id);
						_ ->
							{error, db_create_user_error}
					end;
				_ ->
					{error, make_id_error}
			end;
		_ ->  
			{error, database_error}
	end.
	
createUser_update(Id) ->
	%Updated maxid
	case dbcs_user:update_maxId(Id) of
		{ok, _} ->
			ok;
		{_E, _W} ->
			%Written to the log
			io:format("Update maxid failed::  ~p:~p~n", [_E, _W])
	end,
	{ok, create_user_ok}.

%% @spec createUser(Name, Password, LdapServer, LdapSecurity, Desc, Title, Email, Disabled, Group) -> ({ok,[Result]} | {error,[Reason]})
%% where
%%	Name = string()
%%	Password = string()
%%	LdapServer = string()
%%	Desc = string()
%%	Title = string()
%%	Email = string()
%%	Disabled = string()
%%	Group = string()
%%	Result = string()
%%	Reason = string()
%% @doc create a new user
createUser(Name, Password, LdapServer, LdapSecurity, Desc, Title, Email, Disabled, Group) ->		
	
	case dbcs_user:get_maxId() of
		"login"++StrN ->
			Num = list_to_integer(StrN) + 1,
			Id = "login" ++ integer_to_list(Num),
			Data = [{name, string, list_to_binary(Name)},
					{password, string, list_to_binary(Password)},
					{ldapServer, string, list_to_binary(LdapServer)},
					{ldapSecurity, string, list_to_binary(LdapSecurity)},
					{title, string, list_to_binary(Title)},
					{desc, string, list_to_binary(Desc)},
					{email, string, list_to_binary(Email)},					
					{disabled, string, list_to_binary(Disabled)},
					{group, string, list_to_binary(Group)}
				   ],
			%io:format("LdapServer:~p LdapSecurity:~p~n", [LdapServer, LdapSecurity]),
			case LdapServer of
				[LServer] ->
					case LdapSecurity of
						[LSecurity] ->
							Ret = ldapData_add(Name, LServer, LSecurity),
							io:format("ldapData_add: ~p ~n", [Ret]);
						[] ->
							ok
					end;
				[] ->
					ok
			end,		
			createUser(Id, Data);
		error ->
			{error, make_id_error}
	end.	
	
% Parameters: username, password
% User. Returns the user permissions (read, write, time management, writing management), the node information (the user can see the node)
user_login(Name, Password) ->
	case dbcs_user:get_logininfo(Name, Password) of
		not_existed ->
			{error, not_existed_or_password_error};
		raw_user ->
			{user, [{name, [Name]}, {nodes, []}, {rights, []}]};
		[Rights, Nodes] ->%Rights: [{Did, Dright}]  Nodes: [{NId, NName, Url}]
			RightStruct = lists:map(fun({DId, DRight}) ->
									 {right, [{id, [atom_to_list(DId)]}, {right, [DRight]}]}
									end, Rights),
			NodeStruct = lists:map(fun({NId, NName, Url}) ->
									 {node, [{id, [atom_to_list(NId)]}, {name, [NName]}, {url, [Url]}]}
									end, Nodes),
			{user, [{name, [Name]}, {nodes, NodeStruct}, {rights, RightStruct}]};
		_ ->
			{user, []}
	end.
	%{user,[{name,["admin"]},{nodes,[{node,[{id,["1"]},{name,["changsha"]},{url,["http://www.sina.com"]}]},{node,[{id,["2"]},{name,["zhuzhou"]},{url,["http://192.168.1.2:8080"]}]}]},{gright,[{right,[{gid,["1.1.1"]},{right,["read"]}]}]},{dright,[{right,[{did,["1.1.2"]},{right,["write"]}]}]}]}.
	

%% @spec user_validate(Name, Password) ->({ok, Info,GroupName} | {error,Reason} )
%% where
%%	Name = string()
%%	Password = string()
%%	Info = string()
%%	GroupName = string()
%%	Reason = atom()
%% @doc validate a user with name and password
user_validate(Name, Password) when is_list(Name) andalso is_list(Password) ->
	case dbcs_user:get_userInfoByName(Name) of
		{ok, Infos} ->
			case proplists:get_value(ldapServer, Infos) of
				undefined ->% DB Authentication
					user_validateByDB(Password, Infos);
				[] ->% DB Authentication
					user_validateByDB(Password, Infos);	
				LServer ->% LDAP Authentication
					%io:format("LDAP validata:: Server:~p Name: ~p~n", [LServer, Name]),
					user_validateByLDAP(LServer, Name, Password, Infos)				
			end;
		Other ->
			{error,Other}
	end;
user_validate(_,_)->{error,parameter_invalid}.

user_validateByDB(Password, Infos) ->
	case proplists:get_value(password, Infos) of
		Password ->
			case proplists:get_value(disabled, Infos) of
				"1" ->
					{error,disabled};
				_ ->
					{ok,atom_to_list(proplists:get_value(id, Infos)), proplists:get_value(group, Infos)}
			end;
		_ ->
			{error,not_existed_or_password_error}
	end.		
	
user_validateByLDAP(LServer, Name, Password, Infos) ->
	case ldapData_search(LServer,Name,Password) of
		true ->
			case proplists:get_value(disabled, Infos) of
				"1" ->
					{error,disabled};
				_ ->
					{ok, proplists:get_value(id, Infos), proplists:get_value(group, Infos)}
			end;
		false ->
			{error,not_existed_or_password_error};
		_ ->
			{error,ldap_error}
	end.
	

%%
%%
%%
query_user(UserName) ->
	dbcs_user:get_userPassword(UserName).

%% @spec changePassword(Id, NewPass) -> ({ok,change_password_ok} | {error,db_error }|{error,parameter_error})
%% where
%%	Id = string()
%%	NewPass = string()
%% @doc change a user's password
changePassword(Id, NewPass) when is_list(Id) andalso is_list(NewPass)->
	case dbcs_user:change_password(Id, NewPass) of
		{ok, _} ->
			{ok,change_password_ok};
		_ ->
			{error,db_error}
	end;
changePassword(_,_)->{error,parameter_error}.		


%% @spec create(Name, Nodes, RightStruct, AdminiRights)->({ok,Result} | {error,Reason})
%% where
%%	Name=string()
%%	Nodes=string()
%%	RightStruct=({GIDs, Rights} | [{GIDs, Rights}])
%%	GIDs=list()
%%	Rights= [Right]
%%	Right=atom()
%% @doc create a user group with right on some groups or nodes,Nodes is a string contains of group id or  node id,at:"Id1,Id2,..."
%%  GIDs is like["gid1","gid2"]
create(Name, Nodes, RightStruct, AdminiRights) -> 
	%Be created to determine whether the user group name already in use
	case dbcs_usergroup:get_usergroup(Name) of
		existed ->
			{error, usergroup_existed};
		not_existed ->			
			createUserGroup(Name, Nodes, RightStruct, AdminiRights);
		error ->
			{error, db_error}
	end.
%% @see create/4
%%	
createUserGroup(Name, Nodes, RightStruct, AdminiRights) ->
	
	case dbcs_usergroup:get_maxId() of
		"usergroup"++StrN ->
			Num = list_to_integer(StrN) + 1,
			Id = "usergroup" ++ integer_to_list(Num),
			Data = [{name, string, list_to_binary(Name)},
					{node, string, list_to_binary(Nodes)},
					{adminrights, list, term_to_binary(AdminiRights)}
				    ],
			createUserGroup(Id, Data, RightStruct);
		error ->
			{error, make_id_error}
	end.
	
%% @see create/4
%%	
createUserGroup(Id, Data, RightStruct) ->
	case dbcs_usergroup:create_usergroup(Id, Data) of	
		{ok, _} ->
			updateMaxId(Id),
			createRights(Id, RightStruct),					
			{ok, [Id]};
		{error, existed} ->
			%The ID already exists, generate a new ID, re-inserted
			case dbcs_usergroup:get_maxIdByusergroup() of
				"usergroup"++_StrN ->
					_Num = list_to_integer(_StrN) + 1,
					_Id = "usergroup" ++ integer_to_list(_Num),
					case dbcs_usergroup:create_usergroup(_Id, Data) of
						{ok, _} ->
							updateMaxId(Id),
							createRights(Id, RightStruct),	
							{ok, [Id]};
						_ ->
							{error, create_usergroup_error}
					end;
				_ ->
					{error, make_id_error}
			end;
		_ ->  
			{error, db_error}
	end.
	
	
%%Parameters: Id, the new group name and a new node
%%Modify user group basic information (name, node). Return success

%% @spec update(Id, NewName, Nodes) ->({error,[Reason]} | {ok,[Return]})
%% where
%%	Id = string()
%%	NewName = string()
%%	Nodes = string()
%%	Reason = string()
%%	Return = string()
%% @doc modify usergroup's base information
update(Id, NewName, Nodes) ->	
	
	case dbcs_usergroup:get_usergroupId(NewName) of		
		not_existed ->			
			update_usergroup(Id, NewName, Nodes);
		error ->
			{error, ["0"]};
		Id ->
			update_usergroup(Id, NewName, Nodes);
		_ExistedId ->
			{error, ["existed"]}
	end.

%% @spec update_usergroup(Id, NewName, Nodes) -> ({ok,[Result]} | {error,[Reason]})
%% where
%%	Id = string()
%%	NewName  = string()
%%	Nodes = string()
%%	Reason = string()
%%	Return = string()
%% @doc update user group 's name and node
update_usergroup(Id, NewName, Nodes) ->
	case dbcs_usergroup:update_usergroup(Id, NewName, Nodes) of		
		{ok, _} ->
			{ok, ["1"]};				
		{error, content_empty} -> 
			{error, ["not_existed"]};
		_ ->  
			{error, ["0"]}
	end.


%% @spec update_usergroup(Id, NewName, Nodes, AdminiRightsList, RightStruct) -> ({ok,[Result]} | {error,[Reason]})
%% where
%%	Id = string()
%%	NewName  = string()
%%	Nodes = string()
%%	AdminiRightsList = [{Gids,Rights}]
%%	RightStruct = [{Gids,Rights}]
%%	Gids = list()
%%	Rights= [Right]
%%	Right=atom()
%%	Reason = string()
%%	Return = string()
%% @see update_usergroup/3
update_usergroup(Id, NewName, Nodes, AdminiRightsList, RightStruct) ->
	case dbcs_usergroup:update_usergroup(Id, NewName, Nodes, AdminiRightsList) of		
		{ok, _} ->
			
			Ret = dbcs_usergroup:update_rights(Id, RightStruct),
			io:format("update rights: ~p~n", [Ret]),
			{ok, ["1"]};				
		{error, content_empty} -> 
			{error, ["not_existed"]};
		_ ->  
			{error, ["0"]}
	end.
	
%%Parameters: Id, the new group name, the new node, management rights and authority to operate
%%Modify the basic information and user group permissions. Return success

%% @see update_usergroup/5
update(Id, NewName, Nodes, AdminiRightsList, RightStruct) ->
	%To be updated to determine whether the user group name already exists
	case dbcs_usergroup:get_usergroupId(NewName) of		
		not_existed ->			
			update_usergroup(Id, NewName, Nodes, AdminiRightsList, RightStruct);
		error ->
			{error, ["0"]};
		Id ->
			update_usergroup(Id, NewName, Nodes, AdminiRightsList, RightStruct);
		_ExistedId ->
			{error, ["existed"]}
	end.

%%Parameters: Id, the old and the new group name group name
%%Modify the user group name. Return success

%% update(Id, {_OldName, Name})->({ok,[Result]} | {error,[Reason]})
%% where
%%	Id = string()
%%	_OldName = string()
%%	Name = string()
%%	Reason = string()
%%	Return = string()
%% @doc change a group's name
update(_Id, {Name, Name}) ->
	{ok, ["1"]};
update(Id, {_OldName, NewName}) ->	
	%To be updated to determine whether the user group name already exists
	case dbcs_usergroup:get_usergroup(NewName) of
		existed ->
			{error, ["existed"]};
		not_existed ->			
			case dbcs_usergroup:update_usergroup(Id, NewName) of		
				{ok, _} ->
					{ok, ["1"]};				
				not_existed -> %The Id of the user group does not exist
					{error, ["not_existed"]};
				_ ->  %rpc call to an error, the node may be down the wrong parameters or rpc
					{error, ["0"]}
			end;
		error ->
			{error, ["0"]}
	end.

%%Parameters: Group ID
%%Delete group, return success
%% @spec delete(Id) -> ({error,[Reason]}|{ok,[Result]})
%% where
%%	Id = string()
%%	Reason = string()
%%	Result = string()
%% @doc delete a user group
delete(Id) ->	
	%First determine whether there is under the user group user
	case dbcs_user:isExistedUserOfGroup(Id) of
		not_existed ->			
			case dbcs_usergroup:delete_usergroup(Id) of
				{ok, _} ->
					{ok, ["1"]};
				_ ->
					{error, ["0"]}
			end;
		existed ->
			{error, ["existed_user"]};
		_ ->
			{error, ["0"]}
	end.		


%%Parameters: NULL
%%Browse all user groups, Group Id and returns a list of group names
%% @spec get_all_group() ->[UserInfo]
%% where
%%	UserInfo = {GroupId,Name,Role}
%%	GroupName = string()
%%	Name = string()
%%	Role = string()
%% @doc get all group's info
get_all_group() ->
	dbcs_usergroup:get_all_usergroup().
	

%%Parameters: Group ID
%%Query a user group, returns a list of group permissions
%% @spec get_right(Id) -> Rights
%% @doc get usergroup's right
%%
get_right(Id) ->
	dbcs_usergroup:queryrightOfOneUsergroup(Id).
	
	
%%Parameters: Group ID
%%Query a user group information (name, authority and management nodes)
%% @spec get_nodeandadmin(Id) -> {NameStr, AdminRightList, NodeStr}
%% where
%%	Id = string()
%%	NameStr=string()
%%	AdminRightList = list()
%%	NodeStr = string()
%% @doc query a usergroup's information,Id is usergroup's id,NameStr is name of usergroup
get_nodeandadmin(Id) ->
	Infos = dbcs_usergroup:queryInfo_Usergroup(Id),
	Name = proplists:get_value(name, Infos),
	NameStr = case is_atom(Name) of
				true ->
					"";
				false ->	
					Name
			  end, 
	Node = proplists:get_value(node, Infos),
	NodeStr = case is_atom(Node) of
				true ->
					"";
				false ->	
					Node
			  end, 
	AdminRights = proplists:get_value(adminrights, Infos),
	AdminRightList = case is_atom(AdminRights) of
						true ->
							[];
						false ->	
							AdminRights
					 end, 
	{NameStr, AdminRightList, NodeStr}.	


%% Parameters: None
%% Check all the permissions, permission to return to the list
%% @spec get_all_rights() ->[atom()]
%% @doc get all rights,return a list contains all right
%%
get_all_rights() ->	
	dbcs_usergroup:get_all_operateRs().
	
	
%% Parameters: group IDs, Rights
%% Change (multiple) user group, return success
update_right(Params) ->
	[do(GroupID, RestGroup) || [[GroupID] | RestGroup] <- Params].
	
	
%% Parameters: group ID, user IDs
%% For the group of users to add multiple users, return success
add_user(GId, UIdList) ->	
	[add_oneuser(GId, UId) || UId <- UIdList].
	
%% Parameters: group ID, user ID
%% Add a single user for the user group, return success	
add_oneuser(GId, UId) ->	
	case dbcs_user:add_group(GId, UId) of
		not_existed ->
			{error, ["not_existed"]};
		{ok, _} ->
			{ok, ["1"]};
		_ ->
			{error, ["0"]}
	end.
	

%% Parameters: group ID, user ID
%% Remove user groups, return success
remove_user(GId, UId) ->
	case dbcs_user:delete_group(GId, UId) of		
		{ok, _} ->
			{ok, ["1"]};
		_ ->
			{error, ["0"]}
	end.


%% Parameters: Group ID
%% User groups under the user browser to return to the list of users (ID, user name, Title, is disabled and the respective user group)
%% List_user (Id) -> [{UserId, UserName,}]
list_user(Id) ->
	dbcs_user:query_group(Id) .


% Parameters: None
%% View all users to return to the list of users (ID, user name, Title, is disabled and the respective user group)
browse_users() ->
	dbcs_user:browse_users().
	

% Parameters: None
%% View all does not belong to any users in the group, return to the list of users (ID, user name, Title, and is disabled)
browse_rawusers() ->
	dbcs_user:browse_rawusers().
	

%% Parameters: user ID
%% Query User Name
get_username(UId) ->
	case dbcs_user:get_name(UId) of		
		error ->
			{error, ["0"]};
		not_existed ->
			{error, ["not_existed"]};
		N ->
			{ok, [N]}
	end.
	
	
%% Parameters: user ID
%% Check user name, password and user groups. Returns: {UserName, Pass, GroupId} | error | not_existed | disabled
get_usernamepassword(UId) ->	
	case dbcs_user:get_userInfo(UId) of
		{ok, Infos} ->
			%io:format("get_usernamepassword: ~n", [Infos]),
			case proplists:get_value(ldapServer, Infos) of
				undefined ->
					user_validateByDB(Infos);
				[] ->
					user_validateByDB(Infos);
				LServer ->
					io:format("LDAP validata:: Server:~p UId: ~p~n", [LServer, UId]),
					user_validateByLDAP(LServer, Infos)				
			end;
		Other ->
			Other
	end.
	%dbcs_user:get_nameandpass(UId).
	
user_validateByDB(Infos) ->
	case proplists:get_value(disabled, Infos) of
		"1" ->
			disabled;
		_ ->
			{proplists:get_value(name, Infos), proplists:get_value(password, Infos), proplists:get_value(group, Infos)}
	end.		
	
user_validateByLDAP(LServer, Infos) ->
	Name = proplists:get_value(name, Infos),
	case ldapData_search(LServer, "sn", Name) of
		true ->
			case proplists:get_value(disabled, Infos) of
				"1" ->
					disabled;
				_ ->
					{Name, proplists:get_value(password, Infos), proplists:get_value(group, Infos)}
			end;
		false ->
			not_existed;
		_ ->
			error
	end.	
	
%% Parameters: user ID
%% User information
get_userInfo(UId) ->
	dbcs_user:get_userInfo(UId).
	
	
%% Parameters: user group ID
%% Query name of the group
get_usergroupname(GId) ->
	case dbcs_usergroup:get_name(GId) of		
		error ->
			{error, ["0"]};
		not_existed ->
			{error, ["not_existed"]};
		N ->
			{ok, [N]}
	end.
	
	
%% Parameters: user ID
%% Delete users, return success
%% delete_user(UId) -> ({ok,Result} | {error, Reason})
%% where
%%	UID = string()
%%	Result = atom()
%%	Reason = atom()
%% @doc delete user,UID is user's id.
delete_user(UId) ->
	
	case dbcs_user:get_userInfo(UId) of
		{ok, Infos} ->
			case proplists:get_value(ldapServer, Infos) of
				[] ->
					ok;
				LServer ->
					case proplists:get_value(ldapSecurity, Infos) of
						[] ->
							ok;
						LSecurity ->
							Ret = ldapData_delete(LServer, LSecurity),
							io:format("ldapData_delete: ~p ~n", [Ret])
					end		
			end;
		_ ->
			ok
	end,		
	
	case dbcs_user:delete_user(UId) of
		{ok, _} ->
			{ok, delete_user_ok};
		_ ->
			{error, delete_user_error}
	end.
	
%% Parameters: user ID and other information
%% Update the user, return success
update_user (Id, NewName, Infos) ->
% To be updated to determine whether the user name already exists
	case dbcs_user:get_userId(NewName) of		
		not_existed ->			
			update_user_0(Id, NewName, Infos);
		error ->
			{error, ["0"]};
		Id ->
			update_user_0(Id, NewName, Infos);
		_ExistedId ->
			{error, ["existed"]}
	end.

update_user_0(Id, Name, Infos) ->
	case proplists:get_value(ldapServer, Infos) of
		[LServer] ->
			case proplists:get_value(ldapSecurity, Infos) of
				[LSecurity] ->
					Ret = ldapData_update(Name, LServer, LSecurity),
					io:format("ldapData_update: ~p ~n", [Ret]);
				_ ->
					ok
			end;		
		_ ->
			ok
	end,
	
	case dbcs_user:update_user(Id, Infos) of
		{ok, _} ->
			{ok, ["1"]};
		_ ->
			{error, ["0"]}
	end.


% Parameters: None
%% Get all of the nodes, return the node information list [{Id, Name, Url}]
get_nodesInfo() ->
	dbcs_node:get_nodeInfos().

% Parameters: None
%% Get all of the group, return to group information list [{ParentId, {Id, Name}}]
get_groupsInfo() ->
	dbcs_node:get_groupInfos().


% Parameters: User Id, Device Id
% Query a user's authority to operate on a device, return the list of permissions
query_useropeareteRights(UId, DId) ->
	%Obtain UId user group belongs to Id, followed by the user group permissions
	case dbcs_user:get_nameandpass(UId) of
		{_UserName, _Password, GroupId} ->
			case dbcs_usergroup:get_rights(GroupId, DId) of
				{atomic, []} ->
					false; 
				{atomic, [{_, _, Rights}]} ->
					{ok, Rights};							
				_ ->
					{error, get_rights_failed}
			end;		
		Other ->
			{error, Other}
	end.		

check_permission("administrator",_,_)->true;
check_permission(UId,DId,Right)->
	case query_useropeareteRights(UId,all) of
		false->
			case query_useropeareteRights(UId,DId) of
				false->
					false;
				{ok,Rights}->
					case lists:member(Right,Rights) of
						true->
							true;
						_->
							false
					end;
				_->
					false
			end;
		{ok,Rights}->
			case lists:member(Right,Rights) of
				true->
					true;
				_->
					case query_useropeareteRights(UId,DId) of
						false->
							false;
						{ok,Rights}->
							case lists:member(Right,Rights) of
								true->
									true;
								_->
									false
							end;
						_->
							false
					end
			end;
		_->
			case query_useropeareteRights(UId,DId) of
				false->
					false;
				{ok,Rights}->
					case lists:member(Right,Rights) of
						true->
							true;
						_->
							false
					end;
				_->
					false
			end
	end.
	
% Parameters: User Id, Device Id
% Query a user of a device management authority, permission to return to the list
query_usermanageRights(UId, DId) ->
	case dbcs_user:get_nameandpass(UId) of
		{_UserName, _Password, GroupId} ->
			case dbcs_usergroup:get_rights(GroupId, DId) of
				{atomic, [_R]} ->
					Rigths = dbcs_usergroup:get_manageRights(GroupId),
					{ok, Rigths};
				_ ->
					false
			end;
		Other ->
			{error, Other}
	end.		


% Parameters: User Id
% Query a user's role permissions, permission to return to the list
query_userRoleRights(UId) ->
	case dbcs_user:get_nameandpass(UId) of
		{_UserName, _Password, GroupId} ->			
			Rigths = dbcs_usergroup:get_manageRights(GroupId),
			UsergroupEdit = case lists:member(usergroupEdit, Rigths) of
								true ->
									[usergroupEdit];
								false ->
									[]
							end,
			UserEdit = case lists:member(userEdit, Rigths) of
							true ->
								[userEdit];
							false ->
								[]
						end,
			{ok, UsergroupEdit++UserEdit};				
		Other ->
			{error, Other}
	end.		

% Parameters: User Group Id
% Query a user group role permission to return to the list of permissions
query_usergroupRoleRights("adminusergroup") ->
	[supervisor];
query_usergroupRoleRights(GId) ->		
	Rigths = dbcs_usergroup:get_manageRights(GId),
	UsergroupEdit = case lists:member(usergroupEdit, Rigths) of
						true ->
							[usergroupEdit];
						false ->
							[]
					end,
	UserEdit = case lists:member(userEdit, Rigths) of
					true ->
						[userEdit];
					false ->
						[]
				end,
	UsergroupEdit++UserEdit.
	
	
%% Data reset
reset() ->
	UserGroups = dbcs_usergroup:get_all_usergroup(),
	lists:foreach(fun({Id, _, _}) -> 
					dbcs_usergroup:delete_usergroup(Id)
				  end, UserGroups),
	Users = dbcs_user:browse_users(),
	lists:foreach(fun({Id, _, _, _,_}) ->
					dbcs_user:delete_user(Id)
				  end, Users),
	
	db_mnesia:drop(), % delete all rights tables
	db_mnesia:init(),
	
	dbcs_usergroup:init_maxId(),
	dbcs_user:init_maxId(),
	UGData = [{name, string, <<"admingroup">>}, 
			  {adminrights, list, term_to_binary(dbcs_usergroup:get_all_adminRs())}, 
			  {node, string, <<"all">>}],
	dbcs_usergroup:create_usergroup("adminusergroup", UGData),
	UData = [{name, string, <<"admin">>},
			 {password, string, <<"admin">>},
			 {title, string, <<"Administrator">>},
			 {group, string, <<"adminusergroup">>}],
	dbcs_user:create_user("administrator", UData),
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% GID of the form: "usergroup: 111:12212:11212", Rights of the form :[[[" 1.2.1 "],[" read "]],[[" 1.22.3 "],[" read, write_asl "]],...]
do(GId, Rights) ->
	RightList = [{list_to_atom(DId), Right} || [[DId], [Right]] <- Rights],
	case dbcs_usergroup:update_right_usergroup(GId, RightList) of
		{ok, _} ->
			{group, [{id, [GId]}, {ok, ["1"]}]};
		{error,content_empty} -> 
			{group, [{id, [GId]}, {error, ["not_existed"]}]};
		_ ->  
			{group, [{id, [GId]}, {error, ["badrpc"]}]}
	end.


updateMaxId(Id) ->
	case dbcs_usergroup:update_maxId(Id) of
		{ok, _} ->
			ok;
		{_E, _W} ->
			
			io:format("Update maxid failed::  ~p:~p~n", [_E, _W])
	end.
	

createRights(Id, RightStruct) ->
	case dbcs_usergroup:create_rights(Id, RightStruct) of
		{error, _W} ->
			
			io:format("Create rights table failed::  ~p~n", [_W]);
		_ ->
			ok
	end.	
	
	
ldapData_add(User, "ldap://"++LdapServer, LdapSecurity) ->	
	case string:tokens(LdapServer, ":") of
		[Host] ->
			case eldap_auth:start(Host) of
				{ok, Pid} ->
					io:format("LdapSecurity::  ~p User: ~p Host: ~p~n", [LdapSecurity, User, Host]),
					eldap_auth:add_user(LdapSecurity, User, Pid);
				E ->
					E
			end;		
		[Host, PortStr] ->
			Port =  try list_to_integer(PortStr)
					catch
						_:_ ->
							389
					end,	
			case eldap_auth:start(Host, Port) of
				{ok, Pid} ->
					eldap_auth:add_user(LdapSecurity, User, Pid);
				E ->
					E
			end
	end;
ldapData_add(_, _, _) ->
	false.
	
ldapData_update(User, "ldap://"++LdapServer, LdapSecurity) ->	
	case string:tokens(LdapServer, ":") of
		[Host] ->
			case eldap_auth:start(Host) of
				{ok, Pid} ->
					eldap_auth:modify_user(LdapSecurity, User, Pid);
				E ->
					E
			end;		
		[Host, PortStr] ->
			Port =  try list_to_integer(PortStr)
					catch
						_:_ ->
							389
					end,	
			case eldap_auth:start(Host, Port) of
				{ok, Pid} ->
					eldap_auth:modify_user(LdapSecurity, User, Pid);
				E ->
					E
			end
	end;
ldapData_update(_, _, _) ->
	false.	
	
ldapData_delete("ldap://"++LdapServer, LdapSecurity) ->	
	case string:tokens(LdapServer, ":") of
		[Host] ->
			case eldap_auth:start(Host) of
				{ok, Pid} ->
					eldap_auth:delete_user(LdapSecurity, Pid);
				E ->
					E
			end;		
		[Host, PortStr] ->
			Port =  try list_to_integer(PortStr)
					catch
						_:_ ->
							389
					end,	
			case eldap_auth:start(Host, Port) of
				{ok, Pid} ->
					eldap_auth:delete_user(LdapSecurity, Pid);
				E ->
					E
			end
	end;
ldapData_delete(_, _) ->
	false.		


ldapData_search("ldap://"++LdapServer, Name, Password) ->
	Server = string:tokens(LdapServer, ":"),
    if
        length(Server)==1 ->
            [Host|_] = Server,
            Port = 389;
		true ->
            Port = list_to_integer(lists:nth(2,Server)),
			[Host|_] = Server
    end,
    case eldap:open([Host], [{port,Port}]) of
        {ok, Pid} ->
            io:format("opend~n"),
            case eldap:simple_bind(Pid, ?LDAPUseName, ?LDAPPassword) of
				ok ->
                    io:format("first bind~n"),
                    DN = "mail="++ Name ++ ",ou=Users," ++ ?BaseDN,
                    Result = case eldap:simple_bind(Pid, DN, Password) of
                        ok ->
                            true;
                        R ->
                            io:format("sec bind~p~n",[R]),
                            false
                    end,
					eldap:close(Pid),
                    Result;
				_ ->
					eldap:close(Pid),
					false
			end;
		_ ->
            io:format("closed~n"),
			false
    end;
ldapData_search(_, _, _) ->
	false.
	
check_ldap_dn([]) ->	
	ok;
check_ldap_dn([DN]) ->	
	io:format("DN:~p~n", [DN]),
	case eldap:parse_dn(DN) of
		{ok, _} ->
			ok;
		_ ->
			error
	end;
check_ldap_dn(_) ->
	error.
	