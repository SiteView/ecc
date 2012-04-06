%% @doc interface of simple user manager
%% @version{1.0}
%% @copyright 2009 dragonflow.com

-module(api_user_spl).
-include("user_spl.hrl").

-export([user_create/12,create_user/1,user_update/13,update_user/1,delete_user/1,browse_users/0,get_user/1]).

-export([user_validate/2,changePassword/2,changePassword/3]).

-export([check_permission/3,check_permission/2]).

-export([add_right/3,update_right/3,get_right/2,remove_right/2,check_right/3,remove_right/1]).

%% @spec user_create(Name, Password, LdapServer, LdapSecurity, Desc, Title, Disabled,Groups,Rights,Cpes,NT,Unix)->({ok,Result} | {error,Reason})
%% where
%%	Name = string()
%%	Password = string()
%%	LdapServer = string()
%%	LdapSecurity = string()
%%	Desc = string()
%%	Title = string()
%%	Disabled = string()
%%	Group = list()
%%	Rights = list()
%%	Cpes = list()
%%	NT  = list()
%%	Unix = list()
%%	Result = atom()
%%	Reason = atom()
%% @doc create a new user,return {ok,create_user_ok} is ok,else is error
%%
user_create(Name, Password, LdapServer, LdapSecurity, Desc, Title, Disabled,Groups,Rights,Cpes,NT,Unix) ->
	Data = [{name, Name},
				{password, Password},
				{ldapServer, LdapServer},
				{ldapSecurity, LdapSecurity},
				{title, Title},
				{desc, Desc},
				%%{email, Email},					
				{disabled, Disabled},
				{rights,{Groups,Rights}},
                {cpe,Cpes},
                {nt,NT},
                {unix,Unix}
			   ],
	create_user(Data).

user_create(AppName,Name, Password, LdapServer, LdapSecurity, Desc, Title, Disabled,Groups,Rights,Cpes,NT,Unix) ->
	Data = [{name, Name},
				{password, Password},
				{ldapServer, LdapServer},
				{ldapSecurity, LdapSecurity},
				{title, Title},
				{desc, Desc},
				%%{email, Email},					
				{disabled, Disabled},
				{rights,{Groups,Rights}},
                {cpe,Cpes},
                {nt,NT},
                {unix,Unix}
			   ],
	create_user(AppName,Data).

%% @spec user_update(Id,Name, Password, LdapServer, LdapSecurity, Desc, Title, Disabled,Groups,Rights,Cpes,NT,Unix)->({ok,Result} | {error,Reason})
%% where
%%	Name = string()
%%	Password = string()
%%	LdapServer = string()
%%	Desc = string()
%%	Title = string()
%%	Email = string()
%%	Disabled = string()
%%	Result = atom()
%%	Reason = atom()
%% @doc update user's infomation
%%
user_update(Id,Name, Password, LdapServer, LdapSecurity, Desc, Title, Disabled,Groups,Rights,Cpes,NT,Unix) ->
	Data = [
				{id,Id},
				{name, Name},
				{password, Password},
				{ldapServer, LdapServer},
				{ldapSecurity, LdapSecurity},
				{title, Title},
				{desc, Desc},
				%%{email, Email},					
				{disabled, Disabled},
				{rights,{Groups,Rights}},
                {cpe,Cpes},
                {nt,NT},
                {unix,Unix}
			   ],
	update_user(Data).

create_user(Data) when is_list(Data)->
	%%Name = proplists:get_value(name,Data),
    %%io:format("create user:~p~n",[Name]),
	%%case proplists:get_value(ldapServer,Data) of
		%%""->
			%%pass;
		%%LServer->
			%%case proplists:get_value(ldapSecurity,Data) of
				%%""->
					%%pass;
				%%LSecurity->
					%%Ret = api_usergroup:ldapData_add(Name, LServer, LSecurity),
					%%io:format("ldapData_add: ~p ~n", [Ret])
			%%end
	%%end,
	dbcs_user_spl:create_user(Data);   
create_user(_)->{error,parameter_error}.

create_user(App,Data) when is_list(Data)->
	dbcs_user_spl:create_user(app,App,Data); 
create_user(_,_)->{error,parameter_error}.

update_user(Data) when is_list(Data)->
	Name = proplists:get_value(name,Data),
	%%case proplists:get_value(ldapServer,Data) of
		%%""->
			%%pass;
		%%LServer->
			%%case proplists:get_value(ldapSecurity,Data) of
				%%""->
					%%pass;
				%%LSecurity->
					%%Ret = api_usergroup:ldapData_update(Name, LServer, LSecurity),
					%%io:format("ldapData_update: ~p ~n", [Ret])
			%%end
	%%end,
	dbcs_user_spl:update_user(Data);
update_user(_)->{error,parameter_error}.


delete_user(Id) when is_list(Id)->
	case dbcs_user_spl:get_user(Id) of
		{error,_}->
			{error,not_exist};
		Data->
			%%Name = proplists:get_value(name,Data),
			case proplists:get_value(ldapServer,Data) of
				""->
					pass;
				LServer->
					case proplists:get_value(ldapSecurity,Data) of
						""->
							pass;
						LSecurity->
							Ret = api_usergroup:ldapData_delete(LServer, LSecurity),
							io:format("ldapData_delete: ~p ~n", [Ret])
					end
			end,
			dbcs_user_spl:remove_user(Id)
	end;
delete_user(_)->{error,parameter_error}.

%% @spec user_validate(Name, Password) ->({ok, Info,GroupName} | {error,Reason} )
%% where
%%	Name = string()
%%	Password = string()
%%	Info = string()
%%	GroupName = string()
%%	Reason = atom()
%% @doc validate a user with name and password
user_validate(Name, Password) when is_list(Name) andalso is_list(Password) ->
    io:format("_________~p~n",[dbcs_user_spl:get_user_by_name(Name)]),
	case dbcs_user_spl:get_user_by_name(Name) of
		{ok, Infos} ->
			case proplists:get_value(ldapServer, Infos) of
				undefined ->
					user_validateByDB(Password, Infos);
				[] ->
					user_validateByDB(Password, Infos);	
				LServer ->
					io:format("LDAP validata:: Server:~p~n", [LServer]),
					api_usergroup:user_validateByLDAP(LServer, Name, Password, Infos)				
			end;
		{error,Err}->
			{error,Err};
		_ ->
			{error,database_error}
	end;
user_validate(_,_)->{error,parameter_invalid}.

user_validateByDB(Password, Infos) ->
	case proplists:get_value(password, Infos) of
		Password ->
			case proplists:get_value(disabled, Infos) of
				"1" ->
					{error,disabled};
				_ ->
					{ok,proplists:get_value(id, Infos), proplists:get_value(name, Infos)}
			end;
		_ ->
			{error,not_existed_or_password_error}
	end.

changePassword(UId, NewPassword)->
	Data = [
		{id,UId},
		{password,NewPassword}
	],
	dbcs_user_spl:update_user(Data).
	
get_user(Id)->
	dbcs_user_spl:get_user(Id).
	
get_all_users()->
	dbcs_user_spl:get_all().

browse_users()->
	case dbcs_user_spl:get_all() of
		{error,_}->
			[];
		All->
			[{proplists:get_value(id,X),proplists:get_value(name,X),proplists:get_value(title,X),proplists:get_value(disabled,X)} || X<-All]
	end.

%% @spec check_permission(UId,Group,Right)->( true | false )
%% where
%%	UId = string()
%%	Group = atom()
%%	Right = atom()
%% @doc check permission of user
check_permission(?ADMIN,_,_)->true;
%Just check the user permissions for the specified group
check_permission(UId,Group,null)->
	case dbcs_user_spl:get_user(UId) of
		{error,_}->
			false;
		User->
			{Groups,Rights} = 
            case proplists:get_value(rights,User) of
                undefined->
                    {[],[]};
                Else->
                    Else
            end,
			case lists:member("all",Groups) of
				true->
					true;
				_->
					check_group_in(Group,Groups)
			end
    end;
check_permission(UId,Group,Right)->
	case dbcs_user_spl:get_user(UId) of
		{error,_}->
			false;
		User->
			{Groups,Rights} = case proplists:get_value(rights,User) of
								undefined->
									{[],[]};
								Else->
									Else
							end,
			case lists:member(Right,Rights) of	
				true->
					case lists:member("all",Groups) of
						true->
							true;
						_->
							check_group_in(Group,Groups)
					end;
				_->
					false
			end
			
	end.

check_group_in(Group,Groups)->
	case lists:member(atom_to_list(Group),Groups) of
		true ->
			true;
		_->
			case api_group:find_object(Group) of
				[G|_]->
					case G:get_property(parent) of
						{ok,{_,PId}}->
							check_group_in(PId,Groups);
						_->
							false
					end;
				_->
					false
			end
	end.

%% @spec check_permission(UId,Right)->( true | false )
%% where
%%	UId = string()
%%	Right = atom()
%% @doc check permission of user
check_permission(?ADMIN,_)->true;
check_permission(UId,Right)->
	case get_user(UId) of
		{error,_}->
			false;
		User->
			case proplists:get_value(rights,User) of
				undefined->
					false;
				{_,Rights}->
					lists:member(Right,Rights)
			end
	end.
	
%% @spec add_right(UId,GId,Rights)->({ok,{UId,GId,Rights}} | {error,Reason} )
%% where
%%	UId = string()
%%	GId = atom()
%%	Rights = list()
%%	Reason = atom()
%% @doc grants rights of a object to user,UId is user id
add_right(UId,GId,Rights)->
	case dbcs_user_spl:add_right(UId,GId,Rights) of
		{ok,Ret}->
			{ok,{proplists:get_value(uid,Ret),proplists:get_value(gid,Ret),proplists:get_value(rights,Ret)}};
		Else->
			Else
	end.

%% @spec get_right(UId,GId)->({ok,{UId,GId,Rights}} | {error,Reason} )
%% where
%%	UId = string()
%%	GId = atom()
%%	Rights = list()
%%	Reason = atom()
%% @doc get rights
get_right(UId,GId)->
	case dbcs_user_spl:get_right(UId,GId) of
		{error,Err}->
			{error,Err};
		[Ret|_]->
			{ok,{proplists:get_value(uid,Ret),proplists:get_value(gid,Ret),proplists:get_value(rights,Ret)}};
		[]->
			{error,not_existed}
	end.

%% @spec remove_right(UId,GId)->({ok,Result} | {error,Reason} )
%% where
%%	UId = string()
%%	GId = atom()
%%	Result = atom()
%%	Reason = atom()
%% @doc remove user's rights 
remove_right(UId,GId)->
	dbcs_user_spl:remove_right(UId,GId).

%% @spec update_right(UId,GId,Rights)->({ok,{UId,GId,Rights}} | {error,Reason} )
%% where
%%	UId = string()
%%	GId = atom()
%%	Rights = list()
%%	Reason = atom()
%% @doc update user's rights 	
update_right(UId,GId,Rights)->
	case dbcs_user_spl:update_right(UId,GId,Rights) of
		{error,Err}->
			{error,Err};
		{ok,Ret}->
			{ok,{proplists:get_value(uid,Ret),proplists:get_value(gid,Ret),proplists:get_value(rights,Ret)}}
	end.
	
%% @spec check_right(UId,GId,Right)->(true | false)
%% where
%%	UId = string()
%%	GId = atom()
%%	Right = term()
%% @doc check right
check_right(?ADMIN,_,_)->true;
check_right(UId,GId,Right)->
	case get_right(UId,GId) of
		{ok,{_,_,Rights}}->
			lists:member(Right,Rights);
		Else->
			false
	end.
	
changePassword(UId, OldPassword,NewPassword)->
	case get_user(UId) of
		{error,Err}->
			{error,Err};
		User->
			case proplists:get_value(password,User) of
				OldPassword->
					Data = [
						{id,UId},
						{password,NewPassword}
					],
					dbcs_user_spl:update_user(Data);
				_->
					{error,invalid_old_password}
			end
	end.
	
%% @spec remove_right(UId)->({ok,Result} | {error,Reason} )
%% where
%%	UId = string()
%%	Result = atom()
%%	Reason = atom()
%% @doc remove user's rights 
remove_right(UId)->
	dbcs_user_spl:remove_right(UId).
	