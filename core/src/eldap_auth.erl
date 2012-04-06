-module(eldap_auth).

-compile(export_all).

-include("../include/eldap.hrl").

%-define(LDAP_SEARCH_TIMEOUT, 5). % Timeout for LDAP search queries in seconds
-define(BaseDN, "o=tcl,c=cn").
-define(RootDN, "cn=manager,o=tcl,c=cn").
-define(Passwd, "secret").

start(Host) ->	
	do_open_bind(Host, [{timeout, 5000}], ?RootDN, ?Passwd).
	
start(Host, Port) ->    
	Opts = [{port, Port}, {timeout, 5000}],
	do_open_bind(Host, Opts, ?RootDN, ?Passwd).

do_open_bind(Host, Opts, RootDN, Passwd) ->    
    case eldap:open([Host], Opts) of
		{ok, Handle} ->
			{eldap:simple_bind(Handle, RootDN, Passwd), Handle};
		R ->
			R  % {error,"connect failed"}
	end.

%% @spec is_user_exists(User, Server) -> true | false | {error, Error}
%% 区分大小写
is_user_exists(Attribute, Value, Server) ->
	Base = {base, ?BaseDN},
	Scope = {scope, eldap:wholeSubtree()},
	Filter = {filter, eldap:equalityMatch(Attribute, Value)},
	Search = [Base, Scope, Filter],
	case eldap:search(Server, Search) of		
		{ok, {eldap_search_result, [], _}} ->
			false;
		{ok, {eldap_search_result, EntryList, _}} ->			
			parse_Entry(Attribute, Value, EntryList);
		Error ->
			{error, Error}
	end.		
	
parse_Entry(Attribute, Value, [#eldap_entry{attributes  = Attrs}|Rest]) ->
	%io:format("Attrs:~p~n", [Attrs]),
	case proplists:get_value(Attribute, Attrs) of
		[Value] ->
			true;
		_ ->
			parse_Entry(Attribute, Value, Rest) 
	end;
parse_Entry(_, _, []) ->
	false.
	
	
%% @spec add_user(DN, User, Server) -> ok | false | {error, Error}	
add_user(DN, User, Server) ->
	case DN of
		"uid="++Rest ->
			Uid = string:substr(Rest, 1, string:str(Rest, ",")-1),
			Attris = [{"uid", [Uid]}, {"objectClass", ["inetOrgPerson"]}, {"sn", [User]}, {"cn",[User]}],
			eldap:add(Server, DN, Attris);
		_ ->
			false
	end.

%% @spec modify_user(User, Server) -> ok | false | {error, Error}
%% 修改指定Uid的SN属性
modify_user(DN, User, Server) ->
	%io:format("DN:~p User:~p ~n", [DN, User]),
	case DN of
		"uid="++Rest ->
			Uid = string:substr(Rest, 1, string:str(Rest, ",")-1),
			case is_user_exists("uid", Uid, Server) of
				false -> %add
					Attris = [{"uid", [Uid]}, {"objectClass", ["inetOrgPerson"]}, {"sn", [User]}, {"cn",[User]}],
					eldap:add(Server, DN, Attris);
				true -> %modify
					M = eldap:mod_replace("sn", [User]),
					eldap:modify(Server, DN, [M]);
				Other ->
					Other
			end;			
		_ ->
			false
	end.
	
%% @spec delete_user(User, Server) -> ok | false | {error, Error}	
delete_user(DN, Server) ->
	case DN of
		"uid="++_ ->
			eldap:delete(Server, DN);
		_ ->
			false
	end.

stop(Server) ->
	eldap:close(Server).