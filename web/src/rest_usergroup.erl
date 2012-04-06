%% ---
%%user group rest interface
%%
%%---

-module(rest_usergroup).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xs,[value_of/1,select/2,built_in_rules/2]).


parse_simplexml(XmlStr) ->
	Options = [{space, normalize}, {encoding, "utf-8"}],
	{XmlE, _} = xmerl_scan:string(XmlStr, Options),
	template(XmlE).
	
parse_complexxml(XmlStr) ->
	Options = [{space, normalize}, {encoding, "utf-8"}],
	{XmlE, _} = xmerl_scan:string(XmlStr, Options),
	#xmlElement{content=Content} = XmlE,
	[template(Item)|| Item<- Content].
	
template(E = #xmlElement{name = 'params'}) ->	
	[    
		value_of(select(".",E))
	];
template(E) -> built_in_rules(fun template/1, E).

construct_simplexmlString(TupleStruct, Prolog) ->
	lists:flatten(xmerl:export_simple([TupleStruct], xmerl_xml, [{prolog, Prolog}])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface functions
%%

%% create user
func_user_create(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get username,password,desc,email
			[Params] = parse_simplexml(Body),  %Params£º["name","password","desc","email"]
			[Name, Password, Desc, Email] = Params,
			%Name = "admin",
			Ret = api_usergroup:user_create(Name, Password, Desc, Email), 
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><user><name></name><nodes></nodes><gright></gright></user></response>"
			web_common:respond(Result)
	end.
	
	
%% user login
func_user_login(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get username,password
			[Params] = parse_simplexml(Body),  %Params£º["name","password"]
			[Name, Password] = Params,
			%Name = "admin",
			Ret = api_usergroup:user_login(Name), 
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><user><name></name><nodes></nodes><gright></gright></user></response>"
			web_common:respond(Result)
	end.


%% create user group
func_usergroup_add(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get username
			[Params] = parse_simplexml(Body),  %Params£º["groupname"]
			[Name] = Params,
			%Name = "usergroup1",
			Ret = api_usergroup:create(Name), %Ret£º{ok, ["id1"]} {error, ["badrpc"]}
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>id1</ok></response>"
			web_common:respond(Result) 		
	end.

%%updata user group
func_usergroup_update(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get usergroup id,name
			[Params] = parse_simplexml(Body),  %Params£º["groupid","groupname"]
			[Id, Rname] = Params,
			%Id = "11", Rname = "usergroup1_1",
			Ret = api_usergroup:update(Id, Rname),%Ret£º{ok, ["1"]}»ò{error, ["badrpc"]}, "1" success
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>1</ok></response>"
			web_common:respond(Result) 		
	end.	
	
%%delete user group
func_usergroup_delete(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get usergroup id
			[Params] = parse_simplexml(Body),  %Params£º["groupid"]
			[Id] = Params,
			%Id = "11", 
			Ret = api_usergroup:delete(Id),%Ret£º{ok, ["1"]} , "1" success
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>1</ok></response>"
			web_common:respond(Result) 		
	end.
	
%%browse user group
func_usergroup_list(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%% don't parse body if parameter is empty
			%[Params] = parse_simplexml(Body), %Params£º[""]
			Ret = api_usergroup:get_all_group(),%Ret£º{groups, [{group, [{id, ["11"]}, {name, ["usergroup1"]}]},...]}
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><groups><group><id>11</id><name>usergroup1</name></group></groups></response>"
			web_common:respond(Result) 		
	end.
	
%%query right of one user group 
func_groupright_query(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get usergroup id
			[[Id]] = parse_simplexml(Body),  %Params£º["id1"]
			Ret = api_usergroup:get_right(Id),%Ret£º{right, [{device, [{id, ["11"]}, {right, ["read,write"]}]},...]}
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), 
			web_common:respond(Result) 		
	end.
	

%% update user group right(multi-group)
func_groupright_update(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get user group id,rights
			Trans = parse_complexxml(Body),  
			Ret = api_usergroup:update_right(Trans),%Ret£º[{group, [{id, ["id1"]}, {ok, ["1"]}]}]
			%%return result (XML)
			Data = {response, Ret},
			Result = construct_simplexmlString(Data, []), 
			web_common:respond(Result) 		
	end.
	
%%add user( for one specified group)
func_user_add(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse Body,get user id and usergroup id
			[Params] = parse_simplexml(Body),  %Params£º["groupid","userid"]
			[GId, UId] = Params,
			Ret = api_usergroup:add_user(GId, UId), %Ret£º{ok, ["1"]} , "1" success
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>1</ok></response>"
			web_common:respond(Result) 		
	end.
	
%%remove user( for one specified group)
func_user_remove(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse  Body,get usergroup id and user id
			[Params] = parse_simplexml(Body),  %Params£º["groupid","userid"]
			[GId, UId] = Params,
			Ret = api_usergroup:remove_user(GId, UId), %Ret£º{ok, ["1"]} , "1" success
			%%return result (XML)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>1</ok></response>"
			web_common:respond(Result)	
	end.

%%browse user( for one specified group)
func_user_list(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse body ,get usergroup id and user id
			[[Id]] = parse_simplexml(Body), 
			%Id = "11", 
			Ret = api_usergroup:list_user(Id), %Ret£º{users, [{user, [{id, ["id1"]}, {name, ["name1"]}]}]} 
			%%return result (xml)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), 
			web_common:respond(Result) 		
	end.
	
%%browse all rights  ???
func_rights_all(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parameter is empty			
			Ret = api_usergroup:get_all_rights(), 
			
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []),
			web_common:respond(Result) 		
	end.
	
%% browse all users
func_users_browse(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%have not parameter 			
			Ret = api_usergroup:browse_users(),%Ret£º{ok, ["1"]} , "1" success
			%%return result(xml)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>1</ok></response>"
			web_common:respond(Result) 		
	end.
	
%% delete user
func_users_delete(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse body get usergroup id and user id 
			[[Id]] = parse_simplexml(Body),  
			%Id = "11", 
			Ret = api_usergroup:delete_user(Id),%Ret£º{ok, ["1"]} , "1" success
			%%return result (xml)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>1</ok></response>"
			web_common:respond(Result) 		
	end.
	
%% update user info ???
func_users_update(Host,_Req,Path,Raw_path) ->
	case _Req:recv_body() of
		"" ->
			web_common:respond(unknown);
		Body ->
			%%parse body get user id and other info
			[Params] = parse_simplexml(Body),  %Params£º["id","name","desc","email","...",...]
			%[Id|Rest] = Params,
			Ret = api_usergroup:update_user(Params),%Ret£º{ok, ["1"]} , "1" success
			%%return result(xml)
			Data = {response, [Ret]},
			Result = construct_simplexmlString(Data, []), %Result£º"<response><ok>1</ok></response>"
			web_common:respond(Result) 		
	end.
