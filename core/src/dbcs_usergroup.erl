%%
%% @doc Based on the content store usergroup database module.
%%
%%
-module(dbcs_usergroup).
-compile(export_all).
%%-export([create_usergroup/1,get_usergroup/1,update_usergroup/1,remove_usergroup/1,get_next_id/1]).
-define(Table,"usergroup").
-define(MAXIDTable, "usergroupmaxid").
-define(AdminGroup, "adminusergroup").

-include("dbcs_common.hrl").

%% Temporary regulations in my database field in the variable type is string, except right outside for the tuple.


%% @doc Get the current largest user group ID
%% @spec get_maxId() -> Maxid | error
%% where
%%		Maxid = string()
get_maxId() ->
	Ret = db_ecc:get_data(?DBName, ?MAXIDTable, "id = usergroupmaxid"),
	
	case Ret of
		[] ->
			"usergroup0";
		[{content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance}] ->
			case lists:keysearch(maxid, 1, Advance) of		
				{value, {maxid, _, _Bin}} ->
					binary_to_list(_Bin);
				false ->
					"usergroup0"
			end;
		_ ->
			error
	end.


%% @doc Get the current largest user group ID
%% @spec get_maxIdByusergroup() -> Maxid | error
%% where
%%		Maxid = string()
get_maxIdByusergroup() ->
	Ret = db_ecc:get_data(?DBName, ?Table, "type = usergroup"),
	
	case is_list(Ret) of
		false ->
			error;
		true ->
			case length(Ret) of
				0 ->
					"usergroup0";
				_ ->
					CurIdList = [Id || {content, usergroup, Id, _, _, _, _, _, _, _, _, _, _, _, _} <- Ret],
					MaxId = lists:max(CurIdList),
					atom_to_list(MaxId)
			end
	end.
	
	
%% @doc Update the current largest user group ID
%% @spec update_maxId(MaxId) -> {error,Reason} | {badrpc,Reason} | {ok,Result} 
%% where
%%		Maxid = string()
update_maxId(MaxId) ->	
	Advance = [{maxid, string, list_to_binary(MaxId)}],
	NewRecord = {content, list_to_atom(?MAXIDTable), list_to_atom(?MAXIDTable), list_to_binary(?MAXIDTable), null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:update_data(?DBName, ?MAXIDTable, "id = "++?MAXIDTable, NewRecord).
	
	
%% @doc Initialize the largest user group ID
%% @spec init_maxId() -> {error,Reason} | {badrpc,Reason} | {ok,Result} 
%% where
%%		Maxid = string()	
init_maxId() ->	
	Advance = [{maxid, string, <<"usergroup0">>}],
	NewRecord = {content, list_to_atom(?MAXIDTable), list_to_atom(?MAXIDTable), list_to_binary(?MAXIDTable), null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:insert_data(?DBName, ?MAXIDTable, NewRecord).


%% @doc User groups to determine whether there is
%% @spec get_usergroup(Name) -> not_existed | existed | error
%% where
%%		Name = string()
get_usergroup(Name) ->
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

%% @doc User groups to determine whether there is, if there is return ID
%% @spec get_usergroupId(Name) -> not_existed | ID | error
%% where
%%		Name = string()
%%		ID = string()
get_usergroupId(Name) ->
	Where = "my.name = '" ++ Name ++ "'",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, usergroup, Id, _, _, _, _, _, _, _, _, _, _, _, _}] ->
			atom_to_list(Id);
		_ ->
			error
	end.
	
%% @doc Create a new user group
%% @spec create_usergroupByName(Id, Name) -> {error,Reason} | {badrpc,Reason} | {ok,Result}
%% where
%%		Id = string()
%%		Name = string()	
%%		Result = tuple()
create_usergroupByName(Id, Name) ->
	Advance = [{name, string, list_to_binary(Name)}],  % When first built, user groups, my field right and node does not exist. To be added later
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"usergroup">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:insert_data(?DBName, ?Table, NewRecord).

create_usergroup(Id, MyData) ->	
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"usergroup">>, null, null, null, null, 
					<<"zhangyan">>, null, null, null, null, null, MyData},
	db_ecc:insert_data(?DBName, ?Table, NewRecord).

%% Create table and insert permission permission
%% Gids = list(),eg:["id1","id2"]
create_rights(Id, {Gids, Rights}) ->
	Table = list_to_atom(Id),
	Rights_data = [{Table, list_to_atom(Gid), Rights} || Gid <- Gids],
	case db_mnesia:create(Table, [id, rights]) of
		{atomic, ok} ->
			db_mnesia:insert_data(Rights_data); % Successful return: {atomic, ok}
		_W ->
			{error, _W}
	end;
create_rights(_Id, _) ->
	{atomic, ok}.
	
	
%% Update permissions table
%% @spec update_rights(Id, {Gids, Rights}) -> {atomic, ok} | {aborted, Reason}
update_rights(Id, {Gids, Rights}) ->
	Table = list_to_atom(Id),
	Rights_data = [{Table, list_to_atom(Gid), Rights} || Gid <- Gids],
	case db_mnesia:clear_data(Table) of
		{aborted, {no_exists, _}} ->
			db_mnesia:create(Table, [id, rights]);
		_ ->	
			ok
	end,
	db_mnesia:insert_data(Rights_data);
update_rights(_Id, _) ->
	{atomic, ok}.
	

%% @doc Query access table
%% @spec get_rights(Id, DId) -> {atomic, Val} | {aborted, Reason}
get_rights(Id, DId) ->
	Table = list_to_atom(Id),
	db_mnesia:read_data(Table, DId).


%% @doc Query user group management rights
%% @spec get_manageRights(Id) -> [] | [Rights]
get_manageRights(Id) ->
	Where = "id = " ++ Id,
	case db_ecc:get_data(?DBName, ?Table, Where) of
		[{content, usergroup, _, _, _, _, _, _, _, _, _, _, _, _, _Ad}] ->
			case lists:keysearch(adminrights, 1, _Ad) of
				{value, {adminrights, _, _BinRights}} ->
					try binary_to_term(_BinRights)
					catch
						_ : _ -> ""
					end;
				_ ->
					[]
			end;
		_ ->
			[]
	end.
	

%% @doc Modify the group name and node information
%% @spec update_usergroup(Id, NewName, NewNode) -> {error,Reason} | {badrpc,Reason} | {ok,Result}
%% where
%%		Id = string()
%%		NewName = string()
%%		NewNode = string()	
%%		Result = tuple()
update_usergroup(Id, NewName, NewNode) ->	
	Where = "id = " ++ Id,		
	% To extract information from the database AddminRights
	AddminRights = case db_ecc:get_data(?DBName, ?Table, Where) of
						[{content, usergroup, _, _, _, _, _, _, _, _, _, _, _, _, _Ad}] ->
							case lists:keysearch(adminrights, 1, _Ad) of
								{value, RightStruct} ->
									[RightStruct];
								_ ->
									[]
							end;
						_ ->
							[]
					end,
	Advance = [{name, string, list_to_binary(NewName)},
				{node, string, list_to_binary(NewNode)}
				] ++ AddminRights,	
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"usergroup">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:update_data(?DBName, ?Table, Where, NewRecord).

%% @doc Modify the group name, node information and management rights
%% @spec update_usergroup(Id, NewName, NewNode, NewAdminRs) -> {error,Reason} | {badrpc,Reason} | {ok,Result}
%% where
%%		Id = string()
%%		NewName = string()
%%		NewNode = string()	
%%		NewAdminRs = list(),eg:[groupAccess,monitorAccess]
%%		Result = tuple()
update_usergroup(Id, NewName, NewNode, NewAdminRs) ->	
	Where = "id = " ++ Id,		
	Advance = [{name, string, list_to_binary(NewName)},
				{node, string, list_to_binary(NewNode)},
				{adminrights, list, term_to_binary(NewAdminRs)}
				],	
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"usergroup">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:update_data(?DBName, ?Table, Where, NewRecord).
	
	
%% @doc Modify the group name
%% @spec update_usergroup(Id, NewName) -> not_existed | {ok,Result} | {error,Reason} |{badrpc,Reason} | error
%% where
%%		Id = string()
%%		NewName = string()	
%%		Result = tuple()
update_usergroup(Id, NewName) ->	
	Where = "id = " ++ Id,	
	%% Start with the database to extract the user group right, node
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	case Ret of
		[] ->
			not_existed;
		[{content, usergroup, _, _, _, _, _, _, _, _, _, _, _, _, _Ad}] ->
			NewAd = proplists:delete(name, _Ad) ++ [{name, string, list_to_binary(NewName)}],
			NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"usergroup">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, NewAd},
			db_ecc:update_data(?DBName, ?Table, Where, NewRecord);
		_ ->
			error
	end.
	

%% @doc Delete a user group
%% @spec delete_usergroup(Id) -> {error,Reason} | {badrpc,Reason} | {ok,deleted} 
%% where
%%		Id = string()
delete_usergroup(Id) ->
	Where = "id = " ++ Id,
	db_ecc:delete_data(?DBName, ?Table, Where).


%% @doc Browse all user groups (group Id, group name)
%% @spec get_all_usergroup() -> Groups 
%% where
%%		Groups = list() ,eg:[{Id, Name, Role}] | []
get_all_usergroup() ->
	Where = "type = usergroup",
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	case is_list(Ret) of
		false ->
			[];
		true ->
			[extract_userinfo(Id, Advance) || {content, usergroup, Id, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret, is_atom(Id)] 			
	end.

%% @doc Get the user group name
%% @spec get_name(Id) -> UserGroupName | error | not_existed
%% where
%%		UserGroupName = string()
get_name(Id) ->
	Where = "id = " ++ Id,	
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of
		[] ->
			not_existed;
		[{content, usergroup, _, _, _, _, _, _, _, _, _, _, _, _, Ad}] ->
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


%% @doc Check the operation of a user group permissions
%% @spec queryrightOfOneUsergroup(Id) -> Rights | {error,query_table_failed}
%% where
%%		Id = string()
%%		Rights = list() ,eg:[{usergroupid, groupid, Rights}] | []
queryrightOfOneUsergroup(Id) ->
	Table = list_to_atom(Id),	
	case db_mnesia:read_all(Table) of
		{atomic, Val} ->
			Val; 
		_ ->
			{error,query_table_failed}
	end.


%% @doc Query a user group information (name, adminrights, node)
%% @spec queryInfo_Usergroup(Id) -> Infos
%% where
%%		Id = string()
%%		Infos = list() ,eg:[Info] | []
queryInfo_Usergroup(Id) ->
	Where = "type = usergroup & id = " ++ Id,
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of		
		[Group] ->
			db_to_group(Group);  %%[{id,'1'},{adminrights, [access_group,access_monitor]},{name,"xxx"},{node,"node1,node2"}]
		_ ->
			[]
	end.
	
	
%% @doc Check all (operations) access
%% @spec get_all_operateRs() -> Rights 
%% where
%%		Rights = list()
get_all_operateRs() ->
	[
	groupEdit,
	groupRefresh,
	groupDisable,
	monitorEdit,
	monitorRefresh,
	monitorAcknowledge,
	monitorDisable,
	monitorMonTools,
	monitorGenTools,
	alertViewList,
	alertEdit,
	alertTest,
	alertIndDisable,
	alertTemDisable,
	alertViewHistory,
	alertCreateReports,
	reportGenerate,
	reportShow,
	reportEdit,
	reportCreate,
	reportDisable,
	reportView,
	preferenceEdit,
	preferenceTest,
	edit_multiView,
	use_BrowseAndSummary,
	view_progress,
	view_logs,
	view_healthPage,
	edit_healthParameters,
	disable_HealthMonitors,
	use_supportTool
	].


%% @doc Check all the (management) rights
%% @spec get_all_adminRs() -> Rights 
%% where
%%		Rights = list()
get_all_adminRs() ->
	[groupAccess, monitorAccess, alertAccess, reportAccess, preferenceAccess, otherAccess, usergroupEdit, userEdit].

%% @doc Modify a user group
%% @spec update_right_usergroup(Id, Rights)-> {error,Reason} | {ok,Result}
%% where
%%		Id = string()
%%		Rights = list(), eg: [{'1.2.1',"read"},{'1.22.3',"read,write_asl"}]
update_right_usergroup(Id, Rights) ->
	Where = "id = " ++ Id,	
	Advance = [{right, tuple, term_to_binary(Rights)}],	% Note: Rights must be included in the user group permissions of all the equipment, otherwise it will overwrite the permissions of equipment before
	NewRecord = {content, list_to_atom(?Table), list_to_atom(Id), <<"usergroup">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, Advance},
	db_ecc:update_data(?DBName, ?Table, Where, NewRecord).


%% @doc Returns a user group and node information
%% @spec get_rightandnodeinfo(Id) -> Infos
%% where
%%		Id = string()
%%		Infos = list() 
%% Error or the user group does not exist are back [], the user group does not exist return [[], NodeInfo], the node does not exist return [RightInfo ,[]], exist in return [RightInfo, NodeInfo] .
get_rightandnodeinfo(Id) -> 
	Where = "type = usergroup & id = " ++ Id,
	Ret = db_ecc:get_data(?DBName, ?Table, Where),
	
	case Ret of		
		[Group] ->
			InfoList = db_to_group(Group),  %%[{id,'1'},{name,"xxx"},{right,[{'1.2.1',"read"},...]},{node,[{'id1',"name1","url"},...]}]
			RightInfo = case lists:keysearch(right, 1, InfoList) of		
							{value, {right, _Right}} ->
								_Right;
							false ->
								[]
						end,
			NodeInfo = case lists:keysearch(node, 1, InfoList) of		
							{value, {node, _Node}} ->
								_Node;
							false ->
								[]
						end,
			[RightInfo, NodeInfo];
		_ ->
			[]
	end.
	

% Combine multiple user group and node information (temporarily assume that each user group list deviceid not repeated.)
merge_rightandnode(List) ->
	[R, N] = do(List,[]), % Get all the permissions and node list
	
	% Merge permissions
	[HeadR | RestSortR] = lists:keysort(1, R),
	Rights = dealwithR(RestSortR, HeadR),
	
	% Merge Node
	[HeadN | RestSortN] = lists:keysort(1, N),
	Nodes = dealwithN(RestSortN, HeadN),
	
	[Rights, Nodes].


do([[R, N]|Rest], []) ->	
	do(Rest, [R, N]);
do([[R, N]|Rest], [OldR, OldN]) ->
	do(Rest, [OldR ++ R, OldN ++ N]);
do([], Acc) ->
	Acc.
	
% Rights for information, permission to merge mainly
dealwithR(SortR, Pivot) ->
	dealR(SortR, Pivot, []).
	
% Processing node information, mainly to remove duplicate nodes
dealwithN(SortN, Pivot) ->
	dealN(SortN, Pivot, []).
	
% Parameters: The first argument to be processed the List, the second parameter is the reference value, the third parameter to the current results
dealR([{P, V}|_R], {P, OldV}, A) ->
	RightList1 = string:tokens(V, ","),
	RightList2 = string:tokens(OldV, ","),
	RightList = merge(RightList1, RightList2),
	RigthStr = string:join(RightList, ","),
	dealR(_R, {P, RigthStr}, A);
dealR([{K, V}|_R], {P, OldV}, A) ->
	dealR(_R, {K, V}, A ++ [{P, OldV}]);
dealR([], V, A) ->
	A ++ [V].

% (Assuming the nodes correspond to the information ID and Name)
dealN([{P, _, _}|_R], {P, N, U}, A) ->
	dealN(_R, {P, N, U}, A);
dealN([{K, _N, _U}|_R], {P, N, U}, A) ->
	dealN(_R, {K, _N, _U}, A ++ [{P, N, U}]);
dealN([], V, A) ->
	A ++ [V].

merge([H|R], Raw) ->	
	case lists:member(H, Raw) of
		true ->
			%io:format("true ~p ~n", [H]),
			merge(R, Raw);
		false ->
			%io:format("false ~p ~n", [H]),
			merge(R, Raw ++ [H])
	end;
merge([], Raw) ->
	Raw.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_next_id() ->
	% Start with the library to extract the maximum of the ID value (Note: id '0 'of the record used to record the ID of the current maximum value)
	Ret = db_ecc:get_data(?DBName, ?Table, "type=usergroup & id=0"),
	case Ret of			
		[{content, usergroup, '0', _, _, _, _, _, _, _, _, _, _, _, [{maxid, number, Bin}]}] ->
			NewId_I = list_to_integer(binary_to_list(Bin)) + 1, 	% New ID is equal to the maximum of the ID plus 1
			NewId = integer_to_list(NewId_I),
			Advance = [{maxid, number, list_to_binary(NewId)}],
			MaxIDRecord = {content, list_to_atom(?Table), '0', <<"usergroup">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, Advance},
			db_ecc:update_data(?DBName, ?Table, "type=usergroup & id=0", MaxIDRecord),
			{ok, NewId};
		[] ->
			NewId = "1",
			Advance = [{maxid, number, <<"1">>}],
			MaxIDRecord = {content, list_to_atom(?Table), '0', <<"usergroup">>, null, null, null, null, <<"zhangyan">>, null, null, null, null, null, Advance},
			db_ecc:insert_data(?DBName, ?Table, MaxIDRecord),
			{ok, NewId};
		_ ->
			{error, "1"}
	end.


% Extract a user group name and Role
extract_userinfo(adminusergroup, Ad) ->	
	Role = "supervisor",
	case lists:keysearch(name, 1, Ad) of		
		{value, {name, _, NameBin}} ->
			NameStr = try binary_to_list(NameBin)
					  catch
						_ : _ -> ""
					  end,
			{"adminusergroup", NameStr, Role};
		false ->
			{"adminusergroup", "", Role}
	end;
extract_userinfo(Id, Ad) ->	
	AdminrightsList = case lists:keysearch(adminrights, 1, Ad) of		
					{value, {adminrights, _, Bin}} ->
						try binary_to_term(Bin)
						catch
							_ : _ -> ""
						end;
					false ->
						""
				end,
	UsergroupEdit = case lists:member(usergroupEdit, AdminrightsList) of
						true ->
							["usergroupEdit"];
						false ->
							[]
					end,
	UserEdit = case lists:member(userEdit, AdminrightsList) of
					true ->
						["userEdit"];
					false ->
						[]
				end,			
	Role = string:join(UsergroupEdit++UserEdit, ","),
	case lists:keysearch(name, 1, Ad) of		
		{value, {name, _, NameBin}} ->
			NameStr = try binary_to_list(NameBin)
					  catch
						_ : _ -> ""
					  end,
			{atom_to_list(Id), NameStr, Role};
		false ->
			{atom_to_list(Id), "", Role}
	end.		

extract_rightinfo(List) ->
	%{value, {id, Id}} = lists:keysearch(id, 1, List), %usergroupId:atom
	case lists:keysearch(right, 1, List) of		
		{value, {right, Rights}} ->
			RightsInfo = [{device, [{id, [atom_to_list(DId)]}, {right, [Right]}]} || {DId, Right} <- Rights],
			{right, RightsInfo};
		false ->
			% User group information does not exist right
			{right, []}
	end.
	
extract_nodeinfo(List) ->
	%{value, {id, Id}} = lists:keysearch(id, 1, List), %Id:atom
	case lists:keysearch(node, 1, List) of		
		{value, {node, Nodes}} ->
			NodesInfo = [{node, [{id, [atom_to_list(NId)]}, {name, [Name]}, {url, [Url]}]} || {NId, Name, Url} <- Nodes],
			{nodes, NodesInfo};
		false ->
			% User group information in the node does not exist
			{nodes, []}
	end.
	
% The user group database structure into a List, return the group id, and other structures List
db_to_group({content, usergroup, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}) ->
	[{id,Id}] ++ [dbcs_base:db2term(K, T, V) || {K, T, V} <- Advance];
db_to_group(_) ->
	[].
	
db_to_group(Id, Advance) ->
	[{id,Id}] ++ [dbcs_base:db2term(K, T, V) || {K, T, V} <- Advance].	
	
extract_usergroupdata([], Result) ->
	Result;
extract_usergroupdata([{content, usergroup, Id, _, _, _, _, _, _, _, _, _, _, _, Advance}|RestGroups], Result) ->	
	% Extract data of a user group
	case lists:keysearch(name, 1, Advance) of		
		{value, {name, string, _Bin}} ->
			Name = binary_to_list(_Bin);
		false ->
			Name = ""
	end,			
	Item = {group, [{id, [atom_to_list(Id)]}, {name, [Name]}]},
	extract_usergroupdata(RestGroups, [Item|Result]);  % Note: the returned list and taken out of the user group record in reverse order
extract_usergroupdata(_, _) ->
	[].
	