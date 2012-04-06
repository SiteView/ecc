
-module(dbcs_node).
-compile(export_all).
%%-export([create_usergroup/1,get_usergroup/1,update_usergroup/1,remove_usergroup/1,get_next_id/1]).
-define(Table,"node").

-include("dbcs_common.hrl").



%% @spec get_nodeInfos() -> InfoList | error
%% where
%%		InfoList = [] | [Info]
%%		Info = {NodeId, NodeName, Url}
get_nodeInfos() ->
	Ret = db_ecc:get_data(?DBName, ?Table, ""),
	
	case is_list(Ret) of
		false ->
			error;
		true ->			
			[getNodeInfo(atom_to_list(Id), Advance) || {content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
	end.
	
getNodeInfo(Id, MyData) ->		
	NodeName = case lists:keysearch(name, 1, MyData) of
				{value, {name, _, null}} ->			
					"";		
				{value, {name, _, BinName}} ->			
					binary_to_list(BinName);
				false ->
					""
			   end,
	Url = case lists:keysearch(url, 1, MyData) of
			{value, {url, _, null}} ->			
				"";		
			{value, {url, _, BinUrl}} ->			
				binary_to_list(BinUrl);
			false ->
				""
		  end,
	{Id, NodeName, Url}.




%% @spec get_groupInfos()-> error| Groups
%% where
%%		Groups = [] | [GroupInfo]
%%		GroupInfo = {parentId, {Id, Name}}
get_groupInfos()->
	Ret = db_ecc:get_data(?DBName, "group", ""),
	
	case is_list(Ret) of
		false ->
			error;
		true ->			
			[getGroupInfo(atom_to_list(Id), Advance) || {content, _, Id, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret]
	end.
	
getGroupInfo(Id, MyData) ->
	ParentId = case lists:keysearch(parent, 1, MyData) of
				{value, {parent, _, null}} ->			
					"";		
				{value, {parent, _, BinParent}} ->			
					binary_to_list(BinParent);
				false ->
					""
			   end,
	GroupName = case lists:keysearch(name, 1, MyData) of
				{value, {name, _, null}} ->			
					"";		
				{value, {name, _, BinName}} ->			
					binary_to_list(BinName);
				false ->
					""
			   end,
	{ParentId, {Id, GroupName}}.