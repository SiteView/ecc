%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(siteviewtree_generator,[BASE]).
-extends(siteview_object).
-compile(export_all).

%% -export([createHistoryReportObject/1, init/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for historyreport
new(TreeFilterID) ->
	Obj = siteview_object:new(),
	Obj:set_attribute(treeFilterID, TreeFilterID),	
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getTreeData() ->
	SSTreeObject =  ssTreeObject:new(),
	{ok,{_, TreeFilterID}} = THIS:get_attribute(treeFilterID),
	case TreeFilterID of
		"" ->
			siteviewtree_proxy:getSiteViewTreeModel();
		_->
			siteviewtree_proxy:getSiteViewTreeModel(TreeFilterID)
	end,
	SSTreeObject.