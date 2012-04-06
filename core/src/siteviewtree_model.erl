%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(siteviewtree_model,[BASE]).
-extends(siteview_object).
-compile(export_all).

%% -export([createHistoryReportObject/1, init/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for historyreport
new()->
	Obj = siteview_object:new(),
	Obj:set_attribute(filters,[]),	
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addFilter(Filter)->
	{ok,{_, Filters}} = THIS:get_attribute(filters),
	Temp = lists:append(Filters, Filter),
	THIS:set_attribute(filters, Temp).

acceptsNode(NodeConfig)->
	{ok,{_, Filters}} = THIS:get_attribute(filters),
	
	F = fun(X)->
		ok
	end,
	lists:foreach(F, Filters).