%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(siteviewtree_proxy).
-export([getSiteViewTreeModel/0, getSiteViewTreeModel/1]).

getSiteViewTreeModel()->
	ok.

getSiteViewTreeModel(FilterID)->
	filter_monitorGenerator:createFilter(FilterID).
	