%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(filter_monitorGenerator).
-export([createFilter/1]).

createFilter(FilterBean)->	
	Obj = filter_siteviewtree:new(),
	F = fun(X) ->
		{ok,{_, Filters}} = Obj:get_attribute(filters),
		Temp = lists:append(Filters, {X}),
		Obj:set_attribute(filters, Temp)
	end,
	lists:foreach(F, FilterBean),
	Obj.