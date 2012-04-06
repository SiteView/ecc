%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(ssMonitorGroup,[BASE]).
-extends(ssGroup).
-compile(export_all).

%% -export([createHistoryReportObject/1, init/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for historyreport
new()->
	Obj = ssGroup:new(),
	Obj:set_attribute(metaData, []),
	Obj:set_attribute(unsupportedActions, []),
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
