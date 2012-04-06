%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(ssRunTimeMonitor,[BASE]).
-extends(ssMonitor).
-compile(export_all).

%% -export([createHistoryReportObject/1, init/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for historyreport
new()->
	Obj = ssMonitor:new(),
	Obj:set_attribute(metaData, []),	
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
