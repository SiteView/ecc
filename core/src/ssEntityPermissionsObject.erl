%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historyreport
-module(ssEntityPermissionsObject,[BASE]).
-extends(siteview_object).
-compile(export_all).

%% -export([createHistoryReportObject/1, init/2]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for historyreport
new()->
	Obj = siteview_object:new(),
	Obj:set_attribute(entityNotAllowedPermissions,[]),	
	{?MODULE,Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  public  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
