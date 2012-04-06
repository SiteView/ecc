%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc generatehistoryreport
-module(generatehistoryreport, [BASE,Historyreport]).
-extends(action).
%%
%% Include files
%%

%%
%% Exported Functions
%%
%% -export([]).
-compile(export_all).
%%
%% API Functions
%%


%%
%% Local Functions
%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
new(Historyreport) ->
	Obj = action:new(),
	{?MODULE,Obj, Historyreport}.	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
do_action() ->
	case Historyreport:isDisabled() of
		true ->
			continue;
		_ ->
			Historyreport:createFromQuery("")
	end.

execute() ->
	ok.

toString() ->
	ok.
