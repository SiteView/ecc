%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historysummary
-module(historysummary, [Tid]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).

%%
%% API Functions
%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
new(Account, ReportPath, Date, UniqueMonitorIDs, UniqueMonitorNames) ->
	Tid = ets:new(?MODULE,[set,public,protected]),

	ets:insert(Tid, {startDate, ""}),
	ets:insert(Tid, {startTime, ""}),
	ets:insert(Tid, {endDate, ""}),
	ets:insert(Tid, {endTime, ""}),

	ets:insert(Tid, {uniqueMonitorIDs, UniqueMonitorIDs}),
	ets:insert(Tid, {uniqueMonitorNames, UniqueMonitorNames}),
	
	case file:consult(ReportPath ++ Date ++ ".data") of
		{ok, T} ->
			ets:insert(Tid, {itemMap, T});
		_ ->
			ets:insert(Tid, {itemMap, []})
	end,	
	{?MODULE,Tid}.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
addIdEtc_Child([], Ids, Names)->
	{Ids, Names};
addIdEtc_Child([H|T], Ids, Names)->	
	case lists:member(lists:nth(1, tuple_to_list(H)), Ids) of
		true ->
			addIdEtc_Child(T, Ids, Names);
		_->
%% 			[{monitorId, lists:nth(1, T)}, {monitorName, lists:nth(2, T)}]			
			addIdEtc_Child(T, Ids ++ [lists:nth(1, tuple_to_list(H))], Names ++ [lists:nth(2, tuple_to_list(H))])
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
addIdEtc()->
	[{_,Datas}] = ets:lookup(get_tid(), itemMap),
	[{_,UniqueMonitorIDs}] = ets:lookup(get_tid(), uniqueMonitorIDs),
	[{_,UniqueMonitorNames}] = ets:lookup(get_tid(), uniqueMonitorNames),	
	addIdEtc_Child(Datas, UniqueMonitorIDs, UniqueMonitorNames).	  

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
toMap([], Map)->
	Map;
toMap([H|T], Map)->
	toMap(T, dict:append(lists:nth(1, H), H, Map)).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
get_tid() ->
	Tid.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getHistorySummaryEntry(Key) ->
%% 	io:format("getHistorySummaryEntry Key ~p ~n", [Key]),
	[{_,ItemMap}] = ets:lookup(get_tid(), itemMap),
	case lists:keysearch(Key, 1, ItemMap) of
		{value, T} ->
%% 			io:format("getHistorySummaryEntry ItemMap ~p ~n", [T]),
			HistorySummaryEntry = [{monitorName, lists:nth(2, tuple_to_list(T))},  {category, lists:nth(3, tuple_to_list(T))},  {average, lists:nth(4, tuple_to_list(T))}, {maximum, lists:nth(5, tuple_to_list(T))}];
		_->
			HistorySummaryEntry = []
	end,
%% 	[[T]] = dict:fetch(Key, ItemMap),
%% 	[{{monitorName, ""},  {category, ""},  {average, ""}, {maximum, ""}}]	
	HistorySummaryEntry.