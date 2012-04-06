%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc historysummarycollector
-module(historysummarycollector, [Tid]).

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
new(Account, QueryID)->
	Tid = ets:new(?MODULE,[set,public,protected]),
	ets:insert(Tid, {account, Account}),
	ets:insert(Tid, {queryID, QueryID}),
	ets:insert(Tid, {uniqueMonitorIDs, []}),
	ets:insert(Tid, {uniqueMonitorNames, []}),
	ets:insert(Tid, {historySummaries, dict:new()}),
	{?MODULE,Tid}.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
get_tid()->
	Tid.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
add(Date) ->
	[{_,HistorySummaries}] = ets:lookup(get_tid(), historySummaries),
	[{_,Account}] = ets:lookup(get_tid(), account),
	[{_,QueryID}] = ets:lookup(get_tid(), queryID),
	[{_,UniqueMonitorIDs}] = ets:lookup(get_tid(), uniqueMonitorIDs),
	[{_,UniqueMonitorNames}] = ets:lookup(get_tid(), uniqueMonitorNames),
	
%%    historySummaries.add(date, new HistorySummary(account, queryID, date,
%%              uniqueMonitorIDs, uniqueMonitorNames));
	 T2 = historysummary:new(Account, QueryID, Date, UniqueMonitorIDs, UniqueMonitorNames),
	{UniqueMonitorIDs1, UniqueMonitorNames1} = T2:addIdEtc(),
	T1 = dict:append(Date, T2, HistorySummaries),
	
	ets:insert(THIS:get_tid(), {historySummaries, T1}),
	ets:insert(THIS:get_tid(), {uniqueMonitorIDs, UniqueMonitorIDs1}),
	ets:insert(THIS:get_tid(), {uniqueMonitorNames, UniqueMonitorNames1}).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorNames() ->
	[{_,UniqueMonitorNames}] = ets:lookup(get_tid(), uniqueMonitorNames),
	UniqueMonitorNames.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorIDs() ->
	[{_,UniqueMonitorIDs}] = ets:lookup(get_tid(), uniqueMonitorIDs),
	UniqueMonitorIDs.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getEntry(Date, Key) ->
%% 	io:format("Date ~p ~n", [Date]),
%% 	io:format("Key ~p ~n", [Key]),
	[{_,HistorySummaries}] = ets:lookup(get_tid(), historySummaries),
	
	[HistorySummary] = dict:fetch(Date, HistorySummaries),	
	case HistorySummary of
		[] ->
%% 			io:format("HistorySummary11 ~p ~n", [HistorySummary]),
			HistorySummaryentry = [{{monitorName, ""},  {category, ""},  {average, ""}, {maximum, ""}}];
		_->
%% 			io:format("HistorySummary ~p ~n", [HistorySummary]),
			HistorySummaryentry = HistorySummary:getHistorySummaryEntry(Key)
%% 			io:format("HistorySummaryentry ~p ~n", [HistorySummaryentry])
	end,
	HistorySummaryentry.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getCategory(Date, Key) ->	
	case lists:keysearch(category, 1, THIS:getEntry(Date, Key)) of 
   		{value,{category, Category}} ->
			Category;
		_ ->
			["&nbsp;"]
   	end.
%%         return getEntry(date, s).category;

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getAverage(Date, Key) ->
%% 	io:format("getAverage : ~p ~n", [THIS:getEntry(Date, Key)]),
	case lists:keysearch(average, 1, THIS:getEntry(Date, Key)) of 
   		{value,{average, Average}} ->
			Average;
		_ ->
			["&nbsp;"]
   	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMaximum(Date, Key) ->
	case lists:keysearch(maximum, 1, THIS:getEntry(Date, Key)) of 
   		{value,{maximum, Maximum}} ->
			Maximum;
		_ ->
			["&nbsp;"]
   	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getStartDate(Date) ->
	[{_,HistorySummaries}] = ets:lookup(get_tid(), historySummaries),	
	[HistorySummary] = dict:fetch(Date, HistorySummaries),
	case HistorySummary of
		[] ->
			StartDate = "";
		_->
			[{_,StartDate}] = ets:lookup(get_tid(), startDate)			
	end,
	StartDate.	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getStartTime(Date) ->
	[{_,HistorySummaries}] = ets:lookup(get_tid(), historySummaries),	
	[HistorySummary] = dict:fetch(Date, HistorySummaries),
	case HistorySummary of
		[] ->
			StartTime = "";
		_->
			[{_,StartTime}] = ets:lookup(get_tid(), startTime)			
	end,
	StartTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getEndDate(Date) ->
	[{_,HistorySummaries}] = ets:lookup(get_tid(), historySummaries),
	[HistorySummary] = dict:fetch(Date, HistorySummaries),
	case HistorySummary of
		[] ->
			EndDate = "";
		_->
			[{_,EndDate}] = ets:lookup(get_tid(), endDate)			
	end,
	EndDate.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getEndTime(Date) ->
	[{_,HistorySummaries}] = ets:lookup(get_tid(), historySummaries),	
	[HistorySummary] = dict:fetch(Date, HistorySummaries),
	case HistorySummary of
		[] ->
			EndTime = "";
		_->
			[{_,EndTime}] = ets:lookup(get_tid(), endTime)
	end,
	EndTime.
