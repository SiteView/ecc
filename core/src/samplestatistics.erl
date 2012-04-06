%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc samplestatistics
-module(samplestatistics, [Tid]).
%% -module(samplestatistics, [WorstCategory, LastCategory, SampleCount, Minimum, Maximum, Total, TotalSquared, TotalSamples, TotalValue]).
-compile(export_all).

-define(SampleStatistics, samplestatistics).
%%
%% Include files
%%


%%
%% Exported Functions
%%
%% -export([]).

%%
%% API Functions
%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
new() ->
%% 	Tid = ets:new(?MODULE,[set,public,{write_concurrency,true}]),
	Tid = ets:new(?MODULE,[set,public,protected]),
	ets:insert(Tid, {worstCategory, nodata}),
	ets:insert(Tid, {lastCategory, nodata}),
	ets:insert(Tid, {sampleCount, 0}),
	ets:insert(Tid, {minimum, 0.0}),
	ets:insert(Tid, {maximum, 0.0}),
	ets:insert(Tid, {total, 0.0}),
	ets:insert(Tid, {totalSquared, 0.0}),
	ets:insert(Tid, {totalSamples, 0.0}),
	ets:insert(Tid, {totalValue, ""}),
	{?MODULE,Tid}.
%% 	case ets:info(?SampleStatistics) of
%% 		undefined ->
%% 			ets:new(?SampleStatistics, [named_table]);
%% 		_ ->
%% 			ok
%% 	end,
%% 	
%% 	ets:insert(?SampleStatistics, {worstCategory, "nodata"}),
%% 	ets:insert(?SampleStatistics, {lastCategory, "nodata"}),
%% 	ets:insert(?SampleStatistics, {sampleCount, 0}),
%% 	ets:insert(?SampleStatistics, {minimum, 0.0}),
%% 	ets:insert(?SampleStatistics, {maximum, 0.0}),
%% 	ets:insert(?SampleStatistics, {total, 0.0}),
%% 	ets:insert(?SampleStatistics, {totalSquared, 0.0}),
%% 	ets:insert(?SampleStatistics, {totalSamples, 0.0}),
%% 	ets:insert(?SampleStatistics, {totalValue, ""}),	
	
%% 	{?MODULE,"nodata", "nodata", 0, 0.0, 0.0, 0.0, 0.0, 0, ""}.


%%
%% Local Functions
%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
get_tid()->
	Tid.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getSampleCount() ->	
%% 	[{_,SampleCount}] = ets:lookup(?SampleStatistics, sampleCount),
	[{_,SampleCount}] = ets:lookup(get_tid(), sampleCount),	
	SampleCount.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotal() -> 
	[{_,Total}] = ets:lookup(get_tid(), total),
%% 	[{_,Total}] = ets:lookup(?SampleStatistics, total),	
	Total.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalSquared() ->
	[{_,TotalSquared}] = ets:lookup(get_tid(), totalSquared),
%% 	[{_,TotalSquared}] = ets:lookup(?SampleStatistics, totalSquared),	
	TotalSquared.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalSamples() ->
	[{_,TotalSamples}] = ets:lookup(get_tid(), totalSamples),
%% 	[{_,TotalSamples}] = ets:lookup(?SampleStatistics, totalSamples),	
	TotalSamples.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalValue() ->
	[{_,TotalValue}] = ets:lookup(get_tid(), totalValue),
	TotalValue.
%% 	[{_,TotalValue}] = ets:lookup(?SampleStatistics, totalValue),	
%% 	J = string:len(TotalValue),
%%     if
%% 		J == 0  ->
%%             "n/a";
%%         true ->
%%             TotalValue
%% 	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getAverage() ->
%% 	[{_,TotalSamples}] = ets:lookup(?SampleStatistics, totalValue),
%% 	[{_,Total}] = ets:lookup(?SampleStatistics, total),
	[{_,TotalSamples}] = ets:lookup(get_tid(), totalSamples),	
	[{_,Total}] = ets:lookup(get_tid(), total),	
	case TotalSamples == 0 of
		true->
			 if 
				 is_float(Total)->
					float(0.0);
				 is_integer(Total)->
					Total div TotalSamples;
				 true->
					 THIS:getTotalValue()
			end;
		_->
			 if 
				 is_float(Total)->
			 		Total/float(TotalSamples);
				 is_integer(Total)->
					Total div TotalSamples;
				 true->
					 THIS:getTotalValue()
			end
	end.
%%     if
%% 		(TotalSamples == 0) or not(is_float(Total))->
%%             float(0.0);
%% 		(TotalSamples == 0) or not(is_integer(Total))->
%%             0;		
%%         true ->
%%             Total/float(TotalSamples)
%% 	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getStandardDeviation() ->
%% 	[{_,TotalSamples}] = ets:lookup(?SampleStatistics, totalValue),
%% 	[{_,Total}] = ets:lookup(?SampleStatistics, total),	
%% 	[{_,TotalSquared}] = ets:lookup(?SampleStatistics, totalSquared),
	[{_,TotalSamples}] = ets:lookup(get_tid(), totalValue),
	[{_,Total}] = ets:lookup(get_tid(), total),	
	[{_,TotalSquared}] = ets:lookup(get_tid(), totalSquared),	
	TotalSquared
	.

%% managePage.java run 
%% getStandardDeviation() {
%%         if (Float.isNaN(total) || totalSamples < 2)
%%             return Float.NaN;
%%         float f;
%%         try {
%%             double d = Math
%%                     .sqrt((totalSquared - (double) (total * total / (float) totalSamples))
%%                             / (double) (totalSamples - 1));
%%             f = (float) d;
%%         } catch (ArithmeticException arithmeticexception) {
%%             return Float.NaN;
%%         }
%%         return f;
%%     }

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMaximum() ->
%% 	[{_,Maximum}] = ets:lookup(?SampleStatistics, maximum),
	[{_,Maximum}] = ets:lookup(get_tid(), maximum),	
	Maximum.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMinimum() ->
%% 	[{_,Minimum}] = ets:lookup(?SampleStatistics, minimum),
	[{_,Minimum}] = ets:lookup(get_tid(), minimum),	
	Minimum.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getWorstCategory() ->
%% 	[{_,WorstCategory}] = ets:lookup(?SampleStatistics, worstCategory),
	[{_,WorstCategory}] = ets:lookup(get_tid(), worstCategory),	
	WorstCategory.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getInfo()->
	ets:tab2list(get_tid()).