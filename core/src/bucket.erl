%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc bucket
-module(bucket, [BASE,Collector]).
-compile(export_all).
-extends(samplestatistics).

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
%% @doc read report info from contentstore
new(Samplecollector)->
	Obj = samplestatistics:new(),
	{?MODULE,Obj, Samplecollector}.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
add(Time, Value, Category, SampleIsDisabled) ->
	WorstCategory = historyreport_static:getWorstCategory(THIS:getWorstCategory(), Category),
	ets:insert(THIS:get_tid(), {worstCategory, WorstCategory}),
%% 	io:format("~p ~n", [SampleIsDisabled]),
	case SampleIsDisabled of
		false ->
%% 			T4 = Collector:isNumeric(),
%% 			T5 = string:equal(Category, worstCategory),
			case is_integer(Value) or is_float(Value)of 
				true ->
					Value1 = float(Value),
					
					T1 = THIS:getTotal() + Value1,
					ets:insert(THIS:get_tid(), {total, T1}),

					T2 = (THIS:getTotalSamples()) + 1,
					ets:insert(THIS:get_tid(), {totalSamples, T2}),

					case (Value1 =< THIS:getMinimum()) of
						true ->
							ets:insert(THIS:get_tid(), {minimum, Value1});
						_->
							ok
					end,
			
					case (Value1 >= THIS:getMaximum()) of
						 true ->
							ets:insert(THIS:get_tid(), {maximum, Value1});
						 _ ->
							ok
					end;
				 _ ->
					case (WorstCategory =:= Category) or (THIS:getTotalValue() == 0) of
						true->
							ets:insert(THIS:get_tid(), {totalValue, Value});
						_->
							ok
					end
			end;
		_ ->
			ok
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
addTotal() ->
	[{_,Total}] = ets:lookup(THIS:get_tid(), total),
	T1 = Total + 5,
	ets:insert(THIS:get_tid(), {total, T1}).

%% 	[{_,Total}] = ets:lookup(samplestatistics, total),
%% 	ets:insert(samplestatistics, {total, T1}).
