%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc samplecollector
-module(samplecollectorold, [BASE]).
-extends(samplestatistics).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
%%
%% Include files
%%

%%
%% Exported Functions
%%
%% -export([]).

%% @spec new() -> ok 
%% Obj = term()
%% @doc 读数据数据
new()->
	Obj = samplestatistics:new(),
	ets:insert(Obj:get_tid(), {startTime, 0}),	
	ets:insert(Obj:get_tid(), {endTime, 0}),
	ets:insert(Obj:get_tid(), {precision, 0}),
	ets:insert(Obj:get_tid(), {actualStartTime, 0}),
	ets:insert(Obj:get_tid(), {actualEndTime, 0}),
	ets:insert(Obj:get_tid(), {lastValue, "n/a"}),
	ets:insert(Obj:get_tid(), {lastTime, 0}),
	ets:insert(Obj:get_tid(), {minimumTime, 0}),
	ets:insert(Obj:get_tid(), {maximumTime, 0}),
	ets:insert(Obj:get_tid(), {errorTime, 0}),
	ets:insert(Obj:get_tid(), {warningTime, 0}),
	ets:insert(Obj:get_tid(), {goodTime, 0}),
	ets:insert(Obj:get_tid(), {naTime, 0}),
	ets:insert(Obj:get_tid(), {totalTime, 0}),
	ets:insert(Obj:get_tid(), {sampleBuffer, []}),
	ets:insert(Obj:get_tid(), {bufferLastTime, 0}),
	ets:insert(Obj:get_tid(), {averagedSamples, false}),
	ets:insert(Obj:get_tid(), {maxSampleBuffer, []}),
	ets:insert(Obj:get_tid(), {warningNotIncluded, false}),
	ets:insert(Obj:get_tid(), {failureNotIncluded, false}),
	ets:insert(Obj:get_tid(), {showFullMonitorName, false}),
	ets:insert(Obj:get_tid(), {debug, false}),
	ets:insert(Obj:get_tid(), {buckets, []}),
%% 	ets:insert(Obj:get_tid(), {bucketsTemp, []}),	
	ets:insert(Obj:get_tid(), {monitor, []}),
	ets:insert(Obj:get_tid(), {type, []}),
	{?MODULE,Obj}.

%% @spec new() -> ok 
%% Obj = term()
%% @doc 
new(Monitor, Property, KeepSampleBuffer, WarningNotIncluded, FailureNotIncluded)->
	Obj = samplestatistics:new(),
	ets:insert(Obj:get_tid(), {startTime, 0}),
	ets:insert(Obj:get_tid(), {endTime, 0}),
	ets:insert(Obj:get_tid(), {precision, 60}),
	ets:insert(Obj:get_tid(), {actualStartTime, 0}),
	ets:insert(Obj:get_tid(), {actualEndTime, 0}),
	ets:insert(Obj:get_tid(), {lastValue, "n/a"}),
	ets:insert(Obj:get_tid(), {lastTime, 0}),
	ets:insert(Obj:get_tid(), {minimumTime, 0}),
	ets:insert(Obj:get_tid(), {maximumTime, 0}),
	ets:insert(Obj:get_tid(), {errorTime, 0}),
	ets:insert(Obj:get_tid(), {warningTime, 0}),
	ets:insert(Obj:get_tid(), {goodTime, 0}),
	ets:insert(Obj:get_tid(), {naTime, 0}),
	ets:insert(Obj:get_tid(), {totalTime, 0}),
	ets:insert(Obj:get_tid(), {sampleBuffer, []}),
	ets:insert(Obj:get_tid(), {bufferLastTime, 0}),
	ets:insert(Obj:get_tid(), {averagedSamples, false}),
	ets:insert(Obj:get_tid(), {maxSampleBuffer, []}),
	ets:insert(Obj:get_tid(), {warningNotIncluded, WarningNotIncluded}),
	ets:insert(Obj:get_tid(), {failureNotIncluded, FailureNotIncluded}),
	ets:insert(Obj:get_tid(), {showFullMonitorName, false}),
	ets:insert(Obj:get_tid(), {debug, false}),
	ets:insert(Obj:get_tid(), {buckets, []}),
%% 	ets:insert(Obj:get_tid(), {bucketsTemp, []}),
	ets:insert(Obj:get_tid(), {monitor, Monitor}),
	ets:insert(Obj:get_tid(), {property, Property}),
	ets:insert(Obj:get_tid(), {keepsamplebuffer, KeepSampleBuffer}),
	ets:insert(Obj:get_tid(), {type, []}),
	
	{?MODULE,Obj}.

%% @spec new(X) -> ok 
%% Obj = term()
%% @doc 
new(Monitor, Property)->
	Obj = samplestatistics:new(),
	ets:insert(Obj:get_tid(), {startTime, 0}),
	ets:insert(Obj:get_tid(), {endTime, 0}),
	ets:insert(Obj:get_tid(), {precision, 60}),
	ets:insert(Obj:get_tid(), {actualStartTime, 0}),
	ets:insert(Obj:get_tid(), {actualEndTime, 0}),
	ets:insert(Obj:get_tid(), {lastValue, "n/a"}),
	ets:insert(Obj:get_tid(), {lastTime, 0}),
	ets:insert(Obj:get_tid(), {minimumTime, 0}),
	ets:insert(Obj:get_tid(), {maximumTime, 0}),
	ets:insert(Obj:get_tid(), {errorTime, 0}),
	ets:insert(Obj:get_tid(), {warningTime, 0}),
	ets:insert(Obj:get_tid(), {goodTime, 0}),
	ets:insert(Obj:get_tid(), {naTime, 0}),
	ets:insert(Obj:get_tid(), {totalTime, 0}),
	ets:insert(Obj:get_tid(), {sampleBuffer, []}),
	ets:insert(Obj:get_tid(), {bufferLastTime, 0}),
	ets:insert(Obj:get_tid(), {averagedSamples, false}),
	ets:insert(Obj:get_tid(), {maxSampleBuffer, []}),
	ets:insert(Obj:get_tid(), {warningNotIncluded, false}),
	ets:insert(Obj:get_tid(), {failureNotIncluded, false}),
	ets:insert(Obj:get_tid(), {showFullMonitorName, false}),
	ets:insert(Obj:get_tid(), {debug, false}),
	ets:insert(Obj:get_tid(), {buckets, []}),
%% 	ets:insert(Obj:get_tid(), {bucketsTemp, []}),
	ets:insert(Obj:get_tid(), {monitor, Monitor}),
	ets:insert(Obj:get_tid(), {property, Property}),
	ets:insert(Obj:get_tid(), {keepsamplebuffer, false}),	
	ets:insert(Obj:get_tid(), {type, []}),
	
	{?MODULE,Obj}.

%%
%% Local Functions
%%

%% @spec createBuckets(StartTime, EndTime, Precision) -> ok 
%% Obj = term()
%% @doc 
createBuckets(StartTime, EndTime, Precision) ->
	ets:insert(THIS:get_tid(), {startTime, StartTime}),
	ets:insert(THIS:get_tid(), {endTime, EndTime}),
	ets:insert(THIS:get_tid(), {precision, Precision}),
	L3 = EndTime - StartTime,
	L4 = L3/ Precision,
%% 	io:format("samplecollector:createBuckets1: ~p ~n", [THIS:get_tid()]),
	Buckets = constructBuckets(0, [], L4),
%% 	io:format("~p ~n", [L4]),
%% 	T1 = lists:keyreplace(137, 1, Buckets, {137, {xxx, yyy}}),
%% 	io:format("~p ~n", [lists:nth(10, T1)]),	
	ets:insert(THIS:get_tid(), {buckets, Buckets})
%% 	io:format("samplecollector:createBuckets2: ~p ~n", [L4]).
	.

%% @spec constructBuckets(Count, T, Max) -> ok 
%% Obj = term()
%% @doc 
constructBuckets(Count, T, Max) when Count >= Max ->
		T;
constructBuckets(Count, T, Max) ->
%% 		constructBuckets(Count+1, [bucket:new(THIS)|T], Max).
		constructBuckets(Count+1, [{Count+1, {{worstCategory, nodata},{lastCategory, nodata},{sampleCount, 0},{minimum, 0.0}, {maximum, 0.0}, {total, 0.0},{totalSquared, 0.0}, {totalSamples, 0.0}, {totalValue, ""}, {connector, THIS}}}|T], Max).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getBuckets() ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
	Buckets.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitor() ->
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
	Monitor.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
get_parent_full_name()->
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
	Monitor:get_parent_full_name().

%% 
%% 
%% 
get_last_description() ->
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
    {ok,{_,State}} = Monitor:get_attribute(state_string),
	State.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getPropertyLabel() ->
	[{_,Property}] = ets:lookup(THIS:get_tid(), property),
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),	
	{_,{_,Class}}=Monitor:get_property(class),
%% 	Template=api_monitor_template:get_template(Class),
	Template=report_proxy:get_template(Class),
	case lists:keysearch(Property,#property.name, Template) of
		 {_,Plabel} ->
			 Ret =element(3, Plabel);
		 _ ->
			Props = Monitor:get_properties(),
			case lists:keysearch(browse, 1, Props) of
		 		{_,Plabel1} ->
					{_, T1} = Plabel1,
				 	case lists:keysearch(Property, 1, T1) of
					 	{_,Plabel11} ->
				   			Ret =element(2, Plabel11);			 
			 			_->
				   			Ret=Property
					end;
				_ ->	
					Ret=Property		
			end
	end,
    Ret.


%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getPropertyType() ->	
	[{_,Type}] = ets:lookup(THIS:get_tid(), type),
	case Type of 
		[]->
			[{_,Property}] = ets:lookup(THIS:get_tid(), property),
			[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
			{_,{_,Class}}=Monitor:get_property(class),
			%% 	Template=api_monitor_template:get_template(Class),
			Template=report_proxy:get_template(Class),
			case lists:keysearch(Property,#property.name, Template) of
		 	{V,Plabel} ->
				 Ret =element(4, Plabel);
		 	_ ->
			 	Ret=numeric
%% 				Ret=text
			end,
			ets:insert(THIS:get_tid(), {type, Ret}),
    		Ret;
		_->
			Type	
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorName() ->
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
%% 	io:format("~p ~n", [Monitor]),
	{ok,{_,Name}} = Monitor:get_property(name),
%% 	io:format("~p ~n", [Name]),
	Name.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorDescription() ->
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
	{ok,{_,Des}} = Monitor:get_property(description),
	Des.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorFullID() ->
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
	FullID = Monitor:getFullID(),
	FullID.
%% 	{ok,{_,FullID}} = Monitor:get_property(fullid),
%% 	FullID.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorTitle() ->
	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
	{ok,{_,Title}} = Monitor:get_property(title),
	Title.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getProperty() ->
	[{_,Property}] = ets:lookup(THIS:get_tid(), property),
	Property.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMaximumTime() ->
	[{_,MaximumTime}] = ets:lookup(THIS:get_tid(), maximumTime),
	MaximumTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMinimumTime() ->
	[{_,MinimumTime}] = ets:lookup(THIS:get_tid(), minimumTime),
	MinimumTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getLastCategory() ->
	[{_,LastCategory}] = ets:lookup(THIS:get_tid(), lastCategory),
	LastCategory.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getLastValue() ->
	[{_,LastValue}] = ets:lookup(THIS:get_tid(), lastValue),
	[{_,Property}] = ets:lookup(THIS:get_tid(), property),
	case lists:keysearch(Property, 1, LastValue) of
		{value, {_, V1}} ->			
			case is_integer(V1) or is_float(V1) of 
			  true ->
					Value1 = float(V1),
					Value1;
				_ ->
					V1
			end;
		_ ->
			''
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getLastTime() ->
	[{_,LastTime}] = ets:lookup(THIS:get_tid(), lastTime),
	LastTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getErrorTime() ->
	[{_,ErrorTime}] = ets:lookup(THIS:get_tid(), errorTime),
	ErrorTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getWarningTime() ->
	[{_,WarningTime}] = ets:lookup(THIS:get_tid(), warningTime),
	WarningTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getGoodTime() ->
	[{_,GoodTime}] = ets:lookup(THIS:get_tid(), goodTime),
	GoodTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getNaTime() ->
	[{_,NaTime}] = ets:lookup(THIS:get_tid(), lastTime),
	NaTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalTime() ->
	[{_,TotalTime}] = ets:lookup(THIS:get_tid(), totalTime),
	TotalTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getBucketCount() ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
	length(Buckets).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getIDString() ->
	[{_,IdString}] = ets:lookup(THIS:get_tid(), idString),
	IdString.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
isNumeric() ->
%% 	[{_,Property}] = ets:lookup(THIS:get_tid(), property),
%% 	[{_,Monitor}] = ets:lookup(THIS:get_tid(), monitor),
%% 	Props = Monitor:getLogProperties(Monitor),
%% 	case lists:keysearch(Key, N, Props) of
%% 		{value, {_, V}} ->
%% 			
%% 	end
%% 	is_integer(Property).
	true.
%% 	[{_,LastTime}] = ets:lookup(THIS:get_tid(), lastTime),
%% 	LastTime.
%%     {
%%         return property instanceof NumericProperty;
%%     }
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
isPercentage() ->
	[{_,LastTime}] = ets:lookup(THIS:get_tid(), lastTime),
	LastTime.
%%     {
%%         return property instanceof PercentProperty;
%%     }

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getStartTime() ->
	[{_,ActualStartTime}] = ets:lookup(THIS:get_tid(), actualStartTime),
	ActualStartTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getEndTime() ->
	[{_,ActualEndTime}] = ets:lookup(THIS:get_tid(), actualEndTime),
	ActualEndTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getBucketStartTime(I) ->
%% 	io:format("~p ~n", [I]),
	[{_,StartTime}] = ets:lookup(THIS:get_tid(), startTime),
	[{_,EndTime}] = ets:lookup(THIS:get_tid(), endTime),
%% 	io:format("~p ~n", [StartTime]),
%% 	io:format("~p ~n", [EndTime]),
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	io:format("~p ~n", [Buckets]),length(_)
%% 	io:format("~p ~n", [I*((EndTime - StartTime)/(length(Buckets) - 1))]),
%% 	Ret = StartTime + round(I*((EndTime - StartTime)/(length(Buckets) - 1))),
 	Ret = StartTime + trunc(I*((EndTime - StartTime)/(length(Buckets)))),	
%% 	io:format("ok : ~p ~n", [Ret]),
	Ret.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getSampleCount(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getSampleCount().
	THIS:getBucketData(lists:nth(I, Buckets), getSampleCount).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotal(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getTotal().
	THIS:getBucketData(lists:nth(I, Buckets), getTotal).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalSamples(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getTotalSamples().
	THIS:getBucketData(lists:nth(I, Buckets), getTotalSamples).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalValue(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getTotalValue().
	THIS:getBucketData(lists:nth(I, Buckets), getTotalValue).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalValueCarry(I) ->
	(getNonEmptyBucket(I)):getTotalValue().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getWorstCategory(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getWorstCategory().
	THIS:getBucketData(lists:nth(I, Buckets), getWorstCategory).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getWorstCategoryCarry(I) ->
	(getNonEmptyBucket(I)):getWorstCategory().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getAverage(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getAverage().
	THIS:getBucketData(lists:nth(I, Buckets), getAverage).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getAverageCarry(I) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMaximum(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getMaximum().
	THIS:getBucketData(lists:nth(I, Buckets), getMaximum).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMaximumCarry(I) ->
	(getNonEmptyBucket(I)):getMaximum().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMinimum(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
%% 	(lists:nth(I, Buckets)):getMinimum().
	THIS:getBucketData(lists:nth(I, Buckets), getMinimum).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMinimumCarry(I) ->
	(getNonEmptyBucket(I)):getMinimum().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getNonEmptyBucket(0) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),
	(lists:nth(0, Buckets));
getNonEmptyBucket(I) ->
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),	
	
%% 	case (lists:nth(I, Buckets)):getSampleCount() =:= 0 of
	case THIS:getBucketData(lists:nth(I, Buckets), getSampleCount) =:= 0 of
		true ->
			getNonEmptyBucket(I);
		_ ->
			lists:nth(I, Buckets)	
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
add_1(Time, Fields, FieldsLength, Category, ErrorType) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
add(Time, Fields, FieldsLength, Category, ErrorType, BestCaseCalc) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
add_2(Time, Value, Category, ErrorType, BestCaseCalc) ->
%% 	io:format("~p ~n", [Category]),
	[{_,LastTime}] = ets:lookup(THIS:get_tid(), lastTime),	
	case (LastTime =/= 0) of
		true ->
%% 			io:format("~p ~n", ["Time2"]),
%% 			ets:insert(THIS:get_tid(), {lastTime, calendar:datetime_to_gregorian_seconds(Time)}),
%% 			ets:insert(THIS:get_tid(), {lastValue, Value}),
%% 			ets:insert(THIS:get_tid(), {lastCategory, Category}),
%% 			ets:insert(THIS:get_tid(), {lastErrorType, ErrorType}),			
%% 	    	[{_,LastValue}] = ets:lookup(THIS:get_tid(), lastValue),
%% 			[{_,LastCategory}] = ets:lookup(THIS:get_tid(), lastCategory),
%% 			[{_,LastErrorType}] = ets:lookup(THIS:get_tid(), lastErrorType),
%% 			[{_,LastTime1}] = ets:lookup(THIS:get_tid(), lastErrorType),
			THIS:add(Time, calendar:datetime_to_gregorian_seconds(Time) - LastTime, Value, Category, ErrorType);
%% 			ok;
		_->
%% 			A=calendar:datetime_to_gregorian_seconds(Time),
			THIS:add(Time, 0, Value, Category, ErrorType),
			ok
	end,
%% 	io:format("~p ~n", ["Time3"]),
	ets:insert(THIS:get_tid(), {lastTime, calendar:datetime_to_gregorian_seconds(Time)}),
	ets:insert(THIS:get_tid(), {lastValue, Value}),
	ets:insert(THIS:get_tid(), {lastCategory, Category}),
	ets:insert(THIS:get_tid(), {lastErrorType, ErrorType}).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
add(Time, Interval, Value, Category, ErrorType) ->
%% 	io:format("~p ~n", [Time]),
	[{_,ActualStartTime}] = ets:lookup(THIS:get_tid(), actualStartTime),
	if
		(ActualStartTime == 0) ->
			ets:insert(THIS:get_tid(), {actualStartTime, Time});
		true ->
		     ok
	end,
	
	[{_,StartTime}] = ets:lookup(THIS:get_tid(), startTime),
	[{_,EndTime}] = ets:lookup(THIS:get_tid(), endTime),
	
	[{_,Precision}] = ets:lookup(THIS:get_tid(), precision),
	ets:insert(THIS:get_tid(), {actualEndTime, Time}),
	
	BucketNumberOld = trunc(((calendar:datetime_to_gregorian_seconds(Time) - StartTime)/Precision)),
	
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),	
	StepTime = StartTime + trunc(BucketNumberOld*((EndTime - StartTime)/(length(Buckets)))),
%% 	io:format("StepTime: ~p", [calendar:gregorian_seconds_to_datetime(StepTime)]),
%% 	Bucket = lists:nth(BucketNumber, Buckets),
    %io:format("BucketNumberOld:~p~n",[BucketNumberOld]),
	case BucketNumberOld of
		0 ->
			BucketNumber  = 1,
			Bucket = lists:nth(1, Buckets);
		_ ->
			case Time >= StepTime of
				true ->
					BucketNumber = BucketNumberOld;
				_ ->
					BucketNumber = BucketNumberOld - 1
			end,
			
			Bucket = lists:nth(BucketNumber, Buckets)
	end,
%% 	io:format("BucketNumber: ~p ~n",[BucketNumber]),
%% 	io:format("L:~p S:~p B: ~p ~n", [Time, calendar:gregorian_seconds_to_datetime(StepTime), BucketNumber]),
 	%Bucket = lists:nth(BucketNumber, Buckets),
 	%io:format("~p ~n", [Bucket]),
%% 	SampleIsDisabled = ((string:equal(Category, "disabled")) or (string:equal(Category, "filtered"))),
	SampleIsDisabled = ((Category == disabled) or (Category == filtered)),
%% 	io:format("~p ~n", [SampleIsDisabled]),
	[{_,Property}] = ets:lookup(THIS:get_tid(), property),
	case lists:keysearch(Property, 1, Value) of
		{value, {_, V}} ->
%% 			io:format("~p ~n", [V]);
%% 			Bucket:add(Time, V, Category, SampleIsDisabled);
			THIS:addBucket(Buckets, BucketNumber, StepTime, Bucket, Time, V, Category, SampleIsDisabled);
%% 			ok;
		false ->
			error
	end,	
	if
		(SampleIsDisabled == false) ->
			[{_,TotalTime}] = ets:lookup(THIS:get_tid(), totalTime),
			TR = TotalTime + Interval,
			ets:insert(THIS:get_tid(), {totalTime, TR});
		true ->
			ok
	end,
	
	THIS:calculate(Category, Interval, ErrorType),

	case Category =/= THIS:getLastCategory() of
		true ->
			WorstCategory = historyreport_static:getWorstCategory(THIS:getWorstCategory(), Category),
			ets:insert(THIS:get_tid(), {worstCategory, WorstCategory});			
		_->
			ok
	end,

	TT = (THIS:getSampleCount()) + 1,
	ets:insert(THIS:get_tid(), {sampleCount, TT}),	
	
	case SampleIsDisabled of
	    false ->
%% 			T4 = Collector:isNumeric(),
%% 			T5 = string:equal(Category, worstCategory),
			case lists:keysearch(Property, 1, Value) of
				{value, {_, V1}} ->			
					case is_integer(V1) or is_float(V1) of 
					  true ->
%% 						io:format("~p ~n", ["T11otalTime12"]),
						Value1 = float(V1),

						T1 = THIS:getTotal() + Value1,
						ets:insert(THIS:get_tid(), {total, T1}),

						T2 = (THIS:getTotalSamples()) + 1,
						ets:insert(THIS:get_tid(), {totalSamples, T2}),
%% 						this.totalSquared += Math.pow(floatValue, 2.0D);
						case (Value1 == 0.0) of
							true ->						
								MinimumNew = 0.0,
								ets:insert(THIS:get_tid(), {minimum, MinimumNew}),
								ets:insert(THIS:get_tid(), {minimumTime, Time});						
							_->
							case (Value1 =< THIS:getMinimum()) of
								true ->
									MinimumNew = Value1,
									ets:insert(THIS:get_tid(), {minimum, MinimumNew}),
									ets:insert(THIS:get_tid(), {minimumTime, Time});
								_->
									case (THIS:getMinimum() == 0.0) and (THIS:getMinimumTime() == 0) of
										true ->
	 										MinimumNew = Value1,
											ets:insert(THIS:get_tid(), {minimum, MinimumNew}),
											ets:insert(THIS:get_tid(), {minimumTime, Time});
										_ ->
											ok
									end,
									ok
							end
						end,
%% 						case (Value1 =< THIS:getMinimum()) of
%% 							true ->
%% 								ets:insert(THIS:get_tid(), {minimum, Value1}),
%% 								ets:insert(THIS:get_tid(), {minimumTime, Time});
%% 							_ ->
%% 								ok
%% 						end,
			
						case (Value1 >= THIS:getMaximum()) of
							 true ->
								ets:insert(THIS:get_tid(), {maximum, Value1}),
								ets:insert(THIS:get_tid(), {maximumTime, Time});
						 	_ ->
								ok
						end;				
				 	_ ->
						ets:insert(THIS:get_tid(), {totalValue, Value})
				end;
			 _ ->
				error
			end;
		_->
			ok
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
calculate(Category, Interval, ErrorType) ->
	case Category of
		error ->
			[{_,ErrorTime}] = ets:lookup(THIS:get_tid(), errorTime),
			T1 = ErrorTime + Interval,
			ets:insert(THIS:get_tid(), {errorTime, T1});
		good ->
			[{_,GoodTime}] = ets:lookup(THIS:get_tid(), goodTime),
			T1 = GoodTime + Interval,			
			ets:insert(THIS:get_tid(), {goodTime, T1});
		warning ->
			[{_,WarningTime}] = ets:lookup(THIS:get_tid(), warningTime),
			T1 = WarningTime + Interval,			
			ets:insert(THIS:get_tid(), {warningTime, T1});
		_->						
			[{_,NaTime}] = ets:lookup(THIS:get_tid(), naTime),
			T1 = NaTime + Interval,
			ets:insert(THIS:get_tid(), {naTime, T1})
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
clearSampleBuffer() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMaxSampleBuffer() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getSampleBuffer() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
samplesAreAveraged() ->
	[{_,AveragedSamples}] = ets:lookup(THIS:get_tid(), averagedSamples),
	AveragedSamples.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
toString() ->
	[{_,IdString}] = ets:lookup(THIS:get_tid(), idString),
	IdString.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
equals(Obj) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
hashCode() ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec addBucket(Buckets, BucketsNumber, Bucket, Time, Value, Category, SampleIsDisabled) -> ok 
%% Obj = term()
%% @doc 
addBucket(Buckets, BucketsNumber, StepTime, Bucket, Time, Value, Category, SampleIsDisabled) ->
	{Count, {{_,WorstCategory},{_,LastCategory},{_,SampleCount},{_,Minimum},{_,Maximum},{_,Total},{_,TotalSquared},{_,TotalSamples},{_,TotalValue},{_,Connector}}} = Bucket,
%% 	io:format("L:~p S:~p B: ~p TotalSamples: ~p ~n", [Time, calendar:gregorian_seconds_to_datetime(StepTime), BucketsNumber, TotalSamples]),
%% 	case TotalSamples of
%% 		0.0->
%% 			io:format("L:~p S:~p B: ~p ~n", [Time, calendar:gregorian_seconds_to_datetime(StepTime), BucketsNumber]);
%% 		_->
%% 		ok
%% 	end,
	WorstCategoryNew = historyreport_static:getWorstCategory(WorstCategory, Category),	
	case SampleIsDisabled of
		false ->
			case is_integer(Value) or is_float(Value)of 
				true ->
					Value1 = float(Value),					
					TotalNew = Total + Value1,
					TotalSamplesNew = TotalSamples + 1,

					case (Value1 == 0.0) of
						true ->
							MinimumNew = 0.0;
						_->
							case (Value1 =< THIS:getMinimum()) of
								true ->
									MinimumNew = Value1;
								_->
									case THIS:getMinimum() == 0.0 of
										true ->
	 										MinimumNew = Value1;
										_ ->
											MinimumNew = Minimum
									end
							end
					end,
					
					case (Value1 >= THIS:getMaximum()) of
						 true ->
							 MaximumNew = Value1;
						 _ ->
							MaximumNew = Maximum
					end,
					
%% 					io:format("~p ~n", [TotalSamplesNew]),
					[B1] = lists:keyreplace(Count, 1, [Bucket], {Count, {{worstCategory, WorstCategoryNew},{lastCategory, LastCategory},{sampleCount, SampleCount},{minimum, MinimumNew}, {maximum, MaximumNew}, {total, TotalNew},{totalSquared, TotalSquared}, {totalSamples, TotalSamplesNew}, {totalValue, TotalValue}, {connector, THIS}}}),
					{_, B3} = B1,
					T1 = lists:keyreplace(Count, 1, Buckets, {Count, B3}),
%% 					T2 = lists:nth(BucketsNumber + 1, T1),
%% 					io:format("T2:~p ~n", [T2]),
%% 					[{_,BucketsTemp}] = ets:lookup(THIS:get_tid(), bucketsTemp),
%% 					T1 = BucketsTemp ++ B1,
					ets:insert(THIS:get_tid(), {buckets, T1});			
				 _ ->
					case (WorstCategoryNew =:= Category) or (TotalValue == 0) of
						true->
							TotalValueNew = Value;
						_->
							TotalValueNew = TotalValue
					end,					
					[B1] = lists:keyreplace(Count, 1, [Bucket], {Count, {{worstCategory, WorstCategoryNew},{lastCategory, LastCategory},{sampleCount, SampleCount},{minimum, Minimum}, {maximum, Maximum}, {total, Total},{totalSquared, TotalSquared}, {totalSamples, TotalSamples}, {totalValue, TotalValueNew}, {connector, THIS}}}),
					{_, B3} = B1,
					T1 = lists:keyreplace(Count, 1, Buckets, {Count, B3}),
					ets:insert(THIS:get_tid(), {buckets, T1})
			end;
		_->
			ok
	end.

%% @spec addold(Time, Interval, Value, Category, ErrorType) -> ok 
%% Obj = term()
%% @doc 
addold(Time, Interval, Value, Category, ErrorType) ->
%% 	io:format("~p ~n", [Time]),
	[{_,ActualStartTime}] = ets:lookup(THIS:get_tid(), actualStartTime),
	if
		(ActualStartTime == 0) ->
			ets:insert(THIS:get_tid(), {actualStartTime, Time});
		true ->
		     ok
	end,
	[{_,StartTime}] = ets:lookup(THIS:get_tid(), startTime),
	[{_,Precision}] = ets:lookup(THIS:get_tid(), precision),
	ets:insert(THIS:get_tid(), {actualEndTime, Time}),

%% 	io:format("~p ~n", [calendar:datetime_to_gregorian_seconds(Time)]),
	BucketNumber = trunc(((calendar:datetime_to_gregorian_seconds(Time) - StartTime)/Precision)),
%% 	io:format("~p ~n",[BucketNumber]), 
	
	[{_,Buckets}] = ets:lookup(THIS:get_tid(), buckets),

%% 	Bucket = lists:nth(BucketNumber, Buckets),
 	Bucket = lists:nth(BucketNumber + 1, Buckets),
%% 	io:format("~p ~n", [Bucket]),
%% 	SampleIsDisabled = ((string:equal(Category, disabled)) or (string:equal(Category, filtered))),
	SampleIsDisabled = ((Category == disabled) or (Category == filtered)),	
	
	[{_,Property}] = ets:lookup(THIS:get_tid(), property),
	case lists:keysearch(Property, 1, Value) of
		{value, {_, V}} ->
%% 			io:format("~p ~n", [V]),
			Bucket:add(Time, V, Category, SampleIsDisabled);
		false ->
			error
	end,	
%% 	io:format("~p ~n", [SampleIsDisabled]),
	if
		(SampleIsDisabled == false) ->
			[{_,TotalTime}] = ets:lookup(THIS:get_tid(), totalTime),
			TR = TotalTime + Interval,
			ets:insert(THIS:get_tid(), {totalTime, TR});
		true ->
%% 			io:format("~p ~n", ["TotalTime11"]),
			ok
	end,
	
	THIS:calculate(Category, Interval, ErrorType),	
%% 
%% 
%% 	if (!(category.equals(this.lastCategory))) {
%% 	  this.worstCategory = Monitor.getWorstCategory(this.worstCategory, category);
%% 	}
	case Category =/= THIS:getLastCategory() of
		true ->
			WorstCategory = historyreport_static:getWorstCategory(THIS:getWorstCategory(), Category),
			ets:insert(THIS:get_tid(), {worstCategory, WorstCategory});			
		_->
			ok
	end,
	TT = (THIS:getSampleCount()) + 1,
	ets:insert(THIS:get_tid(), {sampleCount, TT}),	
%% 	this.sampleCount += 1;
	
	case SampleIsDisabled of
		false ->
%% 			T4 = Collector:isNumeric(),
%% 			T5 = string:equal(Category, worstCategory),
			case lists:keysearch(Property, 1, Value) of
				{value, {_, V1}} ->			
					case is_integer(V1) or is_float(V1) of 
					  true ->
%% 						io:format("~p ~n", ["T11otalTime12"]),
						Value1 = float(V1),

						T1 = THIS:getTotal() + Value1,
						ets:insert(THIS:get_tid(), {total, T1}),

						T2 = (THIS:getTotalSamples()) + 1,
						ets:insert(THIS:get_tid(), {totalSamples, T2}),
%% 						this.totalSquared += Math.pow(floatValue, 2.0D);
					
						case (Value1 =< THIS:getMinimum()) of
							true ->
								ets:insert(THIS:get_tid(), {minimum, Value1}),
								ets:insert(THIS:get_tid(), {minimumTime, Time});							
							_ ->
								ok
						end,
			
						case (Value1 >= THIS:getMaximum()) of
							 true ->
								ets:insert(THIS:get_tid(), {maximum, Value1}),
								ets:insert(THIS:get_tid(), {maximumTime, Time});
						 	_ ->
								ok
						end;
				 	_ ->
						ets:insert(THIS:get_tid(), {totalValue, Value})
				end;
			 _ ->
				error
			end;
		_->
			 ok
	end.

%% @spec getBucketData(Bucket, FunName) -> ok 
%% Obj = term()
%% @doc 
getBucketData(Bucket, FunName) ->
	{Count, {{_,WorstCategory},{_,LastCategory},{_,SampleCount},{_,Minimum},{_,Maximum},{_,Total},{_,TotalSquared},{_,TotalSamples},{_,TotalValue}, {connector, Connector}}} = Bucket,
	case FunName of
		getSampleCount ->
			SampleCount;
		getTotal -> 
			Total;
		getTotalSquared ->
			TotalSquared;
		getTotalSamples ->
			TotalSamples;
		getTotalValue ->
			TotalValue;
		getAverage ->
			if(TotalSamples == 0) or not(is_float(Total))->
            	float(0.0);
        	true ->
            	Total/float(TotalSamples)
			end;
		getStandardDeviation ->
			Total;
		getMaximum ->
			Maximum;
		getMinimum ->
			Minimum;
		getWorstCategory ->
			WorstCategory;
		getConnector ->
			Connector;
		_->
			error
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%