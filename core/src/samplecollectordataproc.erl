%% @author xingyu.cheng@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc samplecollector
-module(samplecollectordataproc, [BASE]).
-extends(samplestatisticsproc).
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
	Obj = samplestatisticsproc:new(),
	Obj:set_property(startTime, 0),	
	Obj:set_property(endTime, 0),
	Obj:set_property(precision, 0),
	Obj:set_property(actualStartTime, 0),
	Obj:set_property(actualEndTime, 0),
	Obj:set_property(lastValue, "n/a"),
	Obj:set_property(lastTime, 0),
	Obj:set_property(minimumTime, 0),
	Obj:set_property(maximumTime, 0),
	Obj:set_property(errorTime, 0),
	Obj:set_property(warningTime, 0),
	Obj:set_property(goodTime, 0),
	Obj:set_property(naTime, 0),
	Obj:set_property(totalTime, 0),
	Obj:set_property(sampleBuffer, []),
	Obj:set_property(bufferLastTime, 0),
	Obj:set_property(averagedSamples, false),
	Obj:set_property(maxSampleBuffer, []),
	Obj:set_property(warningNotIncluded, false),
	Obj:set_property(failureNotIncluded, false),
	Obj:set_property(showFullMonitorName, false),
	Obj:set_property(debug, false),
	Obj:set_property(buckets, []),
%% 	Obj:set_property(bucketsTemp, []}),	
	Obj:set_property(monitor, []),
	{?MODULE,Obj}.

%% @spec new() -> ok 
%% Obj = term()
%% @doc 
new(Monitor, Property, KeepSampleBuffer, WarningNotIncluded, FailureNotIncluded)->
	Obj = samplestatisticsproc:new(),
	Obj:set_property(startTime, 0),
	Obj:set_property(endTime, 0),
	Obj:set_property(precision, 60),
	Obj:set_property(actualStartTime, 0),
	Obj:set_property(actualEndTime, 0),
	Obj:set_property(lastValue, "n/a"),
	Obj:set_property(lastTime, 0),
	Obj:set_property(minimumTime, 0),
	Obj:set_property(maximumTime, 0),
	Obj:set_property(errorTime, 0),
	Obj:set_property(warningTime, 0),
	Obj:set_property(goodTime, 0),
	Obj:set_property(naTime, 0),
	Obj:set_property(totalTime, 0),
	Obj:set_property(sampleBuffer, []),
	Obj:set_property(bufferLastTime, 0),
	Obj:set_property(averagedSamples, false),
	Obj:set_property(maxSampleBuffer, []),
	Obj:set_property(warningNotIncluded, WarningNotIncluded),
	Obj:set_property(failureNotIncluded, FailureNotIncluded),
	Obj:set_property(showFullMonitorName, false),
	Obj:set_property(debug, false),
	Obj:set_property(buckets, []),
%% 	Obj:set_property(bucketsTemp, []}),
	Obj:set_property(monitor, Monitor),
	Obj:set_property(property, Property),
	Obj:set_property(keepsamplebuffer, KeepSampleBuffer),
	
	{?MODULE,Obj}.

%% @spec new(X) -> ok 
%% Obj = term()
%% @doc 
new(Monitor, Property)->
	Obj = samplestatisticsproc:new(),	
	Obj:set_property(startTime, 0),
	Obj:set_property(endTime, 0),
	Obj:set_property(precision, 60),
	Obj:set_property(actualStartTime, 0),
	Obj:set_property(actualEndTime, 0),
	Obj:set_property(lastValue, "n/a"),
	Obj:set_property(lastTime, 0),
	Obj:set_property(minimumTime, 0),
	Obj:set_property(maximumTime, 0),
	Obj:set_property(errorTime, 0),
	Obj:set_property(warningTime,0),
	Obj:set_property(goodTime,0),
	Obj:set_property(naTime,0),
	Obj:set_property(totalTime,0),
	Obj:set_property(sampleBuffer, []),
	Obj:set_property(bufferLastTime,0),
	Obj:set_property(averagedSamples, false),
	Obj:set_property(maxSampleBuffer, []),
	Obj:set_property(warningNotIncluded, false),
	Obj:set_property(failureNotIncluded, false),
	Obj:set_property(showFullMonitorName, false),
	Obj:set_property(debug, false),
	Obj:set_property(buckets, []),
%% 	Obj:set_property(bucketsTemp, []),
	Obj:set_property(monitor, Monitor),
	Obj:set_property(property, Property),
	Obj:set_property(keepsamplebuffer, false),	
	{?MODULE,Obj}.

%%
%% Local Functions
%%

%% @spec createBuckets(StartTime, EndTime, Precision) -> ok 
%% Obj = term()
%% @doc 
createBuckets(StartTime, EndTime, Precision, Length) ->
	THIS:set_property(startTime, StartTime),
	THIS:set_property(endTime, EndTime),
	THIS:set_property(precision, Precision),
%% 	L3 = EndTime - StartTime,
%% 	L4 = L3/ Precision,
	Buckets = constructBuckets(0, [], Length, StartTime),
%% 	io:format("~p ~n", [Length]),
%% 	T1 = lists:keyreplace(137, 1, Buckets, {137, {xxx, yyy}}),
%% 	io:format("~p ~n", [lists:nth(10, T1)]),	
	THIS:set_property(buckets, Buckets).

%% @spec constructBuckets(Count, T, Max) -> ok 
%% Obj = term()
%% @doc 
constructBuckets(Count, T, Max, StartTime) when Count >= Max ->
		T;
constructBuckets(Count, T, Max, StartTime) ->
%% 	constructBuckets(Count+1, [{Count+1, {{worstCategory, nodata},{lastCategory, nodata},{sampleCount, 0},{minimum, 0.0}, {maximum, 0.0}, {total, 0.0},{totalSquared, 0.0}, {totalSamples, 0.0}, {totalValue, ""}, {time, calendar:gregorian_seconds_to_datetime(StartTime)}, {connector, THIS}}}|T], Max, StartTime).
	constructBuckets(Count+1, [{Count+1, {{worstCategory, nodata},{lastCategory, nodata},{sampleCount, 0},{minimum, 0.0}, {maximum, 0.0}, {total, 0.0},{time, calendar:gregorian_seconds_to_datetime(StartTime)}, {totalSamples, 0.0}, {totalValue, ""},  {connector, THIS}}}|T], Max, StartTime).
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getBuckets() ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	Buckets.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitor() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	Monitor.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
get_parent_full_name()->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	Monitor:get_parent_full_name().

%% 
%% 
%% 
get_last_description() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
    {ok,{_,State}} = Monitor:get_attribute(state_string),
	State.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getPropertyLabel() ->
	{ok,{_,Property}} = THIS:get_property(property),
	{ok,{_,Monitor}} = THIS:get_property(monitor),	
	{_,{_,Class}}=Monitor:get_property(class),
	Template=api_monitor_template:get_template(Class),
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
	{ok,{_,Property}} = THIS:get_property(property),
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	{_,{_,Class}}=Monitor:get_property(class),
	Template=api_monitor_template:get_template(Class),
	case lists:keysearch(Property,#property.name, Template) of
		 {V,Plabel} ->
			 Ret =element(4, Plabel);
		 _ ->
			 Ret=numeric
	end,
    Ret.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorName() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
%% 	io:format("~p ~n", [Monitor]),
	{ok,{_,Name}} = Monitor:get_property(name),
%% 	io:format("~p ~n", [Name]),
	Name.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorDescription() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	{ok,{_,Des}} = Monitor:get_property(description),
	Des.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorFullID() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	FullID = Monitor:getFullID(),
	FullID.
%% 	{ok,{_,FullID}} = Monitor:get_property(fullid),
%% 	FullID.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMonitorTitle() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	{ok,{_,Title}} = Monitor:get_property(title),
	Title.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getProperty() ->
	{ok,{_,Property}} = THIS:get_property(property),
	Property.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMaximumTime() ->
	{ok,{_,MaximumTime}} = THIS:get_property(maximumTime),
	MaximumTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getMinimumTime() ->
	{ok,{_,MinimumTime}} = THIS:get_property(minimumTime),
	MinimumTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getLastCategory() ->
	{ok,{_,LastCategory}} = THIS:get_property(lastCategory),
	LastCategory.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getLastValue() ->
	{ok,{_,LastValue}} = THIS:get_property(lastValue),
	{ok,{_,Property}} = THIS:get_property(property),
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
	{ok,{_,LastTime}} = THIS:get_property(lastTime),
	LastTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getErrorTime() ->
	{ok,{_,ErrorTime}} = THIS:get_property(errorTime),
	ErrorTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getWarningTime() ->
	{ok,{_,WarningTime}} = THIS:get_property(warningTime),
	WarningTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getGoodTime() ->
	{ok,{_,GoodTime}} = THIS:get_property(goodTime),
	GoodTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getNaTime() ->
	{ok,{_,NaTime}} = THIS:get_property(lastTime),
	NaTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalTime() ->
	{ok,{_,TotalTime}} = THIS:get_property(totalTime),
	TotalTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getBucketCount() ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	length(Buckets).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getIDString() ->
	{ok,{_,IdString}} = THIS:get_property(idString),
	IdString.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
isNumeric() ->
%% 	{ok,{_,Property}} = THIS:get_property(property),
%% 	{ok,{_,Monitor}} = THIS:get_property(monitor),
%% 	Props = Monitor:getLogProperties(Monitor),
%% 	case lists:keysearch(Key, N, Props) of
%% 		{value, {_, V}} ->
%% 			
%% 	end
%% 	is_integer(Property).
	true.
%% 	{ok,{_,LastTime}} = THIS:get_property(lastTime),
%% 	LastTime.
%%     {
%%         return property instanceof NumericProperty;
%%     }
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
isPercentage() ->
	{ok,{_,LastTime}} = THIS:get_property(lastTime),
	LastTime.
%%     {
%%         return property instanceof PercentProperty;
%%     }

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getStartTime() ->
	{ok,{_,ActualStartTime}} = THIS:get_property(actualStartTime),
	ActualStartTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getEndTime() ->
	{ok,{_,ActualEndTime}} = THIS:get_property(actualEndTime),
	ActualEndTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getBucketStartTime(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
%% 	io:format("~p ~n", [THIS:getBucketData(lists:nth(I, Buckets), getTime)]),
	calendar:datetime_to_gregorian_seconds(THIS:getBucketData(lists:nth(I, Buckets), getTime)).
%% 	calendar:datetime_to_gregorian_seconds(THIS:getBucketData(lists:nth(I, Buckets), getTime)).
%% %% 	io:format("~p ~n", [I]),
%% 	{ok,{_,StartTime}} = THIS:get_property(startTime),
%% 	{ok,{_,EndTime}} = THIS:get_property(endTime),
%% %% 	io:format("~p ~n", [StartTime]),
%% %% 	io:format("~p ~n", [EndTime]),
%% 	{ok,{_,Buckets}} = THIS:get_property(buckets),
%% %% 	io:format("~p ~n", [Buckets]),length(_)
%% %% 	io:format("~p ~n", [I*((EndTime - StartTime)/(length(Buckets) - 1))]),
%% %% 	Ret = StartTime + round(I*((EndTime - StartTime)/(length(Buckets) - 1))),
%%  	Ret = StartTime + trunc(I*((EndTime - StartTime)/(length(Buckets)))),
%% %% 	io:format("ok : ~p ~n", [Ret]),
%% 	Ret.
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getSampleCount(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
%% 	(lists:nth(I, Buckets)):getSampleCount().
	THIS:getBucketData(lists:nth(I, Buckets), getSampleCount).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotal(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
%% 	(lists:nth(I, Buckets)):getTotal().
	THIS:getBucketData(lists:nth(I, Buckets), getTotal).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalSamples(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
%% 	(lists:nth(I, Buckets)):getTotalSamples().
	THIS:getBucketData(lists:nth(I, Buckets), getTotalSamples).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
getTotalValue(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
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
	{ok,{_,Buckets}} = THIS:get_property(buckets),
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
	{ok,{_,Buckets}} = THIS:get_property(buckets),
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
	{ok,{_,Buckets}} = THIS:get_property(buckets),
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
	{ok,{_,Buckets}} = THIS:get_property(buckets),
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
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	(lists:nth(0, Buckets));
getNonEmptyBucket(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),	
	
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
	{ok,{_,LastTime}} = THIS:get_property(lastTime),	
	case (LastTime =/= 0) of
		true ->
%% 			io:format("~p ~n", ["Time2"]),
%% 			THIS:set_property(lastTime, calendar:datetime_to_gregorian_seconds(Time)}),
%% 			THIS:set_property(lastValue, Value}),
%% 			THIS:set_property(lastCategory, Category}),
%% 			THIS:set_property(lastErrorType, ErrorType}),			
%% 	    	{ok,{_,LastValue}} = THIS:get_property(lastValue),
%% 			{ok,{_,LastCategory}} = THIS:get_property(lastCategory),
%% 			{ok,{_,LastErrorType}} = THIS:get_property(lastErrorType),
%% 			{ok,{_,LastTime1}} = THIS:get_property(lastErrorType),
			THIS:add(Time, calendar:datetime_to_gregorian_seconds(Time) - LastTime, Value, Category, ErrorType);
%% 			ok;
		_->
%% 			A=calendar:datetime_to_gregorian_seconds(Time),
			THIS:add(Time, 0, Value, Category, ErrorType),
			ok
	end,
%% 	io:format("~p ~n", ["Time3"]),
	THIS:set_property(lastTime, calendar:datetime_to_gregorian_seconds(Time)),
	THIS:set_property(lastValue, Value),
	THIS:set_property(lastCategory, Category),
	THIS:set_property(lastErrorType, ErrorType).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
add(Time, Interval, Value, Category, ErrorType) ->
%% 	io:format("~p ~n", [Time]),
	{ok,{_,ActualStartTime}} = THIS:get_property(actualStartTime),
	if
		(ActualStartTime == 0) ->
			THIS:set_property(actualStartTime, Time);
		true ->
		     ok
	end,
	
	{ok,{_,StartTime}} = THIS:get_property(startTime),
	{ok,{_,EndTime}} = THIS:get_property(endTime),
	
	{ok,{_,Precision}} = THIS:get_property(precision),
	THIS:set_property(actualEndTime, Time),	
	TT = (THIS:getSampleCount()) + 1,
	THIS:set_property(sampleCount, TT),
	BucketNumberOld = TT,
	{ok,{_,Buckets}} = THIS:get_property(buckets),	

	case BucketNumberOld of
		0 ->
			BucketNumber  = 1,
			Bucket = lists:nth(1, Buckets);
		_ ->
			BucketNumber = BucketNumberOld,
			Bucket = lists:nth(BucketNumber, Buckets)
	end,

	SampleIsDisabled = ((Category == disabled) or (Category == filtered)),
%% 	io:format("~p ~n", [SampleIsDisabled]),
	{ok,{_,Property}} = THIS:get_property(property),
	case lists:keysearch(Property, 1, Value) of
		{value, {_, V}} ->
%% 			io:format("~p ~n", [V]);
%% 			Bucket:add(Time, V, Category, SampleIsDisabled);
			THIS:addBucket(Buckets, BucketNumber, Bucket, Time, V, Category, SampleIsDisabled);
%% 			ok;
		false ->
			error
	end,	
	if
		(SampleIsDisabled == false) ->
			{ok,{_,TotalTime}} = THIS:get_property(totalTime),
			TR = TotalTime + Interval,
			THIS:set_property(totalTime, TR);
		true ->
			ok
	end,
	
	THIS:calculate(Category, Interval, ErrorType),

	case Category =/= THIS:getLastCategory() of
		true ->
			WorstCategory = historyreport_static:getWorstCategory(THIS:getWorstCategory(), Category),
			THIS:set_property(worstCategory, WorstCategory);			
		_->
			ok
	end,

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
						THIS:set_property(total, T1),

						T2 = (THIS:getTotalSamples()) + 1,
						THIS:set_property(totalSamples, T2),
%% 						this.totalSquared += Math.pow(floatValue, 2.0D);
					
						case (Value1 == 0.0) of
							true ->
%% 								io:format("Minunm ~p ~n", [Value1]),
								MinimumNew = 0.0,
								THIS:set_property(minimum, MinimumNew),
								THIS:set_property(minimumTime, Time);						
							_->
							case (Value1 =< THIS:getMinimum()) of
								true ->
									MinimumNew = Value1,
									THIS:set_property(minimum, MinimumNew),
									THIS:set_property(minimumTime, Time);
								_->
									case (THIS:getMinimum() == 0.0) and (THIS:getMinimumTime() == 0) of
										true ->
	 										MinimumNew = Value1,
											THIS:set_property(minimum, MinimumNew),
											THIS:set_property(minimumTime, Time);
										_ ->
											ok
									end,
									ok
							end
						end,
						
%% 						case (Value1 =< THIS:getMinimum()) of
%% 							true ->
%% 								THIS:set_property(minimum, Value1}),
%% 								THIS:set_property(minimumTime, Time);							
%% 							_ ->
%% 								ok
%% 						end,

%% 								THIS:set_property(minimum, Value1}),
%% 								THIS:set_property(minimumTime, Time),
						
						case (Value1 >= THIS:getMaximum()) of
							 true ->
								THIS:set_property(maximum, Value1),
								THIS:set_property(maximumTime, Time);
						 	_ ->
								ok
						end;				
				 	_ ->
						THIS:set_property(totalValue, Value)
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
			{ok,{_,ErrorTime}} = THIS:get_property(errorTime),
			T1 = ErrorTime + Interval,
			THIS:set_property(errorTime, T1);
		good ->
			{ok,{_,GoodTime}} = THIS:get_property(goodTime),
			T1 = GoodTime + Interval,			
			THIS:set_property(goodTime, T1);
		warning ->
			{ok,{_,WarningTime}} = THIS:get_property(warningTime),
			T1 = WarningTime + Interval,			
			THIS:set_property(warningTime, T1);
		_->						
			{ok,{_,NaTime}} = THIS:get_property(naTime),
			T1 = NaTime + Interval,
			THIS:set_property(naTime, T1)
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
	{ok,{_,AveragedSamples}} = THIS:get_property(averagedSamples),
	AveragedSamples.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc 
toString() ->
	{ok,{_,IdString}} = THIS:get_property(idString),
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
addBucket(Buckets, BucketsNumber, Bucket, Time, Value, Category, SampleIsDisabled) ->
	{Count, {{_,WorstCategory},{_,LastCategory},{_,SampleCount},{_,Minimum},{_,Maximum},{_,Total},{_,Time1},{_,TotalSamples},{_,TotalValue}, {_,Connector}}} = Bucket,
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
					[B1] = lists:keyreplace(Count, 1, [Bucket], {Count, {{worstCategory, WorstCategoryNew},{lastCategory, LastCategory},{sampleCount, SampleCount},{minimum, MinimumNew}, {maximum, MaximumNew}, {total, TotalNew}, {time, Time}, {totalSamples, TotalSamplesNew}, {totalValue, TotalValue}, {connector, THIS}}}),
					{_, B3} = B1,
					T1 = lists:keyreplace(Count, 1, Buckets, {Count, B3}),
%% 					T2 = lists:nth(BucketsNumber + 1, T1),
%% 					io:format("T2:~p ~n", [T2]),
%% 					{ok,{_,BucketsTemp}} = THIS:get_property(bucketsTemp),
%% 					T1 = BucketsTemp ++ B1,
					THIS:set_property(buckets, T1);			
				 _ ->
					case (WorstCategoryNew =:= Category) or (TotalValue == 0) of
						true->
							TotalValueNew = Value;
						_->
							TotalValueNew = TotalValue
					end,					
					[B1] = lists:keyreplace(Count, 1, [Bucket], {Count, {{worstCategory, WorstCategoryNew},{lastCategory, LastCategory},{sampleCount, SampleCount},{minimum, Minimum}, {maximum, Maximum}, {total, Total}, {time, Time}, {totalSamples, TotalSamples}, {totalValue, TotalValueNew}, {connector, THIS}}}),
					{_, B3} = B1,
					T1 = lists:keyreplace(Count, 1, Buckets, {Count, B3}),
					THIS:set_property(buckets, T1)
			end;
		_->
			ok		
	end.

%% @spec getBucketData(Bucket, FunName) -> ok 
%% Obj = term()
%% @doc 
getBucketData(Bucket, FunName) ->
	{Count, {{_,WorstCategory},{_,LastCategory},{_,SampleCount},{_,Minimum},{_,Maximum},{_,Total},{_, Time},{_,TotalSamples},{_,TotalValue}, {connector, Connector}}} = Bucket,
	case FunName of
		getSampleCount ->
			SampleCount;
		getTotal -> 
			Total;
%% 		getTotalSquared ->
%% 			TotalSquared;
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
		getTime ->
			Time;
		_->
			error
	end.

dropwhileBucket() ->
	{ok,{_,StartTime}} = THIS:get_property(startTime),	
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	F = fun(X)->
		{_, X1} = X,
		T1 = element(10,X1)=:={time, calendar:gregorian_seconds_to_datetime(StartTime)},
		T1
	end,
	lists:dropwhile(F, Buckets).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%