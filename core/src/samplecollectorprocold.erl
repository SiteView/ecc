%% Author: Administrator
%% Created: 2010-1-7
%% Description: TODO: Add description to samplecollectorproc
-module(samplecollectorprocold,[]).

%%
%% Include files
%%
-extends(samplestatisticsproc).
-compile(export_all).
-include("monitor.hrl").
-include("historyreport.hrl").
-include("monitor_template.hrl").

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
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
	Obj:set_property(monitor, []),
	{?MODULE,Obj}.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
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
	Obj:set_property(monitor, Monitor),
	Obj:set_property(property, Property),
	Obj:set_property(keepsamplebuffer, KeepSampleBuffer),
	
	{?MODULE,Obj}.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
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
	Obj:set_property(monitor, Monitor),
	Obj:set_property(property, Property),
	Obj:set_property(keepsamplebuffer, false),	
	{?MODULE,Obj}.

%%
%% Local Functions
%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
createBuckets(StartTime, EndTime, Precision) ->
	THIS:set_property(startTime, StartTime),
	THIS:set_property(endTime, EndTime),
	THIS:set_property(precision, Precision),
	L3 = EndTime - StartTime,
	L4 = L3/ Precision + 1,
	Buckets = THIS:constructBuckets(1, [], L4),
	io:format("~p ~n", [L4]),
	THIS:set_property(buckets, Buckets).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
constructBuckets(Count, T, Max) when Count > Max ->
		T;
constructBuckets(Count, T, Max) ->
		constructBuckets(Count+1, [{Count+1, {{worstCategory, nodata},{lastCategory, nodata},{sampleCount, 0},{minimum, 0.0}, {maximum, 0.0}, {total, 0.0},{totalSquared, 0.0}, {totalSamples, 0.0}, {totalValue, ""}, {connector, THIS}}}|T], Max).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMonitor() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	Monitor.
get_parent_full_name()->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	Monitor:get_parent_full_name().

get_last_description() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
    {ok,{_,State}} = Monitor:get_attribute(state_string),
	State.
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getPropertyLabel() ->
	{ok,{_,Property}} = THIS:get_property(property),
	Property.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMonitorName() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	{ok,{_,Name}} = Monitor:get_property(name),
	Name.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMonitorDescription() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	{ok,{_,Des}} = Monitor:get_property(description),
	Des.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMonitorFullID() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	FullID = Monitor:getFullID(),
	FullID.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMonitorTitle() ->
	{ok,{_,Monitor}} = THIS:get_property(monitor),
	{ok,{_,Title}} = Monitor:get_property(title),
	Title.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getProperty() ->
	{ok,{_,Property}} = THIS:get_property(property),
	Property.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMaximumTime() ->
	{ok,{_,MaximumTime}} = THIS:get_property(maximumTime),
	MaximumTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMinimumTime() ->
	{ok,{_,MinimumTime}} = THIS:get_property(minimumTime),
	MinimumTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getLastCategory() ->
	{ok,{_,LastCategory}} = THIS:get_property(lastCategory),
	LastCategory.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
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
%% @doc read report info from contentstore
getLastTime() ->
	{ok,{_,LastTime}} = THIS:get_property(lastTime),
	LastTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getErrorTime() ->
	{ok,{_,ErrorTime}} = THIS:get_property(errorTime),
	ErrorTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getWarningTime() ->
	{ok,{_,WarningTime}} = THIS:get_property(warningTime),
	WarningTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getGoodTime() ->
	{ok,{_,GoodTime}} = THIS:get_property(goodTime),
	GoodTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getNaTime() ->
	{ok,{_,NaTime}} = THIS:get_property(lastTime),
	NaTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotalTime() ->
	{ok,{_,TotalTime}} = THIS:get_property(totalTime),
	TotalTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getBucketCount() ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	length(Buckets).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getIDString() ->
	{ok,{_,IdString}} = THIS:get_property(idString),
	IdString.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
isNumeric() ->
	true.
	
%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
isPercentage() ->
	{ok,{_,LastTime}} = THIS:get_property(lastTime),
	LastTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getStartTime() ->
	{ok,{_,ActualStartTime}} = THIS:get_property(actualStartTime),
	ActualStartTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getEndTime() ->
	{ok,{_,ActualEndTime}} = THIS:get_property(actualEndTime),
	ActualEndTime.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getBucketStartTime(I) ->
	{ok,{_,StartTime}} = THIS:get_property(startTime),
	{ok,{_,EndTime}} = THIS:get_property(endTime),
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	Ret = StartTime + round(I*((EndTime - StartTime)/(length(Buckets) - 1))),
	Ret.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getSampleCount(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getSampleCount).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotal(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getTotal).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotalSamples(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getTotalSamples).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotalValue(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getTotalValue).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getTotalValueCarry(I) ->
	(THIS:getNonEmptyBucket(I)):getTotalValue().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getWorstCategory(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getWorstCategory).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getWorstCategoryCarry(I) ->
	(THIS:getNonEmptyBucket(I)):getWorstCategory().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getAverage(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getAverage).	

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getAverageCarry(I) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMaximum(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getMaximum).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMaximumCarry(I) ->
	(THIS:getNonEmptyBucket(I)):getMaximum().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMinimum(I) ->	
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	THIS:getBucketData(lists:nth(I, Buckets), getMinimum).

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMinimumCarry(I) ->
	(THIS:getNonEmptyBucket(I)):getMinimum().

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getNonEmptyBucket(0) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),
	(lists:nth(0, Buckets));
getNonEmptyBucket(I) ->
	{ok,{_,Buckets}} = THIS:get_property(buckets),	
	
	case THIS:getBucketData(lists:nth(I, Buckets), getSampleCount) =:= 0 of
		true ->
			THIS:getNonEmptyBucket(I - 1);
		_ ->
			lists:nth(I, Buckets)	
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
add_1(Time, Fields, FieldsLength, Category, ErrorType) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
add(Time, Fields, FieldsLength, Category, ErrorType, BestCaseCalc) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
add_2(Time, Value, Category, ErrorType, BestCaseCalc) ->
	{ok,{_,LastTime}} = THIS:get_property(lastTime),	
	case (LastTime =/= 0) of
		true ->
	    	{ok,{_,LastValue}} = THIS:get_property(lastValue),
			{ok,{_,LastCategory}} = THIS:get_property(lastCategory),
			{ok,{_,LastErrorType}} = THIS:get_property(lastErrorType),		 
			THIS:add(calendar:gregorian_seconds_to_datetime(LastTime), calendar:datetime_to_gregorian_seconds(Time) - LastTime, LastValue, LastCategory, LastErrorType);
		_->
			ok
	end,

	THIS:set_property(lastTime, calendar:datetime_to_gregorian_seconds(Time)),
	THIS:set_property(lastValue, Value),
	THIS:set_property(lastCategory, Category),
	THIS:set_property(lastErrorType, ErrorType).		

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
add(Time, Interval, Value, Category, ErrorType) ->
	{ok,{_,ActualStartTime}} = THIS:get_property(actualStartTime),
	if
		(ActualStartTime == 0) ->
			THIS:set_property(actualStartTime, Time);
		true ->
		     ok
	end,
	{ok,{_,StartTime}} = THIS:get_property(startTime),
	{ok,{_,Precision}} = THIS:get_property(precision),
	THIS:set_property(actualEndTime, Time),

	BucketNumber = trunc(((calendar:datetime_to_gregorian_seconds(Time) - StartTime)/Precision)),
	{ok,{_,Buckets}} = THIS:get_property(buckets),

 	Bucket = lists:nth(BucketNumber + 1, Buckets),
	SampleIsDisabled = ((Category == disabled) or (Category == filtered)),
	{ok,{_,Property}} = THIS:get_property(property),
	case lists:keysearch(Property, 1, Value) of
		{value, {_, V}} ->
			THIS:addBucket(Buckets, BucketNumber, Bucket, Time, V, Category, SampleIsDisabled);
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

	TT = (THIS:getSampleCount()) + 1,
	THIS:set_property(sampleCount, TT),	
	
	case SampleIsDisabled of
	    false ->
			case lists:keysearch(Property, 1, Value) of
				{value, {_, V1}} ->			
					case is_integer(V1) or is_float(V1) of 
					  true ->
						Value1 = float(V1),

						T1 = THIS:getTotal() + Value1,
						THIS:set_property(total, T1),

						T2 = (THIS:getTotalSamples()) + 1,
						THIS:set_property(totalSamples, T2),
					
						case (Value1 =< THIS:getMinimum()) of
							true ->
								THIS:set_property(minimum, Value1),
								THIS:set_property(minimumTime, Time);							
							_ ->
								ok
						end,
			
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
%% @doc read report info from contentstore
addold(Time, Interval, Value, Category, ErrorType) ->
	{ok,{_,ActualStartTime}} = THIS:get_property(actualStartTime),
	if
		(ActualStartTime == 0) ->
			THIS:set_property(actualStartTime, Time);
		true ->
		     ok
	end,
	{ok,{_,StartTime}} = THIS:get_property(startTime),
	{ok,{_,Precision}} = THIS:get_property(precision),
	THIS:set_property(actualEndTime, Time),

	BucketNumber = trunc(((calendar:datetime_to_gregorian_seconds(Time) - StartTime)/Precision)),
	{ok,{_,Buckets}} = THIS:get_property(buckets),

 	Bucket = lists:nth(BucketNumber + 1, Buckets),
	SampleIsDisabled = ((Category == disabled) or (Category == filtered)),	
	
	{ok,{_,Property}} = THIS:get_property(property),
	case lists:keysearch(Property, 1, Value) of
		{value, {_, V}} ->
			Bucket:add(Time, V, Category, SampleIsDisabled);
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
	TT = (THIS:getSampleCount()) + 1,
	THIS:set_property(sampleCount, TT),	
	
	case SampleIsDisabled of
		false ->
			case lists:keysearch(Property, 1, Value) of
				{value, {_, V1}} ->			
					case is_integer(V1) or is_float(V1) of 
					  true ->
						Value1 = float(V1),

						T1 = THIS:getTotal() + Value1,
						THIS:set_property(total, T1),

						T2 = (THIS:getTotalSamples()) + 1,
						THIS:set_property(totalSamples, T2),
					
						case (Value1 =< THIS:getMinimum()) of
							true ->
								THIS:set_property(minimum, Value1),
								THIS:set_property(minimumTime, Time);							
							_ ->
								ok
						end,
			
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
%% @doc read report info from contentstore
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
%% @doc read report info from contentstore
clearSampleBuffer() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getMaxSampleBuffer() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
getSampleBuffer() ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
samplesAreAveraged() ->
	{ok,{_,AveragedSamples}} = THIS:get_property(averagedSamples),
	AveragedSamples.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
toString() ->
	{ok,{_,IdString}} = THIS:get_property(idString),
	IdString.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
equals(Obj) ->
	ok.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
hashCode() ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
addBucket(Buckets, BucketsNumber, Bucket, Time, Value, Category, SampleIsDisabled) ->
	{Count, {{_,WorstCategory},{_,LastCategory},{_,SampleCount},{_,Minimum},{_,Maximum},{_,Total},{_,TotalSquared},{_,TotalSamples},{_,TotalValue},{_,Connector}}} = Bucket,
	WorstCategoryNew = historyreport_static:getWorstCategory(WorstCategory, Category),	
	case SampleIsDisabled of
		false ->
			case is_integer(Value) or is_float(Value)of 
				true ->
					Value1 = float(Value),					
					TotalNew = Total + Value1,
					TotalSamplesNew = TotalSamples + 1,

					case (Value1 =< THIS:getMinimum()) of
						true ->
							MinimumNew = Value1;
						_->
							MinimumNew = Minimum
					end,
			
					case (Value1 >= THIS:getMaximum()) of
						 true ->
							 MaximumNew = Value1;
						 _ ->
							MaximumNew = Maximum
					end,
					[B1] = lists:keyreplace(Count, 1, [Bucket], {Count, {{worstCategory, WorstCategoryNew},{lastCategory, LastCategory},{sampleCount, SampleCount},{minimum, MinimumNew}, {maximum, MaximumNew}, {total, TotalNew},{totalSquared, TotalSquared}, {totalSamples, TotalSamplesNew}, {totalValue, TotalValue}, {connector, THIS}}}),
					{_, B3} = B1,
					T1 = lists:keyreplace(Count, 1, Buckets, {Count, B3}),
					THIS:set_property(buckets, T1);			
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
					THIS:set_property(buckets, T1)
			end;
		_->
			ok		
	end.

%% @spec readFromHashMap(X) -> ok 
%% Obj = term()
%% @doc read report info from contentstore
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