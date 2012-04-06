%% Author: Administrator
%% Created: 2011-1-10
%% Description: TODO: Add description to report_statics
-module(report_statics).

%%
%% Include files
%%

%%
%% Exported Functions
%%
%-export([]).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

%% {monitorlog,'0.2.208',"Ping:www.sina.com",
%%             {{2010,12,22},{2,58,26}},
%%             good," Average = 104ms",
%%             [{percentgood,...},{...}|...]},

%
%{Name, 0}->flag
%{Name, 1}->bucket

init([])->
	done;
init([{Name, Type}|MeasurementType])->
	
	put({Name, {time, filtered}}, 0),
	put({Name, {time, nodata}}, 0),
	put({Name, {time, disabled}}, 0),
	put({Name, {time, good}}, 0),
	put({Name, {time, error}}, 0),
	put({Name, {time, warning}}, 0),
	put({Name, {time, worst}}, 0),
	
%% 	put({Name, totalTime}, 0),
%% 	put({Name, errorTime}, 0),
%% 	put({Name, goodTime}, 0),
%% 	put({Name, warningTime}, 0),
%% 	put({Name, naTime}, 0),
	
	put({Name, 0}, Type =:= numeric),
	put({Name, 1}, []),
	put({Name, lastTime}, undefined),
	put({Name, lastValue}, undefined),
	put({Name, lastCategory}, undefined),
	put({Name, lastErrorType}, undefined),
	put({Name, actualStartTime}, undefined),
	put({Name, actualEndTime}, undefined),
	put({Name, worstCategory}, undefined),
	put({Name, total}, 0),
	put({Name, totalValue}, 0),
	put({Name, totalSamples}, 0),
	put({Name, sampleCount}, 0),
	put({Name, minimum}, undefined),
	put({Name, minimumTime}, undefined),
	put({Name, maximum}, 0),
	put({Name, maximumTime}, undefined),
	init(MeasurementType).

access(_, _, [])->
	done;
access(Time, Category, [{Name, Value}|Measurment])->
	Flag = get({Name, 0}),
	case Flag =/= undefined of
		true->
			ActualStartTime = get({Name, actualStartTime}),
			case ActualStartTime of
				undefined->
					put({Name, actualStartTime}, Time);
				_ ->
					nothing
			end,
			put({Name, actualEndTime}, Time),

			SampleIsSkip = ((Category == disabled) or (Category == filtered)),	
			case get(Category) > get(get({Name, worstCategory})) of
				true->
					put({Name, worstCategory}, Category);
				_ ->
					nothing
			end,
			
			LastTime = get({Name, lastTime}),
			CurrentTime = calendar:datetime_to_gregorian_seconds(Time),
			
			%io:format("Name----------------->~p~n",[Name]),
			%io:format("Time----------------->~p~n",[Time]),
			Interval = case LastTime =/= undefined of
				true->
					CurrentTime - LastTime;
				_ ->
					0
			end,
			%io:format("CurrentTime----------------->~p~n",[CurrentTime]),
			put({Name, lastTime}, CurrentTime),
%% 			io:format("get({Name, {time, Category}})----------------->~p~n",[get({Name, {time, Category}})]),
			put({Name, {time, Category}}, get({Name, {time, Category}}) + Interval),
			
			put({Name, 1}, [{Time, Category, Value}|get({Name, 1})]),
			
			%io:format("get({Name,sampleCount})----------------->~p~n",[get({Name, {time, sampleCount}})]),
			put({Name, sampleCount}, get({Name,sampleCount}) + 1),
			put({Name, totalSamples}, get({Name, totalSamples}) + 1),
			
%% 			put({Name, lastTime}, Time),
			put({Name, lastValue}, Value),
			put({Name, lastCategory}, Category),
			put({Name, lastErrorType}, Category),
			case SampleIsSkip of
			    false ->	
							case is_integer(Value) or is_float(Value) of 
%% 								is_list(Value)->
%% %% 									io:format("Value1----------------->~p~n",[Value]),
%% 									put({Name, totalValue}, Value);
%% 								is_atom(Value)->
%% %% 									io:format("Value1----------------->~p~n",[Value]),
%% 									put({Name, totalValue}, Value);	
								true->
%% 									io:format("Value2----------------->~p~n",[Value]),
									put({Name, total}, get({Name,total}) + Value),
									Min = get({Name, minimum}),
									if 
										Min == undefined ->
											put({Name, minimum}, Value),
											put({Name, minimumTime}, Time);											
										Value =< Min ->
											put({Name, minimum}, Value),
											put({Name, minimumTime}, Time);
										Min == 0 ->											
											put({Name, minimum}, Min),
											put({Name, minimumTime}, Time);
										true->
											nothing
									end, 
										
									Max = get({Name, maximum}),
									if 
										Value > Max ->
											put({Name, maximum}, Value),
											put({Name, maximumTime}, Time);
										true->
											nothing
									end;
								_->
								case Value of
									"n/a" ->
										put({Name, minimum}, 0),
										put({Name, minimumTime}, Time),
										put({Name, totalValue}, Value);
									"denied" ->
										put({Name, minimum}, 0),
										put({Name, minimumTime}, Time),
										put({Name, totalValue}, Value);										
									_->
										put({Name, totalValue}, Value)
								end
							end;
				_->
					put({Name, totalValue}, Value)
			end;
		_ ->
			nothing
	end,
	access(Time, Category, Measurment).

traverse([])->
	done;
traverse([Record|Rest])->
	Time = element(4, Record),	
	Category = element(5, Record),
	
	Measurment=element(7, Record),
%% 	Measurment = try
%% 		binary_to_term(element(7, Record))
%% 	catch
%% 		_:_->
%% %% 			io:format("Record: ~p~n", [binary_to_term(base64:decode(element(7, Record)))]),
%% 			binary_to_term(base64:decode(element(7, Record)))
%% 	end,
	
	try
		access(Time, Category, Measurment),
		traverse(Rest)
	catch
		E:E1->
			io:format("Record traverse: ~p, ~p ~n", [E, E1])
%% 			io:format("Record: ~p~n", [Record])
%% 			[H|T] = Rest,
%% 			traverse(T)
	end.



statics(Records, MeasurementType)->
	put(undefined, -1),
	put(filtered, 0),
	put(nodata, 1),
	put(disabled, 2),
	put(good, 3),
	put(warning, 4),
	put(error, 5),
	put(worst, 6),
	
    init(MeasurementType),
    traverse(Records).

translate([])->
	done;
translate([{Name, _}|MeasurementType])->
	FilterTime = get({Name, {time, filtered}}),
	erase({Name, {time, filtered}}),
	NodataTime = get({Name, {time, nodata}}),
	erase({Name, {time, nodata}}),
	DisabledTime = get({Name, {time, disabled}}),
	erase({Name, {time, disabled}}),
	GoodTime = get({Name, {time, good}}),
	erase({Name, {time, good}}),
	ErrorTime = get({Name, {time, error}}),
	erase({Name, {time, error}}),
	WarningTime = get({Name, {time, warning}}),
	erase({Name, {time, warning}}),
	WorstTime = get({Name, {time, worst}}),
	erase({Name, {time, worst}}),

%% 	io:format("GoodTime-------------->~p~n", [GoodTime]),
	put({Name, goodTime}, GoodTime),
	put({Name, warningTime}, WarningTime),
	put({Name, errorTime}, ErrorTime),
	NATime = FilterTime + NodataTime + DisabledTime + WorstTime,
	put({Name, naTime}, NATime),
	put({Name, totalTime}, GoodTime + WarningTime + ErrorTime + NATime),

	LastTime = get({Name, lastTime}),
%% 	io:format("Name-------------->~p~n", [Name]),
%% 	io:format("lastTime-------------->~p~n", [LastTime]),
	case LastTime of
		undefined->
			put({Name, lastTime}, 0);
%% 			nothing;
		_->
			put({Name, lastTime}, calendar:gregorian_seconds_to_datetime(LastTime))
	end,
    translate(MeasurementType).

destruct_bucket(Name, N)->
	erase({Name, N, worstCategory}),
	erase({Name, N, lastCategory}),
	erase({Name, N, sampleCount}),
	erase({Name, N, minimum}),
	erase({Name, N, maximum}),
	erase({Name, N, total}),
	erase({Name, N, time}),
	erase({Name, N, totalSamples}),
	erase({Name, N, totalValue}),
	erase({Name, N, connector}).

construct_bucket(Name, N)->
	put({Name, N, worstCategory}, undefined),
	put({Name, N, lastCategory}, undefined),
	put({Name, N, sampleCount}, 0),
	put({Name, N, minimum}, undefined),
	put({Name, N, maximum}, 0),
	put({Name, N, total}, 0),
	put({Name, N, time}, undefined),
	put({Name, N, totalSamples}, 0),
	put({Name, N, totalValue}, 0),
	put({Name, N, connector}, undefined).

build_bucket(Name, N, Time, Category, Value)->
	SampleIsSkip = ((Category == disabled) or (Category == filtered)),	
	case get(Category) > get(get({Name, N, worstCategory})) of
		true->
			put({Name, N, worstCategory}, Category);
		_ ->
			nothing
	end,
	put({Name, N, time}, Time),
	put({Name, N, lastCategory}, Category),
	put({Name, N, totalSamples}, get({Name, N, totalSamples}) + 1),
	put({Name, N, sampleCount}, get({Name, N, sampleCount}) + 1),
	
	case SampleIsSkip of
		false ->
			case is_integer(Value) or is_float(Value) of 
%% 				is_list(Value)->
%% 					put({Name, N, totalValue}, Value);				
%% 				is_atom(Value)->
%% 		`			put({Name, N, totalValue}, Value);
				true->
					put({Name, N, total}, get({Name,N, total}) + Value),
					Min = get({Name, N, minimum}),
					if
						Min == undefined ->
							put({Name, N, minimum}, Value);
						Value =< Min ->
							put({Name, N, minimum}, Value);						
						Min == 0 ->
							put({Name, N, minimum}, Min);							
						true->
							nothing
					end, 
										
					Max = get({Name, N, maximum}),
					if 
						Value > Max ->
							put({Name, N, maximum}, Value);
						true->
							nothing
					end;
				_->
					case Value of
						"n/a"->
							put({Name, N, minimum}, 0),
							put({Name, N, totalValue}, Value);
						"denied"->
							put({Name, N, minimum}, 0),
							put({Name, N, totalValue}, Value);
						_->								
							put({Name, N, totalValue}, Value)
					end					
					
			end;
		_->
			put({Name, N, totalValue}, Value)
	end.

prepare(_, 0)->
	done;
prepare(Name, N)->
	construct_bucket(Name, N),
	prepare(Name, N-1).

post(_, 0)->
	done;
post(Name, N)->
	WorstCategory = get({Name, N, worstCategory}),
	LastCategory = get({Name, N, lastCategory}),
	SampleCount = get({Name, N, sampleCount}),
	Minimum = get({Name, N, minimum}),
	Maximum = get({Name, N, maximum}),
	Total = get({Name, N, total}),
	Time = get({Name, N, time}),
	TotalSamples = get({Name, N, totalSamples}),
	TotalValue = get({Name, N, totalValue}),
	Connector = get({Name, N, connector}),

	Bucket = {N, {{worstCategory,WorstCategory},
				  {lastCategory,LastCategory},
				  {sampleCount,SampleCount},
				  {minimum,Minimum},
				  {maximum,Maximum},
				  {total,Total},
				  {time,Time},
				  {totalSamples,TotalSamples},
				  {totalValue,TotalValue},
				  {connector,Connector}}},
	put({Name, {bucket}}, [Bucket|get({Name, {bucket}})]),
	destruct_bucket(Name, N),
	post(Name, N-1).

bucket(0, _, _)->
	done;
bucket(Count, Name, [{Time, Category, Value}|Rest])->
	build_bucket(Name, Count, Time, Category, Value),
	bucket(Count - 1, Name, Rest).

nocompress_bucket([])->
	done;
nocompress_bucket([{Name, _}|Rest])->
	Flag = get({Name, 0}),
	case Flag =/= undefined of
		true->
			Count = get({Name, sampleCount}),
			prepare(Name, Count),
			put({Name, {bucket}}, []),
			bucket(Count, Name, get({Name,1})),
			erase({Name,1}),
			post(Name, Count);
		_ ->
			nothing
	end,
	nocompress_bucket(Rest).


multi([{Time, Category, Value}|Rest], Name, BucketNo, Capacity, Capacity)->
	build_bucket(Name, BucketNo, Time, Category, Value),
	multi(Rest, Name, BucketNo + 1, 1, Capacity);
multi([{Time, Category, Value}|Rest], Name, BucketNo, Index, Capacity)->
	build_bucket(Name, BucketNo, Time, Category, Value),
	multi(Rest, Name, BucketNo, Index + 1, Capacity);
multi([], _, _, _, _)->
	done.

multi_bucket([], _)->
	done;
multi_bucket([{Name, _}|Rest], Limit)->
	Flag = get({Name, 0}),
	case Flag =/= undefined of
		true->
			Count = get({Name, sampleCount}),
			{Capacity, N} = case Count > Limit of
				true->
					Div = Count div Limit,
					case Count rem Limit of
						0 ->
							{Div, Limit};
						_ ->
							{Div + 1, Count div (Div + 1) + 1}
					end;
				_ ->
					{1, Count}
			end,

			prepare(Name, N),
			put({Name, {bucket}}, []),
			Bucket = lists:reverse(get({Name,1})),
			multi(Bucket, Name, 1, 1, Capacity),
			erase({Name,1}),
			post(Name, N);
		_ ->
			nothing
	end,
	multi_bucket(Rest, Limit).


precision([], _, _, _)->
	done;
precision([{Time, Category, Value}|Rest], Name, [Start, End|_] = Intervals, BucketNo) when Time >= Start andalso Time =< End ->
	build_bucket(Name, BucketNo, Time, Category, Value),
	precision(Rest, Name, Intervals, BucketNo);
precision([{Time, _, _}|Rest], Name, [Start|_] = Intervals, BucketNo) when Time < Start->
	precision(Rest, Name, Intervals, BucketNo);
precision([{Time, _, _}|_], _, [End], _) when Time > End->
	done;
precision(Buckets, Name, [_, End|Intervals], BucketNo)->
	precision(Buckets, Name, [End|Intervals], BucketNo + 1).

precision_bucket([], _, _)->
	done;
precision_bucket([{Name, _}|Rest], Intervals, N)->
	Flag = get({Name, 0}),
	case Flag =/= undefined of
		true->
			prepare(Name, N),
			put({Name, {bucket}}, []),
			Bucket = lists:reverse(get({Name,1})),
			
			erase({Name,1}),			
			
			precision(Bucket, Name, Intervals, 1),
			
			post(Name, N);
		_ ->
			nothing
	end,
	precision_bucket(Rest, Intervals, N).

build_interval(Start, End, Precision, Intervals, N) when Start < End ->
	build_interval(Start + Precision, End, Precision, [calendar:gregorian_seconds_to_datetime(Start)|Intervals], N+1);
build_interval(_, _, _, Intervals, N)->
	{N, lists:reverse(Intervals)}.

compress_bucket(noCompress, MeasurementType)->
	nocompress_bucket(MeasurementType);
compress_bucket({_, Count}, MeasurementType)->
	multi_bucket(MeasurementType, Count);
compress_bucket({_, StartTime, EndTime, Precision}, MeasurementType)->
	StartSeconds = calendar:datetime_to_gregorian_seconds(StartTime),
	EndSeconds = calendar:datetime_to_gregorian_seconds(EndTime),
	{N, Intervals} = build_interval(StartSeconds, EndSeconds, Precision, [], 0),
	precision_bucket(MeasurementType, Intervals, N);
compress_bucket(_, _)->
	done.

process(Name)->
	LastTime = get({Name, lastTime}),
	erase({Name, lastTime}),
	
	LastValue = get({Name, lastValue}),
	erase({Name, lastValue}),
	
	LastCategory = get({Name, lastCategory}),
	erase({Name, lastCategory}),
	
	LastErrorType = get({Name, lastErrorType}),
	erase({Name, lastErrorType}),
	
	ActualStartTime = get({Name, actualStartTime}),
	erase({Name, actualStartTime}),
	
	ActualEndTime = get({Name, actualEndTime}),
	erase({Name, actualEndTime}),
	
	WorstCategory = get({Name, worstCategory}),
	erase({Name, worstCategory}),
	
	Total = get({Name, total}),
	erase({Name, total}),
	
	TotalValue = get({Name, totalValue}),
	erase({Name, totalValue}),
	
	TotalSamples = get({Name, totalSamples}),
	erase({Name, totalSamples}),
	
	SampleCount = get({Name, sampleCount}),
	erase({Name, sampleCount}),
	
	Minimum = get({Name, minimum}),
	erase({Name, minimum}),
	
	MinimumTime = get({Name, minimumTime}),
	erase({Name, minimumTime}),
	
	Maximum = get({Name, maximum}),
	erase({Name, maximum}),
	
	MaximumTime = get({Name, maximumTime}),
	erase({Name, maximumTime}),
	
	GoodTime = get({Name, goodTime}),
	erase({Name, goodTime}),

	WarningTime = get({Name, warningTime}),
	erase({Name, warningTime}),

	ErrorTime = get({Name, errorTime}),
	erase({Name, errorTime}),

	NaTime = get({Name, naTime}),
	erase({Name, naTime}),

	TotalTime = get({Name, totalTime}),
	erase({Name, totalTime}),	
	
	Bucket = get({Name,{bucket}}),
	erase({Name, {bucket}}),
	
 	{Name, [{lastTime, LastTime},
	  {lastValue, LastValue},
	  {lastCategory, LastCategory},
	  {lastErrorType, LastErrorType},
	  {actualStartTime, ActualStartTime},
	  {actualEndTime, ActualEndTime},
	  {worstCategory, WorstCategory}, 
	  {total, Total},
	  {totalValue, TotalValue},
	  {totalSamples, TotalSamples},
	  {sampleCount, SampleCount},
	  {minimum, Minimum},
	  {minimumTime, MinimumTime},
	  {maximum, Maximum},
	  {maximumTime, MaximumTime},	
	  {goodTime, GoodTime},
	  {warningTime, WarningTime},
	  {errorTime, ErrorTime},
	  {naTime, NaTime},
	  {totalTime, TotalTime},
	  {buckets, Bucket}]}.

format([],Result)->
	Result;
format([{Name, _}|Rest], Result)->
	Flag = get({Name, 0}),
	erase({Name,0}),
	%io:format("Flag:~p--->~p~n", [Name, Flag]),
	case Flag =/= undefined of
		true->
			Measurement = process(Name),
	%		io:format("Measurement--->~p~n", [Measurement]),
			format(Rest, [Measurement|Result]);
		_ ->
			format(Rest, Result)
	end.

buckets(StaticsType, MeasurementType)->
	translate(MeasurementType),
	
	compress_bucket(StaticsType, MeasurementType),
	
	erase(undefined),
	erase(filtered),
	erase(nodata),
	erase(disabled),
	erase(good),
	erase(warning),
	erase(error),
	erase(worst),
	
	format(MeasurementType, []).

statics(Parent, Records, MeasurementType, StaticsType)->
	try
		%T1 = erlang:now(),
%% 		io:format("report_statics:MeasurementType  ~p ~n", [MeasurementType]),
		statics(Records, MeasurementType),
		Result = buckets(StaticsType, MeasurementType),
		%T2 = erlang:now(),
		%Diff = timer:now_diff(T2, T1),
    	%io:format("Stat Total Time:~p~n", [Diff]),
		Parent!Result
	catch
		E:E1->
			io:format("report_statics:statics catch  ~p,~p ~n", [E, E1]),
			Parent![]
	end.

%%StaticsType = noCompress, {multipleCompress, Count}, {precisionCompress, StartTime, EndTime, Precision}
%%MeasurementType = [{name, type}...] type=numeric,.....
report(Records, MeasurementType, StaticsType)->
	try
		Pid = spawn_opt(?MODULE, statics, [self(), Records, lists:usort(MeasurementType), StaticsType], [{priority, high}]),
%% 		lib_misc:on_exit(Pid, fun(Why)-> io:format("died with ~p~n", [Why]) end),
    	receive
			Result->
				Result
    	end
	catch
		E:E1->
%% 			io:format("report_statics:report catch  ~p,~p ~n", [E, E1]),
			[]
	end.

test(ID, Count, Interval, StaticsType)->
	[Monitor] = report_proxy:find_object(ID),
	Props=Monitor:getLogProperties(Monitor),
	
	{_,{_,Class}}=Monitor:get_property(class),
	Template=report_proxy:get_template(Class),
	
	F = fun(X)->
		case lists:keysearch(X,#property.name, Template) of
		 	{_,Type} ->
				{X, element(4, Type)};
	 		_ ->
		 		{X, numeric}
		end
	end,
	MeasurementType = lists:map(F, Props),
	io:format("MeasurementType:~p~n", [MeasurementType]),
	T1 = erlang:now(),
	{_, Records} = report_proxy:qc([ID], Count, Interval),
%% 	io:format("Records:~p~n", [Records]),
	T2 = erlang:now(),
	Diff1 = timer:now_diff(T2, T1),
	io:format("Query Time:~p~n", [Diff1]),
	Result = report(Records, MeasurementType, StaticsType),
	T3 = erlang:now(),
	Diff2 = timer:now_diff(T3, T2),
    io:format("Statics Record Time:~p~n", [Diff2]),
	Result.

test(ID)->
	test(ID, 10000, 30, noCompress).

test1(ID)->
	test(ID, 20, 1, noCompress).

test2(ID)->
	test(ID, 10000, 30, {compress, 200}).

test3(ID)->
	test(ID, 10000, 30, {precisionCompress, {{2010,11,28},{12,0,0}}, {{2010,12,28},{17,31,59}}, 3600}).
