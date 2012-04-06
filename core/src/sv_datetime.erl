%% ---
%% sv_datetime
%%
%%---
-module(sv_datetime).
-compile(export_all).



%now() ->  calendar:datetime_to_gregorian_seconds(erlang:localtime())*1000.

now() ->  
	{A,B,C}=erlang:now(),
	round((A*1000000+B)*1000+C/1000).

now2str(Now)->
	C = (Now rem 1000)*1000,
	B = (Now div 1000) rem 1000000,
	A = (Now div 1000) div 1000000,
	{{Y,M,D},{HH,MM,SS}}=calendar:now_to_local_time({A,B,C}),
	lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",[Y,M,D,HH,MM,SS])).

time({{Y,M,D},{HH,MM,SS}}) when (M>0) and (M<13) and (D>0) and (D<32) ->
	(calendar:datetime_to_gregorian_seconds({{Y,M,D},{HH,MM,SS}})-calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time({{1970,1,1},{0,0,0}})))*1000;
time({HH,MM,SS}) when (HH > -1) and (HH < 25) and (MM > -1) and (MM < 61) and (SS>-1) and (SS<61) ->
	(calendar:datetime_to_gregorian_seconds({date(),{HH,MM,SS}})-calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time({{1970,1,1},{0,0,0}})))*1000;
time(_)->0.

localtime() ->  erlang:localtime().

next_date({Y,M,D})->
	calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({Y,M,D}) +1).
    
lastday({Y,M,D}) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({Y,M,D}) -1).

prev_date({Y,M,D})->
	calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({Y,M,D}) -1).

%% get_day()-> (1~7)
%%
get_day()->
	{Y,M,D} = date(),
	calendar:day_of_the_week(Y,M,D).
	

get_day({Y,M,D})->
	calendar:day_of_the_week(Y,M,D);
get_day(_)->
	{error,'{YYYY,MM,DD} format'}.
	
%Micro Seconds(integer) -> Seconds(string)	
microSecondsToStrSeconds(MicroS) ->
	%io:format("MicroS:~p~n", [MicroS]),
	Str_cost = integer_to_list(round(MicroS/1000)), %保留三位小数位
	Right = string:right(Str_cost, 3, $0),
	Index = string:str(Str_cost, Right),
	case Index<2 of
		true ->
			"0."++Right;
		false ->	
			string:substr(Str_cost, 1, Index-1)++"."++Right
	end.	
gettimeseconds() ->
	{_,{HH,MM,SS}}=erlang:localtime(),
	Ret=HH*3600+MM*60+SS,
	Ret.
getDate(MicroSeconds)->
  BaseDate= calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time({{1970,1,1},{0,0,0}})), 
  Seconds= BaseDate + (MicroSeconds div 1000),
  { Date,_Time} = calendar:gregorian_seconds_to_datetime(Seconds),
  Date.
getDateTime(MicroSeconds)->
  BaseDate= calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time({{1970,1,1},{0,0,0}})), 
  Seconds= BaseDate + (MicroSeconds div 1000),
  { Date,Time} = calendar:gregorian_seconds_to_datetime(Seconds),
  {Date,Time}.	

tostr(Time) ->
	{{Y,M,D},{HH,MM,SS}} = Time,
	lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",[Y,M,D,HH,MM,SS])).
    
tostr_date(Time) ->
	{{Y,M,D},{_HH,_MM,_SS}} = Time,
	lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w",[Y,M,D])).
    
tostr_YearMonth(Time) ->
	{{Y,M,_D},{_HH,_MM,_SS}} = Time,
	lists:flatten(io_lib:format("~4.4.0w-~2.2.0w",[Y,M])).
	
tostr_HourMinute(Time) ->
	{HH,MM} = Time,
	lists:flatten(io_lib:format("~2.2.0w:~2.2.0w",[HH,MM])).
    
tostr_HourMinute1(Time) ->
	{{_Y,_M,_D},{HH,MM,_SS}} = Time,
	lists:flatten(io_lib:format("~2.2.0w:~2.2.0w",[HH,MM])).
	
next_time(Sec)->
	calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) + Sec).
    
time_compare({H1,M1,_S1},{H2,M2,_S2})->
    case H1>H2 of
    true-> 1;
    _   ->
        case H1<H2 of
        true-> -1;
        _-> 
            case M1>M2 of
            true-> 1;
            _->
                case M1<M2 of
                true-> -1;
                _ ->0
                end
            end
        end
    end.


  

