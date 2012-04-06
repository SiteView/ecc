%% ---
%% schedule property
%%
%%---
-module(schedule_property).
-compile(export_all).
-include("monitor.hrl").

is_enabled(null)->true;
is_enabled(S=#schedule{type="range"})->
	Day = sv_datetime:get_day(),
	Now = sv_datetime:localtime(),
	ScheDay = proplists:get_value(Day,S#schedule.days),
	%%io:format("is_enabled:~p~n",[ScheDay]),
	case ScheDay of
		{"enabled",""}->
			false;
		{"enabled",Start}->
			check_time_hours(Start,Now);
		{"disabled",""}->
			true;
		{"disabled",Start}->
			not check_time_hours(Start,Now);
		_->
			false
	end;
is_enabled(S=#schedule{type="hours"})->
	Day = sv_datetime:get_day(),
	Now = sv_datetime:now(),
	ScheDay = proplists:get_value(Day,S#schedule.days),
	%%io:format("is_enabled:~p~n",[ScheDay]),
	case ScheDay of
		{"enabled","",""}->
			true;
		{"enabled",Start,End}->
			check_time_range(Start,Now,End);
		{"disabled","",""}->
			false;
		{"disabled",Start,End}->
			not check_time_range(Start,Now,End);
		_->
			false
	end;
is_enabled(S=#schedule{type="absolute"})->
	% io:format("schedule error,should set schedule absolute_event:~p~n",[S]),
	Day = sv_datetime:get_day(),
	Now = sv_datetime:now(),
	ScheDay = proplists:get_value(Day,S#schedule.days),
	F = fun(X,R)->
		Tm = sv_datetime:time(X),
		case R of 
			true->
				true;
			_->
				if
					Now > Tm-10000 andalso Now <Tm + 600000->
						true;
					true ->
						false
				end
		end
	end,
	lists:foldl(F,false,ScheDay);
is_enabled(S)->
	io:format("error schedule:~p~n",[S]),
	false.

history_is_enabled(null, Day, Now)->true;
history_is_enabled(S=#schedule{type="range"}, Day, Now)->
	ScheDay = proplists:get_value(Day,S#schedule.days),
	case ScheDay of
		{"enabled",Start,End}->
			check_time_range(Start,Now,End);
		{"disabled",Start,End}->
			not check_time_range(Start,Now,End);
		_->
			false
	end;
history_is_enabled(S=#schedule{type="absolute"}, Day, Now)->
	io:format("schedule error,should set schedule absolute_event:~p~n",[S]),
	false;
history_is_enabled(S, Day, Now)->
	io:format("error schedule:~p~n",[S]),
	false.

measurement_is_enabled(null)->false;
measurement_is_enabled(S=#schedule{type="range"})->
	Day = sv_datetime:get_day(),
	Now = sv_datetime:localtime(),
	ScheDay = proplists:get_value(Day,S#schedule.days),
	%%io:format("measurement_is_enabled:~p~n",[ScheDay]),
	case ScheDay of
		{"enabled",""}->
			false;
		{"enabled",Start}->
			check_time_hours(Start,Now);
		{"disabled",""}->
			true;
		{"disabled",Start}->
			not check_time_hours(Start,Now);
		_->
			false
	end;
measurement_is_enabled(S=#schedule{type="hours"})->
	Day = sv_datetime:get_day(),
	Now = sv_datetime:now(),
	ScheDay = proplists:get_value(Day,S#schedule.days),
	%%io:format("measurement_is_enabled:~p~n",[ScheDay]),
	case ScheDay of
		{"enabled","",""}->
			true;
		{"enabled",Start,End}->
			check_time_range(Start,Now,End);
		{"disabled","",""}->
			false;
		{"disabled",Start,End}->
			not check_time_range(Start,Now,End);
		_->
			false
	end;
measurement_is_enabled(S=#schedule{type="absolute"})->
	% io:format("schedule error,should set schedule absolute_event:~p~n",[S]),
	Day = sv_datetime:get_day(),
	Now = sv_datetime:now(),
	ScheDay = proplists:get_value(Day,S#schedule.days),
	F = fun(X,R)->
		Tm = sv_datetime:time(X),
		case R of 
			true->
				true;
			_->
				if
					Now > Tm-10000 andalso Now <Tm + 600000->
						true;
					true ->
						false
				end
		end
	end,
	lists:foldl(F,false,ScheDay);
measurement_is_enabled(S)->
	io:format("error schedule:~p~n",[S]),
	false.

check_time_range([],_,[])-> false;
check_time_range([S|Ts],Now,[])->
	Tms = sv_datetime:time(S)-1,
	if
		Now > Tms ->
			true;
		true ->
			check_time_range(Ts,Now,[])
	end;
check_time_range([],Now,[E|Te])->
	Tme = sv_datetime:time(E)+1,
	if
		Now < Tme ->
			true;
		true ->
			check_time_range([],Now,Te)
	end;
check_time_range([S|Ts],Now,[E|Te])->
	Tms = sv_datetime:time(S)-1,
	Tme = sv_datetime:time(E)+1,
	if
		(Now > Tms) and (Now < Tme) ->
			true;
		true ->
			check_time_range(Ts,Now,Te)
	end;
check_time_range(_,_,_)->false.

%% 是绝对事件时，需要一个传入频率来保证一次频率的时间范围内，事件能被触发一次
is_enabled(null,_)->true;
is_enabled(S=#schedule{type="hours"},_)->
	is_enabled(S);
is_enabled(S=#schedule{type="range"},_)->
	is_enabled(S);
is_enabled(S=#schedule{type="absolute"},Freq)->
	Day = sv_datetime:get_day(),
	Now = sv_datetime:now(),
	ScheDay = proplists:get_value(Day,S#schedule.days),
	F = fun(X,R)->
		Tm = sv_datetime:time(X),
		case R of 
			true->
				true;
			_->
				if
					Now > Tm-1000 andalso Now <Tm + Freq->
						true;
					true ->
						false
				end
		end
	end,
	lists:foldl(F,false,ScheDay);
is_enabled(S,_)->
	io:format("error schedule:~p~n",[S]),
	false.
	
check_time_hours([],_)->false;
check_time_hours([H|T],Now)->
	{HH,_,_} = H,
	case Now of
		{_,{HH,_,_}}->
			true;
		_->
			check_time_hours(T,Now)
	end.
	