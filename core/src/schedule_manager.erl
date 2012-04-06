%% ---
%%your comment
%%
%%---
-module(schedule_manager).
-compile(export_all).
-include("monitor.hrl").

init_schedule_monitor() -> ok.


add_monitor_to_schedule()->ok.


get_schedule_id_from_monitor(Monitor)->
	Monitor:get_property(id).

get_schedule(Monitor)->
	case Monitor:get_property(schedule) of
		{ok,{schedule,"all"}}->
			null;
		{ok,{schedule,S}}->
			case api_schedule:get_info(S) of
				[]->
					null;
				Info->
					Id = proplists:get_value(id,Info),
					Name = proplists:get_value(name,Info),
					Type = proplists:get_value(type,Info),
					#schedule{id = Id,name=Name,type=Type,days=[{1,get_dayvalue(proplists:get_value(monday,Info),Type)},
						{2,get_dayvalue(proplists:get_value(tuesday,Info),Type)},{3,get_dayvalue(proplists:get_value(wednesday,Info),Type)},
						{4,get_dayvalue(proplists:get_value(thursday,Info),Type)},{5,get_dayvalue(proplists:get_value(friday,Info),Type)},
						{6, get_dayvalue(proplists:get_value(saturday,Info),Type)},{7,get_dayvalue(proplists:get_value(sunday,Info),Type)}]
						}
			end;
		_->
			null
	end.

get_history_schedule_filter(ScheduleFilter)->
	case api_schedule:get_info(ScheduleFilter) of
		[]->
			null;
		Info->
			Id = proplists:get_value(id,Info),
			Name = proplists:get_value(name,Info),
			Type = proplists:get_value(type,Info),
			#schedule{id = Id,name=Name,type=Type,days=[{1,get_dayvalue(proplists:get_value(monday,Info),Type)},
				{2,get_dayvalue(proplists:get_value(tuesday,Info),Type)},{3,get_dayvalue(proplists:get_value(wednesday,Info),Type)},
				{4,get_dayvalue(proplists:get_value(thursday,Info),Type)},{5,get_dayvalue(proplists:get_value(friday,Info),Type)},
				{6, get_dayvalue(proplists:get_value(saturday,Info),Type)},{7,get_dayvalue(proplists:get_value(sunday,Info),Type)}]
				}
	end.

get_measurement_schedule_filter(ScheduleFilter)->
	case api_schedule:get_info_by_name(ScheduleFilter) of
		[]->
			null;
		[Info]->
%% 			io:format("get_measurement_schedule_filter:~p~n", [Info]),
			Id = proplists:get_value(id,Info),
			Name = proplists:get_value(name,Info),
			Type = proplists:get_value(type,Info),
			#schedule{id = Id,name=Name,type=Type,days=[{1,get_dayvalue(proplists:get_value(monday,Info),Type)},
				{2,get_dayvalue(proplists:get_value(tuesday,Info),Type)},{3,get_dayvalue(proplists:get_value(wednesday,Info),Type)},
				{4,get_dayvalue(proplists:get_value(thursday,Info),Type)},{5,get_dayvalue(proplists:get_value(friday,Info),Type)},
				{6, get_dayvalue(proplists:get_value(saturday,Info),Type)},{7,get_dayvalue(proplists:get_value(sunday,Info),Type)}]
				}
	end.

string2time(S)->
	case string:tokens(S,":") of
		[H,M]->
			{list_to_integer(H),list_to_integer(M),0};
		[H2,M2,S2|_]->
			{list_to_integer(H2),list_to_integer(M2),list_to_integer(S2)};
		_->
			null
	end.

get_dayvalue(Times,Type)->
	case Type of
		"absolute"->
			Time = [string2time(X) || X<- string:tokens(Times,",")],
			Tms = lists:filter(fun(X)-> X=/= null end,Time),
			lists:sort(fun(X,Y)->sv_datetime:time(X)>sv_datetime:time(Y) end,Tms);
		"range"->
			case Times of
				{Enable,S}->
					Ts = [string2time(X) || X<- string:tokens(S,",")],
					{Enable,Ts};
				_->
					{}
			end;
		"hours"->
			case Times of
				{Enable,S,E}->
					Ts = [string2time(X) || X<- string:tokens(S,",")],
					Te = [string2time(X) || X<- string:tokens(E,",")],
					{Enable,Ts,Te};
				_->
					{}
			end;
		_->
			[]
	end.