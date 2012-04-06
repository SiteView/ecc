-module(example_monitor).
-compile(export_all).
-include("../../include/object.hrl").
-include("../../include/monitor.hrl").
-include("../../include/monitor_template.hrl").

%% extends () -> atomic_monitor.
extends () -> nil.

?ACTION(start) -> {wakeup_event,init};
?ACTION(disabled) -> [{timed_enable_event,enable},{enable_event, enable}];
?ACTION(running) -> {triggering_event, logging};
?ACTION(waiting) -> [{frequency_event,update},{disable_event,disable}];
?SUPERCLAUSE(action).
%% ?ACTION(waiting) -> [{timeout_event,update}].

?EVENT(wakeup_event)-> {eresye,wakeup};
?EVENT(disable_event)-> {eresye,disable};
?EVENT(enable_event)-> {eresye,enable};
?EVENT(frequency_event) -> {timeout,frequency};
?EVENT(timeout_event) -> {timeout,timeout_value};
?EVENT(timed_enable_event) -> {timeout,disable_time}.

?PATTERN(timeout_value ) -> 5000;
%% 	?VALUE(?FREQUENCY)*1000;
?PATTERN(wakeup)-> {?VALUE(name), get, {wakeup}};
?PATTERN(enable)-> [{?VALUE(name), get, {enable}},{?VALUE(name), get, {enable,fun(Time)-> Time > 0 end}}];
?PATTERN(disable)-> [{?VALUE(name), get, {disable}},{?VALUE(name), get, {disable,fun(Time)-> Time > 0 end}}];
?PATTERN(?FREQUENCY ) -> ?VALUE(?FREQUENCY)*1000;
?PATTERN(disable_time) -> ?VALUE(disable_time)*1000.

init(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=init,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,waiting).

update(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=update,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
  	Start = erlang:now(),
	object:do(Self,running),
	io:format("[~w] Running: Hostname:~w, Timeout:~w, Size:~w\n", [?VALUE(name),?VALUE(timeout),?VALUE(size)]),
%% 	Cmd = "ping -n 4 -l " ++ object:get(Size,'value') ++ " -w " ++ object:get(Timeout,'value') ++ "  " ++ object:get(Hostname,'value'),

%% 	simulated random data

	?SETVALUE(round_trip_time,100 * random:uniform(10)),
	?SETVALUE(packetsgood,random:uniform(4)),
	
	Diff = timer:now_diff(erlang:now(), Start)/1000000,
	?SETVALUE(?MEASUREMENTTIME,Diff),
	?SETVALUE(?LASTUPDATE,erlang:now()),	
 	io:format("[~w] finish in ~w ms, return: RoundTripTime:~w, PacketsGood:~w\n", [?MODULE,Diff,?VALUE(round_trip_time),?VALUE(packetsgood)]),
	object:do(Self,waiting).

example_monitor (Self,Name) ->
	?SETVALUE(timeout,1000),
	?SETVALUE(size,4),
	?SETVALUE(name,Name),
	eresye:start(Name),
	object:super(Self, [Name]).

example_monitor_(Self)-> 
	eresye:stop(?VALUE(name)).

disable(Self,EventType,Pattern,State) ->
	io:format ( "Action: disable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,disabled).

enable(Self,EventType,Pattern,State) ->
	io:format ( "Action: enable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,waiting).

logging(Self) -> 
	%TODO: logging data to database
	object:do(Self,waiting).

on_starting(Self) ->
	io:format("This [~w] ~w object is starting \n",[?VALUE(name),?MODULE]).

on_stopping(Self) ->
	io:format("This [~w] ~w object is stopping \n",[?VALUE(name),?MODULE]).

test(Name) ->
	case object:get_by_name(Name) of
		[] -> 
				X = object:new(?MODULE,[Name]),
				object:start(X),
				eresye:assert(Name,{wakeup}),
				X;
		_ -> atom_to_list(Name) ++ " not available, choose a new name"
	end.