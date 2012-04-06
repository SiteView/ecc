-module (pingpong).
-compile(export_all).
-include("../../include/object.hrl").

extends () -> nil .

?ACTION(start) -> {triggering_event,do_ping};
?ACTION(pong) -> [{triggering_event,do_pong},
					  {timeout_event,do_timed_pong}].

event(Self,triggering_event)-> {eresye,pingpong_pattern};
event(Self,timeout_event) -> {timeout,timeout_value}.

pattern(Self,pingpong_pattern)-> {?VALUE(name),get,{tick}};
pattern(Self, timeout_value ) -> getFrequency(Self).

pingpong(Self)->pingpong(Self,pingpong1,5000).

pingpong(Self, Name,Frequency)->
	object:set (Self, 'Frequency' , Frequency),
	object:set (Self, name, Name),
	eresye:start(Name). 

pingpong_(Self)->eresye:delete(?VALUE(name)).

getFrequency(Self) -> object:get(Self,'Frequency').

do_ping(Self,EventType,Pattern,State) ->
	io:format ( "[~w ] event `~w ,~w '\n" ,
	[State,EventType,Pattern]),
	object:do(Self,pong).

do_pong(Self,EventType,Pattern,State) ->
	io:format("[~w ] event `~w ,~w '\n",[State,EventType,Pattern]),
	object:do(Self,start).

do_timed_pong(Self,EventType,Pattern,State) ->
	io:format ("[~w]: Timeout \n" , [State]),
	object:do (Self,pong).

start(Name) ->
	X = object:new(pingpong,[Name,10000]),
	object:start(X),
	eresye:assert(Name,{tick}),
	X.