-module (point).
-compile(export_all).
-include("../../include/object.hrl").

extends () -> nil .

?ACTION(start) -> {wakeup_event,init};
?ACTION(running) -> {wakeup_event,update}. 
?EVENT(wakeup_event)-> {eresye,wakeupPattern}. 
?PATTERN(wakeupPattern)-> {?VALUE(name), get, {wakeup}}.

init(Self,EventType,Pattern,State) ->
	io:format ( "[~w:~w]:Action=init,State=~w,Event=~w,Pattern=~w,State=~w\n",[?MODULE,?VALUE(name),State,EventType,Pattern,State]),
	object:do(Self,running).

update(Self,EventType,Pattern,State) ->
	io:format ( "[~w:~w]:Action=update,State=~w,Event=~w,Pattern=~w,State=~w\n",[?MODULE,?VALUE(name),State,EventType,Pattern,State]),
	object:do(Self,start).

point(Self, Name,X , Y) ->
	object:set (Self,'Y' ,Y),
	object:set (Self,'X' ,X),
	?SETVALUE(name,Name),
	eresye:start(Name).

point (Self,Name) ->
	object:set (Self,'Y' ,10),
	object:set (Self,'X' ,10),
	?SETVALUE(name,Name),
	eresye:start(Name).

point_(Self)-> 
%% 	io:format("point destructor."),
	eresye:stop(?VALUE(name)).

get_resource_type() -> ?MODULE.

print(Self,P)  ->
	io:format("X:~w,Y:~w,P:~s~n", [object:get(Self,'X'),object:get(Self,'Y'),P]).

print(Self)  ->
	io:format("printing in [~w:~w]~n",[?MODULE,?VALUE(name)]).

on_starting(Self) ->
	io:format("This [~w] ~w object is starting \n",[?VALUE(name),?MODULE]).

on_stopping(Self) ->
	io:format("This [~w] ~w object is stopping \n",[?VALUE(name),?MODULE]).

start(Name) ->
	case object:get_by_name(Name) of
		[] -> 
				X = object:new(?MODULE,[Name]),
				object:start(X),
				eresye:assert(Name,{wakeup}),
				X;
		_ -> atom_to_list(Name) ++ " already existed, choose a new name"
	end.