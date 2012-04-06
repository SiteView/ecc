-module(atomic_monitor).
-compile(export_all).
-include("../../include/object.hrl").
-include("../../include/monitor.hrl").
-include("../../include/monitor_template.hrl").

extends () -> base_monitor.

?SUPERCLAUSE(action).
?SUPERCLAUSE(event).
?SUPERCLAUSE(pattern).

init(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=init,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,start).

update(Self,EventType,Pattern,State) ->
%% 	super:runClassifiers(Self, Classifiers),	
	io:format ( "[~w]:Type=~w,Action=update,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,start).

atomic_monitor (Self,Name) ->
	object:super(Self, [Name]).

atomic_monitor_ (Self)-> 
	eresye:stop(?VALUE(name)).

on_starting(Self) -> ok.
%% 	io:format("This [~w] ~w object is starting \n",[?VALUE(name),?MODULE]).

on_stopping(Self) ->
	io:format("This [~w] ~w object is stopping \n",[?VALUE(name),?MODULE]).

%% runClassifiers(Self, This) ->
%% 	object:super(Self, runClassifiers, [Self, This]).



test(Name) ->
	case object:get_by_name(Name) of
		[] -> 
				X = object:new(?MODULE,[Name]),
				object:start(X),
				eresye:assert(Name,{wakeup}),
				X;
		_ -> atom_to_list(Name) ++ " not available, choose a new name"
	end.