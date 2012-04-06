-module(threeDpoint).
-compile(export_all).
-include("../../include/object.hrl").

extends () -> relativepoint.

action(Self,start) -> {wakeup_event,init1}.

threeDpoint (Self,Mind,X,Y,Z,OffX,OffY,OffZ) ->
	object:super(Self,[Mind,X,Y]),
	object:set(Self, 'Z' , Z ),
	object:set(Self, 'offsetX' , OffX ),
	object:set(Self, 'offsetY' , OffY ),
	object:set(Self, 'offsetZ' , OffZ ).

threeDpoint ( Self , Mind,X , Y,Z) ->
 threeDpoint ( Self , Mind,X , Y ,Z, 0 , 0,0).

getx(Self) ->
 object:super(Self,getx) + object:get(Self,'offsetX').

gety(Self) ->
	object:super(Self,gety) + object:get(Self,'offsetY').


on_starting(Self) ->
	io:format("This threeD point behaviour is starting \n").

on_stopping(Self) ->
	io:format("This threeD point behaviour is going to be terminated \n"). 

init1(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=init,State=~w,Event=~w,Pattern=~w\n",	[object:get(Self,mind),?MODULE,State,EventType,Pattern]),
	object:do(Self,start).


test()->test(point3).

test(Mind) -> 
	X = object:new(?MODULE,[Mind,10,10,10]),
	object:start(X),
%% 	eresye:assert(Mind,{wakeup}),
	X.


