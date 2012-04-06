-module(jointest).
-export([ extends /0 , action /2 , event /2 , jointest /1 , do_action /4 , test /0]).

extends () -> nil .

action(Self,start) -> {silent_evt,do_action}.
event(Self,silent_evt) -> {silent,nil }.

jointest ( Self ) ->
 object:set ( Self , counter , 0) ,
 object:start ( Self ).

do_action(Self, _, _, _) ->
 	C = object:get(Self,counter),
 	io:format("[~w ] step ~w\n",[Self, C]),
 if
 	C<10 -> object:set(Self,counter, C+1) ,
 					object : do ( Self , start );
 	true -> object : stop ( Self )
 end .

test () ->
 X = object:new(jointest),
 io:format("Waiting ...\n"),
 object:join(X),
 io:format(" Behaviour terminated .\n"),
 object:delete (X ).