-module(cache).
-export([create/1, destory/1, server/1, get/2, set/3, delete/2, clear/1, test/3, test_get/2]).

test(Cache, Count1, Count2)->
   cache:create(Cache),
   T1 = now(),
   test1(Count1, Cache),
   T2 = now(),
   test2(Count2, Cache, <<"000000000099999">>),
   T3 = now(),
   Interval1 = timer:now_diff(T2, T1),
   Interval2 = timer:now_diff(T3, T2),
   io:format("1->~p~n2->~p~n", [Interval1, Interval2]).


test_get(Cache, Count)->
   T1 = now(),
   test2(Count, Cache, <<"000000000099999">>),
   T2 = now(),
   Interval = timer:now_diff(T2, T1),
   io:format("Interval->~p~n", [Interval]).


test1(0, _)->
	done;
test1(Count, Cache)->
	Key = lists:flatten(io_lib:format("~15..0w", [Count])), 
	cache:set(Cache, list_to_binary(Key), Count),
	test1(Count - 1, Cache).
	
test2(0, _, _)->
	done;
test2(Count, Cache, Key)->
	cache:get(Cache, Key),
	test2(Count - 1, Cache, Key).

create(Name)->
	case whereis(Name) of
		undefined->
			PID = spawn_opt(?MODULE, server, [Name], [{priority, high}]),
			register(Name, PID),
			ok;
		_ ->
			error
	end.

destory(Name)->
	PID = whereis(Name),
	unregister(Name),
	exit(PID, destory).

get(Cache, Key)->
	client(Cache, {self(), Key}).

delete(Cache, Key)->
	Cache!Key.

clear(Cache)->
	Cache!clear.

set(Cache, Key, Value)->
	Cache!{set, Key, Value}.

client(Server, Request)->
	Server!Request,
	receive
		{Server, Response}->
			Response
	after 
		1000 ->
			nothing
	end.

server(Name)->
	receive
		{Client, Key}->
			Respone = {Name, get(Key)},
			Client ! Respone;
		{set, Key, Value}->
			put(Key, Value);
		clear->
			erase();
		Key ->
			erase(Key)
	end,
	server(Name).
		