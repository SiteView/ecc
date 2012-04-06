-module(kvs_cache).
-compile(export_all).

-include("kvs_define.hrl").

init()->
	case lists:member(kvs_cache,  ets:all()) of
		true->
			ets:delete_all_objects(kvs_cache);
		_ ->
			ets:new(kvs_cache, [public, set, named_table])
	end,
	PID = spawn(fun()->loop() end),
	register(kvs_cache, PID).

free(_, '$end_of_table', _, Acc)->
	Acc;
free(Seconds, Key, Base, Acc)->
	[{_, _, {MegaSecs, Secs, _}, Times}] = ets:lookup(kvs_cache, Key),
	Interval = MegaSecs * 1000000 + Secs - Seconds,
	case Interval > Base orelse  Times < ?TIMES of
		true->
			free(Seconds, ets:next(kvs_cache, Key), Base, [Key|Acc]);
		_ ->
			free(Seconds, ets:next(kvs_cache, Key), Base, Acc)
	end.

delete([Key|Rest])->
	ets:delete(kvs_cache, Key),
	delete(Rest);
delete([])->
	done.

free(Seconds)->
	Base = 86400 * ?DAY,  %%86400 = 3600 * 24
	FreeList = free(Seconds, ets:first(kvs_cache), Base, []),
	delete(FreeList).

sleep()->
    receive 
	stop->
		stop
        after 3600000 ->
		free
    end.

loop()->
    case sleep() of
	free->
		{MegaSecs, Secs, _} = now(),
		free(MegaSecs * 1000000 + Secs),
		loop();
	_ ->
		stop
     end.

stop()->
	kvs_cache!stop,
	unregister(kvs_cache),
	ets:delete(kvs_cache).

read(ID)->
	case ets:lookup(kvs_cache, ID) of
		[]->
			null;
		[{ID, Record, _, Times}]->
			ets:insert(kvs_cache, {ID, Record, now(), Times + 1}),
			Record
	end.

write(ID, Record)->
	ets:insert(kvs_cache, {ID, Record, now(), 1}).