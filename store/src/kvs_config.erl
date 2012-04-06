-module(kvs_config).
-compile(export_all).

-include("kvs_define.hrl").

get(Key)->
	case ets:lookup(kvs_config, Key) of
		[{Key, Value}]->
			Value;
		_ ->
			null
	end.

put(Value)->
	ets:insert(kvs_config, Value).

put(Key, Value)->
	put({Key, Value}).

insert([Item|Rest])->
	ets:insert(kvs_config, Item),
	insert(Rest);
insert([])->
	done.

init()->
	case lists:member(kvs_config, ets:all()) of
		false ->
			ets:new(kvs_config, [public, set, named_table]);
		_ ->
			ets:delete_all_objects(kvs_config)
	end,
	init("./conf/content.config").

init(FileName)->
	Result = file:consult(FileName),
	case Result of
		{ok, Configs}->
			insert(Configs),
			ok;
		Error->
			Error
	end.

stop()->
	ets:delete(kvs_config),
	done.