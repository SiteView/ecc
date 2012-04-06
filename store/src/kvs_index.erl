-module(kvs_index).
-compile(export_all).

-include("kvs_define.hrl").

%-record(log, {id, time, facility, level, machine, program, pid, message, master_decoder, sub_decoder, rule_id, rule_match,rule_description, my}).

map(Field)->
	case ets:lookup(kvs_map, Field) of
		[{_, Position}]->
			Position;
		_ ->
			position(Field)
	end.

position(Field)when is_binary(Field)->
	<<MD5:128/integer>> = erlang:md5(Field),
	Position = MD5 rem ?INTERVAL,
	case Position < ?BASE of
		true->
			position(list_to_binary(integer_to_list(Position)));
		_ ->
			case ets:lookup(kvs_map, Field) of
				[]->
					insert({Field, Position}),
					Position;
				[{_, Value}] ->
					Value
			end
	end;
position(Field) when is_atom(Field)->
	position(erlang:atom_to_binary(Field, latin1)).

insert(Value)->
	dets:insert(kvs_map, Value),
	ets:insert(kvs_map, Value).

insert([Field|Rest], Index)->
	insert({Field, Index}),
	insert(Rest, Index + 1);
insert([], _)->
	done.

insert()->
	Fields = record_info(fields, log),
	insert({log, 1}),
	insert(Fields, 2).
	
init()->
	File = "./conf/map.dat",
	Flag = filelib:is_file(File),
	dets:open_file(kvs_map, [{auto_save, infinity}, {file, File}, {keypos, 1}, {type, set}]),
	ets:new(kvs_map, [public, set, named_table]),
	case Flag of
		true->
			dets:to_ets(kvs_map, kvs_map);
		_ ->
			insert()
	end.

stop()->
	dets:close(kvs_map),
	ets:delete(kvs_map).

addKVList(Table, ID, [{Key, Value}|Rest])->
	N = map(Key),
	kvs_storage:write(Table, {{N, Value}, ID}),
	addKVList(Table, ID, Rest);
addKVList(_, _, [])->
	done.

add(Table, ID, Content, N, Index)->
	case element(N, Index) of
		1->
			kvs_storage:write(Table, {{N, element(N, Content)}, ID});
		2->
			addKVList(Table, ID, element(N, Content));
		_ ->
			nothing
	end,
	case N < tuple_size(Index) of
		true->
			add(Table, ID, Content, N + 1, Index);
		_ ->
			nothing
	end.	

add(Table, Content)->
	add(Table, element(2, Content), Content, 3, #index{}).


	
	