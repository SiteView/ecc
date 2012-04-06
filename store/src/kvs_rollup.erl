-module(kvs_rollup).
-compile(export_all).

-include("kvs_define.hrl").


init()->
	File = "./data/rollup.dat",
	dets:open_file(kvs_rollup, [{auto_save, infinity}, {file, File}, {keypos, 1}, {type, bag}]).

stop()->
	dets:close(kvs_rollup).

read(Key)->
	Index = kvs_index:map(Key),
	Records = dets:lookup(kvs_rollup, Index),
	lists:map(fun({_, Value})->Value end, Records).

insert(Content, [Field|Rest], Rollup, Index)->
	case element(Index, Rollup) of
		1 ->
			dets:insert(kvs_rollup, {kvs_index:map(Field), element(Index, Content)});
		_ ->
			nothing
	end,
	insert(Content, Rest, Rollup, Index + 1);
insert(_, [], _, _)->
	done.

write(Content)when is_record(Content, log) ->
	Fields = record_info(fields, rollup),
	insert(Content, Fields, #rollup{}, 2);
write(_)->nothing.