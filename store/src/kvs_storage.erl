-module(kvs_storage).
-compile(export_all).

-include("kvs_define.hrl").

id2storage(Str)->
	IndexFile = "./data/" ++ Str ++ ".idx",
	IndexTable = list_to_atom(Str ++ "_idx"),
	DataFile = "./data/" ++ Str ++ ".dat",
	DataTable = list_to_atom(Str ++ "_dat"),
	#storage{
			id = list_to_integer(Str), 
			index_name = IndexFile, 
			index_table = IndexTable, 
			data_name = DataFile, 
			data_table = DataTable,
			lasted_time = now()
			}.

init()->
	case lists:member(kvs_storage, ets:all()) of
		false ->
			ets:new(kvs_storage, [public, set, named_table, {keypos, 2}]);
		_ ->
			ets:delete_all_objects(kvs_storage)
	end,
	{Year, Month, Day} = date(),
	Str = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day])),
	Storage = id2storage(Str),
	kvs_storage:open(Storage).

stop([Record|Rest])->
	close(Record),
	stop(Rest);
stop([])->
	done.

stop()->
	stop(ets:tab2list(kvs_storage)).

%%-record(storage, {id, index_name, index_table, data_name, data_table, lasted_time=now()}).

get()->
	ets:tab2list(kvs_storage).

get(ID)->
	case ets:lookup(kvs_storage, ID) of
		[Record]->
			case {lists:member(Record#storage.index_table, dets:all()), lists:member(Record#storage.data_table, dets:all()) } of
				{true, true}->
					Record;
				{true, false}->
					dets:close(Record#storage.index_table),
					null;
				{false, true}->
					dets:close(Record#storage.data_table),
					null;
				_ ->
					null
			end;
		_ ->
			null
	end.

put(Storage)when is_record(Storage, storage)->
	ets:insert(kvs_storage, Storage);
put(_)->
	error.

open(Storage) when is_record(Storage, storage)->
	IndexTable = Storage#storage.index_table,
	IndexFile = Storage#storage.index_name,

	DataTable = Storage#storage.data_table,
	DataFile = Storage#storage.data_name,

	case dets:open_file(IndexTable, [{auto_save, infinity}, {file, IndexFile}, {keypos, 1}, {type, duplicate_bag}])  of
		{ok, _}->
				case dets:open_file(DataTable, [{auto_save, infinity}, {file, DataFile}, {keypos, 2}, {type, set}])  of
					{ok, _}->
						put(Storage),
						{ok, Storage};
					Error2->
						dets:close(IndexTable),
						Error2
				end;
		Error1->
			Error1
	end.

close(Storage) when is_record(Storage, storage)->
	dets:close(Storage#storage.index_table),
	dets:close(Storage#storage.data_table),
	ok;
close(_)->
	error.

select(Table, Pattern)->
	dets:select(Table, Pattern).

lookup(Table, Key)->
	dets:lookup(Table, Key).

read(Table, ID)->
	case kvs_cache:read(ID) of
		null->
			case dets:lookup(Table, ID) of
				[Record]->
					kvs_cache:write(ID, Record),
					Record;
				_->
					null
			end;
		Cache->
			Cache
	end.

write(Table, Content) ->
	dets:insert(Table, Content),
	kvs_rollup:write(Content),
	case is_record(Content, log) of
		true->
			kvs_cache:write(Content#log.id, Content);
		_ ->
			nothing
	end.