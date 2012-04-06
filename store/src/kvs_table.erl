-module(kvs_table).
-compile(export_all).

-include("kvs_define.hrl").

%%%{Index, Data}
%%IndexTableName->YYYYMMDD_idx
%%DataTableName->YYYYMMDD_dat

check(Content)->
	check(Content, 3).

%%-record(log, {id, time, facility, level, machine, program, pid, message, master_decoder, sub_decoder, rule_id, rule_match,rule_description, my}).
check(Content, N)->
	Element = element(N, Content),
	case N of
		3->	%%time
			case is_integer(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, time}
			end;
		4->   %%facility
			case is_binary(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, facility}
			end;
		5->	%level
			case is_atom(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, level}
			end;
		6->	%machine
			case is_binary(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, machine}
			end;
		7->	%program
			case is_binary(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, program}
			end;
		8->	%pid
			case is_integer(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, pid}
			end;
		9->%message
			case is_binary(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, message}
			end;
		10->	%master_decoder
			case Element=:= null orelse is_binary(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, message}
			end;
		11->	 %sub_decoder
			case Element =:= null orelse is_binary(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, sub_decoder}
			end;
		12->	 %rule_id
			case is_integer(Element) of
				true->
					check(Content, N+1);
				_ ->
					{error, rule_id}
			end;
		13->	 %rule_match
			check(Content, N+1);
		14->	 %rule_description
			case is_binary(Element) of
				true->
					checkKVList(element(N+1, Content));
				_ ->
					{error, rule_description}
			end
	end.

checkKVList([{Key, Value}|Rest]) ->
	%my
	case is_binary(Key) andalso is_binary(Value) of
		true->
			checkKVList(Rest);
		_ ->
			{error, Key}
	end;
checkKVList([])->
	{ok, done}.

storage(Time)->
	ID = list_to_integer(Time),
	case kvs_storage:get(ID) of
		null->
			Storage = kvs_storage:id2storage(Time),
			kvs_storage:open(Storage);
		Result ->
			{ok, Result}
	end.

insert(Content) when is_record(Content, log)->
    case check(Content) of
	{ok, _} ->
		Time = Content#log.time,
		Str =  lists:sublist(integer_to_list(Time), 1, 8),
		case storage(Str) of
			{ok, Storage}->
				Sequence = dets:info(Storage#storage.data_table, size) + 1,
				IDString = lists:flatten(io_lib:format("~s~10..0w",[Str, Sequence])),
				ID =list_to_integer(IDString),
				Log = Content#log{id=ID},
				kvs_storage:write(Storage#storage.data_table, Log),
				kvs_index:add(Storage#storage.index_table, Log),
				kvs_output:output(Log),
				{ok, done};
			Error2->
				Error2
		end;
	Error1 ->
		Error1
   end;
insert(_)->
	{error, format}.