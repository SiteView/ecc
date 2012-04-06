-module(kvs_output).
-compile(export_all).

-include("kvs_define.hrl").

output(Record)->
	case kvs_config:get(output) of
		null ->
			nothing;
		 Path ->
			case filelib:is_dir(Path) of
				true ->
					Data = [<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>">>, 
							<<"<FMRequest>">>, 
							  <<"<Transaction Type=\"UpdateAdd\">">>, 
							    <<"<BusinessObjectList>">>]
							    ++ xml(Record) ++
						    [      <<"</BusinessObjectList>">>, 
							  <<"</Transaction>">>, 
							<<"</FMRequest>">>],
				     FileName = filename:join(Path, integer_to_list(Record#log.id) ++ ".xml"),
				     file:write_file(FileName, list_to_binary(Data));
				 _ ->
					nothing
			end
	end.

xml([Record|Rest], Acc)->
	xml(Rest, Acc ++ xml(Record));
xml([], Acc)->
	Acc.

find(Key, List)->
	case lists:keyfind(Key, 1, List) of
		{_, Value}->
			Value;
		_ ->
			<<>>
	end.

xml(Record)->
	My = Record#log.my,
	[<<"<BusinessObject Name=\"SysLog\">">>, 
	     <<"<FieldList>">>, 
	     	<<"<Field Name=\"Syslogid\">">>, integer_to_list(Record#log.id), <<"</Field>">>, 
		  <<"<Field Name=\"loglocation\">">>,  Record#log.machine, <<"</Field>">>, 
		  <<"<Field Name=\"srcuser\">">>, find(<<"from">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"dstuser\">">>, find(<<"to">>, My),  <<"</Field>">>, 
		  <<"<Field Name=\"loguser\">">>, find(<<"to">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"srcip\">">>, find(<<"srcip">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"dstip\">">>, find(<<"dstip">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"srcport\">">>, find(<<"srcport">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"dstport\">">>, find(<<"dstport">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"protocol\">">>, find(<<"protocol">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"evtid\">">>, find(<<"id">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"url\">">>, find(<<"url">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"evtaction\">">>, find(<<"action">>, My), <<"</Field>">>, 
		  <<"<Field Name=\"status\">">>, atom_to_binary(Record#log.level, latin1), <<"</Field>">>, 
	   <<"</FieldList>">>, 
      <<"</BusinessObject>">>].

storage(Time)->
	ID = list_to_integer(Time),
	case kvs_storage:get(ID) of
		null->
			Storage = kvs_storage:id2storage(Time),
			case filelib:is_file(Storage#storage.index_name) of
				true->
					kvs_storage:open(Storage);
				_ ->
					{error, "file_not_exist"}
			end;
		Result ->
			{ok, Result}
	end.

get_record(LogID)->
	case length(LogID) =:= 18 of
		true->
			Time = lists:sublist(LogID, 8),
			case storage(Time) of
				{ok, Storage}->
					case kvs_storage:read(Storage#storage.data_table, list_to_integer(LogID)) of
						null->
							{error, "id_not_exist"};
						Record->
							{ok, Record}
					end;
				Error->
					Error
			end;
		_ ->
			{error, "id_error"}
	end.

get_extra_data(Syslogid)->
	case get_record(Syslogid) of
		{ok, Record}->
			{ok, Record#log.message};
		Error->
			Error
	end.
	
days(Time)->
	Year = list_to_integer(lists:sublist(Time, 1, 4)),
	Month = list_to_integer(lists:sublist(Time, 5, 2)),
	Day = list_to_integer(lists:sublist(Time, 7, 2)),
	calendar:date_to_gregorian_days(Year, Month, Day).

get_relation_log_before(_, _, _, _, 0, Acc)->
	{0, Acc};	
get_relation_log_before(Table, Mache, StartPosition, EndPosition, Num, Acc)->
	case StartPosition > EndPosition of
		true->
			case kvs_storage:read(Table, StartPosition) of
				null->
					get_relation_log_before(Table, Mache, StartPosition - 1, EndPosition, Num, Acc);
				Record->
					case Record#log.machine =:= Mache of
						true->
							get_relation_log_before(Table, Mache, StartPosition - 1, EndPosition, Num - 1, [Record#log.message, <<"\r\n">>|Acc]);
						_ ->
							get_relation_log_before(Table, Mache, StartPosition - 1, EndPosition, Num, Acc)
					end
			end;
		_ ->
			{Num, Acc}
	end.

get_relation_log_before(_, _, _, 0, Acc)->
	Acc;
get_relation_log_before(Days, Mache, Position, Num, Acc)->
	{Year, Month, Day} = calendar:gregorian_days_to_date(Days),
	Time = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day])),
	case storage(Time) of
		{ok, Storage}->
			Table = Storage#storage.data_table,
			EndPosition = list_to_integer(Time ++ "0000000000"),
			StartPosition = case Position > 0 of
				false ->
					list_to_integer(lists:flatten(io_lib:format("~s~10..0w", [Time, dets:info(Table, size)])));
				_ ->
					Position
			end,
			{NewNum, NewAcc} = get_relation_log_before(Table, Mache, StartPosition, EndPosition, Num, Acc),
			get_relation_log_before(Days - 1, Mache, -1, NewNum, NewAcc);
		_ ->
			Acc
	end.

get_relation_log_before(Days, Mache, Position, Num)->
	get_relation_log_before(Days, Mache, Position, Num, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_relation_log_after(_, _, _, _, 0, Acc)->
	{0, Acc};
get_relation_log_after(Table, Mache, StartPosition, EndPosition, Num, Acc)->
	case StartPosition < EndPosition of
		true ->
			case kvs_storage:read(Table, StartPosition) of
				null ->
					get_relation_log_after(Table, Mache, StartPosition + 1, EndPosition, Num, Acc);
				Record ->
					case Record#log.machine =:= Mache of
						true ->
							get_relation_log_after(Table, Mache, StartPosition + 1, EndPosition, Num - 1, [Record#log.message, <<"\r\n">>|Acc]);
						_ ->
							get_relation_log_after(Table, Mache, StartPosition + 1, EndPosition, Num, Acc)
					end
			end;
		_ ->
			{Num, Acc}
	end.

get_relation_log_after(_, _, _, 0, Acc)->
	Acc;
get_relation_log_after(Days, Mache, Position, Num, Acc)->
	{Year, Month, Day} = calendar:gregorian_days_to_date(Days),
	Time = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day])),
	case storage(Time) of
		{ok, Storage} ->
			Table = Storage#storage.data_table,
			EndPosition = list_to_integer(lists:flatten(io_lib:format("~s~10..0w", [Time, dets:info(Table, size)]))),
			StartPosition = case Position > 0 of
				false ->
					list_to_integer(Time ++ "0000000000");
				_ ->
					Position
			end,
			{NewNum, NewAcc} = get_relation_log_after(Table, Mache, StartPosition, EndPosition, Num, Acc),
			get_relation_log_after(Days + 1, Mache, -1, NewNum, NewAcc);
		_ ->
			Acc
	end.

get_relation_log_after(Days, Mache, Position, Num)->
	get_relation_log_after(Days, Mache, Position, Num, []).

get_relation_log(Syslogid, Num)->
	case get_record(Syslogid) of
		{ok, Record} ->
			ID = Record#log.id,
			Machine = Record#log.machine,
			Time = lists:sublist(integer_to_list(ID), 8),
			Days = days(Time),
			BeforeResult = get_relation_log_before(Days, Machine, ID - 1, Num),
			AfterResult = lists:reverse(get_relation_log_after(Days, Machine, ID + 1, Num)),
			Data = list_to_binary([BeforeResult, <<"\r\n">>,  binary_to_list(Record#log.message), <<"\r\n">>, AfterResult]),
			{ok, Data};
		Error->
			Error
	end.