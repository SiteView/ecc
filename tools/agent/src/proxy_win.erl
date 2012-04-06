-module(proxy_win).
-compile(export_all).

bytes_convertion(Type,Column) ->
	case Type of
		memory ->
			convert({kb,mb,Column});
		diskspace ->
			[H|T] = Column,
			Converted_value = [convert({b,gb,Value})||Value <- T],
			[H|Converted_value]
	end.
	
convert(Value) ->
	case Value of
		{kb,mb,[{Key,Kbytes}|T]} -> 
			[{Key,integer_to_list(round(list_to_integer(Kbytes)/1024))++"MB"}|convert({kb,mb,T})];
		{kb,mb,[]} -> [];
		{b,gb,[{Key,Kbytes}|T]} ->
			case Key of
				'DeviceID' ->
					[{Key,Kbytes}|convert({b,gb,T})];
				_ -> 
					[{Key,integer_to_list(round(list_to_integer(Kbytes)/1024/1024/1024))++"GB"}|convert({b,gb,T})]
			end;
		{b,gb,[]} -> []
		
	end.

filter_name(Column) ->
	lists:keydelete('Name',1,Column).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diskspace(Host) ->
	case agent_win:getdatabykey(Host,'diskspace') of  
		{ok,{simple_list,Result}} -> {ok,bytes_convertion(diskspace,Result)};
		ErrorReason -> ErrorReason
	end.
	
os(Host) ->
	case agent_win:getdatabykey(Host,'os') of
		{ok,{table,Result}} -> {ok,filter_name(Result)};
		ErrorReason -> ErrorReason
	end.

cpu(Host) ->
	case agent_win:getdatabykey(Host,'cpu') of
		{ok,{simple_list,Result}} -> {ok,Result};
		ErrorReason -> ErrorReason
	end.
	
processes_count(Host) ->
	case agent_win:getdatabykey(Host,'processes_count') of
		{ok,{table,Result}} -> {ok,Result};
		ErrorReason -> ErrorReason
	end.

uptime(Host) ->
	case agent_win:getdatabykey(Host,'uptime') of
		{ok,{table,Result}} -> {ok,filter_name(Result)};
		ErrorReason -> ErrorReason
	end.
	
physical_memory(Host) ->
	case agent_win:getdatabykey(Host,'physical_memory') of
		{ok,{table,Result}} -> {ok,bytes_convertion(memory,filter_name(Result))};
		ErrorReason -> ErrorReason
	end.
	
virtual_memory(Host) ->
	case agent_win:getdatabykey(Host,'virtual_memory') of
		{ok,{table,Result}} -> {ok,bytes_convertion(memory,filter_name(Result))};
		ErrorReason -> ErrorReason
	end.
	
paging_files(Host) ->
	case agent_win:getdatabykey(Host,'paging_files') of
		{ok,{table,Result}} -> {ok,bytes_convertion(memory,filter_name(Result))};
		ErrorReason -> ErrorReason
	end.
	
