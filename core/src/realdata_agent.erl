-module(realdata_agent).
-export([get_sysinfo/1,get_data/2]).

-include("monitor.hrl").


get_agent_node()->
	case server_conf:getServerConf(realdataAgent) of
		undefined->
			{ok, Host} = inet:gethostname(),
			list_to_atom( "agent@" ++ Host);
		Node->
			Node
	end.
	
start()->
	rpc:call(get_agent_node(),agent,start,[]).
	
stop()->
	rpc:call(get_agent_node(),agent,stop,[]).
	
open(Mach) when is_record(Mach,machine)->
	case Mach#machine.method of
		"Telnet"->
			open(Mach#machine.host,23,Mach#machine.login,Mach#machine.passwd,telnet);
		"SSH"->
			open(Mach#machine.host,Mach#machine.sshport,Mach#machine.login,Mach#machine.passwd,ssh);
		_->
			{error,not_support_method}
	end;
open(_)->{error,parameter_error}.
	
open(Host, User, Password,telnet) ->
	open(Host,23,User, Password,telnet);
open(Host, User, Password,ssh) ->
	open(Host,22,User, Password,ssh);
open(_,_,_,_)->{error,parameter_error}.
	
open(Host, Port,User, Password,Method) ->
	rpc:call(get_agent_node(),agent,open,[Host,Port,User,Password,Method]).
	
getdatabykey(Host,Key)->
	rpc:call(get_agent_node(),agent,getdatabykey,[Host,Key]).
	
get_from_cache(Key)->
	erlcache:start_link(),
	erlcache:get(Key).
	
save_to_cache(Key,Val)->
	erlcache:start_link(),
	erlcache:set(Key,Val,10).
	
proxy(Host,F)->
	Key = erlang:phash2({proxy,Host,F}),
	case get_from_cache(Key) of
		{ok,R}->
			R;
		_->
			NR = rpc:call(get_agent_node(),proxy,F,[Host]),
			save_to_cache(Key,NR),
			NR
	end.
	
get_sysinfo(Mach)->
	start(),
	open(Mach),
	Ver = 
	case proxy(Mach#machine.host,os) of
		{ok,V1}->
			V1;
		_->
			"n/a"
	end,
	Uptime =
	case proxy(Mach#machine.host,'uptime') of
		{ok,V2}->
			V2;
		_->
			"n/a"
	end,
	{Date,Time} = 
	case proxy(Mach#machine.host,'datetime') of
		{ok,[{_,V3},{_,V4}]}->
			{V3,V4};
		_->
			{"n/a","n/a"}
	end,
	[Ver,Uptime,Date,Time].
	
get_data(Mach,Keys)->
	start(),
	open(Mach),
	get_data(Mach#machine.host,Keys,[]).
	
get_data(Host,[],R)->
	R;
get_data(Host,[{"logins",_}|T],R)->
	case proxy(Host,logins_count) of
		{ok,V1}->
			get_data(Host,T,R ++ [{"logins",V1}]);
		Err->
			throw(Err)
			% get_data(Host,T,R ++ [{"logins","n/a"}])
	end;
get_data(Host,[{C,_}|T],R) when C == "connection_established" orelse C == "connection_time_wait"
											orelse C == "connection_close_wait"->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,established) of
				{ok,Data}->
					get_data(Host,T,R ++ [{"connection_established",proplists:get_value(established,Data,"n/a")},
										{"connection_time_wait",proplists:get_value(time_wait,Data,"n/a")},
										{"connection_close_wait",proplists:get_value(close_wait,Data,"n/a")}]);
				Err->
					throw(Err)
					% get_data(Host,T,R ++ [{"connection_established","n/a"},{"connection_time_wait","n/a"},{"connection_close_wait","n/a"}])
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="disk_0" orelse C=="disk_0_size" orelse C=="disk_0_used" orelse C=="disk_0_available" orelse C=="disk_0_capacity"
						orelse C=="disk_1" orelse C=="disk_1_size" orelse C=="disk_1_used" orelse C=="disk_1_available" orelse C=="disk_1_capacity"
						orelse C=="disk_2" orelse C=="disk_2_size" orelse C=="disk_2_used" orelse C=="disk_2_available" orelse C=="disk_2_capacity"
						orelse C=="disk_3" orelse C=="disk_3_size" orelse C=="disk_3_used" orelse C=="disk_3_available" orelse C=="disk_3_capacity"->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,diskspace) of
				{ok,DiskData}->
					Disks = lists:sort(fun(X,Y)->proplists:get_value('Capacity',X) > proplists:get_value('Capacity',Y) end,DiskData),
					F = fun(X,Ret)->
						if
							X < length(Disks)->
								Dsk = lists:nth(X+1,Disks),
								Ret ++ [{"disk_"++integer_to_list(X),proplists:get_value('Filesystem',Dsk)},
									 {"disk_"++integer_to_list(X)++"_size",proplists:get_value('Space',Dsk)},
									 {"disk_"++integer_to_list(X)++"_used",proplists:get_value('Used',Dsk)},
									 {"disk_"++integer_to_list(X)++"_mount",proplists:get_value('Mounted on',Dsk)},
									 {"disk_"++integer_to_list(X)++"_available",proplists:get_value('Available',Dsk)},
									 {"disk_"++integer_to_list(X)++"_capacity",proplists:get_value('Capacity',Dsk)}
									 ];
							true->
								Ret ++ [{"disk_"++integer_to_list(X),"n/a"},
									 {"disk_"++integer_to_list(X)++"_size","n/a"},
									  {"disk_"++integer_to_list(X)++"_mount","n/a"},
									 {"disk_"++integer_to_list(X)++"_used","n/a"},
									 {"disk_"++integer_to_list(X)++"_available","n/a"},
									 {"disk_"++integer_to_list(X)++"_capacity","n/a"}
									 ]
						end
					end,
					Data = lists:foldl(F,[],lists:seq(0,3)),
					get_data(Host,T,R ++ Data);
				Err->
					throw(Err)
					% Data = lists:foldl(fun(X,Ret)->
							% Ret ++ [{"disk_"++integer_to_list(X),"n/a"},
								 % {"disk_"++integer_to_list(X)++"_size","n/a"},
								 % {"disk_"++integer_to_list(X)++"_used","n/a"},
								 % {"disk_"++integer_to_list(X)++"_available","n/a"},
								 % {"disk_"++integer_to_list(X)++"_capacity","n/a"}
								 % ]
							% end,[],lists:seq(0,3)),
					% get_data(Host,T,R ++ Data)
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="disk_writes" orelse C=="disk_reads" 
								orelse C == "disk_io_0_name" orelse C == "disk_io_0_value"
								orelse C == "disk_io_1_name" orelse C == "disk_io_1_value"
								orelse C == "disk_io_2_name" orelse C == "disk_io_2_value"
								orelse C == "disk_io_3_name" orelse C == "disk_io_3_value" ->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,iostat) of
				{ok,[{_,Rps},{_,Wps},BusyData]}->
					F = fun(X,Ret)->
						if
							X < length(BusyData)->
								{N,V} = lists:nth(X+1,BusyData),
								Ret ++ [{"disk_io_"++integer_to_list(X) ++ "_name",N},
									 {"disk_io_"++integer_to_list(X)++"_value",V}
									 ];
							true->
								Ret ++ [{"disk_io_"++integer_to_list(X) ++ "_name","n/a"},
									 {"disk_io_"++integer_to_list(X)++"_value","n/a"}
									 ]
						end
					end,
					BData = lists:foldl(F,[],lists:seq(0,3)),
					get_data(Host,T,R++[{"disk_reads",Rps},
										{"disk_writes",Wps}] ++ BData);
				Err->
					throw(Err)
					% Data = lists:foldl(fun(X,Ret)->
						% Ret ++ [
						 % {"disk_io_"++integer_to_list(X)++"_name","n/a"},
						 % {"disk_io_"++integer_to_list(X)++"_value","n/a"}
						 % ]
					% end,[],lists:seq(0,3)),
					% get_data(Host,T,R ++ Data ++ [{"disk_reads","n/a"},{"disk_writes","n/a"}])
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="total_cpu" orelse C=="user_percent" orelse C=="system_percent" orelse C=="wait_percent"->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,cpu) of
				{ok,{{_,Idle},{_,Usr},{_,Sys},{_,Wait}}}->
					get_data(Host,T,R ++ [{"total_cpu",100-Idle},{"user_percent",Usr},{"system_percent",Sys},{"wait_percent",Wait}]);
				Err->
					throw(Err)
					% get_data(Host,T,R ++ [{"total_cpu","n/a"},{"user_percent","n/a"},{"system_percent","n/a"},{"wait_percent","n/a"}])
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="processor_count" ->
	case proxy(Host,cpu_count) of
		{ok,CpuCount}->
			get_data(Host,T,R ++ [{"processor_count",CpuCount}]);
		Err->
			throw(Err)
	end;
	
get_data(Host,[{C,_}|T],R) when C=="ram_total" orelse C=="ram_free" orelse C=="vm_max" orelse C=="vm_free" orelse C=="ram_free_percent"
							orelse C=="swap_total" orelse C=="swap_used" orelse C=="vm_free_percent" orelse C=="swap_used_percent"->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,memory) of
				{ok,Datas}->
					RamTotal = proplists:get_value(physical_memory,Datas,0),
					RamUsed = proplists:get_value(used_physical_memory,Datas,0),
					VmMax = proplists:get_value(virtual_memory,Datas,0),
					VmUsed = proplists:get_value(used_virtual_memory,Datas,0),
					SwapTotal = proplists:get_value(swap_space,Datas,0),
					SwapUsed = proplists:get_value(used_swap_space,Datas,0),
					get_data(Host,T,R ++ [{"ram_total",RamTotal},{"ram_free",RamTotal-RamUsed},
										 {"ram_free_percent",round((RamTotal-RamUsed)*100/RamTotal)},
										{"vm_max",VmMax},{"vm_free",VmMax-VmUsed},{"swap_total",SwapTotal},
										{"swap_used",SwapUsed},
										{"vm_free_percent",round((VmMax-VmUsed)*100/VmMax)},
										{"swap_used_percent",round(SwapUsed*100/SwapTotal)}]);
				Err->
					throw(Err)
					% get_data(Host,T,R ++ [{"ram_total","n/a"},{"ram_free","n/a"},{"vm_max","n/a"},{"vm_free","n/a"},
					%					{"swap_total","n/a"},{"swap_used","n/a"},{"ram_free_percent","n/a"},{"vm_free_percent","n/a"}])
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="page_in" orelse C=="page_out" orelse C=="swap_writes" orelse C=="swap_reads"->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,swap) of
				{ok,Datas}->
					get_data(Host,T,R ++ [{"page_in",proplists:get_value('page in',Datas,"n/a")},
										 {"page_out",proplists:get_value('page out',Datas,"n/a")},
										 {"swap_writes",proplists:get_value('swap in',Datas,"n/a")},
										 {"swap_reads",proplists:get_value('swap out',Datas,"n/a")}
										 ]);
				Err->
					throw(Err)
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="processes_total"->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,processes_count) of
				{ok,Count}->
					get_data(Host,T,R ++ [{"processes_total",Count}]);
				Err->
					throw(Err)
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="processes_zombies"->
	case proplists:is_defined(C,R) of
		false->
			get_data(Host,T,R ++ [{"processes_zombies",0}]);
			% case proxy(Host,processes_count) of
				% {ok,Count}->
					% get_data(Host,T,R ++ [{"processes_total",Count}]);
				% Err->
					% throw(Err)
			% end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="processes_blocked"->
	case proplists:is_defined(C,R) of
		false->
			get_data(Host,T,R ++ [{"processes_blocked",0}]);
			% case proxy(Host,processes_count) of
				% {ok,Count}->
					% get_data(Host,T,R ++ [{"processes_total",Count}]);
				% Err->
					% throw(Err)
			% end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="queue_length"->
	case proplists:is_defined(C,R) of
		false->
			get_data(Host,T,R ++ [{"queue_length",0}]);
			% case proxy(Host,processes_count) of
				% {ok,Count}->
					% get_data(Host,T,R ++ [{"processes_total",Count}]);
				% Err->
					% throw(Err)
			% end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="processes_swapped"->
	case proplists:is_defined(C,R) of
		false->
			get_data(Host,T,R ++ [{"processes_swapped",0}]);
			% case proxy(Host,processes_count) of
				% {ok,Count}->
					% get_data(Host,T,R ++ [{"processes_total",Count}]);
				% Err->
					% throw(Err)
			% end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[{C,_}|T],R) when C=="nic_0_pkts" orelse C=="nic_0_errors" orelse C=="nic_0_name" orelse C=="nic_1_pkts" orelse C=="nic_1_errors"
							orelse C=="nic_1_name" orelse C=="pkts_in" orelse C=="pkts_out" orelse C=="pkts_error_in" orelse C=="pkts_error_out"->
	case proplists:is_defined(C,R) of
		false->
			case proxy(Host,network) of
				{ok,Datas}->
					NifData = lists:last(Datas),
					Nif = lists:sort(fun({_,NX},{_,NY})-> proplists:get_value('pkts/s',NX)>proplists:get_value('pkts/s',NY) end,NifData),
					get_data(Host,T,R ++ [{"pkts_in",proplists:get_value('Ipkts/s',Datas,"n/a")},
										 {"pkts_out",proplists:get_value('Opkts/s',Datas,"n/a")},
										 {"pkts_error_in",proplists:get_value('Ierrs/s',Datas,"n/a")},
										 {"pkts_error_out",proplists:get_value('Oerrs/s',Datas,"n/a")}
										 ] ++
											case Nif of
												[]->
													[{"nic_0_pkts","n/a"},
													{"nic_0_errors","n/a"},
													{"nic_0_name","n/a"},
													{"nic_1_pkts","n/a"},
													{"nic_1_errors","n/a"},
													{"nic_1_name","n/a"}
													];
												[{N0,ND0}]->
													[{"nic_0_pkts",proplists:get_value('pkts/s',ND0,"n/a")},
													{"nic_0_errors",proplists:get_value('errs/s',ND0,"n/a")},
													{"nic_0_name",N0},
													{"nic_1_pkts","n/a"},
													{"nic_1_errors","n/a"},
													{"nic_1_name","n/a"}
													];
												[{N0,ND0},{N1,ND1}|_]->
													[{"nic_0_pkts",proplists:get_value('pkts/s',ND0,"n/a")},
													{"nic_0_errors",proplists:get_value('errs/s',ND0,"n/a")},
													{"nic_0_name",N0},
													{"nic_1_pkts",proplists:get_value('pkts/s',ND1,"n/a")},
													{"nic_1_errors",proplists:get_value('errs/s',ND1,"n/a")},
													{"nic_1_name",N1}
													]
											end
													
										 );
				Err->
					throw(Err)
					% get_data(Host,T,R ++ [{"pkts_in","n/a"},{"pkts_out","n/a"},{"pkts_error_in","n/a"},{"pkts_error_out","n/a"},
					%					{"nic_0_pkts","n/a"},{"nic_0_errors","n/a"},{"nic_0_name","n/a"},{"nic_1_pkts","n/a"},
					%					{"nic_1_errors","n/a"},{"nic_1_name","n/a"}])
			end;
		_->
			get_data(Host,T,R)
	end;
get_data(Host,[_|T],R)->get_data(Host,T,R);
get_data(_,_,R)->R.