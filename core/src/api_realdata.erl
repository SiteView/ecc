%% 
%% @doc api of realtime data
%% @version{1.0}
%% @copyright 2010 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_realdata).
-export([get_real_data/3,get_classifier/3,save_classifier/4,get_real_data2/3]).

-include("monitor.hrl").

-define(MFREQ,86400).


getid(OsType,DataType,Host)->
	Tmp = lists:flatten(io_lib:format("realdata_~s_~s_~s",[OsType,DataType,Host])),
	list_to_atom(string:join(string:tokens(Tmp,"."),"_")).
	
load_monitor(OsType,DataType,Params)->
	case proplists:get_value(machine,Params) of
		undefined->
			{error,parameter_error};
		Host->
			Id = getid(OsType,DataType,Host),
			io:format("Monitor Id:~p~n",[Id]),
			case api_monitor:find_object(Id) of
				[]->
					case dbcs_monitor:get_monitor(Id) of
						{error,_}->
							create_monitor(OsType,DataType,Params ++ [{id,Id}]),
							{ok,Id};
						Data->
							start_monitor(Id),
							{ok,Id}
					end;
				[M|_]->
					{ok,Id}
			end
	end.
	
start_monitor(Id)->
	case siteview:create_object_by_id(Id) of
		{error,Err}->
			{error,load_monitor_error};
		Obj->
			case Obj:startMonitor(Obj) of
				{ok,_}->
					ok;
				_->
					Obj:delete(),
					{error,start_monitor_error}
			end
	end.
	
% create_monitor(aix,system_info,Params)->
	% create_monitor([{?CLASS,aix_sysinfo_monitor},{?NAME,"Aix System Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"}] ++ Params);
% create_monitor(aix,swap_info,Params)->
	% create_monitor([{?CLASS,aix_swap_monitor},{?NAME,"Aix Swap Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"swap_used_percent",'>=',95}]},
					% {warning_classifier,[{"swap_used_percent",'>=',70}]},
					% {good_classifier,[{"swap_used_percent",'>=',0}]},
					% {browse,[{"swap_total","Total"},{"swap_used","Used"},{"swap_used_percent","Used(%)"}]}] ++ Params);	
% create_monitor(aix,network_info,Params)->
	% create_monitor([{?CLASS,aix_network_monitor},{?NAME,"Aix Network Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"connection_established","Established"},{"connection_time_wait","Time_Wait"},
					% {"connection_close_wait","Close_Wait"},{"nic_0_pkts","NIC 0 Pkts"},
					% {"nic_0_errors","NIC 0 errors"},{"nic_0_name","NIC 0 Name"},{"nic_1_pkts","NIC 1 Pkts"},
					% {"nic_1_errors","NIC 1 errors"},{"nic_1_name","NIC 1 Name"}]}] ++ Params);
% create_monitor(aix,'network_info.pkts_in',Params)->
	% create_monitor([{?CLASS,aix_network_monitor},{?NAME,"Aix Network Pkts In"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"pkts_in","In Pkts/s"}]}] ++ Params);	
% create_monitor(aix,'network_info.pkts_out',Params)->
	% create_monitor([{?CLASS,aix_network_monitor},{?NAME,"Aix Network Pkts Out"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"pkts_out","Out Pkts/s"}]}] ++ Params);					
% create_monitor(aix,'network_info.errors_out',Params)->
	% create_monitor([{?CLASS,aix_network_monitor},{?NAME,"Aix Network Errors Out"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"pkts_error_out","Out Errors/s"}]}] ++ Params);	
% create_monitor(aix,'network_info.errors_in',Params)->
	% create_monitor([{?CLASS,aix_network_monitor},{?NAME,"Aix Network Errors In"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"pkts_error_in","In Errors/s"}]}] ++ Params);
% create_monitor(aix,'network_info.logins',Params)->
	% create_monitor([{?CLASS,aix_network_monitor},{?NAME,"Aix Network Logins"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"logins","Logins"}]}] ++ Params);	
% create_monitor(aix,cpu_info,Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Network Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"processor_count","processor count"}]}] ++ Params);
% create_monitor(aix,'cpu_info.total',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu(Total)"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"total_cpu",'>=',95}]},
					% {warning_classifier,[{"total_cpu",'>=',70}]},
					% {good_classifier,[{"total_cpu",'>=',0}]},
					% {browse,[{"total_cpu","Total %"}]}] ++ Params);	
% create_monitor(aix,'cpu_info.user_percent',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu(User Percent)"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"user_percent",'>=',95}]},
					% {warning_classifier,[{"user_percent",'>=',70}]},
					% {good_classifier,[{"user_percent",'>=',0}]},
					% {browse,[{"user_percent","User %"}]}] ++ Params);	
% create_monitor(aix,'cpu_info.system_percent',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu(System Percent)"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"system_percent",'>=',95}]},
					% {warning_classifier,[{"system_percent",'>=',70}]},
					% {good_classifier,[{"system_percent",'>=',0}]},
					% {browse,[{"system_percent","System %"}]}] ++ Params);	
% create_monitor(aix,'cpu_info.wait_percent',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu(Wait Percent)"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"wait_percent",'>=',95}]},
					% {warning_classifier,[{"wait_percent",'>=',70}]},
					% {good_classifier,[{"wait_percent",'>=',0}]},
					% {browse,[{"wait_percent","Wait %"}]}] ++ Params);
% create_monitor(aix,'cpu_info.queue_length',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu Queue Length"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"queue_length",'>=',10}]},
					% {warning_classifier,[{"queue_length",'>=',5}]},
					% {good_classifier,[{"queue_length",'>=',0}]},
					% {browse,[{"queue_length","Queue Length"}]}] ++ Params);
% create_monitor(aix,'cpu_info.processes_total',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu Processes Total"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"processes_total",'>=',1024}]},
					% {warning_classifier,[{"processes_total",'>=',500}]},
					% {good_classifier,[{"processes_total",'>=',0}]},
					% {browse,[{"processes_total","Processes Total"}]}] ++ Params);
% create_monitor(aix,'cpu_info.processes_zombies',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu Processes Zombies"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"processes_zombies",'>=',50}]},
					% {warning_classifier,[{"processes_zombies",'>=',30}]},
					% {good_classifier,[{"processes_zombies",'>=',0}]},
					% {browse,[{"processes_zombies","Processes Zombies"}]}] ++ Params);
% create_monitor(aix,'cpu_info.processes_blocked',Params)->
	% create_monitor([{?CLASS,aix_cpu_monitor},{?NAME,"Aix Cpu Processes Blocked"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"processes_blocked",'>=',10}]},
					% {warning_classifier,[{"processes_blocked",'>=',5}]},
					% {good_classifier,[{"processes_blocked",'>=',0}]},
					% {browse,[{"processes_blocked","Processes Blocked"}]}] ++ Params);
% create_monitor(aix,memory_info,Params)->
	% create_monitor([{?CLASS,aix_memory_monitor},{?NAME,"Aix Memory Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"ram_free_percent",'<',10}]},
					% {warning_classifier,[{"ram_free_percent",'<',20}]},
					% {good_classifier,[{"ram_free_percent",'>=',20}]},
					% {browse,[{"ram_total","Total(RAM)"},{"ram_free","Free(RAM)"},{"ram_free_percent","Free(RAM)(%)"}]}] ++ Params);
% create_monitor(aix,'memory_info.page_in',Params)->
	% create_monitor([{?CLASS,aix_memory_monitor},{?NAME,"Aix Memory Page In"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"page_in","Page In"}]}] ++ Params);
% create_monitor(aix,'memory_info.page_out',Params)->
	% create_monitor([{?CLASS,aix_memory_monitor},{?NAME,"Aix Memory Page Out"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"page_out","Page Out"}]}] ++ Params);
% create_monitor(aix,'memory_info.ram',Params)->
	% create_monitor([{?CLASS,aix_memory_monitor},{?NAME,"Aix Memory(Ram)"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"ram_free_percent",'<',10}]},
					% {warning_classifier,[{"ram_free_percent",'<',20}]},
					% {good_classifier,[{"ram_free_percent",'>=',20}]},
					% {browse,[{"ram_total","Total(RAM)"},{"ram_free","Free(RAM)"},{"ram_free_percent","Free(RAM)(%)"}]}] ++ Params);
% create_monitor(aix,'memory_info.vm',Params)->
	% create_monitor([{?CLASS,aix_memory_monitor},{?NAME,"Aix Memory(Vm)"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"vm_free_percent",'<',10}]},
					% {warning_classifier,[{"vm_free_percent",'<',40}]},
					% {good_classifier,[{"vm_free_percent",'>=',40}]},
					% {browse,[{"vm_max","Max Size(VM)"},	{"vm_free","Free(VM)"},{"vm_free_percent","Free(VM)(%)"}]}] ++ Params);
% create_monitor(aix,'memory_info.processes_swapped',Params)->
	% create_monitor([{?CLASS,aix_memory_monitor},{?NAME,"Aix Memory Processes Swapped"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"processes_swapped","Processes Swapped"}]}] ++ Params);
					
% create_monitor(aix,disk_info,Params)->
	% create_monitor([{?CLASS,aix_disk_monitor},{?NAME,"Aix Disk Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"disk_0","Disk 0"},
							% {"disk_0_size","Disk 0 Size"},
							% {"disk_0_used","Disk 0 Used"},
							% {"disk_0_available","Disk 0 Available"},
							% {"disk_0_capacity","Disk 0 Capacity"},
							% {"disk_1","Disk 1"},
							% {"disk_1_size","Disk 1 Size"},
							% {"disk_1_used","Disk 1 Used"},
							% {"disk_1_available","Disk 1 Available"},
							% {"disk_1_capacity","Disk 1 Capacity"},
							% {"disk_2","Disk 2"},
							% {"disk_2_size","Disk 2 Size"},
							% {"disk_2_used","Disk 2 Used"},
							% {"disk_2_available","Disk 2 Available"},
							% {"disk_2_capacity","Disk 2 Capacity"},
							% {"disk_3","Disk 3"},
							% {"disk_3_size","Disk 3 Size"},
							% {"disk_3_used","Disk 3 Used"},
							% {"disk_3_available","Disk 3 Available"},
							% {"disk_3_capacity","Disk 3 Capacity"}]}] ++ Params);
% create_monitor(aix,'disk_info.disk_0',Params)->
	% create_monitor([{?CLASS,aix_disk_monitor},{?NAME,"Aix Disk Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {error_classifier,[{"disk_0_capacity",'>',98}]},
					% {warning_classifier,[{"disk_0_capacity",'>=',90}]},
					% {good_classifier,[{"disk_0_capacity",'>',0}]},
					% {browse,[{"disk_0","Disk 0"},
							% {"disk_0_mount","Disk 0 Mount"},
							% {"disk_0_size","Disk 0 Size"},
							% {"disk_0_used","Disk 0 Used"},
							% {"disk_0_available","Disk 0 Available"},
							% {"disk_0_capacity","Disk 0 Capacity"}
							% ]}] ++ Params);		
% create_monitor(aix,'disk_info.disk_1',Params)->
	% create_monitor([{?CLASS,aix_disk_monitor},{?NAME,"Aix Disk Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"disk_1","Disk 1"},
							% {"disk_1_mount","Disk 1 Mount"},
							% {"disk_1_size","Disk 1 Size"},
							% {"disk_1_used","Disk 1 Used"},
							% {"disk_1_available","Disk 1 Available"},
							% {"disk_1_capacity","Disk 1 Capacity"}
							% ]}] ++ Params);		
% create_monitor(aix,'disk_info.disk_2',Params)->
	% create_monitor([{?CLASS,aix_disk_monitor},{?NAME,"Aix Disk Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"disk_2","Disk 2"},
							% {"disk_2_mount","Disk 2 Mount"},
							% {"disk_2_size","Disk 2 Size"},
							% {"disk_2_used","Disk 2 Used"},
							% {"disk_2_available","Disk 2 Available"},
							% {"disk_2_capacity","Disk 2 Capacity"}
							% ]}] ++ Params);	
% create_monitor(aix,'disk_info.disk_3',Params)->
	% create_monitor([{?CLASS,aix_disk_monitor},{?NAME,"Aix Disk Info"},
					% {frequency,?MFREQ},{disabled,false},{verfiy_error,false},{error_frequency,0},
					% {depends_on,"none"},{depends_condition,"good"},{schedule,"all"},
					% {browse,[{"disk_3","Disk 3"},
							% {"disk_3_mount","Disk 3 Mount"},
							% {"disk_3_size","Disk 3 Size"},
							% {"disk_3_used","Disk 3 Used"},
							% {"disk_3_available","Disk 3 Available"},
							% {"disk_3_capacity","Disk 3 Capacity"}
							% ]}] ++ Params);			
create_monitor(Os,Type,Params)->
	case file:consult("conf/realdata.conf") of
		{ok,Conf}->
			OData = lists:filter(fun({X,Y,_})->if X==Os andalso Y==Type->true;
											true->false
									 end end,Conf),
			case OData of
				[{_,_,Data}|_]->
					create_monitor(Data ++ Params);
				_->
					throw({error,no_such_data_type})
			end;
		Else->
			Else
	end.

create_monitor(Data)->
	case dbcs_monitor:create_monitor(Data) of
		{ok,_}->
			Id = proplists:get_value(id,Data),
			case siteview:create_object_by_id(Id) of
				{error,Err}->
					{error,load_monitor_error};
				Obj->
					case Obj:startMonitor(Obj) of
						{ok,_}->
							ok;
						_->
							Obj:delete(),
							{error,start_monitor_error}
					end
			end;
		Else->
			io:fromat("create_monitor:~p~n",[Else]),
			Else
	end.
	
%% @spec get_classifier(OsType,DataType,Params)-> ({error,Reason}|{ok,Result})
%% where
%%		OsType = aix | linux | unix | freebsd | windows
%%		DataType = system_info | network_info | cpu_info | memory_info | swap_info | disk_info | 'network_info.pkts_in'
%%					| 'network_info.pkts_out' | 'network_info.errors_out' | 'network_info.errors_in' | 'network_info.logins'
%%					| 'cpu_info.total' | 'cpu_info.user_percent' | 'cpu_info.system_percent' | 'cpu_info.wait_percent'
%%					| 'cpu_info.queue_length' | 'cpu_info.processes_total' | 'cpu_info.processes_zombies' | 'cpu_info.processes_blocked'
%%					| 'memory_info.page_in' | 'memory_info.page_out' | 'memory_info.ram' | 'memory_info.vm' | 'memory_info.processes_swapped'
%%					| 'swap_info.swap_writes' | 'swap_info.swap_reads' | 'swap_info.swap_used' | 'disk_info.disk_writes' | 'disk_info.disk_reads'
%%					 
%%		Params = [{Key,Value}]
%%		Result = [{Key,Value}]
%%		Key = atom()
%%		Value = term()
%%		Reason = connect_fail | parameter_error | no_data
%% @doc get classifier of device.
%%
get_classifier(OsType,DataType,Params)->
	Id = proplists:get_value(id,Params),
	Mach = dbcs_machine:get_machineById(Id),
	
	case Mach#machine.id of
		undefined->
			{error,parameter_error};
		_->
			MId = getid(OsType,DataType,Mach#machine.name),
			{ok,api_monitor:get_all_classifier(MId)}
	end.
	
save_classifier(OsType,DataType,Params,Classifier)->
	Id = proplists:get_value(id,Params),
	Mach = dbcs_machine:get_machineById(Id),
	case Mach#machine.id of
		undefined->
			{error,parameter_error};
		_->
			MId = getid(OsType,DataType,Mach#machine.name),
			dbcs_monitor:update_monitor([{id,MId}]++make_classifier(Classifier))
	end.

make_classifier([])->[];	
make_classifier([{error,Clsfier}|T])->
	[{error_classifier,Clsfier}] ++ make_classifier(T);
make_classifier([{warning,Clsfier}|T])->
	[{warning_classifier,Clsfier}] ++ make_classifier(T);
make_classifier([{good,Clsfier}|T])->
	[{good_classifier,Clsfier}] ++ make_classifier(T);
make_classifier([_|T])->make_classifier(T).

%% @spec get_real_data(OsType,DataTypes,Params)-> ({error,Reason}|{ok,Result})
%% where
%%		OsType = aix | linux | unix | freebsd | windows
%%		DataTypes = DataType | [DataType]
%%		DataType = system_info | network_info | cpu_info | memory_info | swap_info | disk_info | 'network_info.pkts_in'
%%					| 'network_info.pkts_out' | 'network_info.errors_out' | 'network_info.errors_in' | 'network_info.logins'
%%					| 'cpu_info.total' | 'cpu_info.user_percent' | 'cpu_info.system_percent' | 'cpu_info.wait_percent'
%%					| 'cpu_info.queue_length' | 'cpu_info.processes_total' | 'cpu_info.processes_zombies' | 'cpu_info.processes_blocked'
%%					| 'memory_info.page_in' | 'memory_info.page_out' | 'memory_info.ram' | 'memory_info.vm' | 'memory_info.processes_swapped'
%%					| 'swap_info.swap_writes' | 'swap_info.swap_reads' | 'swap_info.swap_used' | 'disk_info.disk_writes' | 'disk_info.disk_reads'
%%					 | 'disk_info.disk_io_0' | 'disk_info.disk_io_1' | 'disk_info.disk_io_2' | 'disk_info.disk_io_3'
%%		Params = [{Key,Value}]
%%		Result = [{Key,Value}]
%%		Key = atom()
%%		Value = term()
%%		Reason = connect_fail | parameter_error | no_data
%% @doc get real data of device.
%%
get_real_data(OsType,DataTypes,Params)->
	Key = erlang:phash2({OsType,DataTypes,Params}),
	case get_from_cache(Key) of
		{ok,R}->
			R;
		_->
			try
				Id = proplists:get_value(id,Params),
				case Id of
					undefined->
						{error,parameter_error};
					_->
						Mach = dbcs_machine:get_machineById(Id),
						case Mach#machine.id of
							undefined->
								{error,parameter_error};
							_->
								MParams = [{machine,Mach#machine.name}],
								Nr = case is_atom(DataTypes) of
										true->
											get_data(OsType,[DataTypes],MParams,[]);
										_->
											get_data(OsType,DataTypes,MParams,[])
									end,
								save_to_cache(Key,Nr),
								Nr
						end
				end
			catch
				error:Msg->
					{error,Msg};
				Else->
					Else
			end
	end.
	
	
get_real_data2(OsType,DataTypes,Params)->
	Key = erlang:phash2({OsType,DataTypes,Params}),
	case get_from_cache(Key) of
		{ok,R}->
			R;
		_->
			try
				Machine = proplists:get_value(machine,Params),
				case Machine of
					undefined->
						{error,parameter_error};
					_->
						Nr = case is_atom(DataTypes) of
								true->
									get_data(OsType,[DataTypes],Params,[]);
								_->
									get_data(OsType,DataTypes,Params,[])
							end,
						save_to_cache(Key,Nr),
						Nr
				end
			catch
				_:Msg->
					{error,Msg};
				Else->
					Else
			end
	end.

get_from_cache(Key)->
	erlcache:start_link(),
	erlcache:get(Key).
	
save_to_cache(Key,Val)->
	erlcache:start_link(),
	erlcache:set(Key,Val,5).

get_data(_,[],_,R)->
	{ok,R};
get_data(OsType,[DataType|T],Params,R)->
	case load_monitor(OsType,DataType,Params) of
		{ok,Id}->
			try
				case get_data(OsType,DataType,Params++[{id,Id}],[]) of
					{ok,Data}->
						get_data(OsType,T,Params,R ++ [{DataType,Data}]);
					{error,Err}->
						get_data(OsType,T,Params,R ++ [{DataType,Err}])
				end
			catch
				_:Msg->
					get_data(OsType,T,Params,R ++ [{DataType,Msg}])
			end;
		Else->
			Else
	end;
get_data(_,_,Params,_)->
	run_monitor(Params).
	
run_monitor(Params)->
	Id = proplists:get_value(id,Params),
	case api_monitor:find_object(Id) of
		[M|_]->
			case M:update() of
				{ok,_}->
					M:runClassifiers(M);
				Else->
					throw(Else)
			end,
			F = fun(X)->
				case M:get_attribute(X) of
					{ok,Val}->
						Val;
					_->
						{X,not_found}
				end
			end,
			Ret = lists:map(F,M:getLogProperties(M)++[category]),
			{ok,Ret};
		_->
			{error,monitor_not_found}
	end.
	% M = MonitorClass:new(),
	% M:add_properties(Params),
	% case M:update() of
		% {ok,_}->
			% F = fun(X)->
					% case M:get_attribute(X) of
						% {ok,Val}->
							% Val;
						% _->
							% {X,not_found}
					% end
				% end,
			% Ret = lists:map(F,M:getLogProperties(M)),
			% M:delete(),
			% {ok,Ret};
		% {error,Err}->
			% {error,Err};
		% Else->
			% {error,Else}
	% end.
