-module(snmp_machine).
-include_lib("snmp/include/snmp_types.hrl").
-include("monitor.hrl").
-include("snmp_machine.hrl").

-define(SYSOBJECTID, [1,3,6,1,2,1,1,2]).        %% System Object id: uniquely identifies a device manufacturer model

-compile(export_all).

-export([getSnmpMachine/1]).

getSnmpMachine(Host) ->
    case api_machine:getMachineByHost(Host, 1, 0, "", "") of
        [Mach=#machine{}] ->
            if
                Mach#machine.method =:= "Snmp" ->
                    {ok, Mach};
                true ->
                    {error, 'get_machine_error'}
            end;
        _ ->
            {error, 'get_machine_error'}
    end.

%% According to the host ip to get to the cpu usage oid
getCpuUsedOid(Host) ->
    case api_machine:getMachineByHost(Host, 1, 0, "", "") of
        [Mach=#machine{}] ->
            Session = constructSnmpSession(Mach),
            case Session:g(?SYSOBJECTID) of
                {ok, {noError, _, [Vbs=#varbind{}]}, _} ->
                    Oid = Vbs#varbind.value,
                    case snmp_machine_config:readSysOidConfig(Oid) of
                        {ok, Param} ->
                            case lists:keysearch(?CPUDUTYUSED, 1, Param) of
                                {value, {?CPUDUTYUSED, SOid}} ->
                                    {ok, [{oid, SOid}, {machine, Mach}]};
                                _ ->
                                    {error, 'get_cpu_used_error'}
                            end;
                        Other ->
                            Other
                    end;
                OO ->
                    {error, 'get_cpu_used_error'}
            end;
        _ ->
            {error, 'get_machine_error'}
    end.

getMemoryUsedOid(Host) ->
    %~ io:format("getMemoryUsedOid ~p~n ",[Host]),
    case api_machine:getMachineByHost(Host, 1, 0, "", "") of
        [Mach=#machine{}] ->
            Session = constructSnmpSession(Mach),
            case Session:g(?SYSOBJECTID) of
                {ok, {noError, _, [Vbs=#varbind{}]}, _} ->
		    io:format("Vbs Vbs Vbs Vbs Vbs Vbs Vbs ~p~n",[Vbs]),
                    Oid = Vbs#varbind.value,
		    io:format("Oid Oid Oid Oid Oid Oid  Oid~p~n",[Oid]),
                    case snmp_machine_config:readSysOidConfig(Oid) of
                        {ok, Param} ->
                            Free =
                            case lists:keysearch(?MEMORYFREE, 1, Param) of
                                {value, {?MEMORYFREE, SOid}} ->
                                    [{?MEMORYFREE, SOid}];
                                _ ->
                                    []
                            end,
                            Size =
                            case lists:keysearch(?MEMORYSIZE, 1, Param) of
                                {value, {?MEMORYSIZE, SizeOid}} ->
                                    [{?MEMORYSIZE, SizeOid}];
                                _ ->
                                    []
                            end,
                            Used =
                            case lists:keysearch(?MEMORYUSED, 1, Param) of
                                {value, {?MEMORYUSED, UsedOid}} ->
                                    [{?MEMORYUSED, UsedOid}];
                                _ ->
                                    []
                            end,
			    Untis = 
			    case lists:keysearch(?MEMORYUNTIS, 1, Param) of
                                {value, {?MEMORYUNTIS, UntisOid}} ->
                                    [{?MEMORYUNTIS, UntisOid}];
                                _ ->
                                    []
                            end,
                            {ok, [{oid, lists:append([Free, Size, Used,Untis])}, {machine, Mach},{sysobjectid,Oid}]};
                        Other ->
				io:format("other Oid Oid Oid Oid Oid Oid  Oid~p~n",[Oid]),
                            Other
                    end;
                OO ->
                    {error, 'get_cpu_used_error'}
            end;
        _ ->
		io:format("api_machine:getMachineByHost  fail~n ",[]),
            {error, 'get_machine_error'}
    end.

%% get cpu rate
cpuUsed(Host) ->
    case getCpuUsedOid(Host) of
        {ok, [{oid, SOid}, {machine, Mach}]} ->
            Session = constructSnmpSession(Mach),
            {ok, Session:get_table_col(SOid)};
        Other ->
            Other
    end.

%% get memory
getMemoryFull(Host) ->
    case getMemoryUsedOid(Host) of
        {ok, [{oid, SOid}, {machine, Mach},{sysobjectid,Objid}]} ->
		io:format("SOid = ~p~n  Mach= ~p~n Objid= ~p~n",[SOid,Mach,Objid]),
			case lists:prefix([1,3,6,1,4,1,311,1,1,3,1], Objid) of
				true ->
					io:format("getwin32_memory~n",[]),
					getwin32_memory(Mach,SOid);
				_ ->
					Session = constructSnmpSession(Mach),
					Frees =
					case lists:keysearch(?MEMORYFREE, 1, SOid) of
						{value, {?MEMORYFREE, Free}} ->
							{?MEMORYFREE, Session:get_table_col(Free)};
						_ ->
							{?MEMORYFREE, -1}
					end,
					Sizes =
					case lists:keysearch(?MEMORYSIZE, 1, SOid) of
						{value, {?MEMORYSIZE, Size}} ->
							{?MEMORYSIZE, Session:get_table_col(Size)};
						_ ->
							{?MEMORYSIZE, -1}
					end,
					Useds =
					case lists:keysearch(?MEMORYUSED, 1, SOid) of
						{value, {?MEMORYUSED, Used}} ->
							{?MEMORYUSED, Session:get_table_col(Used)};
						_ ->
							{?MEMORYUSED, -1}
					end,
					io:format("Frees = ~p~n  Sizes= ~p~n Useds= ~p~n",[Frees,Sizes,Useds]),
					{ok, [Frees, Sizes, Useds]}
			end;
        Other ->
		io:format("Other = ~p~n",[Other]),
            Other
    end.
    
    

    
    
    
constructSnmpSession(Mach=#machine{}) ->
    Params = 
        case lists:keysearch(snmpParam, 1, Mach#machine.other) of
            {value, {snmpParam, Pm}} ->
                Pm;
            _ ->
                []
        end,
    ErIp = textutils:ipStr2ErIp(Mach#machine.host),
    Port = 
        case lists:keysearch(snmpPort, 1, Params) of
            {value, {snmpPort, P}} ->
                P;
            _ ->
                161
        end,
    Version =
        case lists:keysearch(snmpVer, 1, Params) of
            {value, {snmpVer, V}} ->
                V;
            _ ->
                "V1"
        end,
    Community = 
        case lists:keysearch(getCommunity, 1, Params) of
            {value, {getCommunity, C}} ->
                C;
            _ ->
                "public"
        end,
    AuthType = 
        case lists:keysearch(authType, 1, Params) of
            {value, {authType, AType}} ->
                AType;
            _ ->
                "MD5"
        end,
    User = 
        case lists:keysearch(user, 1, Params) of
            {value, {user, U}} ->
                U;
            _ ->
                ""
        end,
    Passwd = 
        case lists:keysearch(passwd, 1, Params) of
            {value, {passwd, Pwd}} ->
                Pwd;
            _ ->
                ""
        end,
    PrivPasswd = 
        case lists:keysearch(privPasswd, 1, Params) of
            {value, {privPasswd, PriPwd}} ->
                PriPwd;
            _ ->
                ""
        end,
    ContextID = 
        case lists:keysearch(contextId, 1, Params) of
            {value, {contextId, CtxId}} ->
                CtxId;
            _ ->
                ""
        end,
    ContextName = 
        case lists:keysearch(contextName, 1, Params) of
            {value, {contextName, CtxName}} ->
                CtxName;
            _ ->
                ""
        end,
    Timeout = 
        case lists:keysearch(timeout, 1, Params) of
            {value, {timeout, T}} ->
                T;
            _ ->
                5000
        end,
    Session = snmp_session:new(ErIp,Port,Version,Community,AuthType,User,Passwd,PrivPasswd,ContextID,ContextName,Timeout),
    Session.

debug(Msg) ->
    {ok, S} = file:open("snmp_machine_debug.log", write),
    io:format(S, "~p.~n", [Msg]).
	
format_result(Value) ->
	case Value of
		{ok,{noError,_,[Vb|_]},_}->
			case Vb#varbind.variabletype of
				'NULL'-> [];
				_->
					[Vb]
			end;
		_->
			[]
	end.
	
getwin32_memory(Mach,SOid) ->
	Session = constructSnmpSession(Mach),
	Storage_type = [1,3,6,1,2,1,25,2,1,2],
	Result = Session:get_table_col_filter_column(Storage_type,[1,3,6,1,2,1,25,2,3,1,2]),
	io:format("getwin32_memory       ..........~p~n",[Result]),
	Storage_typeoid = Result#varbind.oid,
	Index = [hd(lists:reverse(Storage_typeoid))],
	io:format("Index:~p~n",[Index]),
	Frees =
	case lists:keysearch(?MEMORYFREE, 1, SOid) of
		{value, {?MEMORYFREE, Free}} ->
			Freeoid = Free++Index,
			{?MEMORYFREE, format_result(Session:gt(Freeoid))};
		_ ->
			{?MEMORYFREE, -1}
	end,
	io:format("Frees:~p~n",[Frees]),
	Sizes =
	case lists:keysearch(?MEMORYSIZE, 1, SOid) of
		{value, {?MEMORYSIZE, Size}} ->
			Sizeoid = Size++Index,
			{?MEMORYSIZE, format_result(Session:gt(Sizeoid))};
		_ ->
			{?MEMORYSIZE, -1}
	end,
	io:format("Sizes:~p~n",[Sizes]),
	Useds =
	case lists:keysearch(?MEMORYUSED, 1, SOid) of
		{value, {?MEMORYUSED, Used}} ->
			Usedoid = Used++Index,
			{?MEMORYUSED, format_result(Session:gt(Usedoid))};
		_ ->
			{?MEMORYUSED, -1}
	end,
	io:format("Useds:~p~n",[Useds]),
	Units = 
	case lists:keysearch(?MEMORYUNTIS, 1, SOid) of
		{value, {?MEMORYUNTIS, Unit}} ->
			Unitoid = Unit++Index,
			{?MEMORYUNTIS, format_result(Session:gt(Unitoid))};
		_ ->
			{?MEMORYUNTIS, -1}
	end,
	io:format("Units:~p~n",[Units]),
	{ok, [Frees, Sizes, Useds,Units]}.
	


getDiskUsedOid(Host) ->
    case api_machine:getMachineByHost(Host, 1, 0, "", "") of
        [Mach=#machine{}] ->
            Session = constructSnmpSession(Mach),
            case Session:g(?SYSOBJECTID) of
                {ok, {noError, _, [Vbs=#varbind{}]}, _} ->
                    Oid = Vbs#varbind.value,
                    case snmp_machine_config:readSysOidConfig(Oid) of
                        {ok, Param} ->
                            DiskDesc =
                            case lists:keysearch(?DISKDESC, 1, Param) of
                                {value, {?DISKDESC, DiskDescOid}} ->
                                    [{?DISKDESC, DiskDescOid}];
                                _ ->
                                    []
                            end,
                            DiskTotal =
                            case lists:keysearch(?DISKTOTAL, 1, Param) of
                                {value, {?DISKTOTAL, DiskTotalOid}} ->
                                    [{?DISKTOTAL, DiskTotalOid}];
                                _ ->
                                    []
                            end,
                            DiskUsed =
                            case lists:keysearch(?DISKUSED, 1, Param) of
                                {value, {?DISKUSED, DiskUsedOid}} ->
                                    [{?DISKUSED, DiskUsedOid}];
                                _ ->
                                    []
                            end,
			    DiskPercentUsed = 
			    case lists:keysearch(?DISKPERCENTUSED, 1, Param) of
                                {value, {?DISKPERCENTUSED, DiskPercentUsedOid}} ->
                                    [{?DISKPERCENTUSED, DiskPercentUsedOid}];
                                _ ->
                                    []
                            end,
                            {ok, [{oid, lists:append([DiskDesc, DiskTotal, DiskUsed,DiskPercentUsed])}, {machine, Mach},{sysobjectid,Oid}]};
                        Other ->
                            Other
                    end;
                OO ->
                    {error, 'get_disk_used_error'}
            end;
        _ ->
            {error, 'get_machine_error'}
    end.

%% get disk
getDiskFull(Host) ->
    case getDiskUsedOid(Host) of
        {ok, [{oid, SOid}, {machine, Mach},{sysobjectid,Objid}]} ->
			case lists:prefix([1,3,6,1,4,1,311,1,1,3,1], Objid) of
				true ->
					getwin32_disk(Mach,SOid);
				_ ->
					Session = constructSnmpSession(Mach),
					DiskDescs =
					case lists:keysearch(?DISKFREE, 1, SOid) of
						{value, {?DISKDESC, DiskDesc}} ->
							{?DISKDESC, Session:get_table_col(DiskDesc)};
						_ ->
							{?DISKDESC, -1}
					end,
					DiskTotals =
					case lists:keysearch(?DISKTOTAL, 1, SOid) of
						{value, {?DISKTOTAL, DiskTotal}} ->
							{?DISKTOTAL, Session:get_table_col(DiskTotal)};
						_ ->
							{?DISKTOTAL, -1}
					end,
					DiskUseds =
					case lists:keysearch(?DISKUSED, 1, SOid) of
						{value, {?DISKUSED, DiskUsed}} ->
							{?DISKUSED, Session:get_table_col(DiskUsed)};
						_ ->
							{?DISKUSED, -1}
					end,
					DiskPercentUseds =
					case lists:keysearch(?DISKPERCENTUSED, 1, SOid) of
						{value, {?DISKPERCENTUSED, DiskPercentUsed}} ->
							{?DISKPERCENTUSED, Session:get_table_col(DiskPercentUsed)};
						_ ->
							{?DISKPERCENTUSED, -1}
					end,
					{ok, [DiskDescs, DiskTotals, DiskUseds,DiskPercentUseds]}
			end;
        Other ->
            Other
    end.
    
    
    
getwin32_disk(Mach,SOid) ->
	Session = constructSnmpSession(Mach),
	Result = Session:get_table_col([1,3,6,1,2,1,25,2,3,1,2]),
	ResultDiskCol = get_disk_col(Result,[]),
	%~ io:format("ResultDiskCol  ~p~n",[ResultDiskCol]),
	get_disk(ResultDiskCol,[],SOid,Mach).

get_disk([H|T],Value,SOid,Mach) ->
	Session = constructSnmpSession(Mach),
	Storage_typeoid =H#varbind.oid,
	Index = [hd(lists:reverse(Storage_typeoid))],
	DiskDescs =
	case lists:keysearch(?DISKDESC, 1, SOid) of
		{value, {?DISKDESC, DiskDesc}} ->
			DiskDescid = DiskDesc++Index,
			{?DISKDESC, format_result(Session:gt(DiskDescid))};
		_ ->
			{?DISKDESC, -1}
	end,
	DiskTotals =
	case lists:keysearch(?DISKTOTAL, 1, SOid) of
		{value, {?DISKTOTAL, DiskTotal}} ->
			DiskTotalid = DiskTotal++Index,
			{?DISKTOTAL, format_result(Session:gt(DiskTotalid))};
		_ ->
			{?DISKTOTAL, -1}
	end,
	DiskUseds =
	case lists:keysearch(?DISKUSED, 1, SOid) of
		{value, {?DISKUSED, DiskUsed}} ->
			DiskUsedid = DiskUsed++Index,
			{?DISKUSED, format_result(Session:gt(DiskUsedid))};
		_ ->
			{?DISKUSED, -1}
	end,
	DiskPercentUseds = 
	case lists:keysearch(?DISKPERCENTUSED, 1, SOid) of
		{value, {?DISKPERCENTUSED, DiskPercentUsed}} ->
			DiskPercentUsedid = DiskPercentUsed++Index,
			{?DISKPERCENTUSED, format_result(Session:gt(DiskPercentUsedid))};
		_ ->
			{?DISKPERCENTUSED, -1}
	end,
	{?DISKDESC, [Desc]} = DiskDescs,
	Descs = Desc#varbind.value,
	End = string:str(Descs,":"),
	DescDisk = string:substr(Descs,1,End-1),
	get_disk(T,[{DescDisk,[DiskTotals, DiskUseds,DiskPercentUseds]}] ++ Value,SOid,Mach);
	
get_disk([],Values,_SOid,_Mach) ->
	{ok,Values}.


%% get disk  OID
get_disk_col([Priv|Next],DiskCol)->
	DiskType = Priv#varbind.value,
	if
	     DiskType =:= [1,3,6,1,2,1,25,2,1,4] ->
                    get_disk_col(Next,[Priv] ++ DiskCol);
	     true -> 
		     get_disk_col(Next,DiskCol)
	end;
		
get_disk_col([],DiskCols)->
	DiskCols.


%%get all disk
getDisks(Host) ->
	case getDiskUsedOid(Host) of
        {ok, [{oid, SOid}, {machine, Mach},{sysobjectid,Objid}]} ->
			case lists:prefix([1,3,6,1,4,1,311,1,1,3,1], Objid) of
				true ->
					getwin32_disks(Mach,SOid);
				_ ->
					[]
			end
	end.

getwin32_disks(Mach,SOid) ->
	Session = constructSnmpSession(Mach),
	Result = Session:get_table_col([1,3,6,1,2,1,25,2,3,1,2]),
	ResultDiskCol = get_disk_col(Result,[]),
	get_disks(ResultDiskCol,[],SOid,Mach).

get_disks([H|T],Value,SOid,Mach) ->
	Session = constructSnmpSession(Mach),
	Storage_typeoid =H#varbind.oid,
	Index = [hd(lists:reverse(Storage_typeoid))],
	DiskDescs =
	case lists:keysearch(?DISKDESC, 1, SOid) of
		{value, {?DISKDESC, DiskDesc}} ->
			DiskDescid = DiskDesc++Index,
			{?DISKDESC, format_result(Session:gt(DiskDescid))};
		_ ->
			{?DISKDESC, -1}
	end,
	{?DISKDESC, [Desc]} = DiskDescs,
	Descs = Desc#varbind.value,
	End = string:str(Descs,":"),
	DescDisk = string:substr(Descs,1,End-1),
	get_disks(T,[DescDisk] ++ Value,SOid,Mach);

get_disks([],Values,_SOid,_Mach) ->
	Values.
	
%% According to the process to get the host ip oid
getwin32_processname(Host) ->
	case api_machine:getMachineByHost(Host, 1, 0, "", "") of
		[Mach=#machine{}] ->  
			
			Session = constructSnmpSession(Mach),
			case Session:g(?SYSOBJECTID) of
			   {ok, {noError, _, [Vbs=#varbind{}]}, _} ->
					SOid = Vbs#varbind.value,
					io:format("SOid:~p~n",[SOid]),
					case snmp_machine_config:readSysOidConfig(SOid) of
						{ok, Param} ->
						io:format("Param:~p~n",[{ok, Param}]),
							{?PROCESSNAME,Process_name}=
								case lists:keysearch(?PROCESSNAME, 1, Param) of
									{value, {?PROCESSNAME, Processoid}} ->
										{?PROCESSNAME, Session:get_table_col(Processoid)};
									_ ->
										{?PROCESSNAME, -1}
								end,
								io:format("Process_name:~p~n",[Process_name]),
								F = fun(Element) ->
									Oid= Element#varbind.oid,
									Value=Element#varbind.value,
									Index = hd(lists:reverse(Oid)),
									{Value,integer_to_list(Index)}
								end,
								Restult = lists:map(F,Process_name),
								io:format("Restult:~p~n",[Restult]),
							{ok, Restult};
						Other ->
                            Other
					end;	
				OO ->
                    {error, 'get_process_error'}
			end;
		 _ ->
				{error, 'get_machine_error'}
	end.
	
%% Process to process according to the state index for
getwin32_processStatus(Mach,Index) ->
        io:format("getwin32_processStatus:~p~n",[Index]),
		Session = constructSnmpSession(Mach),
		case Session:g(?SYSOBJECTID) of
		    {ok, {noError, _, [Vbs=#varbind{}]}, _} ->
				SOid = Vbs#varbind.value,
				io:format("process SOid:~p~n",[SOid]),
				    case snmp_machine_config:readSysOidConfig(SOid) of
						{ok, Param} ->
							io:format("Param:~p~n",[{ok, Param}]),
							{?PROCESSSTATUS,Status} =
							case lists:keysearch(?PROCESSSTATUS, 1, Param) of
								{value, {?PROCESSSTATUS, Processsts}} ->
									Statusname = Processsts++[list_to_integer(Index)],
									io:format("Statusname:~p~n",[Statusname]),
									{?PROCESSSTATUS, format_result(Session:gt(Statusname))};
								_ ->
									{?PROCESSSTATUS, -1}
							end,
							io:format("Status:~p~n",[Status]),
							Result = hd(Status),
							ProcessResult = Result#varbind.value,
							io:format("Result:~p~n",[ProcessResult]),
							{ok, ProcessResult};
						Other ->
                            Other	
					end;		
			OO ->
                    {error, 'get_process_error'}		
		end.

getwin32_cpuUsed(Mach,Index) ->
        io:format("getwin32_cpuUsedIndex:~p~n",[Index]),
		Session = constructSnmpSession(Mach),
		case Session:g(?SYSOBJECTID) of
		    {ok, {noError, _, [Vbs=#varbind{}]}, _} ->
				SOid = Vbs#varbind.value,
				io:format("process SOid:~p~n",[SOid]),
				    case snmp_machine_config:readSysOidConfig(SOid) of
						{ok, Param} ->
							io:format("Param:~p~n",[{ok, Param}]),
							{?CPUUSED,Cpus} =
							case lists:keysearch(?CPUUSED, 1, Param) of
								{value, {?CPUUSED, Processsts}} ->
									Statusname = Processsts++[list_to_integer(Index)],
									io:format("Statusname:~p~n",[Statusname]),
									{?CPUUSED, format_result(Session:gt(Statusname))};
								_ ->
									{?CPUUSED, -1}
							end,
							io:format("Cpus:~p~n",[Cpus]),
							Result = hd(Cpus),
							CpuResult = Result#varbind.value,
							io:format("CpuResult:~p~n",[CpuResult]),
							{ok, CpuResult};
						Other ->
                            Other	
					end;		
			OO ->
                    {error, 'get_process_error'}		
		end.			
	

	
	

	
