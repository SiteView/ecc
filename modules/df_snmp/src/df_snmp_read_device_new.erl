-module(df_snmp_read_device_new).
-compile(export_all).

-include_lib("df_snmp_record.hrl").
-include_lib("snmp/include/snmp_types.hrl").

get_Device_By_Snmp(IPList,CommunityList,Timeout,Port,Retry) ->
	SP_List = lists:map(fun(IP) -> 
								#snmpPara{
										   server = IP, 
										   port = Port, 
										   community = "public2", 
										   timeout = Timeout, 
										   retry = Retry}
						end, IPList),
	io:format("SP_List:~p~n",[SP_List]),

	SysObjectID = [[1,3,6,1,2,1,1,2]],
	SysDescrID = [[1,3,6,1,2,1,1,1]],
	snmp_ex2_manager:start_link(),
%%	SysObjects = df_snmp_agn:main(SP_List, SysObjectID, Timeout),
%%	SysDescrs = df_snmp_agn:main(SP_List, SysDescrID, Timeout),
	SysObjects = df_snmp_info:get_object_next_async(IPList,CommunityList,Timeout,Port,Retry,SysObjectID),
	SysDescrs = df_snmp_info:get_object_next_async(IPList,CommunityList,Timeout,Port,Retry,SysDescrID),
	io:format("SysObject:~p~n", [SysObjects]),
	io:format("SysDescr:~p~n", [SysDescrs]),
	DeviceList = tranToDevice(SP_List, SysObjects, SysDescrs),
	{ok,Enterprises} = file:consult("conf/enterprises.dat"),
	{ok,Devicetype} = file:consult("conf/devicetype.dat"),
	Device = getDeviceData(DeviceList, Devicetype),
	io:format("Device:~p~n",[Device]),
	DeviceResult = getDeviceData2(Device, Enterprises),
	io:format("Deviceresult:~p~n", [DeviceResult]),
	DeviceResult2 = getDeviceData3(DeviceResult, Devicetype),
	io:format("DeviceResult2:~p~n", [DeviceResult2]),
	DeviceResult2.

%%result
%%SysObject:[{{snmpPara,{192,168,0,254},161,"public1",10000,2},
%%            [{[1,3,6,1,2,1,1,2,0],[1,3,6,1,4,1,2011,2,23,21]}]},
%%           {{snmpPara,{192,168,0,251},161,"public1",10000,2},
%%            [{[1,3,6,1,2,1,1,2,0],[1,3,6,1,4,1,9,1,324]}]}]
%%SysDescr:[{{snmpPara,{192,168,0,254},161,"public1",10000,2},
%%           [{[1,3,6,1,2,1,1,1,0],
%%             "Huawei Versatile Routing Platform Software, Software Version 3.10,
%% Release 0042\r\nQuidway S3526E\r\nCopyright (c) 1998-2007 Huawei Technologies C
%%o., Ltd. All rights reserved.\r\n"}]},
%%          {{snmpPara,{192,168,0,251},161,"public1",10000,2},
%%           [{[1,3,6,1,2,1,1,1,0],
%%             "Cisco Internetwork Operating System Software \r\nIOS (tm) C2950 So
%%ftware (C2950-I6Q4L2-M), Version 12.1(22)EA6, RELEASE SOFTWARE (fc1)\r\nCopyrigh
%%t (c) 1986-2005 by cisco Systems, Inc.\r\nCompiled Fri 21-Oct-05 01:59 by yenanh
%%"}]}]


			
%%[{[IP],Community,Sysobject,Type,TypeName,Model,Enterprise,SysDescr,Timeout,Port,Retry}|T]
tranToDevice([], _, _) -> [];
tranToDevice([SP|SP_List], SysObjects, SysDescrs) ->
	{Community,ResultObject} = findDeviceObject(SP, SysObjects),
	ResultDescr = findDeviceDescr(SP, SysDescrs),
	[{SP#snmpPara.server,Community,ResultObject,"","","","",ResultDescr,SP#snmpPara.timeout,SP#snmpPara.port,SP#snmpPara.retry} | tranToDevice(SP_List, SysObjects, SysDescrs)].

findDeviceObject(_, []) -> [];
findDeviceObject(SP, [H|Sysobjects]) ->
	{{IP,Community,_,_,_},[{_,Value}]} = H,
	case SP#snmpPara.server =:= IP of
		true ->
			{Community,Value};
		false ->
			findDeviceObject(SP, Sysobjects)
	end.
		
findDeviceDescr(_, []) -> "";
findDeviceDescr(SP, [H|SysDescrs]) ->
	{{IP,_,_,_,_},[{_,Value}]} = H,
	case SP#snmpPara.server =:= IP of
		true ->
			Value;
		false ->
			findDeviceDescr(SP, SysDescrs)
	end.

getDeviceData([], _) -> [];
getDeviceData([H|DeviceList], DeviceType) ->
	{_,_,A,_,_,_,_,_,_,_,_} = H,
	case A of
		[] ->
			[H | getDeviceData(DeviceList, DeviceType)];
		_ ->
			[getDeviceType(H, DeviceType) | getDeviceData(DeviceList, DeviceType)]
	end.

getDeviceType(Device, []) -> Device;
getDeviceType(Device, [H|DeviceType]) ->
	{A,B,C,D,E} = H,
	{IP,Community,SysObject,_,_,_,_,SysDescr,Timeout,Port,Retry} = Device,
	case SysObject =:= A of
		true ->
			{IP,Community,SysObject,B,C,D,E,SysDescr,Timeout,Port,Retry};
		false ->
			getDeviceType(Device, DeviceType)
	end.

getDeviceEnterprise(Device, []) -> Device;
getDeviceEnterprise(Device, [H|Enterprises]) ->
	{A,B} = H,
	{IP,Community,SysObject,Type,TypeName,Model,_,SysDescr,Timeout,Port,Retry} = Device,
	case lists:sublist(SysObject, 7) =:= A of
		true ->
			{IP,Community,SysObject,Type,TypeName,Model,B,SysDescr,Timeout,Port,Retry};
		false ->
			getDeviceEnterprise(Device, Enterprises)
	end.

getDeviceData2([], _) -> [];
getDeviceData2([H|Device], Enterprises) ->
	{_,_,_,_,_,_,A,_,_,_,_} = H,
	io:format("H:~p~n", [H]),
	io:format("A:~p~n", [A]),
	case A of
		"" ->
			[getDeviceEnterprise(H, Enterprises) | getDeviceData2(Device, Enterprises)];
		_ ->
			[H | getDeviceData2(Device, Enterprises)]
	end.	
	
getDeviceData3([], _) -> [];
getDeviceData3([H|DeviceList], DeviceType) ->
	{_,_,_,_,A,_,_,B,_,_,_} = H,
	case A of
		[] ->
			case B of
				[] -> 
					[H | getDeviceData3(DeviceList, DeviceType)];
				_ ->
					[getDeviceTypeName(H, DeviceType) | getDeviceData3(DeviceList, DeviceType)]
			end;
		_ ->
			[H | getDeviceData3(DeviceList, DeviceType)]
	end.

getDeviceTypeName(Device, []) -> Device;
getDeviceTypeName(Device, [H|DeviceType]) ->
	{_,B,C,D,E} = H,
	{IP,Community,SysObject,_,_,_,Enterprise,SysDescr,Timeout,Port,Retry} = Device,
	case ((string:to_lower(E) =:= string:to_lower(Enterprise)) and (string:str(string:to_lower(SysDescr), string:to_lower(D)) > 0)) of
		true ->
			{IP,Community,SysObject,B,C,D,Enterprise,SysDescr,Timeout,Port,Retry};
		false ->
			getDeviceTypeName(Device, DeviceType)
	end.

test() ->
	get_Device_By_Snmp([{192,168,0,254},{192,168,0,97},{192,168,0,251}],["public","public1"],10000,161,2).

