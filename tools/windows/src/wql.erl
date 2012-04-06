-module(wql).
-compile(export_all).
-include("../include/define.hrl").

string([?CPU])->
	{?CPU, [], "SELECT DeviceID, LoadPercentage FROM Win32_Processor"};
string([?MEMORY])->
	{?MEMORY, [?MEMORY], "SELECT FreePhysicalMemory, FreeVirtualMemory, TotalVirtualMemorySize, TotalVisibleMemorySize FROM CIM_OperatingSystem"};
string([?DISK])->
	{?DISK, [], "SELECT DeviceID FROM Win32_LogicalDisk WHERE MediaType=12"};
string([?DISK, Disk])->
	{?DISK, [?DISK], "SELECT FreeSpace, Size FROM Win32_LogicalDisk WHERE MediaType=12 and DeviceID='" ++ Disk ++ "'"};
string([?SERVICE])->
	{?SERVICE, [], "SELECT DisplayName FROM Win32_Service"};
string([?SERVICE, Service])->
	{?SERVICE, [Service], "SELECT ProcessId, State, Status FROM Win32_Service WHERE DisplayName='" ++ Service ++ "'"};
string([?PROCESS])->
	{?PROCESS, [], "SELECT Name FROM Win32_PerfRawData_PerfProc_Process WHERE Name<>'_Total'"};
string([?PROCESS, Process])->
	{?PROCESS, [Process], "SELECT PercentProcessorTime, PrivateBytes, ThreadCount, WorkingSet FROM Win32_PerfRawData_PerfProc_Process WHERE Name='" ++ Process ++ "'"};
string([?NETWORK])->
	{?NETWORK, [], "SELECT Name FROM Win32_PerfRawData_Tcpip_NetworkInterface"};
string([?NETWORK, Network])->
	{?NETWORK, [Network], "SELECT BytesReceivedPerSec, BytesSentPerSec, Frequency_PerfTime, PacketsOutboundErrors, PacketsReceivedErrors, Timestamp_PerfTime FROM Win32_PerfRawData_Tcpip_NetworkInterface WHERE Name='" ++ Network ++ "'"};
string([?DIRECTORY, Path, Recursive, Match])->
	{?DIRECTORY, [Path, Recursive, Match], "SELECT name FROM Win32_Directory WHERE Name='" ++ path_process(Path) ++ "'"};
string(_)->
	{?DISK, [], "SELECT DeviceID FROM Win32_LogicalDisk WHERE MediaType=12"}.
	
path_process(Path)->
	Temp = string:strip(Path, right, $\\),
	case length(Temp) of
		2->
			Temp ++ "\\\\\\\\";
		_ ->
			path_process(Temp, [])
	end.

path_process([], Acc)->
	lists:reverse(Acc);
path_process([$\\|Rest], Acc)->
	path_process(Rest, [$\\, $\\|Acc]);
path_process([C|Rest], Acc)->
	path_process(Rest, [C|Acc]).
	
string(?MEMORY, _)->
	"SELECT Frequency_PerfTime, PagesPersec, Timestamp_PerfTime FROM Win32_PerfRawData_PerfOS_Memory";
string(?SERVICE, ProcessId)->
	"SELECT Name FROM Win32_PerfRawData_PerfProc_Process where IDProcess=" ++ ProcessId.