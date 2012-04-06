-module(test).
-compile(export_all).


%%-record(state, {host,connection, prompt, channel, replyto, received, command,eof}).

test(disk)->
	wmic:start(),
	io:format("~p~n",[wmi:disk(1, "192.168.0.185","administrator","888888")]);
test(cpu)->
	wmic:start(),
	io:format("~p~n",[wmi:cpu(1, "192.168.0.185","administrator","888888")]);
test(memory)->
	wmic:start(),
	io:format("~p~n",[wmi:memory(1, "192.168.0.185","administrator","888888")]);
test(process)->
	wmic:start(),
	io:format("~p~n",[wmi:process(1, "192.168.0.185","administrator","888888")]);
test(service)->
	wmic:start(),
	io:format("~p~n",[wmi:service(1, "192.168.0.185","administrator","888888")]);
test(directory)->
	wmic:start(),
	io:format("~p~n",[wmi:directory(1, "192.168.0.185","administrator","888888","E:\\", "false", "")]).


test(disk, Disk)->
	wmic:start(),
	io:format("~p~n",[wmi:disk(1, "192.168.0.185","administrator","888888", Disk)]);
test(process, Process)->
	wmic:start(),
	io:format("~p~n",[wmi:process(1, "192.168.0.185","administrator","888888", Process)]);
test(service, Service)->
	wmic:start(),
	io:format("~p~n",[wmi:service(1, "192.168.0.185","administrator","888888", Service)]).
 
start() -> test().  
stop() -> ok.

test()->
    erlang:set_cookie(node(),'3ren'),
    wmic:start().
    
t()->   
    wmic:start(),
    io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","SELECT Name FROM Win32_PerfRawData_Tcpip_NetworkInterface")]),
    io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","SELECT BytesReceivedPerSec, BytesSentPerSec, Frequency_PerfTime, PacketsOutboundErrors, PacketsReceivedErrors, Timestamp_PerfTime FROM Win32_PerfRawData_Tcpip_NetworkInterface")]),
    ok.    
    
test1()->
    wmic:start(),
    %~ io:format("~p~n",[wmic:wmic("192.168.4.240","ClubsAdmin","clubs","SELECT * FROM Win32_DiskDrive")]),
    %~ io:format("~p~n",[wmic:wmic("local","ClubsAdmin","clubs","SELECT * FROM Win32_DiskDrive")]),
    %~ io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888",["select * from Win32_LogicalMemoryConfiguration",
    %~ "select * from CIM_OperatingSystem","select * from Win32_PerfRawData_PerfOS_Memory"])]),
      io:format("192.168.0.185:~p~n",[erlwmi:cpu("192.168.0.185","administrator","888888")]),		       
      io:format("~p~n",[erlwmi:disk("192.168.0.185","administrator","888888")]),     
      io:format("~p~n",[erlwmi:memory("192.168.0.185","administrator","888888")]), 
      io:format("~p~n",[erlwmi:process("192.168.0.185","administrator","888888")]), 
      io:format("~p~n",[erlwmi:service("192.168.0.185","administrator","888888")]), 
      io:format("~p~n",[erlwmi:directory("192.168.0.185","administrator","888888","E:\\")]), 
    
      io:format("localhost:~p~n",[erlwmi:cpu("localhost","administrator","888888")]),
      io:format("localhost:~p~n",[erlwmi:disk("localhost","administrator","888888")]),     
      io:format("localhost:~p~n",[erlwmi:memory("localhost","administrator","888888")]).
      
    %%io:format("~p~n",[erlwmi:subdirectory("192.168.0.185","administrator","888888","C:\\Inetpub")]), 
    %%io:format("~p~n",[erlwmi:subdirectory("192.168.0.185","administrator","888888","F:\3ren\ThreeRen")]), 
    %%io:format("~p~n",[erlwmi:subdirectory("192.168.0.185","administrator","888888","C:\\Inetpub\\wwwroot\\aspnet_client\\system_web\\2_0_50727\\CrystalReportWebFormViewer3")]), 
    
    
    %%erlwmi:sleep(2000),
    %%test().
     
    %%io:format("~p~n",[#state{received=[], command=[]}]),
%%io:format("~p~n",[wmic:wmic("192.168.0.192","administrator","888888","SELECT * FROM Win32_PerfRawData_PerfOS_Processor")]).
    %~ io:format("~p~n",[wmic:wmic("192.168.0.192","administrator","888888","SELECT * FROM Win32_DiskDrive")]).
     %%io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888",["select * from Win32_BaseBoard","select * from Win32_DiskDrive"])]),
   %% io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","select * from Win32_DiskDrive")]),
   %% io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","select * from CIM_DataFile where Drive= 'C:' and Path = '\\\\' and FileName = 'ntdetect'")]),
   %% ok.
   