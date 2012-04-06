%%%http://localhost:8080/ecc/nnmservices/nnmapi/getRealData?arg0=aix&arg1=system_info&arg2=id&arg3=1295:853704:301310
%%%http://localhost:8080/ecc/nnmservices/nnmapi/getRealData?arg0=aix&arg1=disk_info,disk_info.disk_0,disk_info.disk_1,disk_info.disk_2,disk_info.disk_3,disk_info.disk_writes,disk_info.disk_reads&arg2=id&arg3=1296:202803:385141
%~ var types1:String = "system_info,cpu_info,cpu_info.total,cpu_info.user_percent,cpu_info.system_percent,cpu_info.wait_percent,cpu_info.queue_length,cpu_info.processes_total,cpu_info.processes_zombies,cpu_info.processes_blocked";
%~ var types2:String = "memory_info,memory_info.page_in,memory_info.page_out,memory_info.ram,memory_info.vm,memory_info.processes_swapped";
%~ var types3:String = "disk_info,disk_info.disk_0,disk_info.disk_1,disk_info.disk_2,disk_info.disk_3,disk_info.disk_writes,disk_info.disk_reads";
%~ var types4:String = "swap_info,swap_info.swap_writes,swap_info.swap_reads";
%~ var types5:String = "network_info,network_info.pkts_in,network_info.pkts_out,network_info.errors_out,network_info.errors_in,network_info.logins";

%~ getRealData?arg0=%s&arg1=%s&arg2=%s&arg3=%s", "aix", types1, "id", Main.id)


-module(test).
-compile(export_all).

start() -> test().  
stop() -> ok.

regex(Pattern,Data) ->
    regex:start(),
    regex:regex_str(Pattern,Data).


test()->
    erlang:set_cookie(node(),'3ren'),
    %~ wmic:start(),
    
    %~ io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","SELECT * FROM Win32_DiskDrive")]),
    %~ io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888",["select * from Win32_LogicalMemoryConfiguration",
    %~ "select * from CIM_OperatingSystem","select * from Win32_PerfRawData_PerfOS_Memory"])]),
    
    %~ sysmon:start(),
    
    
    agent:start(),
     
 
    statistics(wall_clock),
   
    %~ io:format("aix:~p~n",[agent:open("192.168.0.68", 23, "root", "rootroot",telnet)]), 
    %~ io:format("aix:~p~n",[proxy:os("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:datetime("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:uptime("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:processes_count("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:logins_count("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:established("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:diskspace("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:cpu("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:memory("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:iostat("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:network("192.168.0.68")]),
    %~ io:format("aix:~p~n",[proxy:swap("192.168.0.68")]),
    
    %~ io:format("aix:~p~n",[agent:open("58.20.43.138", 2233, "siteview", "dragonflow",telnet)]), 
    %~ io:format("aix:~p~n",[proxy:os("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:datetime("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:uptime("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:processes_count("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:logins_count("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:established("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:diskspace("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:cpu("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:memory("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:iostat("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:network("58.20.43.138")]),
    %~ io:format("aix:~p~n",[proxy:swap("58.20.43.138")]),
    
    
    {_, AixTime} = statistics(wall_clock),
    io:format("Aix[~pms]~n", [AixTime]),
    
    io:format("redhat5:~p~n",[agent:open("192.168.0.225", 22, "root", "siteview800903",ssh)]),
    io:format("redhat5:~p~n",[proxy:os("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:datetime("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:uptime("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:processes_count("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:logins_count("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:established("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:diskspace("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:cpu("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:memory("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:iostat("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:network("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:swap("192.168.0.225")]),
    io:format("redhat5:~p~n",[proxy:page("192.168.0.225")]),

    %~ io:format("centos5:~p~n",[agent:open("192.168.0.118", 23, "root", "siteview123",telnet)]), 
    %~ io:format("centos5:~p~n",[proxy:os("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:datetime("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:uptime("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:processes_count("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:logins_count("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:established("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:diskspace("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:cpu("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:memory("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:iostat("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:network("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:swap("192.168.0.118")]),
    %~ io:format("centos5:~p~n",[proxy:page("192.168.0.118")]),
    
    %~ {_, Time} = statistics(wall_clock),
    %~ io:format("centos5[~pms]~n", [Time]), 
    
     %~ io:format("Redhat4:~p~n",[agent:open("192.168.0.43", 22, "root", "rootroot",ssh)]), 
    %~ io:format("Redhat4:~p~n",[proxy:os("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:datetime("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:uptime("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:processes_count("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:logins_count("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:established("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:diskspace("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:cpu("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:memory("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:iostat("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:network("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:swap("192.168.0.43")]),
    %~ io:format("Redhat4:~p~n",[proxy:page("192.168.0.43")]),
    
     %~ {_, Redhat4Time} = statistics(wall_clock),
    %~ io:format("Redhat4[~pms]~n", [Redhat4Time]),
    
    
     %~ io:format("Sun Solaris:~p~n",[agent:open("192.168.0.162", 22, "root", "root",ssh)]), 
    %~ io:format("Sun Solaris:~p~n",[proxy:os("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:datetime("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:uptime("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:processes_count("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:logins_count("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:established("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:diskspace("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:cpu("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:memory("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:iostat("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:network("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:swap("192.168.0.162")]),
    %~ io:format("Sun Solaris:~p~n",[proxy:page("192.168.0.162")]),
    
    
    
     %~ io:format("SUSE Linux11:~p~n",[agent:open("192.168.0.46", 22, "root", "rootroot",ssh)]), 
    %~ io:format("SUSE Linux11:~p~n",[proxy:os("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:datetime("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:uptime("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:processes_count("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:logins_count("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:established("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:diskspace("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:cpu("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:memory("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:iostat("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:network("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:swap("192.168.0.46")]),
    %~ io:format("SUSE Linux11:~p~n",[proxy:page("192.168.0.46")]),
    
    %~ io:format("SUSE Linux10:~p~n",[agent:open("192.168.0.23", 22, "root", "rootroot",ssh)]), 
    %~ io:format("SUSE Linux10:~p~n",[proxy:os("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:datetime("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:uptime("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:processes_count("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:logins_count("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:established("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:diskspace("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:cpu("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:memory("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:iostat("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:network("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:swap("192.168.0.23")]),
    %~ io:format("SUSE Linux10:~p~n",[proxy:page("192.168.0.23")]),
    
    
    %~ io:format("Ubuntu10:~p~n",[agent:open("192.168.0.48", 22, "root", "rootroot",ssh)]), 
    %~ io:format("Ubuntu10:~p~n",[proxy:os("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:datetime("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:uptime("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:processes_count("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:logins_count("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:established("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:diskspace("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:cpu("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:memory("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:iostat("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:network("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:swap("192.168.0.48")]),
    %~ io:format("Ubuntu10:~p~n",[proxy:page("192.168.0.48")]),
    
    %~ io:format("FreeBSD:~p~n",[agent:open("192.168.0.41", 22, "root", "rootroot",ssh)]), 
    %~ io:format("FreeBSD:~p~n",[proxy:os("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:datetime("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:uptime("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:processes_count("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:logins_count("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:established("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:diskspace("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:cpu("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:memory("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:iostat("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:network("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:swap("192.168.0.41")]),
    %~ io:format("FreeBSD:~p~n",[proxy:page("192.168.0.41")]),
    %~ {_, FreeBSDTime} = statistics(wall_clock),
    %~ io:format("FreeBSD[~pms]~n", [FreeBSDTime]), 
    
    
    %~ io:format("HP-UX:~p~n",[agent:open("192.168.0.69", 22, "root", "rootroot",ssh)]), 
    %~ io:format("HP-UX:~p~n",[proxy:os("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:datetime("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:uptime("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:processes_count("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:logins_count("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:established("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:diskspace("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:cpu("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:memory("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:iostat("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:network("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:swap("192.168.0.69")]),
    %~ io:format("HP-UX:~p~n",[proxy:page("192.168.0.69")]),
    %~ {_, HP_UXTime} = statistics(wall_clock),
    %~ io:format("HP-UX[~pms]~n", [HP_UXTime]), 
    
    ok.
    
  