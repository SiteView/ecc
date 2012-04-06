-module(test_win).
-compile(export_all).

start() -> test().
stop() -> ok.

test() ->
	%%erlang:set_cookie(node(),'3ren'),
	%%wmic:start(),
	      %%memory
	%%io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","select CurrentClockSpeed from Win32_OperatingSystem")]).
	%%io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","select * from Win32_Processor")]),
	%%io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","select * from Win32_PerfRawData_PerfOS_System")]).
	%%io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","select * from Win32_PhysicalMemory")]),
	%%io:format("~p~n",[wmic:wmic("192.168.0.185","administrator","888888","select * from Win32_OperatingSystem")]).
	
	%%cpu  NumberOfProcessors, Win32_ComputerSystem
	%%network NumberOfUsers,NumberOfLicensedUsers      Win32_OperatingSystem
	%%paging files SizeStoredInPagingFiles,FreeSpaceInPagingFiles	Win32_OperatingSystem
	%%Virtual Memory TotalVirtualMemorySize,FreeVirtualMemory    Win32_OperatingSystem
	%%Physical Memory TotalVisibleMemorySize,FreePhysicalMemory    Win32_OperatingSystem
	
	%%agent_win:open("192.168.0.185","administrator","888888"),
	%%io:format("diskspace:~p~n~n",[proxy_win:diskspace("192.168.0.185")]),
	%%io:format("os:~p~n~n",[proxy_win:os("192.168.0.185")]),
	%%io:format("memory:~p~n~n",[proxy_win:memory("192.168.0.185")]).
	
	
	
	
	
	agent_win:start(),
	%%For windows2003
	agent_win:open("192.168.0.185",23,"administrator","888888",win),
	io:format("os:~p~n~n~n",[proxy_win:os("192.168.0.185")]),
	io:format("cpu:~p~n~n~n",[proxy_win:cpu("192.168.0.185")]),
	io:format("uptime:~p~n~n~n",[proxy_win:uptime("192.168.0.185")]),
	io:format("processes:~p~n~n~n",[proxy_win:processes_count("192.168.0.185")]),
	io:format("physical_memory:~p~n~n~n",[proxy_win:physical_memory("192.168.0.185")]),
	io:format("virtual_memory:~p~n~n~n",[proxy_win:virtual_memory("192.168.0.185")]),
	io:format("paging_files:~p~n~n~n",[proxy_win:paging_files("192.168.0.185")]),
	io:format("diskspace:~p~n~n~n",[proxy_win:diskspace("192.168.0.185")]),
	
	
	%%For windows xp
	agent_win:open("192.168.9.36",23,"administrator","menkin",win),
	io:format("os:~p~n~n~n",[proxy_win:os("192.168.9.36")]),
	io:format("cpu:~p~n~n~n",[proxy_win:cpu("192.168.9.36")]),
	io:format("uptime:~p~n~n~n",[proxy_win:uptime("192.168.9.36")]),
	io:format("processes:~p~n~n~n",[proxy_win:processes_count("192.168.9.36")]),
	io:format("physical_memory:~p~n~n~n",[proxy_win:physical_memory("192.168.9.36")]),
	io:format("virtual_memory:~p~n~n~n",[proxy_win:virtual_memory("192.168.9.36")]),
	io:format("paging_files:~p~n~n~n",[proxy_win:paging_files("192.168.9.36")]),
	io:format("diskspace:~p~n~n~n",[proxy_win:diskspace("192.168.9.36")]).
	
	
	
	
	
	
	%%agent:start(),
	%%agent:open("192.168.0.68", 23, "root", "rootroot",telnet).
	%%io:format("open:~p~n",[agent:open("192.168.0.68", 23, "root", "rootroot",telnet)]), 
	%%io:format("open:~p~n",[agent:open("192.168.0.185", 23, "administrator", "888888",telnet)]).