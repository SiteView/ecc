-module(disk_monitor_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%%--------------------------------------------------------------------
%% Test server callback functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> DefaultData
%% DefaultData: [tuple()]  
%% Description: Require variables and set default values for the suite
%%--------------------------------------------------------------------
suite() -> [{timetrap,{minutes,1}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config: [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config: [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.
%%--------------------------------------------------------------------
%% Function: all() -> TestCases
%% TestCases: [Case] 
%% Case: atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------      
all() -> 
    [disk_machine_error,disk_disk_error,disk,disk_linux_machine_error,disk_linux_disk_error,disk_linux].

%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[].
	
%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
disk_machine_error(_Config) ->
	ok = ct:require({disk_monitor, machine}),
	ok = ct:require({disk_monitor, disk}),
	ok = ct:require({disk_monitor, result}),

	P = diskspace_monitor:new(),
	P:set_property(machine, "errormachine"),
	P:set_property(disk, ct:get_config({disk_monitor,disk}, "")),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),

	P:update(),
	
	file:set_cwd(Cwd),
	
	Good = ct:get_config({disk_monitor, result}, ""),
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment,Ret}.
	
disk_disk_error(_Config) ->
	ok = ct:require({disk_monitor, machine}),
	ok = ct:require({disk_monitor, disk}),
	ok = ct:require({disk_monitor, result}),

	P = diskspace_monitor:new(),
	P:set_property(machine, ct:get_config({disk_monitor,machine}, "")),
	P:set_property(disk, "z"),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),

	P:update(),
	
	file:set_cwd(Cwd),
	
	Good = ct:get_config({disk_monitor, result}, ""),
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment,Ret}.
	
disk(_Config) ->
	ok = ct:require({disk_monitor, machine}),
	ok = ct:require({disk_monitor, disk}),
	ok = ct:require({disk_monitor, result}),

	P = diskspace_monitor:new(),
	P:set_property(machine, ct:get_config({disk_monitor,machine}, "")),
	P:set_property(disk, ct:get_config({disk_monitor,disk}, "")),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),

	P:update(),
	
	file:set_cwd(Cwd),
	
	Good = ct:get_config({disk_monitor, result}, ""),
	{ok, {_, Ret}} = P:get_attribute(percentFull),
	true =  (Ret > Good),
	{comment,Ret}.

disk_linux_machine_error(_Config) ->
	ok = ct:require({disk_monitor_linux, machine}),
	ok = ct:require({disk_monitor_linux, disk}),
	ok = ct:require({disk_monitor_linux, result}),

	P = diskspace_monitor:new(),
	P:set_property(machine, "error_machine"),
	P:set_property(disk, ct:get_config({disk_monitor_linux,disk}, "")),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),

	P:update(),
	
	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment,Ret}.

disk_linux_disk_error(_Config) ->
	ok = ct:require({disk_monitor_linux, machine}),
	ok = ct:require({disk_monitor_linux, disk}),
	ok = ct:require({disk_monitor_linux, result}),

	P = diskspace_monitor:new(),
	P:set_property(machine, ct:get_config({disk_monitor_linux,machine}, "")),
	P:set_property(disk, "error_disk"),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),

	P:update(),
	
	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(percentFull),
	{comment,Ret}.
	
disk_linux(_Config) ->
	ok = ct:require({disk_monitor_linux, machine}),
	ok = ct:require({disk_monitor_linux, disk}),
	ok = ct:require({disk_monitor_linux, result}),

	P = diskspace_monitor:new(),
	P:set_property(machine, ct:get_config({disk_monitor_linux,machine}, "")),
	P:set_property(disk, ct:get_config({disk_monitor_linux,disk}, "")),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),

	P:update(),
	
	file:set_cwd(Cwd),
	
	Good = ct:get_config({disk_monitor_linux, result}, 0),
	{ok, {_, Ret}} = P:get_attribute(percentFull),
	true =  (Ret > Good),
	{comment,Ret}.