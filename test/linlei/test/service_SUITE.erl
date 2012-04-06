-module(service_SUITE).

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
    [windows_host_rigth,redhat_linux_host_rigth,no_host,error_host].

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


get_windows_service(_Config) ->
    P = service_monitor:new(),
    P:set_property(machine,"\\\\192.168.0.52"),
	P:set_property(service,"HTTP"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

get_windows_process(_Config) ->
    P = service_monitor:new(),
    P:set_property(machine,"\\\\192.168.0.52"),
	P:set_property(process,"System"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
    
get_unix_process(_Config) ->
    P = service_monitor:new(),
    P:set_property(machine,"\\\\192.168.0.181"),
	P:set_property(process,"init [3]"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

get_unix_process_memory_true(_Config) ->
    P = service_monitor:new(),
    P:set_property(machine,"\\\\192.168.0.181"),
	P:set_property(process,"init [3]"),
    P:set_property(checkMemory,true),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
    
    
get_unix_process_memory_false(_Config) ->
    P = service_monitor:new(),
    P:set_property(machine,"\\\\192.168.0.181"),
	P:set_property(process,"init [3]"),
    P:set_property(checkMemory,false),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

get_windows_service_error(_Config) -> 
    P = service_monitor:new(),
    P:set_property(,machine"\\\\192.168.0.52"),
	P:set_property(service,"test_error"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

get_windows_process_error(_Config) -> 
    P = service_monitor:new(),
    P:set_property(,machine"\\\\192.168.0.52"),
	P:set_property(process,"test_error"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
    
get_unix_process_error(_Config) -> 
    P = service_monitor:new(),
    P:set_property(,machine"\\\\192.168.0.181"),
	P:set_property(process,"test_error"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
    
get_unix_process_monitory_error(_Config) -> 
    P = service_monitor:new(),
    P:set_property(,machine"\\\\192.168.0.181"),
	P:set_property(process,"test_error"),
    P:set_property(checkMemory,true),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.    
    
    


    
    

