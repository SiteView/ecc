-module(diskspace_SUITE).

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

diskspace_windows(_Config) ->
    P = diskspace:new(),
    P:set_property(machine,"\\\\192.168.0.52"),
    P:set_property(disk,"C:"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

diskspace_windows_2000(_Config) ->
    P = diskspace:new(),
    P:set_property(machine,"\\\\192.168.3.30"),
    P:set_property(disk,"C:"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
    
diskspace_unix(_Config) ->
    P = diskspace:new(),
    P:set_property(machine,"\\\\192.168.0.181"),
    P:set_property(disk,"/"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.    

