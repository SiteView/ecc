-module(dhcp_SUITE).

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
    [correct_no_addrss, correct_have_addrss,
	error_address].

%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[].
	
correct_no_addrss(_Config) ->	
	P = dhcp_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(clientaddress, ""),	

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_have_addrss(_Config) ->	
	P = dhcp_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(clientaddress, "192.168.6.208"),	

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
error_address(_Config) ->
	P = dhcp_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(clientaddress, "192.168.6.209"),	

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		