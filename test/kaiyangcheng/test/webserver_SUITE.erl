-module(webserver_SUITE).

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
suite() -> [{timetrap,{minutes,5}}].

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
    [from_right_web,from_error_web,from_right_log,from_error_log].

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
from_right_web(_Config) ->
	ok = ct:require({web_server_monitor, machine}),
	ok = ct:require({web_server_monitor, right_server}),
	ok = ct:require({web_server_monitor, empty_file}),

    M = web_server_monitor:new(),

	M:set_property(machine,ct:get_config({web_server_monitor, machine}, "")),
	M:set_property(server, ct:get_config({web_server_monitor, right_server}, "")),
	M:set_property(file, ct:get_config({web_server_monitor, empty_file}, "")),
    M:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 


from_error_web(_Config) ->
	ok = ct:require({web_server_monitor, machine}),
	ok = ct:require({web_server_monitor, error_server}),
	ok = ct:require({web_server_monitor, empty_file}),
	
    M = web_server_monitor:new(),
	
	M:set_property(machine,ct:get_config({web_server_monitor, machine}, "")),
	M:set_property(server, ct:get_config({web_server_monitor, error_server}, "")),
	M:set_property(file, ct:get_config({web_server_monitor, empty_file}, "")),
    M:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}.
	
from_right_log(_Config) ->
	ok = ct:require({web_server_monitor, machine}),
	ok = ct:require({web_server_monitor, empty_server}),
	ok = ct:require({web_server_monitor, right_file}),

    M = web_server_monitor:new(),

	M:set_property(machine,ct:get_config({web_server_monitor, machine}, "")),
	M:set_property(server, ct:get_config({web_server_monitor, empty_server}, "")),
	M:set_property(file, ct:get_config({web_server_monitor, right_file}, "")),
    M:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
from_error_log(_Config) ->
	ok = ct:require({web_server_monitor, machine}),
	ok = ct:require({web_server_monitor, empty_server}),
	ok = ct:require({web_server_monitor, error_file}),

    M = web_server_monitor:new(),

	M:set_property(machine,ct:get_config({web_server_monitor, machine}, "")),
	M:set_property(server, ct:get_config({web_server_monitor, empty_server}, "")),
	M:set_property(file, ct:get_config({web_server_monitor, error_file}, "")),
    M:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 