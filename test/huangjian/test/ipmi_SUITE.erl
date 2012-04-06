-module(ipmi_SUITE).

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
    [correct_counter, correct_value, 
	error_counter_server, error_counter_user,
	error_counter_passwd, eorror_counter_port,
	error_value_server, error_value_user,
	error_value_passwd, error_value_port].

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
correct_counter(_Config) ->
	P = ipmi_monitor:new(),
	Params = [{timeout, 30}, {usr, "root"}, {pwd, "root"}, {port, 623}, {server, "192.168.3.2"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
correct_value(_Config) ->
	P = ipmi_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(usr, "root"),
	P:set_property(pwd, "root"),
	P:set_property(port, "623"),
	P:set_property(server, "192.168.3.2"),
	P:set_property(browse, [{"ChassisPowerOn", "Chassis/Power On"}, {"ChassisPowerOverload", "Chassis/Power Overload"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_counter_server(_Config) -> 
	P = ipmi_monitor:new(),
	Params = [{timeout, 30}, {usr, "root"}, {pwd, "root"}, {port, 623}, {server, "192.168.3.3"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
 	
error_counter_user(_Config) ->
	P = ipmi_monitor:new(),
	Params = [{timeout, 30}, {usr, "roo"}, {pwd, "root"}, {port, 623}, {server, "192.168.3.2"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
 	
error_counter_passwd(_Config) ->
	P = ipmi_monitor:new(),
	Params = [{timeout, 30}, {usr, "root"}, {pwd, "roo"}, {port, 623}, {server, "192.168.3.2"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
 	
eorror_counter_port(_Config) ->	
	P = ipmi_monitor:new(),
	Params = [{timeout, 30}, {usr, "root"}, {pwd, "root"}, {port, 62}, {server, "192.168.3.2"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_value_server(_Config) ->
	P = ipmi_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(usr, "root"),
	P:set_property(pwd, "root"),
	P:set_property(port, "623"),
	P:set_property(server, "192.168.3.3"),
	P:set_property(browse, [{"ChassisPowerOn", "Chassis/Power On"}, {"ChassisPowerOverload", "Chassis/Power Overload"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_user(_Config) ->
	P = ipmi_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(usr, "roo"),
	P:set_property(pwd, "root"),
	P:set_property(port, "623"),
	P:set_property(server, "192.168.3.2"),
	P:set_property(browse, [{"ChassisPowerOn", "Chassis/Power On"}, {"ChassisPowerOverload", "Chassis/Power Overload"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_passwd(_Config) ->
	P = ipmi_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(usr, "root"),
	P:set_property(pwd, "roo"),
	P:set_property(port, "623"),
	P:set_property(server, "192.168.3.2"),
	P:set_property(browse, [{"ChassisPowerOn", "Chassis/Power On"}, {"ChassisPowerOverload", "Chassis/Power Overload"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_port(_Config) ->
	P = ipmi_monitor:new(),
	P:set_property(timeout, 30),
	P:set_property(usr, "root"),
	P:set_property(pwd, "root"),
	P:set_property(port, "62"),
	P:set_property(server, "192.168.3.2"),
	P:set_property(browse, [{"ChassisPowerOn", "Chassis/Power On"}, {"ChassisPowerOverload", "Chassis/Power Overload"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
