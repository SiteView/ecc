-module(db2monitor_SUITE).

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
suite() -> [{timetrap,{minutes,10}}].

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
    [correct_counter_current, correct_counter_all, current_value,
	error_counter_server, error_counter_port, error_counter_user,
	error_counter_password, error_counter_database, error_value_server, 
	error_value_port, error_value_user, error_value_password, 
	error_value_database, error_value_counter].


%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[].
	
correct_counter_current(_Config) ->
	P = db2_monitor:new(),
	Params = [{server, "192.168.0.20"}, {port, 50000}, {user, "db2admin"}, {password, "db2admin"},	{instance, "cy"}, {partition, "-1"}, {timeout, 60}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
correct_counter_all(_Config) ->
	P = db2_monitor:new(),
	Params = [{server, "192.168.0.20"}, {port, 50000}, {user, "db2admin"}, {password, "db2admin"},	{instance, "cy"}, {partition, "-2"}, {timeout, 60}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
current_value(_Config) ->
	P = db2_monitor:new(),	
	P:set_property(server, "192.168.0.20"),
	P:set_property(user, "db2admin"),
	P:set_property(port, 50000),
	P:set_property(timeout, 60),
	P:set_property(partition, "-1"),
	P:set_property(password, "db2admin"),
	P:set_property(instance, "cy"),
	P:set_property(browse, [{"Application|||db2jcc_application|||AGENT_SYS_CPU_TIME_S","Application/db2jcc_application/AGENT_SYS_CPU_TIME_S"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_counter_server(_Config) ->
	P = db2_monitor:new(),
	Params = [{server, "192.168.0.2"}, {port, 50000}, {user, "db2admin"}, {password, "db2admin"},	{instance, "cy"}, {partition, "-1"}, {timeout, 60}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_counter_port(_Config) ->
	P = db2_monitor:new(),
	Params = [{server, "192.168.0.20"}, {port, 5000}, {user, "db2admin"}, {password, "db2admin"},	{instance, "cy"}, {partition, "-1"}, {timeout, 60}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_counter_user(_Config) ->
	P = db2_monitor:new(),
	Params = [{server, "192.168.0.20"}, {port, 50000}, {user, "db2admi"}, {password, "db2admin"},	{instance, "cy"}, {partition, "-1"}, {timeout, 60}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
	
error_counter_password(_Config) ->
	P = db2_monitor:new(),
	Params = [{server, "192.168.0.20"}, {port, 50000}, {user, "db2admin"}, {password, "db2admi"},	{instance, "cy"}, {partition, "-1"}, {timeout, 60}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_counter_database(_Config) ->
	P = db2_monitor:new(),
	Params = [{server, "192.168.0.20"}, {port, 50000}, {user, "db2admin"}, {password, "db2admin"},	{instance, "c"}, {partition, "-1"}, {timeout, 60}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_value_server(_Config) ->
	P = db2_monitor:new(),	
	P:set_property(server, "192.168.0.2"),
	P:set_property(user, "db2admin"),
	P:set_property(port, 50000),
	P:set_property(timeout, 60),
	P:set_property(partition, "-1"),
	P:set_property(password, "db2admin"),
	P:set_property(instance, "cy"),
	P:set_property(browse, [{"Application|||db2jcc_application|||AGENT_SYS_CPU_TIME_S","Application/db2jcc_application/AGENT_SYS_CPU_TIME_S"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_port(_Config) ->
	P = db2_monitor:new(),	
	P:set_property(server, "192.168.0.20"),
	P:set_property(user, "db2admin"),
	P:set_property(port, 5000),
	P:set_property(timeout, 60),
	P:set_property(partition, "-1"),
	P:set_property(password, "db2admin"),
	P:set_property(instance, "cy"),
	P:set_property(browse, [{"Application|||db2jcc_application|||AGENT_SYS_CPU_TIME_S","Application/db2jcc_application/AGENT_SYS_CPU_TIME_S"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_user(_Config) ->
	P = db2_monitor:new(),	
	P:set_property(server, "192.168.0.20"),
	P:set_property(user, "db2admi"),
	P:set_property(port, 50000),
	P:set_property(timeout, 60),
	P:set_property(partition, "-1"),
	P:set_property(password, "db2admin"),
	P:set_property(instance, "cy"),
	P:set_property(browse, [{"Application|||db2jcc_application|||AGENT_SYS_CPU_TIME_S","Application/db2jcc_application/AGENT_SYS_CPU_TIME_S"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_password(_Config) ->
	P = db2_monitor:new(),	
	P:set_property(server, "192.168.0.20"),
	P:set_property(user, "db2admin"),
	P:set_property(port, 50000),
	P:set_property(timeout, 60),
	P:set_property(partition, "-1"),
	P:set_property(password, "db2admi"),
	P:set_property(instance, "cy"),
	P:set_property(browse, [{"Application|||db2jcc_application|||AGENT_SYS_CPU_TIME_S","Application/db2jcc_application/AGENT_SYS_CPU_TIME_S"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_database(_Config) ->
	P = db2_monitor:new(),	
	P:set_property(server, "192.168.0.20"),
	P:set_property(user, "db2admin"),
	P:set_property(port, 50000),
	P:set_property(timeout, 60),
	P:set_property(partition, "-1"),
	P:set_property(password, "db2admin"),
	P:set_property(instance, "c"),
	P:set_property(browse, [{"Application|||db2jcc_application|||AGENT_SYS_CPU_TIME_S","Application/db2jcc_application/AGENT_SYS_CPU_TIME_S"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_counter(_Config) ->
	P = db2_monitor:new(),	
	P:set_property(server, "192.168.0.20"),
	P:set_property(user, "db2admin"),
	P:set_property(port, 50000),
	P:set_property(timeout, 60),
	P:set_property(partition, "-1"),
	P:set_property(password, "db2admin"),
	P:set_property(instance, "cy"),
	P:set_property(browse, [{"Application|||db2jcc_application|||AGENT_SYS_CPU_TIME","Application/db2jcc_application/AGENT_SYS_CPU_TIME"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
	