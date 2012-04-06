-module(oraclejdbcmonitor_SUITE).

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
    [correct_counter, current_value,error_counter_server, 
	error_counter_port, error_counter_user, error_counter_password, 
	error_counter_database, error_counter_driver, error_value_server, 
	error_value_port, error_value_user, error_value_password, 
	error_value_database, error_value_counter, error_value_driver].
	
%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[].
	
	
correct_counter(_Config) ->	
	P = oracle_jdbc_monitor:new(),
	Params = [{connection_url, "192.168.0.61:1521/oracle10g"}, {user, "system"}, {driver, "oracle.jdbc.driver.OracleDriver"},			
			{password, "systest"},	{connection_timeout, 60}, {query_timeout, 60}, {max_counter, 10}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
current_value(_Config) ->
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.61:1521/oracle10g"),
	P:set_property(user, "system"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(password, "systest"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block writes","V$SYSSTAT/DBWR undo block writes"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_counter_server(_Config) ->	
	P = oracle_jdbc_monitor:new(),
	Params = [{connection_url, "192.168.0.60:1521/oracle10g"}, {user, "system"}, {driver, "oracle.jdbc.driver.OracleDriver"},			
			{password, "systest"},	{connection_timeout, 60}, {query_timeout, 60}, {max_counter, 10}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_port(_Config) ->	
	P = oracle_jdbc_monitor:new(),
	Params = [{connection_url, "192.168.0.61:1520/oracle10g"}, {user, "system"}, {driver, "oracle.jdbc.driver.OracleDriver"},			
			{password, "systest"},	{connection_timeout, 60}, {query_timeout, 60}, {max_counter, 10}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_user(_Config) ->	
	P = oracle_jdbc_monitor:new(),
	Params = [{connection_url, "192.168.0.61:1521/oracle10g"}, {user, "syste"}, {driver, "oracle.jdbc.driver.OracleDriver"},			
			{password, "systest"},	{connection_timeout, 60}, {query_timeout, 60}, {max_counter, 10}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_password(_Config) ->	
	P = oracle_jdbc_monitor:new(),
	Params = [{connection_url, "192.168.0.61:1521/oracle10g"}, {user, "system"}, {driver, "oracle.jdbc.driver.OracleDriver"},			
			{password, "systes"},	{connection_timeout, 60}, {query_timeout, 60}, {max_counter, 10}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	 
error_counter_database(_Config) ->	
	P = oracle_jdbc_monitor:new(),
	Params = [{connection_url, "192.168.0.61:1521/oracle10"}, {user, "system"}, {driver, "oracle.jdbc.driver.OracleDriver"},			
			{password, "systest"},	{connection_timeout, 60}, {query_timeout, 60}, {max_counter, 10}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_driver(_Config) ->	
	P = oracle_jdbc_monitor:new(),
	Params = [{connection_url, "192.168.0.61:1521/oracle10g"}, {user, "system"}, {driver, "oracle.jdbc.driver.OracleDrive"},			
			{password, "systest"},	{connection_timeout, 60}, {query_timeout, 60}, {max_counter, 10}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
		
error_value_server(_Config) ->	
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.60:1521/oracle10g"),
	P:set_property(user, "system"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(password, "systest"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block writes","V$SYSSTAT/DBWR undo block writes"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
	
error_value_port(_Config) ->	
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.61:1520/oracle10g"),
	P:set_property(user, "system"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(password, "systest"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block writes","V$SYSSTAT/DBWR undo block writes"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_user(_Config) ->
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.61:1521/oracle10g"),
	P:set_property(user, "syste"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(password, "systest"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block writes","V$SYSSTAT/DBWR undo block writes"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_password(_Config) ->
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.61:1521/oracle10g"),
	P:set_property(user, "system"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(password, "systes"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block writes","V$SYSSTAT/DBWR undo block writes"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	 
error_value_database(_Config) ->
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.61:1521/oracle10"),
	P:set_property(user, "system"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(password, "systest"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block writes","V$SYSSTAT/DBWR undo block writes"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_counter(_Config) ->
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.61:1521/oracle10g"),
	P:set_property(user, "system"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(password, "systest"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block write","V$SYSSTAT/DBWR undo block write"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
error_value_driver(_Config) ->	
	P = oracle_jdbc_monitor:new(),	
	P:set_property(connection_url, "192.168.0.61:1521/oracle10g"),
	P:set_property(user, "system"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDrive"),
	P:set_property(password, "systest"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(max_counter, 10),
	P:set_property(browse,[{"V$SYSSTAT DBWR undo block writes","V$SYSSTAT/DBWR undo block writes"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
	