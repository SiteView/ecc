-module(databasemonitor_SUITE).

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
    [{sequence, oracle_8i}].

%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[{mysql, [correct_mysql, correct_mysql_match,
			correct_mysql_file, error_mysql_url,
			error_mysql_user, error_mysql_password,
			error_mysql_drivername, error_mysql_query,
			error_mysql_match, error_mysql_file]},
	 {oracle_8i, [correct_oracle8i, correct_oracle8i_match,
			error_oracle8i_url, error_oracle8i_user, 
			error_oracle8i_password, error_oracle8i_drivername, 
			error_oracle8i_query, error_oracle8i_match]},
	 {oracle_9i, [correct_oracle9i, correct_oracle9i_match,
			error_oracle9i_url, error_oracle9i_user, 
			error_oracle9i_password, error_oracle9i_drivername, 
			error_oracle9i_query, error_oracle9i_match]},
	 {oracle_10g, [correct_oracle10g, correct_oracle10g_match,
			error_oracle10g_url, error_oracle10g_user, 
			error_oracle10g_password, error_oracle10g_drivername, 
			error_oracle10g_query, error_oracle10g_match]},
	 {oracle_11g, [correct_oracle11g, correct_oracle11g_match,
			error_oracle11g_url, error_oracle11g_user, 
			error_oracle11g_password, error_oracle11g_drivername, 
			error_oracle11g_query, error_oracle11g_match]},
	 {db2_9, [correct_db2_9, correct_db2_9_match,
			error_db2_9_url, error_db2_9_user, 
			error_db2_9_password, error_db2_9_drivername, 
			error_db2_9_query, error_db2_9_match]},
	 {sql2000, [correct_sql2000, correct_sql2000_match,
			error_sql2000_url, error_sql2000_user, 
			error_sql2000_password, error_sql2000_drivername, 
			error_sql2000_query, error_sql2000_match]},
	 {sql2005, [correct_sql2005, correct_sql2005_match,
			error_sql2005_url, error_sql2005_user, 
			error_sql2005_password, error_sql2005_drivername, 
			error_sql2005_query, error_sql2005_match]}]
	.
			
correct_mysql(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, "e:\\1.txt"),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_mysql_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_mysql_file(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_mysql_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_mysql_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "roo"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_mysql_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "88888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_mysql_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_mysql_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from c"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_mysql_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, "123456"),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "88888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
				
error_mysql_file(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.50:3306/mysql"),
	P:set_property(query_property, ""),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "root"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, "e:\\2.txt"),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
				
correct_oracle8i(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/cy"),
	P:set_property(query_property, "select * from sysobjects"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_oracle8i_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/cy"),
	P:set_property(query_property, "select * from sysobjects"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle8i_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/c"),
	P:set_property(query_property, "select * from sysobjects"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "12"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_oracle8i_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/cy"),
	P:set_property(query_property, "select * from sysobjects"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "syste"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}. 

error_oracle8i_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/cy"),
	P:set_property(query_property, "select * from sysobjects"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manage"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}. 
	
error_oracle8i_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/cy"),
	P:set_property(query_property, "select * from sysobjects"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDrive"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle8i_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/cy"),
	P:set_property(query_property, "select * from erlan"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_oracle8i_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.37:1521/cy"),
	P:set_property(query_property, "select * from sysobjects"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "123"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_oracle9i(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_oracle9i_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle9i_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/c"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle9i_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "syste"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	 
error_oracle9i_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manage"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle9i_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDrive"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	 
error_oracle9i_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/cy"),
	P:set_property(query_property, "select * from v$logfil"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle9i_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.40:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "123"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
			
correct_oracle10g(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "systest"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_oracle10g_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "systest"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle10g_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/c"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "systest"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	 
error_oracle10g_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "syste"),
	P:set_property(db_password, "systest"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	 
error_oracle10g_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "systes"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle10g_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDrive"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "systest"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle10g_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/cy"),
	P:set_property(query_property, "select * from v$logfil"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "systest"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	 
error_oracle10g_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.61:1521/cy"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "123"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "systest"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_oracle11g(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orcl"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

correct_oracle11g_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orcl"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle11g_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orc"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_oracle11g_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orcl"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "syste"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
	
error_oracle11g_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orcl"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manage"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_oracle11g_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orcl"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDrive"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_oracle11g_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orcl"),
	P:set_property(query_property, "select * from v$logfil"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_oracle11g_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.49:1521/orcl"),
	P:set_property(query_property, "select * from v$logfile"),
	P:set_property(driver, "oracle.jdbc.driver.OracleDriver"),
	P:set_property(content_match, "123"),
	P:set_property(db_user, "system"),
	P:set_property(db_password, "manager"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_db2_9(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "db2admin"),
	P:set_property(db_password, "db2admin"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_db2_9_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "db2admin"),
	P:set_property(db_password, "db2admin"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_db2_9_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/c"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "db2admin"),
	P:set_property(db_password, "db2admin"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_db2_9_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "db2admi"),
	P:set_property(db_password, "db2admin"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_db2_9_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "db2admin"),
	P:set_property(db_password, "db2admi"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_db2_9_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Drive"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "db2admin"),
	P:set_property(db_password, "db2admin"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_db2_9_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/cy"),
	P:set_property(query_property, "select * from c"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "db2admin"),
	P:set_property(db_password, "db2admin"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_db2_9_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.233:50000/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, "123"),
	P:set_property(db_user, "db2admin"),
	P:set_property(db_password, "db2admin"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
	
correct_sql2000(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
correct_sql2000_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.ibm.db2.jcc.DB2Driver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2000_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/c"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.jdbc.sqlserver.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2000_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.jdbc.sqlserver.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "s"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2000_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.jdbc.sqlserver.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "88888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2000_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.jdbc.sqlserver.SQLServerDrive"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2000_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/cy"),
	P:set_property(query_property, "select * from c"),
	P:set_property(driver, "com.microsoft.jdbc.sqlserver.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2000_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.51:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.jdbc.sqlserver.SQLServerDriver"),
	P:set_property(content_match, "123"),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
correct_sql2005(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
correct_sql2005_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDriver"),
	P:set_property(content_match, "11"),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
		
error_sql2005_url(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/c"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	

error_sql2005_user(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "s"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2005_password(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "88888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	

error_sql2005_drivername(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDrive"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
error_sql2005_query(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/cy"),
	P:set_property(query_property, "select * from c"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDriver"),
	P:set_property(content_match, ""),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	

error_sql2005_match(_Config) ->	
	P = db_monitor_base:new(),
	P:set_property(database, "192.168.0.231:1433/cy"),
	P:set_property(query_property, "select * from cy"),
	P:set_property(driver, "com.microsoft.sqlserver.jdbc.SQLServerDriver"),
	P:set_property(content_match, "123"),
	P:set_property(db_user, "sa"),
	P:set_property(db_password, "888888"),
	P:set_property(query_file_path, ""),
	P:set_property(connect_timeout, 60),
	P:set_property(query_timeout, 60),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	