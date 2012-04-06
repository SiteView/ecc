-module(databasecountermonitor_SUITE).

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
    [correct_counter, correct_value_no_cummulat_no_diveder, correct_value_no_diveder,
	correct_value_no_cummulat, correct_value_no_divide_query, correct_value_no_cummulat_have_divide_query,
	correct_value_have_divide_query, correct_value_no_divide_have_divide_query, 
	correct_value_no_divide_have_divide_query,
	error_counter_driver, error_counter_url, error_counter_pquery, error_counter_user,
	error_counter_password, error_counter_dquery, error_value_driver, error_value_url,
	error_value_pquery, error_value_user, error_value_password, error_value_dquery].


%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[].

correct_counter(_Config) ->
	P = database_counter_monitor:new(),
	Params = [{driver, "com.mysql.jdbc.Driver"}, {connection_url, "192.168.0.50:3306/mysql"}, {pquery, "select * from cy"},
				{user, "root"}, {password, "888888"}, {divisor_query, ""}, {connection_timeout, 60}, {query_timeout, 60},
				{no_cumulative, false}, {no_divisor, false}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
correct_value_no_cummulat_no_diveder(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_value_no_diveder(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, true),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_value_no_cummulat(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, true),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_value_no_divide_query(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, true),
	P:set_property(no_divisor, true),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
			
correct_value_no_cummulat_have_divide_query(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, "select * from hj"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, true),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

correct_value_have_divide_query(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, "select * from hj"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, true),
	P:set_property(no_divisor, true),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_value_no_divide_have_divide_query(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, "select * from hj"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, true),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
correct_value_no_cumulate_no_divide_have_divide_query(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, "select * from hj"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	
	
	
error_counter_driver(_Config) ->
	P = database_counter_monitor:new(),
	Params = [{driver, "com.mysql.jdbc.Drive"}, {connection_url, "192.168.0.50:3306/mysql"}, {pquery, "select * from cy"},
				{user, "root"}, {password, "888888"}, {divisor_query, ""}, {connection_timeout, 60}, {query_timeout, 60},
				{no_cumulative, false}, {no_divisor, false}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_url(_Config) ->
	P = database_counter_monitor:new(),
	Params = [{driver, "com.mysql.jdbc.Driver"}, {connection_url, "192.168.0.51:3306/mysql"}, {pquery, "select * from cy"},
				{user, "root"}, {password, "888888"}, {divisor_query, ""}, {connection_timeout, 60}, {query_timeout, 60},
				{no_cumulative, false}, {no_divisor, false}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.	
	
error_counter_pquery(_Config) ->
	P = database_counter_monitor:new(),
	Params = [{driver, "com.mysql.jdbc.Driver"}, {connection_url, "192.168.0.50:3306/mysql"}, {pquery, "select * from c"},
				{user, "root"}, {password, "888888"}, {divisor_query, ""}, {connection_timeout, 60}, {query_timeout, 60},
				{no_cumulative, false}, {no_divisor, false}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.	
	
error_counter_user(_Config) ->
	P = database_counter_monitor:new(),
	Params = [{driver, "com.mysql.jdbc.Driver"}, {connection_url, "192.168.0.50:3306/mysql"}, {pquery, "select * from cy"},
				{user, "roo"}, {password, "888888"}, {divisor_query, ""}, {connection_timeout, 60}, {query_timeout, 60},
				{no_cumulative, false}, {no_divisor, false}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_counter_password(_Config) ->
	P = database_counter_monitor:new(),
	Params = [{driver, "com.mysql.jdbc.Driver"}, {connection_url, "192.168.0.50:3306/mysql"}, {pquery, "select * from cy"},
				{user, "root"}, {password, "88888"}, {divisor_query, ""}, {connection_timeout, 60}, {query_timeout, 60},
				{no_cumulative, false}, {no_divisor, false}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_dquery(_Config) ->
	P = database_counter_monitor:new(),
	Params = [{driver, "com.mysql.jdbc.Driver"}, {connection_url, "192.168.0.50:3306/mysql"}, {pquery, "select * from cy"},
				{user, "root"}, {password, "888888"}, {divisor_query, "select * from kk"}, {connection_timeout, 60}, {query_timeout, 60},
				{no_cumulative, false}, {no_divisor, false}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_value_driver(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Drive"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
error_value_url(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysq"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_value_pquery(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from c"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_user(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "roo"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_value_password(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Driver"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "88888"),
	P:set_property(divisor_query, ""),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_dquery(_Config) ->	
	P = database_counter_monitor:new(),
	P:set_property(driver, "com.mysql.jdbc.Drive"),
	P:set_property(connection_url, "192.168.0.50:3306/mysql"),
	P:set_property(pquery, "select * from cy"),
	P:set_property(user, "root"),
	P:set_property(password, "888888"),
	P:set_property(divisor_query, "select * from kk"),
	P:set_property(connection_timeout, 60),
	P:set_property(query_timeout, 60),
	P:set_property(no_cumulative, false),
	P:set_property(no_divisor, false),
	P:set_property(browse, [{"0|||cy2", "0/cy2"}]),
	P:set_attribute(last_measurements, {empty, []}),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		