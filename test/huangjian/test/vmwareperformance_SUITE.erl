-module(vmwareperformance_SUITE).

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
    [correct_counter, correct_value, 
	error_counter_url, error_counter_user,
	error_counter_passwd,
	error_value_url, error_value_user,
	error_value_passwd].

%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[].
	
correct_counter(_Config) ->
	P = vmwareperformance_monitor:new(),
	Params = [{url, "http://192.168.2.112/sdk"}, {timeout, 60}, {usr, "root"}, {pwd, "888888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
correct_value(_Config) ->
	P = vmwareperformance_monitor:new(),
	P:set_property(timeout, 60),
	P:set_property(usr, "root"),
	P:set_property(pwd, "888888"),
	P:set_property(url, "http://192.168.2.112/sdk"),
	P:set_property(browse, [{"cpu.usage.maximum(rate)##300#0invpath:/ha-datacenter/host/localhost.localdomain/localhost.localdomain",
  "HostSystem/localhost.localdomain/Historical[300 secs]/cpu/usage.maximum[]"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_counter_url(_Config) ->
	P = vmwareperformance_monitor:new(),
	Params = [{url, "http://192.168.0.55/sdk"}, {timeout, 60}, {usr, "root"}, {pwd, "888888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_user(_Config) ->
	P = vmwareperformance_monitor:new(),
	Params = [{url, "http://192.168.2.112/sdk"}, {timeout, 60}, {usr, "roo"}, {pwd, "888888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.
	
error_counter_passwd(_Config) ->
	P = vmwareperformance_monitor:new(),
	Params = [{url, "http://192.168.2.112/sdk"}, {timeout, 60}, {usr, "root"}, {pwd, "88888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_value_url(_Config) ->
	P = vmwareperformance_monitor:new(),
	P:set_property(timeout, 60),
	P:set_property(usr, "root"),
	P:set_property(pwd, "888888"),
	P:set_property(url, "http://192.168.0.55/sdk"),
	P:set_property(browse, [{"cpu.usage.maximum(rate)##300#0invpath:/ha-datacenter/host/localhost.localdomain/localhost.localdomain",
  "HostSystem/localhost.localdomain/Historical[300 secs]/cpu/usage.maximum[]"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_value_user(_Config) ->
	P = vmwareperformance_monitor:new(),
	P:set_property(timeout, 60),
	P:set_property(usr, "roo"),
	P:set_property(pwd, "888888"),
	P:set_property(url, "http://192.168.2.112/sdk"),
	P:set_property(browse, [{"cpu.usage.maximum(rate)##300#0invpath:/ha-datacenter/host/localhost.localdomain/localhost.localdomain",
  "HostSystem/localhost.localdomain/Historical[300 secs]/cpu/usage.maximum[]"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_value_passwd(_Config) ->
	P = vmwareperformance_monitor:new(),
	P:set_property(timeout, 60),
	P:set_property(usr, "root"),
	P:set_property(pwd, "88888"),
	P:set_property(url, "http://192.168.2.112/sdk"),
	P:set_property(browse, [{"cpu.usage.maximum(rate)##300#0invpath:/ha-datacenter/host/localhost.localdomain/localhost.localdomain",
  "HostSystem/localhost.localdomain/Historical[300 secs]/cpu/usage.maximum[]"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
