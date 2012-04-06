-module(browsable_snmp_monitor_SUITE).

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
    [snmp_v1_server_error,snmp_v1_community_error,snmp_v1,snmp_v2,snmp_v3].

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
snmp_v1_server_error(_Config)->
	ok = ct:require({browsable_snmp_monitor_v1, server}),
	ok = ct:require({browsable_snmp_monitor_v1, browse}),
	ok = ct:require({browsable_snmp_monitor_v1, community}),
	ok = ct:require({browsable_snmp_monitor_v1, timeout}),
	ok = ct:require({browsable_snmp_monitor_v1, snmpversion}),

	ok = ct:require({browsable_snmp_monitor_v1, result}),

	P = browsable_snmp_monitor:new(),
	P:set_property(server, "server_error"),
	P:set_property(browse, ct:get_config({browsable_snmp_monitor_v1, browse}, "")),
	P:set_property(community, ct:get_config({browsable_snmp_monitor_v1, community}, "public")),
	P:set_property(timeout, ct:get_config({browsable_snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({browsable_snmp_monitor_v1, snmpversion}, "v1")),
	P:set_property(snmpv3authtype, ct:get_config({browsable_snmp_monitor_v1, snmpv3authtype}, "MD5")),
	P:set_property(snmpv3username, ct:get_config({browsable_snmp_monitor_v1, snmpv3username}, "")),
	P:set_property(snmpv3authpassword, ct:get_config({browsable_snmp_monitor_v1, snmpv3authpassword}, "")),
	P:set_property(snmpv3privpassword, ct:get_config({browsable_snmp_monitor_v1, snmpv3privpassword}, "")),
	P:set_property(contextEngineID, ct:get_config({browsable_snmp_monitor_v1, contextEngineID}, "")),
	P:set_property(contextName, ct:get_config({browsable_snmp_monitor_v1, contextName}, "")),
	
	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment, Ret}.
	
snmp_v1_community_error(_Config)->
	ok = ct:require({browsable_snmp_monitor_v1, server}),
	ok = ct:require({browsable_snmp_monitor_v1, browse}),
	ok = ct:require({browsable_snmp_monitor_v1, community}),
	ok = ct:require({browsable_snmp_monitor_v1, timeout}),
	ok = ct:require({browsable_snmp_monitor_v1, snmpversion}),

	ok = ct:require({browsable_snmp_monitor_v1, result}),

	P = browsable_snmp_monitor:new(),
	P:set_property(server, ct:get_config({browsable_snmp_monitor_v1, server}, "")),
	P:set_property(browse, ct:get_config({browsable_snmp_monitor_v1, browse}, "")),
	P:set_property(community, "error_community"),
	P:set_property(timeout, ct:get_config({browsable_snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({browsable_snmp_monitor_v1, snmpversion}, "v1")),
	P:set_property(snmpv3authtype, ct:get_config({browsable_snmp_monitor_v1, snmpv3authtype}, "MD5")),
	P:set_property(snmpv3username, ct:get_config({browsable_snmp_monitor_v1, snmpv3username}, "")),
	P:set_property(snmpv3authpassword, ct:get_config({browsable_snmp_monitor_v1, snmpv3authpassword}, "")),
	P:set_property(snmpv3privpassword, ct:get_config({browsable_snmp_monitor_v1, snmpv3privpassword}, "")),
	P:set_property(contextEngineID, ct:get_config({browsable_snmp_monitor_v1, contextEngineID}, "")),
	P:set_property(contextName, ct:get_config({browsable_snmp_monitor_v1, contextName}, "")),
	
	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment, Ret}.
	
snmp_v1(_Config) ->
	ok = ct:require({browsable_snmp_monitor_v1, server}),
	ok = ct:require({browsable_snmp_monitor_v1, browse}),
	ok = ct:require({browsable_snmp_monitor_v1, community}),
	ok = ct:require({browsable_snmp_monitor_v1, timeout}),
	ok = ct:require({browsable_snmp_monitor_v1, snmpversion}),

	ok = ct:require({browsable_snmp_monitor_v1, result}),

	P = browsable_snmp_monitor:new(),
	P:set_property(server, ct:get_config({browsable_snmp_monitor_v1, server}, "")),
	P:set_property(browse, ct:get_config({browsable_snmp_monitor_v1, browse}, "")),
	P:set_property(community, ct:get_config({browsable_snmp_monitor_v1, community}, "public")),
	P:set_property(timeout, ct:get_config({browsable_snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({browsable_snmp_monitor_v1, snmpversion}, "v1")),
	P:set_property(snmpv3authtype, ct:get_config({browsable_snmp_monitor_v1, snmpv3authtype}, "MD5")),
	P:set_property(snmpv3username, ct:get_config({browsable_snmp_monitor_v1, snmpv3username}, "")),
	P:set_property(snmpv3authpassword, ct:get_config({browsable_snmp_monitor_v1, snmpv3authpassword}, "")),
	P:set_property(snmpv3privpassword, ct:get_config({browsable_snmp_monitor_v1, snmpv3privpassword}, "")),
	P:set_property(contextEngineID, ct:get_config({browsable_snmp_monitor_v1, contextEngineID}, "")),
	P:set_property(contextName, ct:get_config({browsable_snmp_monitor_v1, contextName}, "")),
	
	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(countersInError),
	Ret  = 0,
	{comment, Ret}.
	
snmp_v2(_Config) ->
	ok = ct:require({browsable_snmp_monitor_v2, server}),
	ok = ct:require({browsable_snmp_monitor_v2, browse}),
	ok = ct:require({browsable_snmp_monitor_v2, community}),
	ok = ct:require({browsable_snmp_monitor_v2, timeout}),
	ok = ct:require({browsable_snmp_monitor_v2, snmpversion}),

	ok = ct:require({browsable_snmp_monitor_v2, result}),

	P = browsable_snmp_monitor:new(),
	P:set_property(server, ct:get_config({browsable_snmp_monitor_v2, server}, "")),
	P:set_property(browse, ct:get_config({browsable_snmp_monitor_v2, browse}, "")),
	P:set_property(community, ct:get_config({browsable_snmp_monitor_v2, community}, "public")),
	P:set_property(timeout, ct:get_config({browsable_snmp_monitor_v2, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({browsable_snmp_monitor_v2, snmpversion}, "v2")),
	P:set_property(snmpv3authtype, ct:get_config({browsable_snmp_monitor_v2, snmpv3authtype}, "MD5")),
	P:set_property(snmpv3username, ct:get_config({browsable_snmp_monitor_v2, snmpv3username}, "")),
	P:set_property(snmpv3authpassword, ct:get_config({browsable_snmp_monitor_v2, snmpv3authpassword}, "")),
	P:set_property(snmpv3privpassword, ct:get_config({browsable_snmp_monitor_v2, snmpv3privpassword}, "")),
	P:set_property(contextEngineID, ct:get_config({browsable_snmp_monitor_v2, contextEngineID}, "")),
	P:set_property(contextName, ct:get_config({browsable_snmp_monitor_v2, contextName}, "")),
	
	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(countersInError),
	Ret  = 0,
	{comment, Ret}.	
	
snmp_v3(_Config) ->
	ok = ct:require({browsable_snmp_monitor_v3, server}),
	ok = ct:require({browsable_snmp_monitor_v3, browse}),
	ok = ct:require({browsable_snmp_monitor_v3, timeout}),
	ok = ct:require({browsable_snmp_monitor_v3, snmpversion}),

	P = browsable_snmp_monitor:new(),
	P:set_property(server, ct:get_config({browsable_snmp_monitor_v3, server}, "")),
	P:set_property(browse, ct:get_config({browsable_snmp_monitor_v3, browse}, "")),
	P:set_property(community, ct:get_config({browsable_snmp_monitor_v3, community}, "public")),
	P:set_property(timeout, ct:get_config({browsable_snmp_monitor_v3, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({browsable_snmp_monitor_v3, snmpversion}, "v3")),
	P:set_property(snmpv3authtype, ct:get_config({browsable_snmp_monitor_v3, snmpv3authtype}, "MD5")),
	P:set_property(snmpv3username, ct:get_config({browsable_snmp_monitor_v3, snmpv3username}, "")),
	P:set_property(snmpv3authpassword, ct:get_config({browsable_snmp_monitor_v3, snmpv3authpassword}, "")),
	P:set_property(snmpv3privpassword, ct:get_config({browsable_snmp_monitor_v3, snmpv3privpassword}, "")),
	P:set_property(contextEngineID, ct:get_config({browsable_snmp_monitor_v3, contextEngineID}, "")),
	P:set_property(contextName, ct:get_config({browsable_snmp_monitor_v3, contextName}, "")),
	
	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(countersInError),
	Ret  = 0,
	{comment, Ret}.	