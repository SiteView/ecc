-module(snmp_monitor_SUITE).

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
    [snmpv1_host_error,snmpv1_oid_error,snmpv1_oidIndex_error,snmpv1_community_error,snmpv1_with_unit,
	snmpv1_with_scale,snmpv1,snmpv2,snmpv3].

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
snmpv1_host_error(_Config) ->
	ok = ct:require({snmp_monitor_v1, host}),
	ok = ct:require({snmp_monitor_v1, oid}),
	ok = ct:require({snmp_monitor_v1, oidIndex}),
	ok = ct:require({snmp_monitor_v1, community}),
	ok = ct:require({snmp_monitor_v1, timeout}),
	ok = ct:require({snmp_monitor_v1, snmpversion}),
	ok = ct:require({snmp_monitor_v1, snmpv3user}),
	ok = ct:require({snmp_monitor_v1, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, "host_error"),
	P:set_property(oid, ct:get_config({snmp_monitor_v1, oid}, "")),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v1, oidIndex}, "")),
	P:set_property(community, ct:get_config({snmp_monitor_v1, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v1, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v1, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v1, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v1, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v1, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v1, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v1, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v1, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v1, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v1, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v1, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment, Ret}.
	
snmpv1_oid_error(_Config) ->
	ok = ct:require({snmp_monitor_v1, host}),
	ok = ct:require({snmp_monitor_v1, oid}),
	ok = ct:require({snmp_monitor_v1, oidIndex}),
	ok = ct:require({snmp_monitor_v1, community}),
	ok = ct:require({snmp_monitor_v1, timeout}),
	ok = ct:require({snmp_monitor_v1, snmpversion}),
	ok = ct:require({snmp_monitor_v1, snmpv3user}),
	ok = ct:require({snmp_monitor_v1, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v1, host}, "")),
	P:set_property(oid, "oid_error"),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v1, oidIndex}, "")),
	P:set_property(community, ct:get_config({snmp_monitor_v1, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v1, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v1, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v1, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v1, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v1, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v1, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v1, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v1, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v1, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v1, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v1, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment, Ret}.
	
snmpv1_oidIndex_error(_Config) ->
	ok = ct:require({snmp_monitor_v1, host}),
	ok = ct:require({snmp_monitor_v1, oid}),
	ok = ct:require({snmp_monitor_v1, oidIndex}),
	ok = ct:require({snmp_monitor_v1, community}),
	ok = ct:require({snmp_monitor_v1, timeout}),
	ok = ct:require({snmp_monitor_v1, snmpversion}),
	ok = ct:require({snmp_monitor_v1, snmpv3user}),
	ok = ct:require({snmp_monitor_v1, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v1, host}, "")),
	P:set_property(oid, ct:get_config({snmp_monitor_v1, oid}, "")),
	P:set_property(oidIndex, "212")),
	P:set_property(community, ct:get_config({snmp_monitor_v1, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v1, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v1, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v1, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v1, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v1, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v1, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v1, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v1, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v1, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v1, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v1, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment, Ret}.

snmpv1_community_error(_Config) ->
	ok = ct:require({snmp_monitor_v1, host}),
	ok = ct:require({snmp_monitor_v1, oid}),
	ok = ct:require({snmp_monitor_v1, oidIndex}),
	ok = ct:require({snmp_monitor_v1, community}),
	ok = ct:require({snmp_monitor_v1, timeout}),
	ok = ct:require({snmp_monitor_v1, snmpversion}),
	ok = ct:require({snmp_monitor_v1, snmpv3user}),
	ok = ct:require({snmp_monitor_v1, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v1, host}, "")),
	P:set_property(oid, ct:get_config({snmp_monitor_v1, oid}, "")),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v1, oidIndex}, "")),
	P:set_property(community, "community_error"),
	P:set_property(timeout, ct:get_config({snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v1, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v1, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v1, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v1, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v1, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v1, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v1, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v1, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v1, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v1, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v1, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment, Ret}.

snmpv1_with_unit(_Config) ->
	ok = ct:require({snmp_monitor_v1, host}),
	ok = ct:require({snmp_monitor_v1, oid}),
	ok = ct:require({snmp_monitor_v1, oidIndex}),
	ok = ct:require({snmp_monitor_v1, community}),
	ok = ct:require({snmp_monitor_v1, timeout}),
	ok = ct:require({snmp_monitor_v1, snmpversion}),
	ok = ct:require({snmp_monitor_v1, snmpv3user}),
	ok = ct:require({snmp_monitor_v1, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v1, host}, "")),
	P:set_property(oid, ct:get_config({snmp_monitor_v1, oid}, "")),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v1, oidIndex}, "")),
	P:set_property(community, ct:get_config({snmp_monitor_v1, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v1, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v1, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v1, content}, "")),
	P:set_property(units, "UNIT"),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v1, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v1, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v1, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v1, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v1, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v1, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v1, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(state_string),
	{comment, Ret}.

snmpv1_with_scale(_Config) ->
	ok = ct:require({snmp_monitor_v1, host}),
	ok = ct:require({snmp_monitor_v1, oid}),
	ok = ct:require({snmp_monitor_v1, oidIndex}),
	ok = ct:require({snmp_monitor_v1, community}),
	ok = ct:require({snmp_monitor_v1, timeout}),
	ok = ct:require({snmp_monitor_v1, snmpversion}),
	ok = ct:require({snmp_monitor_v1, snmpv3user}),
	ok = ct:require({snmp_monitor_v1, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v1, host}, "")),
	P:set_property(oid, ct:get_config({snmp_monitor_v1, oid}, "")),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v1, oidIndex}, "")),
	P:set_property(community, ct:get_config({snmp_monitor_v1, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v1, snmpversion}, "")),
	P:set_property(scale, "1000"),
	P:set_property(content, ct:get_config({snmp_monitor_v1, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v1, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v1, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v1, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v1, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v1, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v1, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v1, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v1, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(status),
	Ret = "ok",
	{comment, Ret}.
	
snmpv1(_Config) ->
	ok = ct:require({snmp_monitor_v1, host}),
	ok = ct:require({snmp_monitor_v1, oid}),
	ok = ct:require({snmp_monitor_v1, oidIndex}),
	ok = ct:require({snmp_monitor_v1, community}),
	ok = ct:require({snmp_monitor_v1, timeout}),
	ok = ct:require({snmp_monitor_v1, snmpversion}),
	ok = ct:require({snmp_monitor_v1, snmpv3user}),
	ok = ct:require({snmp_monitor_v1, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v1, host}, "")),
	P:set_property(oid, ct:get_config({snmp_monitor_v1, oid}, "")),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v1, oidIndex}, "")),
	P:set_property(community, ct:get_config({snmp_monitor_v1, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v1, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v1, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v1, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v1, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v1, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v1, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v1, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v1, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v1, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v1, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v1, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v1, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(status),
	Ret = "ok",
	{comment, Ret}.
	
snmpv2(_Config) ->
	ok = ct:require({snmp_monitor_v2, host}),
	ok = ct:require({snmp_monitor_v2, oid}),
	ok = ct:require({snmp_monitor_v2, oidIndex}),
	ok = ct:require({snmp_monitor_v2, community}),
	ok = ct:require({snmp_monitor_v2, timeout}),
	ok = ct:require({snmp_monitor_v2, snmpversion}),
	ok = ct:require({snmp_monitor_v2, snmpv3user}),
	ok = ct:require({snmp_monitor_v2, snmpv3passwd}),

	P = snmp_monitor:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v2, host}, "")),
	P:set_property(oid, ct:get_config({snmp_monitor_v2, oid}, "")),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v2, oidIndex}, "")),
	P:set_property(community, ct:get_config({snmp_monitor_v2, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v2, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v2, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v2, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v2, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v2, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v2, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v2, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v2, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v2, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v2, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v2, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v2, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(status),
	Ret = "ok",
	{comment, Ret}.
	
snmpv3(_Config) ->
	ok = ct:require({snmp_monitor_v3, host}),
	ok = ct:require({snmp_monitor_v3, oid}),
	ok = ct:require({snmp_monitor_v3, oidIndex}),
	ok = ct:require({snmp_monitor_v3, community}),
	ok = ct:require({snmp_monitor_v3, timeout}),
	ok = ct:require({snmp_monitor_v3, snmpversion}),
	ok = ct:require({snmp_monitor_v3, snmpv3user}),
	ok = ct:require({snmp_monitor_v3, snmpv3passwd}),

	P = snmp_monitor_v3:new(),
	P:set_property(host, ct:get_config({snmp_monitor_v3, host}, "")),
	P:set_property(oid, ct:get_config({snmp_monitor_v3, oid}, "")),
	P:set_property(oidIndex, ct:get_config({snmp_monitor_v3, oidIndex}, "")),
	P:set_property(community, ct:get_config({snmp_monitor_v3, community}, "")),
	P:set_property(timeout, ct:get_config({snmp_monitor_v3, timeout}, 5)),
	P:set_property(snmpversion, ct:get_config({snmp_monitor_v3, snmpversion}, "")),
	P:set_property(scale, ct:get_config({snmp_monitor_v3, scale}, "no")),
	P:set_property(content, ct:get_config({snmp_monitor_v3, content}, "")),
	P:set_property(units, ct:get_config({snmp_monitor_v3, units}, "")),
	P:set_property(snmpv3user, ct:get_config({snmp_monitor_v3, snmpv3user}, "")),
	P:set_property(snmpv3passwd, ct:get_config({snmp_monitor_v3, snmpv3passwd}, "")),
	P:set_property(measurementDesc, ct:get_config({snmp_monitor_v3, measurementDesc}, "")),
	P:set_property(measureDelta, ct:get_config({snmp_monitor_v3, measureDelta}, false)),
	P:set_property(measureRate, ct:get_config({snmp_monitor_v3, measureRate}, false)),
	P:set_property(percentageBase, ct:get_config({snmp_monitor_v3, percentageBase}, "no")),
	P:set_property(percentageDelta, ct:get_config({snmp_monitor_v3, percentageDelta}, false)),

	{ok,Cwd} = file:get_cwd(),
	file:set_cwd(".."),
	
	P:update(),

	file:set_cwd(Cwd),
	
	{ok, {_, Ret}} = P:get_attribute(status),
	Ret = "ok",
	{comment, Ret}.
	