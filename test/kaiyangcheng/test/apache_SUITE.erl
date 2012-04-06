-module(apache_SUITE).

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
    [right_auto,error_auto,right_refresh,error_refresh].

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
right_auto() ->
	ok = ct:require({apache_monitor, url_auto}),
	ok = ct:require({apache_monitor, counters_auto}),
	ok = ct:require({apache_monitor, timeout}),
	ok = ct:require({apache_monitor, server}),
	ok = ct:require({apache_monitor, proxy}),
	ok = ct:require({apache_monitor, userName}),
	ok = ct:require({apache_monitor, password}),
	ok = ct:require({apache_monitor, proxyUserName}),
	ok = ct:require({apache_monitor, proxyPassword}),
	ok = ct:require({apache_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({apache_monitor, url_auto}, "")),
	P:set_property(counters,ct:get_config({apache_monitor, counters_auto}, "")),
	P:set_property(timeout,ct:get_config({apache_monitor, timeout}, 60)),
	P:set_property(server,ct:get_config({apache_monitor, server}, "")),
	P:set_property(proxy,ct:get_config({apache_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({apache_monitor, userName}, "")),
	P:set_property(password,ct:get_config({apache_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({apache_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({apache_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({apache_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
error_auto() ->
	ok = ct:require({apache_monitor, url_auto}),
	ok = ct:require({apache_monitor, counters_refresh}),
	ok = ct:require({apache_monitor, timeout}),
	ok = ct:require({apache_monitor, server}),
	ok = ct:require({apache_monitor, proxy}),
	ok = ct:require({apache_monitor, userName}),
	ok = ct:require({apache_monitor, password}),
	ok = ct:require({apache_monitor, proxyUserName}),
	ok = ct:require({apache_monitor, proxyPassword}),
	ok = ct:require({apache_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({apache_monitor, url_auto}, "")),
	P:set_property(counters,ct:get_config({apache_monitor, counters_refresh}, "")),
	P:set_property(timeout,ct:get_config({apache_monitor, timeout}, 60)),
	P:set_property(server,ct:get_config({apache_monitor, server}, "")),
	P:set_property(proxy,ct:get_config({apache_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({apache_monitor, userName}, "")),
	P:set_property(password,ct:get_config({apache_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({apache_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({apache_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({apache_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
right_refresh() ->
	ok = ct:require({apache_monitor, url_refresh}),
	ok = ct:require({apache_monitor, counters_refresh}),
	ok = ct:require({apache_monitor, timeout}),
	ok = ct:require({apache_monitor, server}),
	ok = ct:require({apache_monitor, proxy}),
	ok = ct:require({apache_monitor, userName}),
	ok = ct:require({apache_monitor, password}),
	ok = ct:require({apache_monitor, proxyUserName}),
	ok = ct:require({apache_monitor, proxyPassword}),
	ok = ct:require({apache_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({apache_monitor, url_refresh}, "")),
	P:set_property(counters,ct:get_config({apache_monitor, counters_refresh}, "")),
	P:set_property(timeout,ct:get_config({apache_monitor, timeout}, 60)),
	P:set_property(server,ct:get_config({apache_monitor, server}, "")),
	P:set_property(proxy,ct:get_config({apache_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({apache_monitor, userName}, "")),
	P:set_property(password,ct:get_config({apache_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({apache_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({apache_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({apache_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
error_refresh() ->
	ok = ct:require({apache_monitor, url_refresh}),
	ok = ct:require({apache_monitor, counters_auto}),
	ok = ct:require({apache_monitor, timeout}),
	ok = ct:require({apache_monitor, server}),
	ok = ct:require({apache_monitor, proxy}),
	ok = ct:require({apache_monitor, userName}),
	ok = ct:require({apache_monitor, password}),
	ok = ct:require({apache_monitor, proxyUserName}),
	ok = ct:require({apache_monitor, proxyPassword}),
	ok = ct:require({apache_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({apache_monitor, url_refresh}, "")),
	P:set_property(counters,ct:get_config({apache_monitor, counters_auto}, "")),
	P:set_property(timeout,ct:get_config({apache_monitor, timeout}, 60)),
	P:set_property(server,ct:get_config({apache_monitor, server}, "")),
	P:set_property(proxy,ct:get_config({apache_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({apache_monitor, userName}, "")),
	P:set_property(password,ct:get_config({apache_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({apache_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({apache_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({apache_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 



