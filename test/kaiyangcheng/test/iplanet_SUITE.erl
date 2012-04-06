-module(iplanet_SUITE).

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
    [virtualServerStats,connectionStats,dnsStats,keepAliveStats,errorfit].

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
virtualServerStats() ->
	ok = ct:require({iplanet_server_monitor, url_virtualServerStats}),
	ok = ct:require({iplanet_server_monitor, counters_virtualServerStats}),
	ok = ct:require({iplanet_server_monitor, timeout}),
	ok = ct:require({iplanet_server_monitor, server}),
	ok = ct:require({iplanet_server_monitor, proxy}),
	ok = ct:require({iplanet_server_monitor, userName}),
	ok = ct:require({iplanet_server_monitor, password}),
	ok = ct:require({iplanet_server_monitor, proxyUserName}),
	ok = ct:require({iplanet_server_monitor, proxyPassword}),
	ok = ct:require({iplanet_server_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({iplanet_server_monitor, url_virtualServerStats}, "")),
	P:set_property(counters,ct:get_config({iplanet_server_monitor, counters_virtualServerStats}, "")),
	P:set_property(timeout,ct:get_config({iplanet_server_monitor, timeout}, 60)),
	P:set_property(proxy,ct:get_config({iplanet_server_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({iplanet_server_monitor, userName}, "")),
	P:set_property(password,ct:get_config({iplanet_server_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({iplanet_server_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({iplanet_server_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({iplanet_server_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
connectionStats() ->
	ok = ct:require({iplanet_server_monitor, url_connectionStats}),
	ok = ct:require({iplanet_server_monitor, counters_connectionStats}),
	ok = ct:require({iplanet_server_monitor, timeout}),
	ok = ct:require({iplanet_server_monitor, server}),
	ok = ct:require({iplanet_server_monitor, proxy}),
	ok = ct:require({iplanet_server_monitor, userName}),
	ok = ct:require({iplanet_server_monitor, password}),
	ok = ct:require({iplanet_server_monitor, proxyUserName}),
	ok = ct:require({iplanet_server_monitor, proxyPassword}),
	ok = ct:require({iplanet_server_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({iplanet_server_monitor, url_connectionStats}, "")),
	P:set_property(counters,ct:get_config({iplanet_server_monitor, counters_connectionStats}, "")),
	P:set_property(timeout,ct:get_config({iplanet_server_monitor, timeout}, 60)),
	P:set_property(proxy,ct:get_config({iplanet_server_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({iplanet_server_monitor, userName}, "")),
	P:set_property(password,ct:get_config({iplanet_server_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({iplanet_server_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({iplanet_server_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({iplanet_server_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
dnsStats() ->
	ok = ct:require({iplanet_server_monitor, url_dnsStats}),
	ok = ct:require({iplanet_server_monitor, counters_dnsStats}),
	ok = ct:require({iplanet_server_monitor, timeout}),
	ok = ct:require({iplanet_server_monitor, server}),
	ok = ct:require({iplanet_server_monitor, proxy}),
	ok = ct:require({iplanet_server_monitor, userName}),
	ok = ct:require({iplanet_server_monitor, password}),
	ok = ct:require({iplanet_server_monitor, proxyUserName}),
	ok = ct:require({iplanet_server_monitor, proxyPassword}),
	ok = ct:require({iplanet_server_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({iplanet_server_monitor, url_dnsStats}, "")),
	P:set_property(counters,ct:get_config({iplanet_server_monitor, counters_dnsStats}, "")),
	P:set_property(timeout,ct:get_config({iplanet_server_monitor, timeout}, 60)),
	P:set_property(proxy,ct:get_config({iplanet_server_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({iplanet_server_monitor, userName}, "")),
	P:set_property(password,ct:get_config({iplanet_server_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({iplanet_server_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({iplanet_server_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({iplanet_server_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
keepAliveStats() ->
	ok = ct:require({iplanet_server_monitor, url_keepAliveStats}),
	ok = ct:require({iplanet_server_monitor, counters_keepAliveStats}),
	ok = ct:require({iplanet_server_monitor, timeout}),
	ok = ct:require({iplanet_server_monitor, server}),
	ok = ct:require({iplanet_server_monitor, proxy}),
	ok = ct:require({iplanet_server_monitor, userName}),
	ok = ct:require({iplanet_server_monitor, password}),
	ok = ct:require({iplanet_server_monitor, proxyUserName}),
	ok = ct:require({iplanet_server_monitor, proxyPassword}),
	ok = ct:require({iplanet_server_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({iplanet_server_monitor, url_keepAliveStats}, "")),
	P:set_property(counters,ct:get_config({iplanet_server_monitor, counters_keepAliveStats}, "")),
	P:set_property(timeout,ct:get_config({iplanet_server_monitor, timeout}, 60)),
	P:set_property(proxy,ct:get_config({iplanet_server_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({iplanet_server_monitor, userName}, "")),
	P:set_property(password,ct:get_config({iplanet_server_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({iplanet_server_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({iplanet_server_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({iplanet_server_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
errorfit() ->
	ok = ct:require({iplanet_server_monitor, url_virtualServerStats}),
	ok = ct:require({iplanet_server_monitor, counters_keepAliveStats}),
	ok = ct:require({iplanet_server_monitor, timeout}),
	ok = ct:require({iplanet_server_monitor, server}),
	ok = ct:require({iplanet_server_monitor, proxy}),
	ok = ct:require({iplanet_server_monitor, userName}),
	ok = ct:require({iplanet_server_monitor, password}),
	ok = ct:require({iplanet_server_monitor, proxyUserName}),
	ok = ct:require({iplanet_server_monitor, proxyPassword}),
	ok = ct:require({iplanet_server_monitor, ntlm}),
	
	P:apache_monitor:new(),
	
	P:set_property(url,ct:get_config({iplanet_server_monitor, url_virtualServerStats}, "")),
	P:set_property(counters,ct:get_config({iplanet_server_monitor, counters_keepAliveStats}, "")),
	P:set_property(timeout,ct:get_config({iplanet_server_monitor, timeout}, 60)),
	P:set_property(proxy,ct:get_config({iplanet_server_monitor, proxy}, "")),
	P:set_property(userName,ct:get_config({iplanet_server_monitor, userName}, "")),
	P:set_property(password,ct:get_config({iplanet_server_monitor, password}, "")),
	P:set_property(proxyUserName,ct:get_config({iplanet_server_monitor, proxyUserName}, "")),
	P:set_property(proxyPassword,ct:get_config({iplanet_server_monitor, proxyPassword}, "")),
	P:set_property(ntlm,ct:get_config({iplanet_server_monitor, ntlm}, "")),
	P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 