-module(ftp_SUITE).

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
    [correct_active_no_proxy_content, correct_active_no_proxy,
	correct_passive_no_proxy_content, correct_passive_no_proxy,
	correct_proxy_no_content,
	error_host, error_user, error_passwd, 
	error_content, error_proxy, error_proxyuser,
	error_proxypass, error_file].

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

correct_active_no_proxy_content(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, true),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

correct_active_no_proxy(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, "Server"),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, true),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_passive_no_proxy_content(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
correct_passive_no_proxy(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, "Server"),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_proxy_no_content(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, "192.168.0.225:3128"),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, "me"),
	P:set_property(proxypassword, "me"),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	

error_host(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.2"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_user(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "inte"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_passwd(_Config) -> 
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "inte"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_content(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, "123456"),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_proxy(_Config) -> 
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, "192.168.0.224:3128"),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, "me"),
	P:set_property(proxypassword, "me"),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	

error_proxyuser(_Config)  ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, "192.168.0.225:3128"),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, "m"),
	P:set_property(proxypassword, "me"),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	

error_proxypass(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, "192.168.0.225:3128"),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, "me"),
	P:set_property(proxypassword, "m"),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "1.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.	

error_file(_Config) ->
	P = ftp_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "intel"),
	P:set_property(password, "intel"),
	P:set_property(contentmatch, ""),
	P:set_property(ftpproxy, ""),
	P:set_property(mode, false),
	P:set_property(pcheckcontent, "nil"),
	P:set_property(proxyusername, ""),
	P:set_property(proxypassword, ""),
	P:set_property(ftpserver, "192.168.0.1"),
	P:set_property(file, "2.txt"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	