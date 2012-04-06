-module(telnet_SUITE).

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
    [correct, correct_no_match, correct_no_com, error_host,
	error_user, error_pass, error_login, error_password, error_prompt].

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

correct(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, "tel"),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "$"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_no_match(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, ""),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "$"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
correct_no_com(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, ""),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "$"),
	P:set_property(command, ""),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_host(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, "tel"),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "$"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.221"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_user(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tt"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, "tel"),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "$"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_pass(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisc"),
	P:set_property(matchcontent, "tel"),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "$"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_login(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, "tel"),
	P:set_property(loginprom, "ogi:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "$"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
error_password(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, "tel"),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "asswor:"),
	P:set_property(prompt, "$"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_prompt(_Config) ->
	P = telnet_monitor:new(),
	P:set_property(timeout, 10),
	P:set_property(username, "tel"),
	P:set_property(password, "cisco"),
	P:set_property(matchcontent, "tel"),
	P:set_property(loginprom, "ogin:"),
	P:set_property(passwordprom, "assword:"),
	P:set_property(prompt, "#"),
	P:set_property(command, "pwd"),
	P:set_property(telnetserver, "192.168.0.225"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	