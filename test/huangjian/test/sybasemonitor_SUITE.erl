-module(sybasemonitor_SUITE).

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
	error_counter_user, error_counter_password, error_value_server, 
	error_value_user, error_value_password].

	
%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[].
	
correct_counter(_Config) ->
	P = sybase_monitor:new(),
	Params = [{server,"192.168.0.65"},{usr,"sa"},{pwd,"888888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

current_value(_Config) ->
	P = sybase_monitor:new(),	
	P:set_property(server, "192.168.0.65"),
	P:set_property(usr, "sa"),
	P:set_property(pwd, "888888"),
	P:set_property(browse,[{"perfmon$pack_errors","pack errors"}]),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_counter_server(_Config) ->	
	P = sybase_monitor:new(),
	Params = [{server,"192.168.0.60"},{usr,"sa"},{pwd,"888888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_counter_user(_Config) ->
	P = sybase_monitor:new(),
	Params = [{server,"192.168.0.65"},{usr,"s"},{pwd,"888888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}. 
	
error_counter_password(_Config) ->
	P = sybase_monitor:new(),
	Params = [{server,"192.168.0.65"},{usr,"sa"},{pwd,"88888"}],
	
	Ret = P:getBrowseData(Params),
	
	{comment, Ret}.

error_value_server(_Config) ->
	P = sybase_monitor:new(),	
	P:set_property(server, "192.168.0.60"),
	P:set_property(usr, "sa"),
	P:set_property(pwd, "888888"),
	P:set_property(browse,[{"perfmon$pack_errors","pack errors"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_user(_Config) ->
	P = sybase_monitor:new(),	
	P:set_property(server, "192.168.0.65"),
	P:set_property(usr, "s"),
	P:set_property(pwd, "888888"),
	P:set_property(browse,[{"perfmon$pack_errors","pack errors"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_value_password(_Config) ->
	P = sybase_monitor:new(),	
	P:set_property(server, "192.168.0.65"),
	P:set_property(usr, "sa"),
	P:set_property(pwd, "88888"),
	P:set_property(browse,[{"perfmon$pack_errors","pack errors"}]),

	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	



