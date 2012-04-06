-module(directory_SUITE).

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
    [rigth_path,error_path,matched,matched].

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

rigth_path(_Config) ->
    P = ping_monitor:new(),
    P:set_attribute(path,"\\\\192.168.0.54\\WebSphereApplicationServerV5.1win"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}.       

error_path(_Config) ->
    P = ping_monitor:new(),
    P:set_attribute(path,"\\\\192.168.0.2\\WebSphereApplicationServerV5.1win"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}.
    
no_matched(_Config) ->
    P = ping_monitor:new(),
    P:set_attribute(path,"\\\\192.168.0.54\\WebSphereApplicationServerV5.1win"),
    P:set_attribute(match,"tyuyt"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),

matched(_Config) ->
    P = ping_monitor:new(),
    P:set_attribute(path,"\\\\192.168.0.54\\WebSphereApplicationServerV5.1win"),
    P:set_attribute(match,"autorun.inf"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

