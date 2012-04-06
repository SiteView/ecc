-module(port_SUITE).

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
    [ftp_nosend,ftp_send,echo_nosend,echo_send,smtp_nosend,smtp_send,pop_nosend,pop_send].

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
ftp_nosend(_Config) ->
	ok = ct:require({port_monitor, ftp_host}),
	ok = ct:require({port_monitor, ftp_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, message_send}),
	ok = ct:require({port_monitor, message_receive}),

    P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, ftp_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, ftp_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
ftp_send(_Config) ->
	ok = ct:require({port_monitor, ftp_host}),
	ok = ct:require({port_monitor, ftp_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, message_send}),
	ok = ct:require({port_monitor, ftp_message_receive}),
	
    P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, ftp_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, ftp_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, ftp_message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
echo_nosend(_Config) ->
	ok = ct:require({port_monitor, echo_host}),
	ok = ct:require({port_monitor, echo_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, message_send}),
	ok = ct:require({port_monitor, message_receive}),
	
	P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, echo_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, echo_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
echo_send(_Config) ->
	ok = ct:require({port_monitor, echo_host}),
	ok = ct:require({port_monitor, echo_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, echo_message_send}),
	ok = ct:require({port_monitor, echo_message_receive}),
	
	P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, echo_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, echo_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, echo_message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, echo_message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 

smtp_nosend(_Config) ->
	ok = ct:require({port_monitor, smtp_host}),
	ok = ct:require({port_monitor, smtp_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, message_send}),
	ok = ct:require({port_monitor, message_receive}),
	
	P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, smtp_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, smtp_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
smtp_send(_Config) ->
	ok = ct:require({port_monitor, smtp_host}),
	ok = ct:require({port_monitor, smtp_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, message_send}),
	ok = ct:require({port_monitor, message_receive}),
	
	P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, smtp_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, smtp_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, ftp_message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
pop_nosend(_Config) ->
	ok = ct:require({port_monitor, smtp_host}),
	ok = ct:require({port_monitor, pop_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, message_send}),
	ok = ct:require({port_monitor, message_receive}),
	
	P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, smtp_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, pop_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 
	
pop_send(_Config) ->
	ok = ct:require({port_monitor, smtp_host}),
	ok = ct:require({port_monitor, pop_port}),
	ok = ct:require({port_monitor, time}),
	ok = ct:require({port_monitor, message_send}),
	ok = ct:require({port_monitor, message_receive}),
	
	P = port_monitor:new(),
	
	P:set_property(host,ct:get_config({port_monitor, smtp_host}, "")),
	P:set_property(port,ct:get_config({port_monitor, pop_port}, "")),
	P:set_property(time,ct:get_config({port_monitor, time}, 60)),
	P:set_property(message_send,ct:get_config({port_monitor, message_send}, "")),
	P:set_property(message_receive,ct:get_config({port_monitor, pop_message_receive}, "")),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 