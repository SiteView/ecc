-module(ping_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() -> [{timetrap,{minutes,5}}].

init_per_suite(Config) ->
    Config.
    
end_per_suite(_Config) ->
    ok.

all() -> 
    [nohost_test,dereference,timeout].
 
sequences() ->
	[]. 
    
    
    
nohost(_Config) ->
    P = ping_monitor:new(),
	P:set_property(hostname, "192.168.0.2"),
	P:set_property(timeout, 60000),
    P:set_property(size,32),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}. 


dereference(_Config) ->
    P = ping_monitor:new(),
	P:set_property(hostname, "192.168.0.1"),
	P:set_property(timeout, 60000),
    P:set_property(size,32),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}.
    
timeout(_Config) ->
    P = ping_monitor:new(),
	P:set_property(hostname, "192.168.0.1"),
	P:set_property(timeout, 0),
    P:set_property(size,32),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),	
	{comment, Good}.    
    