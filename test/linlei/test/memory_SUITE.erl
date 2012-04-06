-module(memory_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.
    
end_per_suite(_Config) ->
    ok.

all() -> 
    [error_host,no_host,windows_host_rigth,redhat_linux_host_rigth].
 
sequences() ->
	[].


error_host(_Config) ->
    P = memory_monitor:new(),
	P:set_property(machine, "\\\\192.168.0.2"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.    
        
    
no_host(_Config) ->
    P = memory_monitor:new(),
	P:set_property(machine, ""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

windows_host_rigth(_Config) ->
    P = memory_monitor:new(),
	P:set_property(machine, "\\\\192.168.0.52"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

redhat_linux_host_rigth(_Config) ->
    P = memory_monitor:new(),
	P:set_property(machine, "192.168.0.181"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
