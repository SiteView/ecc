-module(dns_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.
    
end_per_suite(_Config) ->
    ok.

all() -> 
    [error_server,error_hostaddress,right_server,right_hostaddress].
 
sequences() ->
	[].

error_server(_Config) ->
    P = dns_monitor:new(),
    P:set_property(server,"192.168.0.2"),
	P:set_property(hostname,"www.siteview.com"),	
	P:set_property(hostaddress,"222.240.176.197"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

error_hostaddress(_Config) ->
    P = dns_monitor:new(),
    P:set_property(server,"222.103.96.112"),
	P:set_property(hostname,"www.siteview.com"),	
	P:set_property(hostaddress,"192.168.0.2"),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}. 

right_server(_Config) ->
    P = dns_monitor:new(),
    P:set_property(server,"222.103.96.112"),
	P:set_property(hostname,"www.siteview.com"),	
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.


right_hostaddress(_Config) ->
    P = dns_monitor:new(),
    P:set_property(server,"222.103.96.112"),
	P:set_property(hostname,"www.siteview.com"),
    P:set_property(hostaddress,"222.240.176.197"),	
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
     


