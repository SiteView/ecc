-module(ldap_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.
    
end_per_suite(_Config) ->
    ok.

all() -> 
    [
        error_server,error_user,error_password,error_match,rigth_match,error_port,rigth,
        rigth_port     
    ].
 
sequences() ->
	[].
    
error_server(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.180"),
    P:set_property(pSecurityPrincipal,"uid=oldhand,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"123456"),
    P:set_property(pMatchString,""),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.

error_user(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.181"),
    P:set_property(pSecurityPrincipal,"uid=error,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"123456"),
    P:set_property(pMatchString,""),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}. 

error_password(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.181"),
    P:set_property(pSecurityPrincipal,"uid=oldhand,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"test"),
    P:set_property(pMatchString,""),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
    
error_match(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.181"),
    P:set_property(pSecurityPrincipal,"uid=oldhand,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"123456"),
    P:set_property(pMatchString,"test"),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.  

rigth_match(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.181"),
    P:set_property(pSecurityPrincipal,"uid=oldhand,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"123456"),
    P:set_property(pMatchString,"oldhand"),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.


error_port(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.181:8080"),
    P:set_property(pSecurityPrincipal,"uid=oldhand,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"123456"),
    P:set_property(pMatchString,"test"),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.
    
rigth(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.181"),
    P:set_property(pSecurityPrincipal,"uid=oldhand,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"123456"),
    P:set_property(pMatchString,""),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.  

rigth_port(_Config) ->
    rigth(_Config) ->
    P = ldap_monitor:new(), 
    P:set_property(pURLProvider,"ldap://192.168.0.181:389"),
    P:set_property(pSecurityPrincipal,"uid=oldhand,o=3ren,c=cn"),
    P:set_property(pSecurityCredential,"123456"),
    P:set_property(pMatchString,""),
    P:set_property(pLdapQuery,""),
    P:set_property(pLdapFilter,""),
    P:update(),
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	{comment, Good}.


