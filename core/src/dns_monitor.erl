%
% DNS Monitor
%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 dragonflow
%% @version 1.0
%% @doc dns monitor
-module(dns_monitor,[BASE]).
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,defaultTitle/1,update/0,get_classifier/1,get_template_property/0]).

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for dns monitor
new()->
	Obj = atomic_monitor:new(),
	Obj:set_attribute(round_trip_time, "n/a"), %"n/a"
	Obj:set_attribute(status, 200),		%"ok"
	{?MODULE,Obj}.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  dns service
update()->    
	{ok, {_, Server}} = THIS:get_property(server),
	{ok, {_, HostName}} = THIS:get_property(hostname),	
	{ok, {_, HostAddress}} = THIS:get_property(hostaddress),
    [A, B, Ipaddr] = platform:dnsLookup(Server, HostName, HostAddress),	
	%io:format("DNSServer:~p HostName:~p HostAddress:~p A:~p B:~p~n", [Server, HostName, HostAddress, A, B]),
	
	Status = list_to_integer(A),
	Duration = sv_datetime:microSecondsToStrSeconds(B),	
	
	THIS:set_attribute(status, Status),%int	
	case Status == 200 of
		true ->        
			THIS:set_attribute(?STATE_STRING, Duration++" sec"),
			THIS:set_attribute(round_trip_time, B);
		false ->
			THIS:set_attribute(?STATE_STRING, "Error"),%´íÎóCode¶ÔÓ¦µÄDESC
			THIS:set_attribute(round_trip_time, "n/a"),
			THIS:set_attribute(?NO_DATA, true)
	end.

%% @spec defaultTitle(Params) -> List
%% Params = [term()]
%% List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Host = proplists:get_value(server,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params)
	end.

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% Params = [term()]
%% Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params) ->
    Errs = 
	case proplists:get_value(server,Params) of
    ""->
		[{server,"DNS Server Address missing"}];
    Server->
	    case string:rstr(Server," ") of
		    0->
			    [];
			_->
			    [{server,"no spaces are allowed"}]
	    end
	end ++
	case proplists:get_value(hostname,Params) of
    ""->
		[{hostname,"Host Name missing"}];
    Hostname ->
	    case string:rstr(Hostname," ") of
		    0->
			    [];
			_->
			    [{hostname,"no spaces are allowed"}]
	    end
	end ++        
	case proplists:get_value(hostaddress,Params) of
    ""->
		[];
    Hostaddress ->
	    case textutils:isIpaddr(Hostaddress) of
		    true ->
			    [];
			_->
			    [{hostaddress,"host address must a legal IP "}]
	    end
	end ++
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,   
    if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.

getHostname()->
	case THIS:get_property(server) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.

%% @spec get_template_property() -> List
%% List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	    #property{name=server, title="DNS Server Address", type=text, order=1, description="IP address of the DNS server (example: 206.168.191.1)"},
		#property{name=hostname, title="Host Name", type=text, order=2, description="the hostname to lookup (example: demo.siteview.com)"},
		
		#property{name=hostaddress, title="Host Address", type=text, advance=true, order=6, description="	optional IP address or list of addresses to verify against for the hostname (examples 206.168.191.21 or 206.168.191.21,206.168.191.46)"},
        #property{name=status, title="status", type=numeric, configurable=false, state=true},       
        #property{name=round_trip_time, title="round trip time(milliseconds)", type=numeric, configurable=false, state=true,baselinable=true}		
	].



%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '!=', 200}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status, '==', 200}]
	end.