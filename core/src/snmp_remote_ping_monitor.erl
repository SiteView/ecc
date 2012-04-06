-module(snmp_remote_ping_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

new() ->
    Base = browsable_base:new(),
    {?MODULE,Base}.

getBrowseData(_Params)->
    [{"max_rtt","Max round trip times"},{"avg_rtt","Avg round trip times"},{"min_rtt","Min round trip times"},{"received_packets","Received packets"},{"sent_packets","Sent packets"}].

update() ->
    {ok,{_,PingDeviceIp}} = THIS:get_property(ping_device),
    {ok,{_,RemoteDeviceId}} = THIS:get_property(remote_device),
    %io:format("~p ~p~n",[PingDeviceIp,RemoteDeviceId]),    
    %io:format("A ~p;Params:~p~n",[A,Params]),
    case THIS:get_property(browse) of
	{ok,{_,Browse}} ->
	    Counters = [Counter || {Counter,_} <- Browse ],
	    Params = nnm_monitor_util:get_snmp_params(list_to_atom(RemoteDeviceId)),
	    Config = [
		      {setCommunity,proplists:get_value(setCommunity,Params,"pravite")}
		      ,{version,proplists:get_value(snmpVer,Params,"v2")}
		      ,{from,proplists:get_value(ip,Params)}
		      ,{to,PingDeviceIp}
		      ,{getCommunity,proplists:get_value(getCommunity,Params,"public")}
		     ],
	    Data = network:remote_ping({snmp_remote_ping,Counters,Config}),
	    ErrorData = [ Counter || {Counter,[]} <- Data],
	    THIS:set_attribute(countersInError,length(ErrorData)),
	    [THIS:set_attribute(Counter,Value) || {Counter,Value} <- Data],
	    Infos = network:get_remote_ping_desc(Data),
	    THIS:set_attribute(?STATE_STRING,Infos);
	_ ->
	    []
    end.

get_classifier(error)->
    case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
	    	Classifier;
		_->
	    	[{countersInError,'>',0}]
    end;
get_classifier(warning)->
    case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
	    	Classifier;
		_->
		    [{countersInError,'>',0}]
    end;
get_classifier(good)-> 
    case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
		    Classifier; 
		_->
		    [{countersInError,'==',0}]
    end.

verify(Params)->
    Errs=case BASE:verify(Params) of
		{error,Be}->
		 	Be;
	     _->
		 	[]
	end,
    if length(Errs)>0 
		->
	    	{error,Errs};
		true ->
	    	{ok,""}
    end.

get_template_property()->
    BASE:get_template_property() ++
	[
	 #property{name=ping_device,title="Ping Device",type=text,editable=true,configurable=true,state=false,order=1,description="the name of the server"},
	 #property{name=remote_device,title="Remote Device",type=text,editable=true,configurable=true,state=false,order=2,description="the name of the remote server"}
	].
