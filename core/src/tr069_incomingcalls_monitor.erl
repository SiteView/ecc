-module(tr069_incomingcalls_monitor,[BASE]).  
-extends(server_monitor).
-export([new/0,update/0,get_classifier/1,get_template_property/0]).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include("head.hrl").
-define(time,5000).
%参数名发送包数暂定packetssent
new() ->
	Base = server_monitor:new(),
	Base:set_attribute(incomingcallsreceived,0),
	Base:set_attribute(incomingcallsanswered,0),
	Base:set_attribute(incomingcallsconnected,0),
	Base:set_attribute(incomingcallsfailed,0),
	{?MODULE,Base}.
	
update() ->
	Pid = acs_helper:get_acs_pid(),
	Ip = THIS:get_property(ip),
	Manufacturer = THIS:get_property(manufacturer), 
	OUI = THIS:get_property(oui), 
	ProductClass = THIS:get_property(productclass),
	SerialNumber = THIS:get_property(serialnumber),
	Cpe_id = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	StringList = ["incomingcallsreceived","incomingcallsanswered","incomingcallsconnected","incomingcallsfailed"],
	Value_list = [{manufacturer, Manufacturer}, {oui, OUI}, {productClass, ProductClass}, {serialNumber, SerialNumber}, {ip, Ip}, {stringList, StringList}],
	%hold a place for method 
	Pid ! {client_conf, self(), {Cpe_id, Ip, id, method,data_conversion:getParameterValues(Value_list)}},
	receive
		{conf_response,id,{ok, Record}} ->
			Res = data_conversion:getParameterValuesResponse(Record),
			for(Res,0);
		{conf_response,id,{error,Reason}} ->
			THIS:set_attribute(?STATE_STRING,Reason);
		_ ->
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?STATE_STRING,"wrong response")
		after ?time ->
			THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?STATE_STRING,"Time Out")
	end.

%假设的阈值以后需做修改
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{incomingcallsreceived,'=',0},{incomingcallsanswered,'=',0},{incomingcallsconnected,'=',0},{incomingcallsfailed,'=',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{incomingcallsreceived,'<',0},{incomingcallsanswered,'<',0},{incomingcallsconnected,'<',0},{incomingcallsfailed,'<',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{incomingcallsreceived,'>',0},{incomingcallsanswered,'>',0},{incomingcallsconnected,'>',0},{incomingcallsfailed,'>',0}]
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=incomingcallsreceived,title="试呼的呼入呼叫次数",type=scalar,editable=true,order=3},
	#property{name=incomingcallsanswered,title="应答的呼入呼叫次数",type=text,editable=true,order=3},
	#property{name=incomingcallsconnected,title="接通的呼入呼叫次数",type=text,editable=true,order=3},
	#property{name=incomingcallsfailed,title="失败的呼入呼叫次数",type=text,editable=true,order=3}
	].

for([],0)->
	THIS:set_attribute(?NO_DATA,true),
	THIS:set_atttibute(?STATE_STRING,"miss parameter");
for([],_)->
	ok;
for([F|R],N) ->
	case F of
		{incomingcallsreceived,Incomingcallsreceived}->
			THIS:set_attribute(incomingcallsreceived,Incomingcallsreceived),for(R,N+1);
		{incomingcallsanswered,Incomingcallsanswered}->
			THIS:set_attribute(incomingcallsanswered,Incomingcallsanswered),for(R,N+1);
		{incomingcallsconnected,Incomingcallsconnected}->
			THIS:set_attribute(incomingcallsconnected,Incomingcallsconnected),for(R,N+1);
		{incomingcallsfailed,Incomingcallsfailed}->
			THIS:set_attribute(incomingcallsfailed,Incomingcallsfailed),for(R,N+1);
		_->
			for(R,N)
	end.