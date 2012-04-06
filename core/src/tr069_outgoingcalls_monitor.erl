-module(tr069_outgoingcalls_monitor,[BASE]).   
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
	Base:set_attribute(outgoingcallsattempted,0),
	Base:set_attribute(outgoingcallsanswered,0),
	Base:set_attribute(outgoingcallsconnected,0),
	Base:set_attribute(outgoingcallsfailed,0),
	{?MODULE,Base}.
	
update() ->
	Pid = acs_helper:get_acs_pid(),
	Ip = THIS:get_property(ip),
	Manufacturer = THIS:get_property(manufacturer), 
	OUI = THIS:get_property(oui), 
	ProductClass = THIS:get_property(productclass),
	SerialNumber = THIS:get_property(serialnumber),
	Cpe_id = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	StringList = ["outgoingcallsattempted","outgoingcallsanswered","outgoingcallsconnected","outgoingcallsfailed"],
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
			[{outgoingcallsattempted,'=',0},{outgoingcallsanswered,'=',0},{outgoingcallsconnected,'=',0},{outgoingcallsfailed,'=',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{outgoingcallsattempted,'<',0},{outgoingcallsanswered,'<',0},{outgoingcallsconnected,'<',0},{outgoingcallsfailed,'<',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{outgoingcallsattempted,'>',0},{outgoingcallsanswered,'>',0},{outgoingcallsconnected,'>',0},{outgoingcallsfailed,'>',0}]
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=outgoingcallsattempted,title="试呼的呼出呼叫次数",type=text,editable=true,order=3},
	#property{name=outgoingcallsanswered,title="应答的呼出呼叫次数",type=text,editable=true,order=3},
	#property{name=outgoingcallsconnected,title="接通的呼出呼叫次数",type=text,editable=true,order=3},
	#property{name=outgoingcallsfailed,title="失败的呼出呼叫次数",type=text,editable=true,order=3}
	].
	
for([],0)->
	THIS:set_attribute(?NO_DATA,true),
	THIS:set_atttibute(?STATE_STRING,"miss parameter");
for([],_)->
	ok;
for([F|R],N) ->
	case F of
		{outgoingcallsattempted,Outgoingcallsattempted}->
			THIS:set_attribute(outgoingcallsattempted,Outgoingcallsattempted),for(R,N+1);
		{outgoingcallsanswered,Outgoingcallsanswered}->
			THIS:set_attribute(outgoingcallsanswered,Outgoingcallsanswered),for(R,N+1);
		{outgoingcallsconnected,Outgoingcallsconnected}->
			THIS:set_attribute(outgoingcallsconnected,Outgoingcallsconnected),for(R,N+1);
		{outgoingcallsfailed,Outgoingcallsfailed}->
			THIS:set_attribute(outgoingcallsfailed,Outgoingcallsfailed),for(R,N+1);
		_->
			for(R,N)
	end.