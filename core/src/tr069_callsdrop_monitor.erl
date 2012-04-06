-module(tr069_callsdrop_monitor,[BASE]).  
-extends(server_monitor).
-export([new/0,update/0,get_classifier/1,get_template_property/0]).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include("head.hrl").
-define(time,5000).
%参数名暂定
new() ->
	Base = server_monitor:new(),
	Base:set_attribute(callsdropped,0),
	{?MODULE,Base}.
	
update() ->
	Pid = acs_helper:get_acs_pid(),
	Ip = THIS:get_property(ip),
	Manufacturer = THIS:get_property(manufacturer), 
	OUI = THIS:get_property(oui), 
	ProductClass = THIS:get_property(productclass),
	SerialNumber = THIS:get_property(serialnumber),
	Cpe_id = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	StringList = ["callsdropped"],
	Value_list = [{manufacturer, Manufacturer}, {oui, OUI}, {productClass, ProductClass}, {serialNumber, SerialNumber}, {ip, Ip}, {stringList, StringList}],
	%hold a place for method 
	Pid ! {client_conf, self(), {Cpe_id, Ip, id, method,data_conversion:getParameterValues(Value_list)}},
	receive
		{conf_response,id,{ok, Record}} ->
			R = data_conversion:getParameterValuesResponse(Record),
			case lists:keysearch(callsdropped,1,R) of
				{value,{callsdropped,Callsdropped}} ->
					THIS:set_attribute(callsdropped,Callsdropped);
				_ ->
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_atttibute(?STATE_STRING,"miss parameter")
			end;
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
			[{callsdropped,'>',100}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{callsdropped,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{callsdropped,'<',80}]
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=callsdropped,title="丢弃的呼叫次数",type=scalar,editable=true,order=3}
	].