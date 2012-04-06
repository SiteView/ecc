-module(tr069_RTP_monitor,[BASE]). 
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
	Base:set_attribute(packetssent,0),
	Base:set_attribute(packetsreceived,0),
	Base:set_attribute(bytessent,0),
	Base:set_attribute(bytesreceived,0),
	Base:set_attribute(packetslost,0),
	{?MODULE,Base}.
	
update() ->
	Pid = acs_helper:get_acs_pid(),
	Ip = THIS:get_property(ip),
	Manufacturer = THIS:get_property(manufacturer), 
	OUI = THIS:get_property(oui), 
	ProductClass = THIS:get_property(productclass),
	SerialNumber = THIS:get_property(serialnumber),
	Cpe_id = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	StringList = ["packetssent","packetsreceived","bytessent","bytesreceived","packetslost"],
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
			[{packetssent,'=',0},{packetsreceived,'=',0},{bytessent,'=',0},{bytesreceived,'=',0},{packetslost,'=',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{packetssent,'<',10},{packetsreceived,'<',10},{bytessent,'<',10},{bytesreceived,'<',10},{packetslost,'<',10}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{packetssent,'>',10},{packetsreceived,'>',10},{bytessent,'>',10},{bytesreceived,'>',10},{packetslost,'>',10}]
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=packets,title="发送包数",type=scalar,editable=true,order=3},
	#property{name=packetsreceived,title="接收包数",type=text,editable=true,order=3},
	#property{name=bytessent,title="发送比特数",type=text,editable=true,order=3},
	#property{name=bytesreceived,title="接收比特数",type=text,editable=true,order=3},
	#property{name=packetslost,title="丢失比特数",type=text,editable=true,order=3}
	].

for([],0)->
	THIS:set_attribute(?NO_DATA,true),
	THIS:set_atttibute(?STATE_STRING,"miss parameter");
for([],_)->
	ok;
for([F|R],N) ->
	case F of
		{packetssent,Packetssent}->
			THIS:set_attribute(packetssent,Packetssent),for(R,N+1);
		{packetsreceived,Packetsreceived}->
			THIS:set_attribute(packetsreceived,Packetsreceived),for(R,N+1);
		{bytessent,Bytessent}->
			THIS:set_attribute(bytessent,Bytessent),for(R,N+1);
		{bytesreceived,Bytesreceived}->
			THIS:set_attribute(bytesreceived,Bytesreceived),for(R,N+1);
		{packetslost,Packetslost}->
			THIS:set_attribute(packetslost,Packetslost),for(R,N+1);
		_->
			for(R,N)
	end.
    
    
defaultTitle(Params)->
	CPE= proplists:get_value(cpe,Params),
	if
		length(CPE)>0->
			BASE:defaultTitle(Params) ++":" ++ CPE;
		true ->
			BASE:defaultTitle(Params)
	end.
    
verify(Params) ->
    Errs = 
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