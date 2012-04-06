-module(dataconv_test).
-compile(export_all).
-include ("head.hrl").


%%%%%%%%%%%%%%%%%
%% 测试request
test1() ->
	Options = [{manufacturer, "siteview"}, 
				{oui, "test"}, 
				{productClass, "good"}, 
				{serialNumber, "123"}, 
				{ip, "221.133.234.54"}, 
				{stringList, ["DeviceInfoModule.SoftwareVersionModule"]}
				],
	%R = data_conversion:getParameterAttributes(Options),
	R = data_conversion:getParameterValues(Options),
	%% 将Record转换为SOAP (xml)
	monitor_soap:writesoap(R).
	
%%getParameterValues SOAP结构如下：
%{ok,
%"<?xml version=\"1.0\" encoding=\"utf-8\"?>
%<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:cwmp=\"urn:dslforum-org:cwmp-1-1\">
%<soap:Body>
%	<cwmp:GetParameterValues>
%		<DeviceId>
%			<Manufacturer>siteview</Manufacturer>
%			<OUI>test</OUI>
%			<ProductClass>good</ProductClass>
%			<SerialNumber>123456</SerialNumber>
%		</DeviceId>
%		<Ip>221.133.234.54</Ip>
%		<ParameterNames xsi:type=\"SOAP-ENC:Array\" SOAP-ENC:arrayType=\"xsd:string[1]\">
%			<string>DeviceInfoModule.SoftwareVersionModule</string>
%		</ParameterNames>
%	</cwmp:GetParameterValues>
%</soap:Body>
%</soap:Envelope>"}

test2() ->
	Options = [{manufacturer, "siteview"}, 
				{oui, "test"}, 
				{productClass, "good"}, 
				{serialNumber, "123"}, 
				{ip, "221.133.234.54"}, 
				{name, "name1"}, 
				{notificationChange, true}, 
				{notification, "0"}, 
				{accessListChange, true}, 
				{stringList, ["DeviceInfoModule.SoftwareVersionModule"]}
				],
	R = data_conversion:setParameterAttributes(Options),
	%将Record转换为SOAP (xml)
	monitor_soap:writesoap(R).
	
test3() ->
	Options = [{manufacturer, "siteview"}, 
				{oui, "test"}, 
				{productClass, "good"}, 
				{serialNumber, "123"}, 
				{ip, "221.133.234.54"}, 
				{name, "name1"}, 
				{value, "value1"}, 
				{parameterKeyType, "testkey"}
				],
	R = data_conversion:setParameterValues(Options),
	%将Record转换为SOAP (xml)
	monitor_soap:writesoap(R).
	
%setParameterValues SOAP结构如下：
%{ok,"
%<?xml version=\"1.0\" encoding=\"utf-8\"?>
%<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:cwmp=\"urn:dslforum-org:cwmp-1-1\">
%<soap:Body>
%	<cwmp:SetParameterValues>
%		<DeviceId>
%			<Manufacturer>siteview</Manufacturer>
%			<OUI>test</OUI>
%			<ProductClass>good</ProductClass>
%			<SerialNumber>123456</SerialNumber>
%		</DeviceId>
%		<Ip>221.133.234.54</Ip>
%		<ParameterList>
%			<ParameterValueStruct>
%				<Name>nametest</Name>
%				<Value>valuetest</Value>
%			</ParameterValueStruct>
%		</ParameterList>
%		<ParameterKey>key</ParameterKey>
%	</cwmp:SetParameterValues>
%</soap:Body>
%</soap:Envelope>
%"}

%%%%%%%%%%%%%%%%%
%% 测试response
test4() ->
	%先构造GetParameterAttributesResponse Record
	Name = "namestr",
	Notification = "0", %enumeration:"0","1","2"
	StringList = ["str1", "str2"],
	AccessList = #'cwmp:AccessList'{'string'=StringList},
	ParameterAttributeStruct = [#'cwmp:ParameterAttributeStruct'{'Name'=Name, 'Notification'=Notification, 'AccessList'=AccessList}],
	ParameterAttributeList = #'cwmp:ParameterAttributeList'{'ParameterAttributeStruct'=ParameterAttributeStruct},	
	R = #'cwmp:GetParameterAttributesResponse'{'ParameterList'=ParameterAttributeList},
	%将Record转换为SOAP (xml)
	{ok, S} = monitor_soap:writesoap(R),
	%将SOAP转换为Record
	case do_post(list_to_binary(S), "/monitor") of
		[] ->
			empty;
		[Body] ->
			%io:format("record : ~p~n",[Body]),
			data_conversion:getParameterAttributesResponse(Body)
	end.
	
test5() ->
	%先构造GetParameterValuesResponse Record
	Name = "namestr",
	Value = "dd0",
	ParameterValueStruct = [#'cwmp:ParameterValueStruct'{'Name'=Name, 'Value'=Value}],
	ParameterValueList = #'cwmp:ParameterValueList'{'ParameterValueStruct'=ParameterValueStruct},
	R = #'cwmp:GetParameterValuesResponse'{'ParameterList'=ParameterValueList},
	%将Record转换为SOAP (xml)
	{ok, S} = monitor_soap:writesoap(R),
	%将SOAP转换为Record
	case do_post(list_to_binary(S), "/monitor") of
		[] ->
			empty;
		[Body] ->
			%io:format("record : ~p~n",[Body]),
			data_conversion:getParameterValuesResponse(Body)
	end.	
	
test6() ->
	%先构造SetParameterValuesResponse Record
	Status = "ss",
	R = #'cwmp:SetParameterValuesResponse'{'Status'=Status},
	%将Record转换为SOAP (xml)
	{ok, S} = monitor_soap:writesoap(R),
	%将SOAP转换为Record
	case do_post(list_to_binary(S), "/monitor") of
		[] ->
			empty;
		[Body] ->
			%io:format("record : ~p~n",[Body]),
			data_conversion:setParameterValuesResponse(Body)
	end.


%%将Bin SOAP解析成Record 
do_post(BinSoap, Uri) ->
	case ets:lookup(soap_callback, Uri) of
		[{_Url, Fun}] ->
			case soap_registry_server:get_xsd(Uri) of
				{ok, Model} ->					
					case (catch erlsom:parse(BinSoap,Model)) of
						{error, Error} ->
						   io:format("could not parse err= ~p~n",[Error]),
							[];
						{ok,Parsed} ->
							io:format("xml isrrrrrrrrrrr  ~p~n",[Parsed]),
							{Header,Body,SoapVer} = mod_soap:header_body(Parsed),
							Body;  %[Record]
						 Other ->
							  []
					end;
                _->
				   ok
			end;
        _-> ok
	end.