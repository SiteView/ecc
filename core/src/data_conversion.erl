%%
%% data conversion (erlang term into standard record)
%%

-module(data_conversion).
-include ("head.hrl").
-compile(export_all).
	
%% Note: getParameterAttributes / 2 and getParameterValues ​​/ 2 exactly the same, but the return value difrrent.

%%Options: [{manufacturer, Manufacturer}, {oui, OUI}, {productClass, ProductClass}, {serialNumber, SerialNumber}, {ip, Ip}, {stringList, StringList}]
getParameterAttributes(Options) ->	
	Manufacturer = proplists:get_value(manufacturer, Options),  %Options if the manufacturer does not exist in the tuple, the Manufacturer is undefined。
	OUI = proplists:get_value(oui, Options),
	ProductClass = proplists:get_value(productClass, Options),
	SerialNumber = proplists:get_value(serialNumber, Options),
	
	DeviceIdStruct = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	Ip = proplists:get_value(ip, Options),
	
	StringList = proplists:get_value(stringList, Options),
	ParameterNames = #'cwmp:ParameterNames'{'string'=StringList},
	
	#'cwmp:GetParameterAttributes'{'DeviceId'=DeviceIdStruct, 'Ip'=Ip, 'ParameterNames'=ParameterNames}.


getParameterAttributesResponse(#'cwmp:GetParameterAttributesResponse'{'ParameterList'=ParameterAttributeList}) ->	
	#'cwmp:ParameterAttributeList'{'ParameterAttributeStruct'=ParameterAttributeStruct} = ParameterAttributeList,
	[#'cwmp:ParameterAttributeStruct'{'Name'=Name, 'Notification'=Notification, 'AccessList'=AccessList}] = ParameterAttributeStruct,  %array
	%Name = "namestr",
	%Notification = "0", %enumeration:"0","1","2"
	%StringList = ["str1", "str2"],
	#'cwmp:AccessList'{'string'=StringList} = AccessList,
	
	[{name, Name}, {notification, Notification}, {stringList, StringList}];
getParameterAttributesResponse(_) ->
	[].
	
	
%%Options: [{manufacturer, Manufacturer}, {oui, OUI}, {productClass, ProductClass}, {serialNumber, SerialNumber}, {ip, Ip}, 
%%	{name, Name}, {notificationChange, NotificationChange}, {notification, Notification}, {accessListChange, AccessListChange}, {stringList, StringList}]
setParameterAttributes(Options) ->
	Manufacturer = proplists:get_value(manufacturer, Options),
	OUI = proplists:get_value(oui, Options),
	ProductClass = proplists:get_value(productClass, Options),
	SerialNumber = proplists:get_value(serialNumber, Options),
	DeviceIdStruct = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	Ip = proplists:get_value(ip, Options),
	
	Name = proplists:get_value(name, Options),
	NotificationChange = proplists:get_value(notificationChange, Options), %bool
	Notification = proplists:get_value(notification, Options), %enumeration:"0","1","2"
	AccessListChange = proplists:get_value(accessListChange, Options), %bool
	StringList = proplists:get_value(stringList, Options), %["str1", "str2"],
	AccessList = [#'cwmp:AccessList'{'string'=StringList}], %list
	SetParameterAttributesStruct = [#'cwmp:SetParameterAttributesStruct'{'Name'=Name, 
																			'NotificationChange'=NotificationChange, 
																			'Notification'=Notification, 
																			'AccessListChange'=AccessListChange, 
																			'AccessList'=AccessList}],
	SetParameterAttributesList = #'cwmp:SetParameterAttributesList'{'SetParameterAttributesStruct'=SetParameterAttributesStruct},  %array
	
	#'cwmp:SetParameterAttributes'{'DeviceId'=DeviceIdStruct, 'Ip'=Ip, 'ParameterList'=SetParameterAttributesList}.
	
	
setParameterAttributesResponse(#'cwmp:SetParameterAttributesResponse'{}) ->
	[];
setParameterAttributesResponse(_) ->
	[].	
	
	
%%Options: [{manufacturer, Manufacturer}, {oui, OUI}, {productClass, ProductClass}, {serialNumber, SerialNumber}, {ip, Ip}, {stringList, StringList}]	
getParameterValues(Options) ->
	Manufacturer = proplists:get_value(manufacturer, Options),
	OUI = proplists:get_value(oui, Options),
	ProductClass = proplists:get_value(productClass, Options),
	SerialNumber = proplists:get_value(serialNumber, Options),
	DeviceIdStruct = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	Ip = proplists:get_value(ip, Options),
	
	StringList = proplists:get_value(stringList, Options), %["str1", "str2"],
	ParameterNames = #'cwmp:ParameterNames'{'string'=StringList},
	
	#'cwmp:GetParameterValues'{'DeviceId'=DeviceIdStruct, 'Ip'=Ip, 'ParameterNames'=ParameterNames}.


getParameterValuesResponse(#'cwmp:GetParameterValuesResponse'{'ParameterList'=ParameterValueList}) ->
	#'cwmp:ParameterValueList'{'ParameterValueStruct'=ParameterValueStruct} = ParameterValueList,
	[#'cwmp:ParameterValueStruct'{'Name'=Name, 'Value'=Value}] = ParameterValueStruct,
	
	[{name, Name}, {value, Value}];
getParameterValuesResponse(_)	 ->
	[].
	
	
%%Options: [{manufacturer, Manufacturer}, {oui, OUI}, {productClass, ProductClass}, {serialNumber, SerialNumber}, {ip, Ip}, 
%%	{name, Name}, {value, Value}, {parameterKeyType, ParameterKeyType}]
setParameterValues(Options) ->
	Manufacturer = proplists:get_value(manufacturer, Options),
	OUI = proplists:get_value(oui, Options),
	ProductClass = proplists:get_value(productClass, Options),
	SerialNumber = proplists:get_value(serialNumber, Options),
	DeviceIdStruct = #'cwmp:DeviceIdStruct'{'Manufacturer'=Manufacturer, 'OUI'=OUI, 'ProductClass'=ProductClass, 'SerialNumber'=SerialNumber},
	Ip = proplists:get_value(ip, Options),
	
	Name = proplists:get_value(name, Options),
	Value = proplists:get_value(value, Options),
	ParameterKeyType = proplists:get_value(parameterKeyType, Options),

	ParameterValueStruct = [#'cwmp:ParameterValueStruct'{'Name'=Name, 'Value'=Value}],
	ParameterValueList = #'cwmp:ParameterValueList'{'ParameterValueStruct'=ParameterValueStruct},
	
	#'cwmp:SetParameterValues'{'DeviceId'=DeviceIdStruct, 'Ip'=Ip, 'ParameterList'=ParameterValueList, 'ParameterKey'=ParameterKeyType}.
	
	
setParameterValuesResponse(#'cwmp:SetParameterValuesResponse'{'Status'=Status}) ->
	[{status, Status}]; %enumeration:"0","1"
setParameterValuesResponse(_) ->
	[].