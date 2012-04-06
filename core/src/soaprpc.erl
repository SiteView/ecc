-module(soaprpc). 
-compile(export_all).
-extends(siteview_object).
-export([execute/19,writerequest/7,httppost/10]).
-define(WSDL_XMLNS,"http://schemas.xmlsoap.org/wsdl/").
-define(SOAPENV_XMLNS,"http://schemas.xmlsoap.org/soap/envelope/").
-define(SOAPENC_XMLNS,"http://schemas.xmlsoap.org/soap/encoding/").
-define(DEF_EL,"definitions").
-define(SERVICE_EL,"service").
-define(SOAPADDRS_EL,"soap:address").
-define(SOAPOPER_EL,"soap:operation").
-define(OPERATION_EL,"operation").
-define(BINDING_EL,"binding").
-define(PART_EL,"part").
-define(MESSAGE_EL,"message").
-define(PORT_EL,"port").
-define(PORTTYPE_EL,"portType").
-define(INPUT_EL,"input").
-define(OUTPUT_EL,"output").
-define(NAME_ATTR,"name").
-define(ROOTNS_ATTR,"targetNamespace").
-define(SOAPACTION_ATTR,"soapAction").
-define(LOCATION_ATTR,"location").
-define(WSDLNS_ATTR,"xmlns").
-define(METHODNS_ATTR,"namespace").
-define(PARAMETERORDER_ATTR,"parameterOrder").
-define(MESSAGE_ATTR,"message").
-define(TYPE_ATTR,"type").
-define(SOAPBODY_EL,"SOAP-ENV:Body").
-define(SOAPENV_EL,"SOAP-ENV:Envelope").
-define(SOAPFAULT_EL,"SOAP-ENV:Fault").


execute(MethodName,Av,Schema,An,WSDLurl,ContentType,MethodNS,ActionURI,UserAgent,ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,UseDotNetSOAP,Buf,This)->
	S16="\""++ActionURI++"\"",
	Raw=rawxmlwriter:writesoapheader(Buf,This),
	writerequest(Raw,MethodName,Av,An,MethodNS,UseDotNetSOAP,This),
	case httppost(ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,S16,UserAgent,This) of
		{ok,{{_,C,Reason},_,Response}} ->
			This:set_attribute(status,C),
			This:set_attribute(response,Response);
		{error,_} ->
			This:set_attribute(status,400)
	end.
	
	
	
	
writerequest(Raw,MethodName,Av,An,MethodNS,UseDotNetSOAP,This)->
	S2="ns1:"++Raw,
	rawxmlwriter:startelement(?SOAPBODY_EL,This),
	case string:len(MethodNS) of
		N when N=/=0->
			case UseDotNetSOAP of
				true ->
					rawxmlwriter:startelement(MethodName++" xmlns=\""++MethodNS++"\"",This);
				false->
					rawxmlwriter:startelement(MethodName++" xmlns:ns1=\""++MethodNS++"\"",This)
			end;
		0->
			rawxmlwriter:startelement(MethodName,This)
	end,
	each(An,Av,This),
	case string:len(MethodNS) of
		Num when Num=/=0 and not UseDotNetSOAP ->
			rawxmlwriter:endelement(S2,This);
		0->
			rawxmlwriter:endelement(MethodName,This)
	end,
	rawxmlwriter:endelement(?SOAPBODY_EL,This),
	rawxmlwriter:endelement(?SOAPENV_EL,This).
	
%each(AN,AV)
each([],[],This)->[];
each([F|R],[F1|R1],This)->
	case string:str(F,"(") of
		N when N=/=0->
			S3 = string:substr(F,0,string:str(F,"(")),
			S4 = string:substr(F,string:str(F)+1,string:len(F)),
			case string:len(F1) of
				0->
					rawxmlwriter:emptyelement(S3,This);
				_->
					rawxmlwriter:startelement(S3++" xsi:type=\"xsd:"++S4++"\"",This),
					rawxmlwriter:write(F1,This),
					rawxmlwriter:endelement(S3,This)
			end;
		0->
			rawxmlwriter:write(F1,This)
	end,
	each(R,R1,This).
	
httppost(ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,S16,UserAgent,This)->
	case Proxy of
		""->
			This:set_attribute(proxy,[]);
		_ ->
			case string:tokens(Proxy,":") of
				[_|W1] when W1=:=[]->
					io:format("wrong proxy~n");
				[N|W1] when W1=/=[]->
					[W|_] = W1,
					This:set_attribute(proxy,[{proxy, {{N,list_to_integer(W)}, []}},{pipeline_timeout, 5000}])
			end
	end,
	case ProxyUserName of
		""->
			ok;
		_->
			case ProxyPassword of
				""->
					ok;
				_->
					Opt = {proxy_auth, {ProxyUserName, ProxyPassword}},
					{ok,{_,Httpoption1}}=This:get_attribute(httpoption),
					This:set_attribute(httpoption,[Opt|Httpoption1])
			end
	end,
	case UserName of
		""->
			ok;
		_->
			case Password of
				""->
					ok;
				_->
					Auth = {"Authorization","Basic "++base64:encode_to_string(UserName++":"++Password)},
					{ok,{_,Head1}}=This:get_attribute(head),
					This:set_attribute(head,[Auth|Head1])
			end
	end,
	case UserAgent of
		""->
			ok;
		_->
			{ok,{_,Head2}}=This:get_attribute(head),
			This:set_attribute(head,[{"User-Agent",UserAgent}|Head2])
	end,
	case S16 of
		""->
			ok;
		_->
			{ok,{_,Head3}}=This:get_attribute(head),
			This:set_attribute(head,[{"SOAPAction",S16}|Head3])
	end,
	{ok,{_,Body}}=This:get_attribute(stringbuf),
	{ok,{_,Httpoption}}=This:get_attribute(httpotion),
	{ok,{_,Head}}=This:get_attribute(head),
	case ServerURL of
		""->
			io:format("please enter the server url~n");
		URL->
			inets:start(),
			case This:get_attribute(proxy) of
				{ok,{_,Proxy1}} when Proxy1=:=[]->
					inets:start(httpc, [{profile, emp}]),
					http:request(post,{URL,Head,[],Body},Httpoption,[],emp);
				{ok,{_,Proxy1}} when Proxy1=/=[]->
					inets:start(httpc, [{profile, service}]),
					http:set_options(Proxy1,service),
					http:request(post,{URL,Head,[],Body},Httpoption,[],service)
			end
	end.
	