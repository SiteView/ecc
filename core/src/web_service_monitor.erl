%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Web Service Monitor.
%% 
%% Description: The Web Service Monitor is used to check Simple Object Access Protocol (SOAP) enabled web services for availability and stability. 
%% The Web Service Monitor sends a SOAP based request to the server and checks the response to verify that the service is responding.
-module(web_service_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include("soap.hrl").

-export([new/0,update/0,rpcCall/18,execute/18,httpPost/11,writeRequest/6,get_classifier/1,getScalarValues/2,get_template_property/0]).

-define(WSDL_XMLNS,"http://schemas.xmlsoap.org/wsdl/").
-define(SOAPENV_XMLNS,"http://schemas.xmlsoap.org/soap/envelope/").
-define(SOAPENC_XMLNS,"http://schemas.xmlsoap.org/soap/encoding/").
-define(SOAP_CONTENT_TYPE,"text/xml; charset=\"utf-8\"").
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

-define(kSOAPFaultError,-983).

-record(soapresponse,{status=-1,body="",totalDuration=0,head=[]}).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = atomic_monitor:new(),
    Base:set_attribute(status,0),
	{?MODULE,Base}.
	
%% @spec update() -> ok
%% @doc Run the monitor.
%%
%% Description: Send a soap request and check response
update()->
	{ok,{_,WSDLurl}}=THIS:get_property(wsdlurl),
    {ok,{_,Methodns}}=THIS:get_property(methodns),
	{ok,{_,ActionURI}}=THIS:get_property(actionuri),
	{ok,{_,ServerURL}}=THIS:get_property(serverurl),
	{ok,{_,MethodName}}=THIS:get_property(methodname),
	{ok,{_,MatchString}}=THIS:get_property(matchstring),
	{ok,{_,ArgNames}}=THIS:get_property(argnames),
	{ok,{_,ContentType}}=THIS:get_property(contenttype),
	{ok,{_,Schema}}=THIS:get_property(schema),
	{ok,{_,UserName}}=THIS:get_property(username),
	{ok,{_,Password}}=THIS:get_property(password),
	{ok,{_,Proxy}}=THIS:get_property(proxy),
	{ok,{_,ProxyPassword}}=THIS:get_property(proxypassword),
	{ok,{_,ProxyUserName}}=THIS:get_property(proxyuser),
	{ok,{_,NTLMDomain}}=THIS:get_property(ntlmdomain),
	{ok,{_,UseDotNetSOAP}}=THIS:get_property(usedotnetsoap),
	{ok,{_,UserAgent}}=THIS:get_property(useragent),
    THIS:set_attribute(flag,false),
	{Result,Exception} = rpcCall(WSDLurl,MethodName,ArgNames,ContentType,Schema,Methodns,ActionURI,UserAgent,ServerURL,"","",UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,UseDotNetSOAP),
    if
        Result=/=null ->
            Status = Result#soapresponse.status,
            Body = Result#soapresponse.body,
            Duration = Result#soapresponse.totalDuration,
            Head = Result#soapresponse.head;
        true ->
            Status = 0,
            Body = "",
            Duration = 0,
            Head = []
    end,
    THIS:set_attribute(status,Status),
    if
        Result=:=null ->
            THIS:set_attribute(?STATE_STRING,"no data");
        Exception=/="" ->
            THIS:set_attribute(?STATE_STRING,Exception),
            THIS:set_attribute(status,?kSOAPFaultError);
        ((Status=/=200) and (Status=/=301) and (Status=/=302)) ->
            THIS:set_attribute(?STATE_STRING,proplists:get_value(Status,?statusMapping,"unkown error"));
        true ->
            THIS:set_attribute(?STATE_STRING,httputils:floatToString(Duration/1000,2)++" sec"),
            THIS:set_attribute(flag,true)
    end,
    {ok,{_,Flag1}} = THIS:get_attribute(flag),
    if
        ((MatchString=/="") and (Result=/=null) and Flag1) ->
            MR = httputils:matchExpressionForWebServiceMonitor(Body,MatchString,[],""),
            I = element(1,MR),
            THIS:set_attribute(status,I),
            NewMR = if
                I=/=200 ->
                    M = url_monitor:new(),
                    S20 = M:getHTMLEncoding(Head),
                    M:delete(),
                    httputils:matchExpression(Body,iconv:convert("utf-8", S20, MatchString),[],"");
                true ->
                    MR
            end,
            I1 = element(1,NewMR),
            THIS:set_attribute(status,I1),
            if
                I1=/=?URLok ->
                    THIS:set_attribute(?STATE_STRING,"content match error ");
                true ->
                    {ok,{_,S17}} = THIS:get_attribute(?STATE_STRING),
                    THIS:set_attribute(?STATE_STRING,"matched "++S17),
                    Len = length(element(3,NewMR)),
                    if
                        Len=/=0 ->
                            THIS:set_attribute(matchvalue,lists:nth(1,element(3,NewMR)));
                        true ->
                            ok
                    end
            end;
        true ->
            ok
    end,
    THIS:set_attribute(roundtriptime,Duration/1000),
    if
        Result=:=null ->
            THIS:set_attribute(roundtriptime,"n/a"),
            THIS:set_attribute(?NO_DATA, true);
        true ->
            ok
    end.

%% @spec rpcCall(WSDLurl,MethodName,ArgNames,ContentType,Schema,Methodns,ActionURI,UserAgent,ServerURL,StringBuffer,StringBuffer1,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,UseDotNetSOAP) ->{Result,StringBuffer}
%% where
%% WSDLurl = string()
%% MethodName = string()
%% ArgNames = list()
%% ContentType = string()
%% Schema = string()
%% Methodns = string()
%% ActionURI = string()
%% UserAgent = string()
%% ServerURL = string()
%% StringBuffer = string()
%% StringBuffer1 = string()
%% UserName = string()
%% Password = string()
%% Proxy = string()
%% ProxyUserName = string()
%% ProxyPassword = string()
%% NTLMDomain = string()
%% UseDotNetSOAP = bool()
%% Result = record()
%% @doc Interface for web service monitor,process request.
rpcCall(WSDLurl,MethodName,ArgNames,ContentType,Schema,Methodns,ActionURI,UserAgent,ServerURL,StringBuffer,StringBuffer1,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,UseDotNetSOAP) ->
    {Vector,AS} = getParameters(ArgNames,[]),
    if
        StringBuffer=/="" ->
            {null,StringBuffer};
        true ->
            R = try execute(MethodName,Vector,Schema,AS,WSDLurl,ContentType,Methodns,ActionURI,UserAgent,ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,StringBuffer1,NTLMDomain,UseDotNetSOAP) of
            Result ->
                Result
            catch
            error:X->X,
            Exception = httputils:exception_to_String(X),
            {null,Exception}
            end,
            R
    end.

%% @spec execute(MethodName,Vector,Schema,AS,WSDLurl,ContentType,Methodns,ActionURI,UserAgent,ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,StringBuffer,NTLMDomain,UseDotNetSOAP) -> {Result,Exception}
%% MethodName = string()
%% Vector = list()
%% Schema = string()
%% AS = list()
%% WSDLurl = string()
%% ContentType = string()
%% Methodns = string()
%% ActionURI = string()
%% UserAgent = string()
%% ServerURL = string()
%% UserName = string()
%% Password = string()
%% Proxy = string()
%% ProxyUserName = string()
%% ProxyPassword = string()
%% StringBuffer = string()
%% NTLMDomain = string()
%% UseDotNetSOAP = bool()
%% Result = record()
%% Exception = string()
%% @doc Create soap request and check response.
%%
%% Description: Using yaws_soap_lib to parse wsdl
execute(MethodName,Vector,Schema,AS,WSDLurl,ContentType,Methodns,ActionURI,UserAgent,ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,StringBuffer,NTLMDomain,UseDotNetSOAP) ->
    RawXmlWriter = rawxmlwriter:new(""),
    RawXmlWriter:writeSOAPHeader([]),
    Request = writeRequest(RawXmlWriter,MethodName,Vector,AS,Methodns,UseDotNetSOAP),
    {AL1,AS1} = httpPost(ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,"\""++ActionURI++"\"",UserAgent,Request,[]),
    Status = lists:nth(1,AL1),
    S15 = lists:nth(3,AS1),
    %%io:format(StringBuffer++"(REQUEST)~n~n"++lists:nth(1,AS1)++"~n~n"++"(RESPONSE)~n~n"++S15),
    %%Fun = fun(Event,Acc) ->Acc++[Event] end,
    %%{ok,R} = erlsom:parse_sax(S15, [], Fun),
    %%io:format("R is:~p~n",[R]),
    %%io:format("~p~n",[xmerl_scan:string(S15)]),
    Index = string:str(S15,"<?xml version="),
    Flag1 = if
        AL1<200 orelse AL1>299 orelse AL1=:=204 ->
            false;
        Index=:=0 ->
            false;
        true ->
            true
    end,
    S14 = if
        Flag1 ->
            %%erlsom:parse_sax(S15, [], fun callback/2),
            Document = "",
            getFaultString(Document);
        true ->
            ""
    end,
    CostTime = lists:nth(2,AL1),
    RH = lists:nth(2,AS1),
    RR = #soapresponse{status=Status,body=S15,totalDuration=CostTime,head=RH},
    {RR,S14}.
           
            
    
getFaultString(Dom)->
    case Dom of
        {ok, #'soap:Envelope'{'Body' = #'soap:Body'{choice = Body}}, _} ->
            "";
        _->
            ""
    end.
        
%% @spec httpPost(ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,ActionURI,UserAgent,Request,ArrayS) -> {AS,AL}
%% where
%% ServerURL = string()
%% UserName = string()
%% Password = string()
%% Proxy = string()
%% ProxyUserName = string()
%% ProxyPassword = string()
%% NTLMDomain = string()
%% ActionURI = string()
%% Request = string()
%% ArrayS = list()
%% AS = list()
%% AL = list()
%% @doc Post soap envelope to server url.
httpPost(ServerURL,UserName,Password,Proxy,ProxyUserName,ProxyPassword,NTLMDomain,ActionURI,UserAgent,Request,ArrayS) ->
    {ok,{_,Class}} = THIS:get_property(?CLASS),
	{ok,{_,Id}} = THIS:get_property(?ID),
    Profile = list_to_atom(atom_to_list(Class) ++ "-" ++ atom_to_list(Id)),
    inets:start(httpc, [{profile, Profile}]),
    ProxyHead = if
        Proxy=/="" ->
            I = string:str(Proxy,":"),
            if
                I=/=0 ->
                    ProxyHost = string:sub_string(Proxy,1,I-1),
                    ProxyPort = list_to_integer(string:sub_string(Proxy,I+1,string:len(Proxy)));
                true ->
                    ProxyHost = Proxy,
                    ProxyPort = 80
            end,
            [{proxy, {{ProxyHost, ProxyPort},[]}}];
        true ->
            []
    end,
    httpc:set_options(ProxyHead,Profile),
    Head1 = [{"Content-type", "text/xml; charset=\"utf-8\""},{"SOAPAction",ActionURI}],
    ContentType = "text/xml; charset=\"utf-8\"",
    Head2 = if
        UserAgent=/="" ->
            Head1++[{"User-Agent",UserAgent}];
        true ->
            Head1
    end,
    HttpOptions = if
        ProxyUserName=/="" ->
            [{proxy_auth,{ProxyUserName,ProxyPassword}},{relaxed, true}];
        true ->
            [{relaxed, true}]
    end,
    L = httputils:currentTimeMillis(),
    Result = httpc:request(post,{ServerURL,Head2,ContentType,Request},HttpOptions,[],Profile),
    L1 = httputils:currentTimeMillis(),
    case Result of
        {ok,{{_,ResponseStatus,_},ResponseHead,ResponseBody}} ->
            if
                ResponseStatus=:=401 ->
                    AuthResponse = string:strip(proplists:get_value("www-authenticate",ResponseHead,"")),
                    AuthRequest = case httputils:startsWithIgnoreCase(AuthResponse,"basic") of
                        0 ->
                            url_auth:digest_auth(UserName,Password,ResponseHead,ServerURL,post);   
                        _->
                            url_auth:basic_auth(UserName,Password)
                    end,
                    case httpc:request(post,{ServerURL,Head2++AuthRequest,ContentType,Request},HttpOptions,[],Profile) of
                        {ok,{{_,ResponseStatus1,_},ResponseHead1,ResponseBody1}} ->
                            L2 = httputils:currentTimeMillis(),
                            AS = [Request,ResponseHead1,ResponseBody1],
                            AL = [ResponseStatus1,L2-L];
                        _ ->
                            AS = ["","",""],
                            AL = [-1,0]
                    end;
                true ->
                    AS = [Request,ResponseHead,ResponseBody],
                    AL = [ResponseStatus,L1-L]
            end;
        _->
            AS = ["","",""],
            AL = [-1,0]
    end,
    {AL,AS}.

%% @spec writeRequest(RawXmlWriter,MethodName,Vector,AS,Methodns,UseDotNetSOAP) -> Soap
%% where
%% RawXmlWriter = instance()
%% MethodName = string()
%% Vector = list()
%% AS = list()
%% Methodns = string()
%% UseDotNetSOAP = bool()
%% Soap = string()
%% @doc Build a soap envelope.
writeRequest(RawXmlWriter,MethodName,Vector,AS,Methodns,UseDotNetSOAP) ->
    S2 = "ns1:"++MethodName,
    R = try
    RawXmlWriter:startElement(?SOAPBODY_EL),
    if
        Methodns=/="" ->
            if
                UseDotNetSOAP ->
                    RawXmlWriter:startElement(MethodName++" xmlns=\""++Methodns++"\"");
                true ->
                    RawXmlWriter:startElement(S2++" xmlns:ns1=\""++Methodns++"\"")
            end;
        true ->
            RawXmlWriter:startElement(MethodName)
    end,
    for1(Vector,AS,RawXmlWriter),
    if
        ((Methodns=/="") and (not UseDotNetSOAP)) ->
            RawXmlWriter:endElement(S2);
        true ->
            RawXmlWriter:endElement(MethodName)
    end,
    RawXmlWriter:endElement(?SOAPBODY_EL),
    RawXmlWriter:endElement(?SOAPENV_EL),
    RawXmlWriter:toString()
    catch
    error:X->X,
    io:format("write request error:~p~n",[X]),
    ""
    after
    RawXmlWriter:delete()
    end,
    R.
    

for1([],[],_)->ok;
for1([F|R],[F1|R1],RawXmlWriter) ->
    S5 = string:strip(F),
    K = string:str(F1,"("),
    if
        K=/=0 ->
            S3 = string:strip(string:sub_string(F1,1,K-1)),
            L = string:str(F1,")"),
            S4 = string:strip(string:sub_string(F1,K+1,L-1)),
            if
                S5=:="" ->
                    RawXmlWriter:emptyElement(S3);
                true ->
                    RawXmlWriter:startElement(S3++" xsi:type=\"xsd:"++S4++"\""),
                    RawXmlWriter:write(S5,str),
                    RawXmlWriter:endElement(S3)
            end;
        true ->
            RawXmlWriter:write(S5,str)
    end,
    for1(R,R1,RawXmlWriter).

getParameters(S,Vector) ->
    AS = splitArguments(S),
    for2(AS,Vector,[]).

for2([],Vector,AS)->{Vector,AS};
for2([F|R],Vector,AS) ->
    AS1 = [httputils:readStringFromStart(F,"=")]++AS,
    J = string:str(F,"="),
    Vector1 = Vector++[string:sub_string(F,J+1,string:len(F))],
    for2(R,Vector1,AS1).
    
splitArguments(S)->
    case string:str(S,"\n") of
        0 ->
            S1 = "\n";
        _->
            S1 = "\n"
    end,
    string:tokens(S,S1).
    
    
%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',200}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',200}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',200}]
	end.
	
%% @spec getScalarValues(Property,Params) -> ValueList
%% where
%% Property = atom()
%% Params = [{PropertyName,PropertyValue}]
%% PropertyName = atom()
%% PropertyValue = string()
%% ValueList = [{Scalarname,Scalarvalue}]
%% Scalarname = string()
%% Scalarvalue = string()
%% @doc Set scalar properties value.
getScalarValues(Prop,Params)->
	case Prop of
        schema ->
            [{"SOAP","SOAP"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

getHostname()->
	case THIS:get_property(serverurl) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++
	  [
		#property{name=wsdlurl,title="URL of the WSDL",type=text,description="the url of the Web Service Description file",order=1},
		#property{name=methodname,title="Method Name",type=text,description="the method name in the server for the call",order=2},
		#property{name=serverurl,title="Server URL",type=text,description="The URL of the web service to be monitored",order=3},
		#property{name=argnames,title="Name of arguments",type=text,description="The name of aguments in the same order as above",multiple=true,order=4},
		#property{name=matchstring,title="Content Match",type=text,description="optional, match against query result, using a string or a regular expression or XML names.",order=5,advance=true,optional=true},
		#property{name=contenttype,title="HTTP Content-Type",type=text,description="The HTTP request header content type (optional)",order=6,advance=true,optional=true},
		#property{name=useragent,title="HTTP User-Agent",type=text,description="The HTTP User-Agent for the SOAP request (optional)",advance=true,optional=true,order=7},
		#property{name=usedotnetsoap,title="Use .NET SOAP",type=bool,description="Check this box if web service is based on MS .NET",advance=true,optional=true,order=8},
		#property{name=schema,title="Request's schema",type=scalar,description="What is the schema SOAP or XML",advance=true,optional=true,order=9},
		#property{name=methodns,title="Method Name Space",type=text,description="Method name space",advance=true,optional=true,order=10},
		#property{name=actionuri,title="SOAP ACTION",type=text,description="The SOAP ACTION url in the request header.",advance=true,optional=true,order=11},
		#property{name=ntlmdomain,title="NTLM Domain",type=text,description="Domain name (required only if using NTLM Authorization)",advance=true,optional=true,order=13},
		#property{name=username,title="Authorization User Name",type=text,description="optional user name if the Web Service requires authorization",order=14,advance=true,optional=true},
		#property{name=password,title="Authorization Password",type=password,description="optional password if the Web Service requires authorization",order=15,advance=true,optional=true},
		#property{name=proxy,title="HTTP Proxy",type=text,description="optional proxy server URL to use (example: localhost:8000)",order=16,advance=true,optional=true},
		#property{name=proxyuser,title="Proxy Server User Name",type=text,description="optional user name if the proxy server requires authorization",order=17,advance=true,optional=true},
		#property{name=proxypassword,title="Proxy Server Password",type=password,description="optional password if the proxy server requires authorization",order=18,advance=true,optional=true},
        #property{name=matchvalue,title="content match",type=numeric,state=true,configurable=false},
		#property{name=status,title="Status",type=numeric,state=true,configurable=false},
		#property{name=roundtriptime,title="round trip time",type=numeric,state=true,configurable=false,baselinable=true}
	  ].