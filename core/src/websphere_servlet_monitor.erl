%%
%% Websphere servlet monitor
%%

%% @author lei.lin@dragonflow.com
%% @version 1.0
%% @copyright 2008-2009 dragonflow
%% @doc websphere servlet monitor
-module(websphere_servlet_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).
-include("monitor.hrl").
-include("monitor_template.hrl").
-export([new/0,verify/1,update/0,get_classifier/1,doRequest/4,lookupStatus/1,remoteCommandLineAllowed/0,get_template_property/0]).
-define(REG_NAME, java_mail_box).
-define(DEBUG_INFO, debug_info).
-define(RECEIVE_TIME_OUT, 10*1000).
-define(TIMEOUT,120*1000).
-define(MaxCounters,30).
-define(Max,10).
-define(PROPERTY_NAME_COUNTER_VALUE,"browsableValue").
-define(PROPERTY_NAME_COUNTER_NAME,"_browseName").
-define(PROPERTY_NAME_COUNTERS_IN_ERROR,"countersInError").


%% @spec new() -> Obj
%% where
%% Obj = term()
%% @doc create a new instance for websphere servlet monitor
new()->
	Obj = browsable_base:new(),
	Obj:set_property(pSecure,false),
    Obj:set_property(pPort,""),
	Obj:set_property(networkTime,0),
    Obj:set_attribute(pStatus,"ERROR"),
    {?MODULE,Obj}.

%% @spec getBrowseData(Params) -> BrowseData
%% Params = term()
%% BrowseData = list
%% @doc overloading function,return browse data.
getBrowseData(Params) ->
    Hostname = proplists:get_value(pHostname,Params),
    Port = proplists:get_value(pPort,Params),
    User = proplists:get_value(pUser,Params),
    Password = proplists:get_value(pPassword,Params),
    Proxy = proplists:get_value(pProxy,Params),
    ProxyUser = proplists:get_value(pProxyUser,Params),
    ProxyPassword = proplists:get_value(pProxyPassword,Params),
    Secure = proplists:get_value(pSecure,Params),
    ServletURL = proplists:get_value(pServletURL,Params),
	Version=proplists:get_value(version,Params),
    if Secure ->
        S8 = "https" ++ "://" ++ Hostname ++ ":"  ++ Port ++ ServletURL;
    true ->    
        S8 = "http" ++ "://" ++ Hostname ++ ":"  ++ Port ++ ServletURL 
    end,
    %{Status,Data} = http_receive:receive_asynchronous_data(S8,Proxy,Port,ProxyUser,ProxyPassword,6000),
    {Status,Data} = http_receive:receive_asynchronous_data(S8,Proxy,Port,ProxyUser,ProxyPassword,6000),    
    if Status == error ->
        [];
    true ->   
        Java_Node = siteview:get_java_node(),
%% 		Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.WebsphereServletMonitor", "getBrowseData", [{xml, Data}]}, ?TIMEOUT)
		case Version of
			"7.x" ->
				Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.WebsphereServletMonitor", "getBrowseData", [{xml,iconv:convert("utf-8", "gbk", Data)}]}, ?TIMEOUT);
			_ ->
				Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.WebsphereServletMonitor", "getBrowseData", [{xml, Data}]}, ?TIMEOUT)
		end
%%         BrowseDataList = process_date(process_xml:process_xml(Data)),
%%         io:format("Begin process data!!!~n"),     
%%         make_tree(BrowseDataList)
    end.        
 getScalarValues(Prop,Params)->
%% 	{"4.x","4.x"},{"5.x","5.x"},
	case Prop of
		version ->
			[{"5.x","5.x"},{"6.x","6.x"},{"7.x","7.x"}];
		_ ->
			BASE:getScalarValues(Prop, Params)
	end. 

getCounterSize()->
	{ok, {_, Browse}} = THIS:get_property(browse),
	Len = length(Browse).
%%@spec rpc(RegName, Node, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
rpc(RegName, Node, Msg, Timeout) ->
	THIS:set_attribute(?DEBUG_INFO, "remote process call java node ..."),
	Ping = net_adm:ping(Node),
    if
        Ping==pang ->
            [{"error","Connect Java Node Error! "}];
        true ->
            {RegName, Node} ! Msg,	
            receive
                {ok, _, Ret} ->	
                    Ret;
                {error, _From, [Ret]} ->
                    [{error,Ret}]		
            after Timeout ->
                [{error, "time is out. "}]
            end
    end.	
        
process_date(List) ->
    process_date_t(List,length(List),[]).
process_date_t(_L,0,E) -> lists:reverse(E);
process_date_t(Li,Num,En) ->
    [A|B] = Li,
    {Bin,End} = A,
    process_date_t(B,Num-1,[Bin|En]).


make_tree(List) ->
    make_tree_t(List,length(List),[]).
make_tree_t(_L,0,E) -> E;
make_tree_t(Li,Num,En) ->
    [A|B] = Li,
    Index = string:rstr(A,"/"),
    Substr= string:substr(A,1,Index-1),    
    {List1,Leve} = get_sublist(Substr,Li),
    L = make_tree_util(A,En),   
    make_tree_t(Leve,length(Leve),lists:append(En,lists:append(L,List1))).
        
make_tree_util(String,List) ->
    Index = string:str(String,"/"), 
    make_tree_util_t(String,List,Index,[]).
make_tree_util_t(_Str,_Li,0,E) -> E;     
make_tree_util_t(Str,Li,Num,En) ->
    Bool = lists:keymember(Str,1,Li),
    Index = string:rstr(Str,"/"),
    if Index == 0 ->
        if Bool ->
            make_tree_util_t(Str,Li,0,En);
        true ->
            make_tree_util_t(Str,Li,0,[{Str,Str}|En])
        end; 
    true ->
        if Bool ->                 
            make_tree_util_t(string:substr(Str,1,Index-1),Li,string:str(Str,"/"),En);
        true ->
            make_tree_util_t(string:substr(Str,1,Index-1),Li,string:str(Str,"/"),[{Str,Str}|En]) 
        end
    end.
    

get_sublist(Substr,[_A|B]) ->
    get_sublist_t(Substr,B,length(B),[],[]).
get_sublist_t(_Subs,_L,0,E,Lev) -> {lists:reverse(E),Lev};
get_sublist_t(SubS,[A|B],Num,En,Le) ->
    Index = string:rstr(A,"/"),
    Substr= string:substr(A,1,Index-1),
    if SubS ==  Substr -> 
        %get_sublist_t(SubS,B,Num-1,lists:append(En,[{A,A}]),Le);
        get_sublist_t(SubS,B,Num-1,[{A,A}|En],Le);
    true ->
        %get_sublist_t(SubS,B,Num-1,En,lists:append(Le,[A]))
        get_sublist_t(SubS,B,0,En,[A|B])
    end. 


%% @spec currentTimeMillis() -> millisecond
%% @doc return current UTC time.
currentTimeMillis() ->
    timer:seconds(calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))). %UTC time
convertbrowse(R,[])->
	R;
convertbrowse(R,[H|E])->
	{K,V}=H,
	T=[{K,convert(V)}]++R,
	convertbrowse(T,E).
%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->
    THIS:set_attribute(networkTime,0),
    THIS:set_attribute(xslTime,0),    
    UpdateStartTime = currentTimeMillis(), 
    {ok,{_,Hostname}} = THIS:get_property(pHostname), 
    {ok,{_,Port}} = THIS:get_property(pPort), 
    {ok,{_,User}} = THIS:get_property(pUser), 
    {ok,{_,Password}} = THIS:get_property(pPassword), 
    {ok,{_,Proxy}} = THIS:get_property(pProxy), 
    {ok,{_,ProxyUser}} = THIS:get_property(pProxyUser),
    {ok,{_,ProxyPassword}} = THIS:get_property(pProxyPassword),     
    {ok,{_,Flag}} = THIS:get_property(pSecure), 
    {ok,{_,Time}} = THIS:get_property(pTimeout),
    {ok,{_,ServletURL}} = THIS:get_property(pServletURL),
    {ok,{_,Browse}} = THIS:get_property(browse),  
	{ok,{_,Version}} = THIS:get_property(version),  
    if length(Browse) > ?MaxCounters ->
		THIS:set_attribute(?NO_DATA,true),
		THIS:set_attribute(?CATEGORY,?NO_DATA),
		THIS:set_attribute(countersInError,?MaxCounters),
		THIS:set_attribute(?STATE_STRING,lists:flatten(io_lib:format("the counters > MAX_COUNTER:~p",[?MaxCounters])));
    true ->    
        if Flag ->
            S8 = "https" ++ "://" ++ Hostname ++ ":"  ++ integer_to_list(getPort()) ++ ServletURL;
        true ->    
            S8 = "http" ++ "://" ++ Hostname ++ ":"  ++ integer_to_list(getPort()) ++ ServletURL 
        end,
        {Status,Data} = http_receive:receive_asynchronous_data(S8,Proxy,getPort(),ProxyUser,ProxyPassword,Time*1000),
        case Status of
        error ->
            ErrorString = lookupStatus(Data),
		    THIS:set_attribute(?NO_DATA,true),
            THIS:set_attribute(pStatus,"error"),
		    THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(countersInError,THIS:getCounterSize()),
		    THIS:set_attribute(?STATE_STRING,ErrorString);
        _ ->
            THIS:set_attribute(pStatus,"ok"),
			 Java_Node = siteview:get_java_node(),
%% 		io:format("data:~p ~n", [Data]),
			case Version of
				"7.x"->
					Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.WebsphereServletMonitor", "update", [{xml, iconv:convert("utf-8", "gbk",Data)},{counters,convertbrowse([],Browse)}]}, ?TIMEOUT);
				_ ->
					Response = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.WebsphereServletMonitor", "update", [{xml, Data},{counters,Browse}]}, ?TIMEOUT)
			end,
            updateValues(Response)
		%%             List = process_xml:process_xml(Data) ,
%%             String = getData(Browse,List),
%%             THIS:set_attribute(?STATE_STRING,String)
        end
    end.

updateValues([])->
	ok;
updateValues([H|T])->
	case H of
		{"error", Error} ->
			THIS:set_attribute(countersInError, THIS:getCounterSize()),
			THIS:set_attribute(?STATE_STRING, Error),
			stop;
		{"stateString", State_String} ->
			ConvertString=convert1(State_String),
			THIS:set_attribute(?STATE_STRING, util:replace(ConvertString,",","<br>") ),
			updateValues(T);
		{"countersInError", Counter_Errors} ->
			THIS:set_attribute(countersInError, Counter_Errors),
			updateValues(T);
%% 		{counters_value, Counters_Value} ->
%%   	set_counter_value(Counters_Value),
%% 			updateValues(T);
		{"pStatus",Status} ->
			THIS:set_attribute(pStatus, Status),
			updateValues(T);
		{_,_}->
			THIS:setCounterValue(H),
			updateValues(T);
		_->
			updateValues(T)
	end.
setCounterValue({Key,Value})->
	TempKey=convert1(Key),
	THIS:set_attribute(TempKey,Value).
convert([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert(R, lists:reverse(List) ++ Acc);
		_ ->
			convert(R, [H|Acc])
	end;
convert([], Acc)->
%% 	lists:reverse(Acc).
	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert(Msg)->
	convert(Msg, []).
convert1([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert1(R, lists:reverse(List) ++ Acc);
		_ ->
			convert1(R, [H|Acc])
	end;
convert1([], Acc)->
lists:reverse(Acc).
%%  io:format("convert1:~p ~n", [L]),
%%  L.
%% %%   io:format("convert1:~p ~n", [L]),
%%  	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert1(Msg)->
%% 	io:format("oldstatus:~p ~n", [Msg]),
	convert1(Msg, []).
getData(Browse,List) ->
    getData_t(Browse,length(Browse),List,"").
getData_t(_B,0,_L,E) -> E;
getData_t(B,N,L,En) ->
    [C|D] = B,
    {Key,Val} = C,
    case lists:keysearch(Key,1,L) of 
    {value,{_,Value}} ->
        I = string:str(Value,"."),
        if I > 0 ->
            THIS:set_attribute(Key,list_to_float(Value));
        true ->
            THIS:set_attribute(Key,list_to_integer(Value))
        end,
        getData_t(D,N-1,L,En ++ Key ++"="++Value++", "++"<br>" );
    _ ->
        THIS:set_attribute(Key,"n/a"), 
        getData_t(D,N-1,L,En ++ Key ++"="++"n/a"++", "++"<br>" ) 
    end.
    


    
 
getPort() ->
   {ok,{_,Secure}} = THIS:get_property(pSecure), 
   {ok,{_,Port}} = THIS:get_property(pPort),
   if length(Port) == 0 ->
        case Secure of
        true ->
            443;
        _ ->
            80
        end;
    true ->
        Port
    end.
   
%% @spec doRequest(MetricDataURL,MetricDataXSL,Content,Buffer) -> Xml
%% MetricDataURL = string()
%% MetricDataXSL = string()
%% Content = null | string()
%% Buffer = string()
%% Xml = string()
%% @doc get xml data
doRequest(MetricDataURL,MetricDataXSL,CustomContent,Stringbuffer) ->
    if MetricDataURL == null ->
        true;
    true ->
        {ok,{_,Proxy}} = THIS:get_property(pProxy),
        {ok,{_,ProxyUser}} = THIS:get_property(pProxyUser),
        {ok,{_,ProxyPassword}} = THIS:get_property(pProxyPassword),
        {ok,{_,User}} = THIS:get_property(pUser),
        {ok,{_,Password}} = THIS:get_property(pPassword),
        {ok,{_,Flag}} = THIS:get_property(pSecure),
        {ok,{_,Hostname}} = THIS:get_property(pHostname),
        {ok,{_,Time}} = THIS:get_property(pTimeout),
        {ok,{_,TNetworkTime}} = THIS:get_property(networkTime),
        if Flag ->
            S8 = "https" ++ "://" ++ Hostname ++ ":"  ++ integer_to_list(getPort()) ++ MetricDataURL;
        true ->    
            S8 = "http" ++ "://" ++ Hostname ++ ":"  ++ integer_to_list(getPort()) ++ MetricDataURL 
        end,
        if  CustomContent /= null ->
            Array = ["Custom-Content: " ++  CustomContent];
        true ->
            Array = []
        end,
        Ltime = currentTimeMillis(),
        %receive_asynchronous_data(Url,ProxyHostname,ProxyPort,ProxyUser,ProxyPassword,Time)
        {Status,Data} = http_receive:receive_asynchronous_data(S8,Proxy,getPort(),ProxyUser,ProxyPassword,Time),
        Etime = currentTimeMillis() - Ltime,
        NetworkTime = TNetworkTime + Etime,  
        THIS:set_property(networkTime,NetworkTime),
        if Status == ok ->
            Bool = (string:str(Data,"<") == 1),
            if Bool ->
                TStringbuffer = Stringbuffer ++ Data,
                {TStringbuffer,true};
            true ->
                {TStringBuffer,true} = transform(Data,MetricDataXSL,Stringbuffer)  
            end;
        true ->
            {Stringbuffer ++ lookupStatus(Data),false} 
        end                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    end.


%% @spec lookupStatus(integer()) -> string()
%% @doc  return error code corresponding string.  
lookupStatus(Int) ->
    case Int of
    201 ->
        "created";
    202 ->
        "accepted";
    203 ->
        "non-authoratative";
    204 ->
        "no content";
    205 ->
        "reset content";
    206 ->
        "partial content";
    301 ->
        "document moved";
    302 ->
        "document moved";
    303 ->
        "document moved";
    307 ->
        "document moved";
    400 ->
        "bad request";
    401 ->
        "unauthorized";
    403 ->
        "forbidden";
    404 ->
        "not found";
    407 ->
        "proxy authentication required";
    500 ->
        "server error";
    501 ->
        "not implemented";
    502 ->
        "proxy received invalid response";
    503 ->
        "server busy";
    504 ->
        "proxy timed out";
    999 ->
        "Timeout";   
    _ ->
        "unknown error"
            
    end.
            

postMetrics() ->
    PostMetricURL = getPostMetricURL(),
    if PostMetricURL /= null ->
        L = currentTimeMillis() / 1000,
        {ok,{_,L1}} = THIS:get_attribute(pPostMetricsLast),
        {ok,{_,L2}} = THIS:get_attribute(pPostMetricsFrequency),
        if (L - L1) >= L2 ->
            %S = getPostXML(),
            S = null,
            THIS:set_attribute(pPostMetricsLast,L);
        true ->
            S = null
        end;
    true ->        
        S = null
    end.
    %if S /= null ->    

%% @spec defaultTitle(Params) ->string()
%%  Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Host = proplists:get_value(pHostname,Params),
	if
		length(Host)>0->
			BASE:defaultTitle(Params) ++":" ++ Host;
		true ->
			BASE:defaultTitle(Params)
	end.
    
isNeedPostMetricsError(String) ->
    false.
    
getMetricListURL() ->
    null.
    
getPostMetricURL() ->
    null.
    
getMetricDataURL() ->
    null.

getMetricListXSL() ->
    null.
   
getPostMetricXSL() ->
    null.

getMetricDataXSL() ->
    null.
    
isPostAllMetrics() ->
    false. 

%return {Stringbufer,Bool}
transform(String,XslFileName,Stringbuffer) ->
    L = currentTimeMillis(),
    if XslFileName == null ->
        {StringBuff = Stringbuffer ++ String,true};
    true ->
        %XslFN =  "./" ++ XslFileName,
        %{ok,Binary} = file:read_file(XslFN), % return {ok,Binary}
        {StringBuff = Stringbuffer ++ String,true}              
    end.
                 


%% @spec remoteCommandLineAllowed() -> bool()
%% @doc overloading function.    
remoteCommandLineAllowed()->
	true.    

getHostname()->
	case THIS:get_property(pHostname) of
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
        #property{name=pHostname,title="Hostname",type=text,editable=true,order=1,description="the name of the server"},
        #property{name=pServletURL,title="Servlet URL",type=text,editable=true,order=2,description="URL of Performance Servlet. On a WAS 4.0 server, the default URL is /wasPerfTool/servlet/perfservlet. On earlier versions, the URL is chosen during the installation of the Servlet. In either case, the URL can be found in the Servlet properties page on the Admin Console."},
        #property{name=pPort,title="Port",type=numeric,editable=true,order=3,default=9080,description="port number on server - default is 9080 for non-secure & 443 for secure"},
        #property{name=version,title="Version",type=scalar,description="WebSphere Version",order=4},
	    #property{name=pSecure,title="Secure Server",type=bool,editable=true,order=5,description="check here if server is secure"},
        #property{name=pProxy,title="Proxy Server",type=text,order=6,editable=true,description="the name of the proxy server server if needed"},        
        #property{name=pProxyUser,title="Proxy Username",type=text,order=7,editable=true,description="the username for the proxy server"},
        #property{name=pProxyPassword,title="Proxy Password",type=password,order=8,editable=true,description="the password for the proxy server"},
        #property{name=pUser,title="User",type=text,order=9,editable=true,description="the username for the URL"},
        #property{name=pPassword,title="Password",type=password,order=10,editable=true,description="the password for the URL"},
        #property{name=pTimeout,title="Timeout",type=numeric,default=60,order=11,editable=true,description="the time out, in seconds, to wait for the response",baselinable=true},        
        #property{name=pStatus,title="status",type=text,order=12,configurable=false,state=true}    
    ].


getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{_,X}<- Counters].
%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% Params = [term()]
%% Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params)->
    Errs = 
	case proplists:get_value(pHostname,Params) of
    ""->
		[{pHostname,"Host Name  missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{hostname,"no spaces are allowed"}]
	    end
	end ++
    case proplists:get_value(pServletURL,Params) of
    "" ->
        [{size,"pServletURL missing."}];
    ServletURL ->
          []
    end ++
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
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


%% @spec get_classifier(error) -> List
%% List = [Tuple]
%% Tule = {status, Logic, Value}
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{pStatus,'!=',"ok"}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?Max - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{pStatus,'!=',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?Max - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{pStatus,'==',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?Max - length(Cls)));
		true ->
			Cls
	end.
