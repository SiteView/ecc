
-module(xml_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).
-define(REG_NAME, java_mail_box).
-define(DEBUG_INFO, debug_info).
-define(RECEIVE_TIME_OUT, 60*1000).
-define(MaxCounters,30).
-define(Max,10).
-include("monitor.hrl").
-include("monitor_template.hrl").


new() ->
    Base = browsable_base:new(),
	Base:set_property(pSecure,false),
    Base:set_property(pPort,""),
	Base:set_property(networkTime,0),
    Base:set_attribute(pStatus,"ERROR"),
    Base:set_attribute(pPostMetricsLast,0),
    Base:set_attribute(pPostMetricsFrequency,20),     
    {?MODULE,Base}.


update() ->
    {ok,{_,Url}} = THIS:get_property(pURL), 
    {ok,{_,Secure}} = THIS:get_property(pSecure), 
    {ok,{_,Proxy}} = THIS:get_property(pProxy), 
    {ok,{_,ProxyUser}} = THIS:get_property(pProxyUser), 
    {ok,{_,ProxyPassword}} = THIS:get_property(pProxyPassword),
    {ok,{_,User}} = THIS:get_property(pUser),    
    {ok,{_,Password}} = THIS:get_property(pPassword),
    {ok,{_,Timeout}} = THIS:get_property(pTimeout),
    {ok,{_,Xsl}} = THIS:get_property(pXSL),
    {ok,{_,TBrowse}} = THIS:get_property(browse),   
    %UpdateStartTime = currentTimeMillis(),    
    %PmResult = postMetrics(), %Bool  
    if length(TBrowse) > ?MaxCounters ->
        Browse = lists:sublist(TBrowse,1,?MaxCounters);
    true ->
        Browse = TBrowse
    end,
    %io:format("$$$~p~n",[Browse]),
    % if  length(Xsl)  == 0,XML is primary data
    case http_receive(Url) of
    {ok,_} ->    
        if length(Xsl) > 0 ->
            case file:read_file(Xsl) of
            {ok,Binary} ->
                Java_Node = siteview:get_java_node(),
                {ok,[{_,Response}]} = rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.XmlMonitor", "update_util", [{xml,Url},{xsl,binary_to_list(Binary)}]}),
                StatusString = update_util(xml_parser:parser({content,Response}),Browse),
                THIS:set_attribute(?STATE_STRING,StatusString);                         
            _ ->
                case http_receive:receive_asynchronous_data(Url,"","","","",30000) of
                {ok,Res} ->
                    StatusString = update_util(xml_parser:parser({content,Res}),Browse), 
                    THIS:set_attribute(?STATE_STRING,StatusString);                    
                _ ->
                    doUpdateError("Url Error!",true)
                end                    
            end;
        true ->    
            case http_receive:receive_asynchronous_data(Url,"","","","",30000) of
            {ok,Res} ->
                StatusString = update_util(xml_parser:parser({content,Res}),Browse), 
                THIS:set_attribute(?STATE_STRING,StatusString);                    
            _ ->
                doUpdateError("Url Error!",true)
            end                        
        end;
    {error,StatuNum} ->  
        Statu = textutils:lookupStatus(StatuNum),  
        doUpdateError(Statu,true)  
    end. 



update_util(List,Browse) ->
    THIS:set_attribute(pStatus,"ok"), 
    update_util_t(List,Browse,length(Browse),"").
update_util_t(_L,_B,0,E) -> E;    
update_util_t(Li,Brow,Num,En) ->
    [A|B] = Brow,
    {N,N2} = A,
    case lists:keysearch(N,1,Li) of
    {value,{_,V}} ->
        if V =:= [] ->     
            THIS:set_attribute(N,"n/a");
        true ->     
            THIS:set_attribute(N,V)
        end;
    _ ->
        V = "n/a",
        THIS:set_attribute(N,"n/a")
    end,    
    update_util_t(Li,B,Num-1,En ++ N ++"="++ V ++ ", <br>"). 


doUpdateError(ErrorStr,Bool) ->
    THIS:set_attribute(pStatus,"error"),
    THIS:set_attribute(?STATE_STRING,ErrorStr),
    {ok,{_,Browse}} = THIS:get_property(browse),
    if Bool ->
        doUpdateError_util(Browse),
        THIS:set_attribute(?NO_DATA,true),     
        THIS:set_attribute(?CATEGORY,?NO_DATA),
        THIS:set_attribute(countersInError,length(Browse));
    true ->
        nothing 
    end.
        

doUpdateError_util(Browse) ->    
    doUpdateError_util_t(Browse,length(Browse)).
doUpdateError_util_t(B,0) -> ok;
doUpdateError_util_t(Brow,Len) ->
    [A|B] = Brow,
    {Key,Value} = A,
    THIS:set_attribute(Key,"n/a"),
    doUpdateError_util_t(B,Len-1).    

getPostMetricURL() ->
    null.

isPostAllMetrics() ->
    false.

postMetrics() ->
    true.

isNeedPostMetricsError(String) ->
    false.

 
 
 
getBrowseData(Params) ->
    Url = proplists:get_value(pURL,Params),
    Xsl = proplists:get_value(pXSL,Params),
    if length(Xsl) > 0 ->
        case file:read_file(Xsl) of
        {ok,Binary} ->               
            case http_receive(Url) of
            {ok,_} ->
                {ok,[{_,Response}]} =transform(Url,binary_to_list(Binary)),
                make_tree(getBrowseData_util(xml_parser:parser({content,Response}))); 
            _ ->
                []  
            end; 
        _->
            case http_receive:receive_asynchronous_data(Url,"","","","",30000) of
            {ok,Response} ->
                make_tree(getBrowseData_util(xml_parser:parser({content,Response})));
            _ ->
                []
            end             
        end;        
    true ->
        case http_receive:receive_asynchronous_data(Url,"","","","",30000) of
        {ok,Response} ->
            make_tree(getBrowseData_util(xml_parser:parser({content,Response})));
        _ ->
            []
        end            
    end.
 


getBrowseData_util(List) ->
    io:format("List:~p~n",[List]),  
    getBrowseData_util_t(List,length(List),[]).
getBrowseData_util_t(_L,0,E) -> E;
getBrowseData_util_t(Li,Num,En) ->
    [A|B] = Li,
    {Key,Value} = A,
    getBrowseData_util_t(B,Num-1,[Key|En]).


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


%%send data to java node,receive return
transform(XmlUrl,Xsl) ->
    Java_Node = siteview:get_java_node(),
    rpc(?REG_NAME, Java_Node, {"com.dragonflow.erlangecc.monitor.XmlMonitor", "transform", [{xml,XmlUrl},{xsl,Xsl}]}).    


%%	-----------	remote process call java node 	----------------------%%
rpc(RegName, Node, Msg) ->
	THIS:set_attribute(?DEBUG_INFO, "remote process call java node ..."),
	Ping = net_adm:ping(Node),
%% 	io:format("ping java node,  regname: ~p, node name: ~p, ping state: ~p~n", [RegName, Node, Ping]),
	{RegName, Node} ! Msg,	
	receive          
		{ok, _, Ret} ->           
			{ok,Ret};
		{error, _From, Ret} ->
			THIS:set_attribute(?DEBUG_INFO, "response data form java node error! "),
			{error,Ret}	;
        _ ->
            {error,"undefined error!"}
     
	after ?RECEIVE_TIME_OUT ->
			case Ping of
				pong ->
					{error, "time is out. "};
				pang ->					
					{error, "Connect Java Node Error! "}                    
			end
	end.
    

getPort() ->
   {ok,{_,Secure}} = THIS:get_property(pSecure), 
    case Secure of
        true ->
            443;
        _ ->
            80
    end.



verify(Params)->
    Errs = 
	case proplists:get_value(pURL,Params) of
    ""->
		[{pURL,"xml url  missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{pURL,"no spaces are allowed"}]
	    end
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


defaultTitle(Params)->
	Url = proplists:get_value(pURL,Params),
	if
		length(Url)>0->
			BASE:defaultTitle(Params) ++":" ++ Url;
		true ->
			BASE:defaultTitle(Params)
	end.

getHostname()->
	case THIS:get_property(pHostname) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
%% @spec get_template_property() -> List
%% @doc overloading function. 
get_template_property() ->
    BASE:get_template_property() ++
    [
        %#property{name=pHostname,title="Hostname",type=text,editable=true,order=1,description="the name of the server"},
        #property{name=pURL,title="XML URL",type=text,editable=true,order=2,description="Enter the URL of the XML file that is the input for the transformation."},
        #property{name=pXSL,title="XSL File",type=text,editable=true,order=3,description="Enter the path to the XSL file you want to test. This path must be relative to SiteView root folder."},
        #property{name=pSecure,title="Secure Server",type=bool,editable=true,order=4,description="check here if server is secure"},
        #property{name=pProxy,title="Proxy Server",type=text,order=5,editable=true,description="the name of the proxy server server if needed"},        
        #property{name=pProxyUser,title="Proxy Username",type=text,order=6,editable=true,description="the username for the proxy server"},
        #property{name=pProxyPassword,title="Proxy Password",type=text,order=7,editable=true,description="the password for the proxy server"},
        #property{name=pUser,title="User",type=text,order=8,editable=true,description="the username for the URL"},
        #property{name=pPassword,title="Password",type=text,order=9,editable=true,description="the password for the URL"},
        #property{name=pTimeout,title="Timeout(Second)",type=numeric,default=30,order=10,editable=true,description="the time out, in seconds, to wait for the response",baselinable=true},        
        #property{name=pStatus,title="status",type=text,order=11,configurable=false,state=true}        
    ].

%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->  
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{pStatus,'==',"error"}]
			end;
get_classifier(warning)->
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end;	
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{pStatus,'<',"ok"}]
	end.


loop(RequestId,Data) ->
    receive
        {http, {RequestId, Result}} ->
            case Result of
            {error,String} ->
                {error,999};                
            {A,B,C} ->
                {Type,Code,Str} = A,
                http:cancel_request(RequestId),
                {error,Code};
            _ ->
                {error,-1}             
            end;    
        {http, {RequestId, stream_start, Headers}} ->
            loop(RequestId,Data);
        {http, {RequestId, stream, BinBodyPart}} ->
            loop(RequestId,Data ++ binary_to_list(BinBodyPart));
        {http, {RequestId, stream_end, Headers}} ->
            {ok,Data};
        {http, {RequestId, {error, Reason}}} ->
            http:cancel_request(RequestId),
            {error,-1};
        _ ->
            http:cancel_request(RequestId),
            {error,-1}       
    end.        
    
http_receive(Url) ->
        {ok,{_,Timeout}} = THIS:get_property(pTimeout), 
        case http:request(get,{Url,[]},[{timeout, Timeout*1000}],[{sync,false},{stream, self}]) of
        {ok,RequestId} ->
            loop(RequestId,"");              
        {error, Reason} ->
            {error, Reason}
        end.  

