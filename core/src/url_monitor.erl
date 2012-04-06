%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc URL Monitor.
%% 
%% Description: URL Monitor core function is to attempt to reach a specified Web page to verify that it can be retrieved, but it can also be used to do the following:
%% Check secure pages using SSL;
%% Check for specific content on the retrieved Web page;
%% Check the Web page for change;
%% Check for specific error messages;
%% Check the Web page for a value...
-module(url_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([verify/1,defaultTitle/1,update/0,new/0,getResults_URLAndImagesAndFrames/3,lookupStatus/1,getHostname/0,authOnFirstRequest/1,getLabels/1,getStateProperties/2,updateMatchValuesAndProperty/3,updateMatchValues/6,checkURL/11,checkURLRetrieveDoneHere/11,resolveURL/3,stripDotDot/1,stripDotSlash/1,getRequest/2,postPairs/3,processRequest/4,finalHTTPClientlRequestPreparation/4,isProxyExcluded/2,getHTMLEncoding/1,getContentType/1,getUserAgent/1,add_urlOtherHeadersTo_postData/2,getHeaderType/1,getScalarValues/2,get_classifier/1,get_template_property/0]).

-record(urlretrieve,{status=-991,totalDuration=0,lastModified=0,date=0,totalBytes=0,location="",refresh=false,errorMessage="",apachehttpmethod=null,body="",head=[],contentBuffer="",sessionBuffer=null,redirectBuffer=""}).

-define(NT_CHALLENGE_RESPONSE_TAG,"[NT]").
-define(CONTENT_TYPE_DEFAULT,"application/x-www-form-urlencoded").
-define(REQUEST_PROTOCOL_DEFAULT,"HTTP/").
-define(DEFAULT_MAX_REDIRECTS,10).
-define(CUSTOM_CONTENT,"Custom-Content: ").
-define(CUSTOM_CONTENT_TYPE,0).
-define(CUSTOM_HEADER,"Custom-Header: ").
-define(CUSTOM_HEADER_TYPE,1).
-define(CONTENT_TYPE_HEADER,"Content-Type: ").
-define(CONTENT_TYPE_HEADER_TYPE,2).
-define(HOST_HEADER,"Host: ").
-define(HOST_HEADER_TYPE,3).
-define(USER_AGENT_HEADER,"User-Agent: ").
-define(USER_AGENT_HEADER_TYPE,4).
-define(SET_COOKIE_HEADER,"Set-Cookie:").
-define(SET_COOKIE_HEADER_TYPE,5).
-define(METHOD_HEADER,"Method: ").
-define(METHOD_HEADER_TYPE,6).
-define(REQUEST_PROTOCOL_HEADER,"Protocol: ").
-define(REQUEST_PROTOCOL_HEADER_TYPE,7).
-define(ACTION_HEADER,"Action: ").
-define(ACTION_HEADER_TYPE,8).
-define(SSLGET_HEADER,"sslgetOptions: ").
-define(SSLGET_HEADER_TYPE,9).
-define(CRLF,"\r\n").
-define(DEFAULT_TIMEOUT,60000).

%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
verify(Params)->
	Errs =
	case proplists:get_value(urlEncoding,Params) of
		""->
			[];
		URLEncoding->
			S1 = "this is a test",
			iconv:start(),
			case iconv:convert("",URLEncoding,S1) of
				[] ->
					[{urlEncoding,"couldn't interpret encoding: " ++ URLEncoding}];
				_->
					[]
			end
	end ++
	case proplists:get_value(url,Params) of
		""->
			[{url,"url is missing"}];
		URL->
			case http_uri:parse(URL) of
				{error,_}->
					[{url,"format error"}];
				{Scheme,_,_,_,_,_}->
					Flag1 = (not ((Scheme=:= http) or (Scheme=:=https))),
					if
						Flag1->
							[{url,"only HTTP, HTTPS are currently supported"}];
						true ->
							[]
					end
			end
	end ++
	case proplists:get_value(contentMatch,Params) of
		""->
			[];
		ContentMatch ->
			N1 = string:str(ContentMatch,"/"),
			N2 = string:rstr(ContentMatch,"/"),
			if
				((N1=:=1) and (N1=/=N2))->
					S2 = string:sub_string(ContentMatch,2,N2-1),
					case re:compile(S2) of
						{ok,_}->
							[];
						{error,_} ->
							[{contentMatch,"error regular expression"}]
					end;
				true ->
					[]
			end
	end ++
	case proplists:get_value(proxy,Params) of
		""->
			[];
		Proxy ->
			case string:str(Proxy," ") of
				0->
					N3 = string:str(Proxy,":"),
					if
						N3=:=0->
							[{proxy,"missing port number in Proxy address"}];
						true ->
							[]
					end;
				_->
					[{proxy,"no space are allowed"}]
			end
	end ++
	case proplists:get_value(errorContent,Params) of
		""->
			[];
		ErrorContent ->
			N4 = string:str(ErrorContent,"/"),
			N5 = string:rstr(ErrorContent,"/"),
			if
				((N4=:=1) and (N5=/=N4))->
					S3 = string:sub_string(ErrorContent,2,N5-1),
					case re:compile(S3) of
						{ok,_}->
							[];
						{error,_} ->
							[{errorContent,"error regular expression"}]
					end;
				true ->
					[]
			end
	end++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.

%% @spec defaultTitle(Params) -> Title
%% where
%% Params = [{key,Vale}]
%% Key = string()
%% Value = string()
%% Title = string()
%% @doc Give monitor a default title.
defaultTitle(Params)->
	URL = proplists:get_value(url,Params),
	if
		length(URL)>0->
			BASE:defaultTitle(Params) ++":" ++ URL;
		true ->
			BASE:defaultTitle(Params)
	end.

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = atomic_monitor:new(),
    Obj:set_attribute(lastChecksum,0),
    Obj:set_attribute(checkContentResetTime,0),
    Obj:set_attribute(lastCheckContentTime,0),
    {?MODULE,Obj}.

%% @spec update() -> ok
%% @doc Run the monitor.
%%
%% Description: Using Proxy to request
update() ->
	{ok,{_,Proxy}} = THIS:get_property(proxy),
	{ok,{_,ProxyPassword}} = THIS:get_property(proxyPassword),
	{ok,{_,ProxyUserName}} = THIS:get_property(proxyUserName),
	getResults_URLAndImagesAndFrames(Proxy,ProxyPassword,ProxyUserName).

%% @spec getResults_URLAndImagesAndFrames(Proxy,ProxyPassword,ProxyUserName) -> ok
%% where 192.168.9.36 808
%% Proxy = string()
%% ProxyPassword = string()
%% ProxyUserName = string()
%% @doc The core function in url monitor,start inets application,prepare the params,send request,parse the response,check page change,content match,retrieve images,build the state string.
getResults_URLAndImagesAndFrames(Proxy,ProxyPassword,ProxyUserName) ->
    {ok,{_,Class}} = THIS:get_property(?CLASS),
	{ok,{_,Id}} = THIS:get_property(?ID),
    Profile = list_to_atom(atom_to_list(Class) ++ "-" ++ atom_to_list(Id)),
	THIS:set_attribute(profile,Profile),
    {ok,{_,Url}} = THIS:get_property(url),
    %%substitute() is Unavailable now
	URL = case httputils:isSubstituteExpression(Url) of
		true ->
			httputils:substitute(Url);
		_->
			Url
	end,
	{ok,{_,ContentMatch}} = THIS:get_property(contentMatch),
	{ok,{_,ErrorContent}} = THIS:get_property(errorContent),
	{ok,{_,UserName}} = THIS:get_property(userName),
	{ok,{_,Password}} = THIS:get_property(authorizationpassword),
	{ok,{_,Domain}} = THIS:get_property(domain),
	{ok,{_,WhenToAuthenticate}} = THIS:get_property(whenToAuthenticate),
	{ok,{_,Post}} = THIS:get_property(postData),
	PostData = string:tokens(Post,"\n"),
	{ok,{_,Time}} = THIS:get_property(timeout),
	Timeout = case Time of
		0->
			?DEFAULT_TIMEOUT;
		_->
			Time*1000
	end,
	{ok,{_,Retry}} = THIS:get_property(retries),
	Retries = if
		Retry>=11 ->
			10;
		true ->
			Retry
	end,
	{ok,{_,URLEncoding}} = THIS:get_property(urlEncoding),
	{ok,{_,EncodePostData}} = THIS:get_property(encodePostData),
	OtherHeader = "Pragma: No-Cache",
	URLContext = urlcontext:new(THIS),
    URLContext:setStreamEncoding(URLEncoding),
    URLContext:setRedirectBase(URL),
    URLContext:setEncodePostData(EncodePostData),
	Flag1 = authOnFirstRequest(WhenToAuthenticate),
    HTTPRequestSettings = httprequestsettings:new(URL,UserName,Password,Domain,Flag1,Proxy,ProxyUserName,ProxyPassword,null,Retries,0,0),
    HTTPRequestSettings:init(),
    URLResults = checkURL(HTTPRequestSettings,URLContext,ContentMatch,ErrorContent,PostData,"",50000,OtherHeader,Timeout,URL,null),
	
    Status = URLResults#urlresults.status,
    Body = URLResults#urlresults.body,
    Head = URLResults#urlresults.head,
    TotalDuration = URLResults#urlresults.totalDuration,
    TotalBytes = URLResults#urlresults.totalBytes,
    LastModified = URLResults#urlresults.lastModified,
    CurrentDate = URLResults#urlresults.currentDate,
    FinalURL = URLResults#urlresults.redirectBuffer,
    THIS:set_attribute(status,Status),
    THIS:set_attribute(body,Body),
    THIS:set_attribute(head,Head),
    THIS:set_attribute(totalDuration,TotalDuration),
    THIS:set_attribute(totalBytes,TotalBytes),
    THIS:set_attribute(lastModified,LastModified),
    THIS:set_attribute(currentDate,CurrentDate),
    {ok,{_,CheckContent}} = THIS:get_property(checkContent),
    if
        ((Status =:= 200) and (CheckContent=/="no content checking")) ->
            {ok,{_,LastChecksum}} = THIS:get_attribute(lastChecksum),
            Crc = erlang:crc32(Body),
            if
                ((LastChecksum>0) and (Crc =/= LastChecksum)) ->
                    THIS:set_attribute(status,?kURLContentChangedError);
                true ->
                    ok
            end,
            {ok,{_,CheckContentResetTime}} = THIS:get_attribute(checkContentResetTime),
            {ok,{_,LastCheckContentTime}} = THIS:get_attribute(lastCheckContentTime),
            Flag = if
                ((CheckContent =:= "baseline") and (CheckContentResetTime<LastCheckContentTime) and (LastChecksum>0)) ->
                    false;
                true ->
                    true
            end,
            if
                (Flag and CheckContent =:= "baseline") ->
                    THIS:set_attribute(status,200);
                true ->
                    ok
            end,
            S12 = if
                Flag ->
                    THIS:set_attribute(lastCheckContentTime,httputils:timeMillis()),
                    Crc;
                true ->
                    LastChecksum
            end;
        true ->
            S12 = 0
    end,
    S21 = case httputils:isValueExpression(ContentMatch) of
        true ->
            updateMatchValuesAndProperty(ContentMatch,Body,"Content Matched: ");
        _->
            ""
    end,
    SS21 = case httputils:isValueExpression(ErrorContent) of
        true ->
            if
                S21=/="" ->
                    S21++updateMatchValuesAndProperty(ContentMatch,Body,", Error Matched: ");
                true ->
                    S21++updateMatchValuesAndProperty(ContentMatch,Body,"Error Matched: ")
            end;
        _->
            S21
    end,
    {ok,{_,Status2}} = THIS:get_attribute(status),
    THIS:set_attribute(statusText,lookupStatus(Status2)),
    THIS:set_attribute(overallStatus,Status2),
    THIS:set_attribute(totalErrors,0),
    THIS:set_attribute(lastChecksum,S12),
    THIS:set_attribute(urlHeader,Head),
    THIS:set_attribute(host,getHostname()),
    if
        (Status2=:=200) ->
            S24 = httputils:floatToString(TotalDuration/1000,2)++"sec",
            S29 = if
                SS21 =/= "" ->
                    ", ";
                true ->
                    ""
            end,
			StateString = "status="++integer_to_list(Status2)++",downloadTime(s)="++S24++",filesize(bytes)="++integer_to_list(TotalBytes),
			THIS:set_attribute(?STATE_STRING,StateString),
            %~ THIS:set_attribute(?STATE_STRING,S24++S29++S21),
            THIS:set_attribute(roundTripTime,TotalDuration),
            THIS:set_attribute(size,TotalBytes);
        true ->
            S23 = if
                SS21 =:= "" ->
                    "";
                true ->
                    ", "
            end,
            SS23 = if
                URL=/=FinalURL ->
                    S23++lookupStatus(Status2)++", "++FinalURL;
                true ->
                    S23++lookupStatus(Status2)
            end,
            THIS:set_attribute(?NO_DATA, true),
            THIS:set_attribute(?STATE_STRING,SS23++" "++URLResults#urlresults.errorMessage),
            THIS:set_attribute(roundTripTime,"n/a"),
            THIS:set_attribute(size,"n/a")
    end.
            
loop1([],S22)->S22;
loop1([F|R],S22) ->
    case httputils:startsWithIgnoreCase(F,?CUSTOM_HEADER) of
        true ->
            loop1(R,S22++F++?CRLF);
        _->
            loop1(R,S22)
    end.

%% @spec lookupStatus(I) -> Status
%% where
%% I = integer()
%% Status = string()
%% @doc Get the status from statusmap,statusmap is defined in monitor.hrl.
lookupStatus(I) ->
    case proplists:get_value(I,?statusMapping) of
        undefined ->
            "unknown error ("++integer_to_list(I)++")";
        Obj ->
            Obj
    end.

%% @spec getHostname() -> HostName
%% where
%% HostName = string()
%% @doc Get hostname.
getHostname() ->
    {ok,{_,URL}} = THIS:get_property(url),
    case http_uri:parse(URL) of
        {_, _, Host, _, _, _} ->
            Host;
        {error,_}->
            URL
    end.

%% @spec authOnFirstRequest(Option) -> Flag
%% where
%% Option = string()
%% Flag = bool()
%% @doc Get the flag for when to auth.
authOnFirstRequest("Use Global Preference") ->null;
authOnFirstRequest("authOnFirst") ->true;
authOnFirstRequest("authOnSecond") ->false;
authOnFirstRequest(_) ->null.

%% @spec getLabels(ValueLabels) -> ok
%% where
%% ValueLabels = string()
%% @doc Put the Value Labels into list.
getLabels(ValueLabels) ->
    if
        ValueLabels=/="" ->
            As = string:tokens(ValueLabels,","),
            forlabels(As,1,[]);
        true ->
            THIS:set_attribute(labelsCache,[{"matchValue", "content match"},{"matchValue2", "second content match"},{"matchValue3", "third content match"},{"matchValue4", "fourth content match"},{"matchValue5", "fifth content match"},{"matchValue6", "sixth content match"},{"matchValue7", "seventh content match"},{"matchValue8", "eigth content match"},{"matchValue9", "ninth content match"},{"matchValue10", "tenth content match"}])
    end.

forlabels([],_,Map)->THIS:set_attribute(labelsCache,Map);
forlabels(_,11,Map)->THIS:set_attribute(labelsCache,Map);
forlabels([F|R],N,Map) ->
	if
		N=:=1 ->
			forlabels(R,N+1,Map++[{"matchValue",string:strip(F)}]);
		true ->
			forlabels(R,N+1,Map++[{"matchValue"++integer_to_list(N),string:strip(F)}])
	end.

string2val(Str)->
	case string:to_integer(Str) of
		{I,[]}->
			I;
		_->
			0
	end.

%% @spec getStateProperties(This,Params) -> ok
%% where
%% This = moduel()
%% Params = list()
%% @doc Reload the function,add appointed properties to state.
getStateProperties(This,Params) ->
	Temp = This:get_template_property(),
	T = [X || X<-Temp,X#property.state=:=true],
    ValueLabels = case proplists:get_value(valueLabels,Params) of
        undefined ->
            case This:get_property(valueLabels) of
                {ok,{_,V}} ->
                    V;
                _->
                    ""
            end;
        V->
           V
    end,
	This:getLabels(ValueLabels),
    R = case This:get_attribute(labelsCache) of
        {ok,{_,[]}} ->
            [];
        {ok,{_,Array}} ->
            buildStateProperty(Array)
    end,
	T++R.
    
buildStateProperty([])->[];
buildStateProperty([{K,V}|R]) ->
    Name = list_to_atom(K),
    [#property{name=Name,title=V,type=numeric,state=true,configurable=false}]++buildStateProperty(R).


%% @spec updateMatchValuesAndProperty(S,S1,S2) -> ok
%% where
%% S = string()
%% S1 = string()
%% S2 = string()
%% @doc Set value into additional state properties,Max number is 10.
updateMatchValuesAndProperty(S,S1,S2) ->
    SS = case THIS:get_property(valueLabels) of
        {ok,_} ->
            {_,NewS,NewArray} = httputils:matchExpression(S1,S,[],""),
            Len = length(NewArray),
            if
                Len>0 ->
                    THIS:set_attribute(matchValue,string2val(lists:nth(1,NewArray)));
                true ->
                    ok
            end,
            if
                Len>1 ->
                    THIS:set_attribute(matchValue2,string2val(lists:nth(2,NewArray)));
                true ->
                    ok
            end,
            if
                Len>2 ->
                    THIS:set_attribute(matchValue3,string2val(lists:nth(3,NewArray)));
                true ->
                    ok
            end,
            if
                Len>3 ->
                    THIS:set_attribute(matchValue4,string2val(lists:nth(4,NewArray)));
                true ->
                    ok
            end,
            if
                Len>4 ->
                    THIS:set_attribute(matchValue5,string2val(lists:nth(5,NewArray)));
                true ->
                    ok
            end,
            if
                Len>5 ->
                    THIS:set_attribute(matchValue6,string2val(lists:nth(6,NewArray)));
                true ->
                    ok
            end,
            if
                Len>6 ->
                    THIS:set_attribute(matchValue7,string2val(lists:nth(7,NewArray)));
                true ->
                    ok
            end,
            if
                Len>7 ->
                    THIS:set_attribute(matchValue8,string2val(lists:nth(8,NewArray)));
                true ->
                    ok
            end,
            if
                Len>8 ->
                    THIS:set_attribute(matchValue9,string2val(lists:nth(9,NewArray)));
                true ->
                    ok
            end,
            if
                Len>9 ->
                    THIS:set_attribute(matchValue10,string2val(lists:nth(10,NewArray)));
                true ->
                    ok
            end,
            NewS;
        _->
            {S3,Array} = updateMatchValues(S,S1,S2,false,[],150),
            if
                Array=/=[] ->
                    THIS:set_attribute(matchValue,string2val(lists:nth(1,Array)));
                true ->
                    ok
            end,
            S3
    end,
    SS.

%% @spec updateMatchValues(S,S1,S2,Flag,Array,I) -> {StateString,MatchValue}
%% where
%% S = string()
%% S1 = string()
%% S2 = string()
%% Flag = bool()
%% Array = list()
%% I = integer()
%% StateString = string()
%% MatchValue = list()
%% @doc Retrieve match values from regular expression.
updateMatchValues(S,S1,S2,Flag,Array,I) ->
	{_,Sb,Array1} = httputils:matchExpression(S1,S,Array,"",S2),
	F1 = string:len(Sb)>I,
	if
		(Flag and F1) ->
			{string:sub_string(Sb,1,I)++"...",Array1};
		true ->
			{Sb,Array1}
	end.

%% @spec checkURL(HTTPRequestSettings,URLContext,ContentMatch,ErrorContent,PostData,StringBuffer,L,OtherHeader,Timeout,URL,StringBuffer2) -> Result
%% where
%% HTTPRequestSettings = moduel()
%% URLContext = moduel()
%% ContentMatch = string()
%% ErrorContent = string()
%% PostData = list()
%% StringBuffer = string()
%% L = integer()
%% OtherHeader = string()
%% Timeout = integer()
%% URL = string()
%% StringBuffer2 = string()
%% Result = record()
%% @doc Interface for url monitor.
%%加入对这几个访问过程动态信息保存的支持,注意并不是所有时候都需要保存它们,需要注意当其为null的情况
checkURL(HTTPRequestSettings,URLContext,ContentMatch,ErrorContent,PostData,ContentBuffer,ContentMax,HeadersOnEveryRequest,Timeout,RedirectBuffer,SessionBuffer) ->
    Match = if
        ContentMatch=:=null ->
            "";
        true ->
            ContentMatch
    end,
    NewErrorContent = if
        ErrorContent=:=null ->
            "";
        true ->
            ErrorContent
    end,
    OtherHeader = if
        HeadersOnEveryRequest=:=null ->
            "";
        true ->
            HeadersOnEveryRequest
    end,
    NewRedirectBuffer = if
        RedirectBuffer=:=null ->
            "";
        true ->
            RedirectBuffer
    end,
    NewPostData = if
        PostData=:=null ->
            [];
        true ->
            PostData
    end,
    Post2 = add_urlOtherHeadersTo_postData(OtherHeader, NewPostData),
    case URLContext:getStreamEncoding() =:= "" of
        true ->
            URLContext:setStreamEncoding("GBK");
        _->
            ok
    end,
    URLContext:setRedirectBase(HTTPRequestSettings:getUrl()),
    %%checkURLRetrieveDoneHere(HTTPRequestSettings,URLContext,Match,NewErrorContent,Post2,ContentBuffer,ContentMax,0,Timeout+httputils:timeMillis(),NewRedirectBuffer,SessionBuffer).
    R = try checkURLRetrieveDoneHere(HTTPRequestSettings,URLContext,Match,NewErrorContent,Post2,ContentBuffer,ContentMax,0,Timeout+httputils:timeMillis(),NewRedirectBuffer,SessionBuffer) of
        Result->
            Result
    catch error:X->X,
    NewResult = #urlresults{},
    NewResult#urlresults{status=?kURLUnknownError}
    end,
    R.


%% @spec checkURL(browsable_urlcontent,SocketSession,URL,S1,S2,ProxyURL,ProxyUserName,ProxyPassword,Post,AuthUserName,AuthPassword,S8,StringBuffer,L,S9,I,J,StringBuffer1) -> Result
%% where
%% SocketSession = moduel()
%% URL = string()
%% S1 = string()
%% S2 = string()
%% ProxyURL = string()
%% ProxyUserName = string()
%% ProxyPassword = string()
%% Post = list()
%% AuthUserName = string()
%%AuthPassword = string()
%% S8 = string()
%% StringBuffer = string()
%% L = integer()
%% S9 = string()
%% I = integer()
%% J = integer()
%% StringBuffer1 = string()
%% Result = record()
%% @doc THE request interface function For the monitors who extend from browsable_urlcontent_base.
%%browsable_urlcontent,SocketSession,S3,null,null,ProxyURL,ProxyUserName,ProxyPassword,Array,AuthUserName,AuthPassword,null,Sb1,16#fffffffffffffff,null,0,J*1000,null
checkURL(browsable_urlcontent,SocketSession,URL,S1,S2,ProxyURL,ProxyUserName,ProxyPassword,Post,AuthUserName,AuthPassword,S8,StringBuffer,L,S9,I,J,StringBuffer1) ->
    Monitor = SocketSession:getMonitor(),
    URLContext = urlcontext:new(Monitor),
    if
        I>0 ->
            Monitor:set_propery(errorOnRedirect,true);
        true ->
            ok
    end,
    URLContext:setCookies(SocketSession:getCookie()),
    URLContext:setRefererURL(SocketSession:getRefererURL()),
    URLContext:setEncodePostData(SocketSession:getEncodePostData()),
    S10 = SocketSession:getDomain(),
    HTTPRequestSettings = httprequestsettings:new(URL,AuthUserName,AuthPassword,S10,null,ProxyURL,ProxyUserName,ProxyPassword,null,3,0,0),
    HTTPRequestSettings:init(),
    %%checkURL(HTTPRequestSettings,URLContext,S1,S2,Post,StringBuffer,L,S9,J,StringBuffer1,URL).
    Result = try checkURL(HTTPRequestSettings,URLContext,S1,S2,Post,StringBuffer,L,S9,J,StringBuffer1,URL) of
        R ->
            R
    catch
    error:X->X,
    #urlresults{status=-1000}
    end,
    SocketSession:setRefererURL(URLContext:getRefererURL()),
    Result.


%% @spec checkURLRetrieveDoneHere(HTTPRequestSettings,URLContext,ContentMatch,ErrorContent,Post,StringBuffer,L,I,Timeout,URL,StringBuffer2) -> Result
%% where
%% HTTPRequestSettings = moduel()
%% URLContext = moduel()
%% ContentMatch = string()
%% ErrorContent = string()
%% Post = list()
%% StringBuffer = string()
%% L = integer()
%% I = integer()
%% Timeout = integer()
%% URL = string()
%% StringBuffer2 = string()
%% Result = record()
%% @doc The main function for http request,support 30X redirect,refresh when pause is 0 second,parse cookies,check content match.
%%contentBuffer:最后请求的请求头,请求体,相应头,响应体；RedirectBuffer:最后的请求url(可能为最后跳转地址),sessionBuffer:整个请求过程的请求与响应过程(由于耗占大量空间一般为null,主要用于用于url sequence).
checkURLRetrieveDoneHere(Settings,URLContext,Match,ErrorContent,PostData,ContentBuffer,ContentMax,CurrentRedirect,TimedOut,RedirectBuffer,SessionBuffer) ->
    URLContext:addCookieParameters(PostData,Settings:getUrl()),
    ContentBuffer1 = if
        ContentBuffer=:=null ->
            "";
        true ->
            ContentBuffer
    end,
    ProxyTemp = Settings:getProxy(),
    URLRetrieve1 = try
    FinalPost = finalHTTPClientlRequestPreparation(URLContext,Settings,PostData,TimedOut),
    BeforeRequest = httputils:currentTimeMillis(),
    {Apachehttpmethod,RequestString,ErrorStr} = if
        FinalPost =:= [] ->
            getRequest(Settings,"");
        true ->
            postPairs(Settings,FinalPost,"")
    end,
    RoundTripEnd = httputils:currentTimeMillis() - BeforeRequest,
    Settings:setProxy(ProxyTemp),
    FirstStatus = Apachehttpmethod:getStatusCode(),
    T2 = TimedOut - httputils:currentTimeMillis(),
    Status = if
        T2=<0 ->
            ?kURLTimeoutError;
        true ->
            FirstStatus
    end,
    ContentBuffer2 = fillContentBuffer(ContentBuffer1,Apachehttpmethod,ContentMax,Settings,RequestString),
    SessionBuffer1 = if
        SessionBuffer=/=null ->
            if
                length(SessionBuffer)>0 ->
                    SessionBuffer++?CRLF++"SITEVIEW                                         BLANK LINE" ++ ?CRLF++ContentBuffer2;
                true ->
                    SessionBuffer++ContentBuffer2
            end;
        true ->
            SessionBuffer
    end,
    LastModified = Apachehttpmethod:getLastModified(),
    Date = Apachehttpmethod:getDate(),
    Location = Apachehttpmethod:getLocation(),
    ContentLength = case Apachehttpmethod:getContentLength() of
        -1 ->
            length(ContentBuffer2);
        CL ->
            CL
    end,
    Flag = Apachehttpmethod:getRefreshRedirect(),
    ErrorStr1 = if
        ErrorStr=/="" ->
            "status="++integer_to_list(Status)++" "++ErrorStr;
        true ->
            ""
    end,
    %~ io:format("retrieve params lists:~p,~p,~p,~p,~p,~p,~p,~p~n",[Status,L3,L8,L9,L10,S3,Flag,S2]),
    #urlretrieve{status=Status,
                    totalDuration=RoundTripEnd,
                    lastModified=LastModified,
                    date=Date,
                    totalBytes=ContentLength,
                    location=Location,
                    refresh=Flag,
                    errorMessage=ErrorStr1,
                    apachehttpmethod=Apachehttpmethod,
                    head = Apachehttpmethod:getResponseHeaders(),
                    body = Apachehttpmethod:getResponseBody(),
                    contentBuffer=ContentBuffer2,
                    redirectBuffer=RedirectBuffer,
                    sessionBuffer=SessionBuffer1}
    catch
    throw:X->X,
    Class = case THIS:get_property(?CLASS) of
        {ok,{_,Cla}} ->
            Cla;
        _->
            null
    end,
    S6 = atom_to_list(Class)++httputils:exception_to_String(X),
    %%io:format("url error, "++Settings:getUrl()++", "++S6++"~n"),
    case string:str(httputils:exception_to_String(X),"timed out") of
        0 ->
            Status1 = ?kURLNoStatusError,
            SS2 = integer_to_list(?kURLNoStatusError)++" "++httputils:exception_to_String(X);
        _->
            Status1 = ?kURLTimeoutError,
            SS2 = ""
    end,
    #urlretrieve{status=Status1,errorMessage=SS2}
    end,
    Status2 = if
        ((URLRetrieve1#urlretrieve.status>200) and (URLRetrieve1#urlretrieve.status<300)) ->
            200;
        true ->
            URLRetrieve1#urlretrieve.status
    end,
    Apachehttpmethod1 = URLRetrieve1#urlretrieve.apachehttpmethod,
    URLContext:updateCookies(Apachehttpmethod1:getResponseHeaders(),Settings:getUrl()),
    if
        Status2=:=301 orelse Status2=:=302 orelse Status2=:=303 orelse Status2=:=307 orelse URLRetrieve1#urlretrieve.refresh ->
            if
                URLRetrieve1#urlretrieve.location=/=null orelse URLRetrieve1#urlretrieve.location=/="" ->
                %%这个location包括两个,一个是响应头里的location,一个是html里当refresh时间为0时的refresh url
                    Location1 = httputils:unescapeHTML(URLRetrieve1#urlretrieve.location),
                    Location2 = resolveURL(Location1,URLContext:getRedirectBase(),""),
                    DEFAULT_MAX_REDIRECTS = 10,
                    %%对于没有error on redirect的监测器,默认不选中
                    NORedirect = case THIS:get_property(errorOnRedirect) of
                        {ok,{_,NR}} ->
                            NR;
                        _->
                            false
                    end,
                    if
                        ((CurrentRedirect=<DEFAULT_MAX_REDIRECTS) and (not NORedirect)) ->
                            %%io:format("URLMonitor.checkURLRetrieveDoneHere: redirect=~p~n",[Location2]),
                            RedirectBuffer1 = if
                                RedirectBuffer=/=null ->
                                    Location2;
                                true ->
                                    RedirectBuffer
                            end,
                            Settings:setUrl(Location2),
                            URLContext:setRedirectBase(Location2),
                            URLContext:setRefererURL(Location2),
                            %%重置contentBuffer
                            URLResults1 = checkURLRetrieveDoneHere(Settings,URLContext,Match,ErrorContent,[],"",ContentMax,CurrentRedirect+1,TimedOut,RedirectBuffer1,URLRetrieve1#urlretrieve.sessionBuffer),
                            RedirectStatus = URLResults1#urlresults.status,
                            RedirectHead = URLResults1#urlresults.head,
                            RedirectBody = URLResults1#urlresults.body,
                            RedirectDuration = URLResults1#urlresults.totalDuration+URLRetrieve1#urlretrieve.totalDuration,
                            RedirectBytes = URLResults1#urlresults.totalBytes+URLRetrieve1#urlretrieve.totalBytes,
                            RedirectlastModified = URLResults1#urlresults.lastModified,
                            RedirectlastDate = URLResults1#urlresults.currentDate,
                            %%io:format("retrieve is:~p,~p,~p,~p,~p~n",[RedirectStatus,RedirectDuration,RedirectBytes,RedirectlastModified,RedirectlastDate]),
                            URLRetrieve2 = #urlretrieve{status=RedirectStatus,
                                            totalDuration=RedirectDuration,
                                            lastModified=RedirectlastModified,
                                            date=RedirectlastDate,
                                            totalBytes=RedirectBytes,
                                            head = RedirectHead,
                                            body = RedirectBody,
                                            errorMessage = URLResults1#urlresults.errorMessage,
                                            contentBuffer=URLResults1#urlresults.contentBuffer,
                                            sessionBuffer=URLResults1#urlresults.sessionBuffer,
                                            redirectBuffer=URLResults1#urlresults.redirectBuffer
                                            };
                        true ->
                            URLRetrieve2=URLRetrieve1
                    end;
                true ->
                    URLRetrieve2=URLRetrieve1
            end;
        true ->
            Apachehttpmethod2 = URLRetrieve1#urlretrieve.apachehttpmethod,
            Head = Apachehttpmethod2:getResponseHeaders(),
            Responsebody = Apachehttpmethod2:getResponseBody(),
            Status3 = URLRetrieve1#urlretrieve.status,
            Status4 = if
                ((Status3=:=200) and (ErrorContent=/=""))->
                    MR1 = httputils:matchExpression(Responsebody,ErrorContent),
                    MR2 = case element(1,MR1) of
                        200 ->
                            200;
                        _->
                            S8 = getHTMLEncoding(Head,Responsebody),
                            element(1,httputils:matchExpression(Responsebody,iconv:convert("utf-8", S8, ErrorContent)))
                    end,
                    if
                        MR2=:=200->
                            ?kURLContentErrorFound;
                        true ->
                            Status3
                    end;
                true ->
                    Status3
            end,
            Status5 = if
                ((Status4=:=200) and (Match=/=""))->
                    MR3 = httputils:matchExpression(Responsebody,Match),
                    MR4 = case element(1,MR3) of
                        200->
                            200;
                        _->
                            S7 = getHTMLEncoding(Head,Responsebody),
                            element(1,httputils:matchExpression(Responsebody,iconv:convert("utf-8", S7, Match)))
                    end,
                    MR4;
                true ->
                    Status4
            end,
            URLRetrieve2 = URLRetrieve1#urlretrieve{status=Status5}
    end,
    URLResults = #urlresults{
                    status = URLRetrieve2#urlretrieve.status,
                    totalDuration = URLRetrieve2#urlretrieve.totalDuration,
                    totalBytes = URLRetrieve2#urlretrieve.totalBytes,
                    lastModified = URLRetrieve2#urlretrieve.lastModified,
                    currentDate = URLRetrieve2#urlretrieve.date,
                    body = URLRetrieve2#urlretrieve.body,
                    head = URLRetrieve2#urlretrieve.head,
                    errorMessage = URLRetrieve2#urlretrieve.errorMessage,
                    contentBuffer=URLRetrieve2#urlretrieve.contentBuffer,
                    sessionBuffer=URLRetrieve2#urlretrieve.sessionBuffer,
                    redirectBuffer=URLRetrieve2#urlretrieve.redirectBuffer
                    },
    URLResults.

%%不对contentbuffer的长度做限制
fillContentBuffer(ContentBuffer,ApacheHttpMethod,Max,HTTPRequestSettings,RequestString) ->
    ContentBuffer1 = if
        ContentBuffer=:=null ->
            "";
        true ->
            ContentBuffer
    end,
    S1 = ContentBuffer1++"SITEVIEW HTTP REQUEST HTTP REQUEST HTTP REQUEST HTTP REQUEST HTTP REQUEST HTTP REQUEST",
    S2 = S1++"\r\n",
    S3 = S2++RequestString ++ ?CRLF,
    S4 = S3++"SITEVIEW                                         BLANK LINE" ++ ?CRLF,
    S5 = S4++"SITEVIEW HTTP RESPONSE HEADERS",
    S6 = S5++?CRLF,
    S7 = S6++ApacheHttpMethod:getResponseHeadersAsString()++?CRLF,
    S8 = S7++"SITEVIEW HTTP RESPONSE BODY",
    S9 = S8++?CRLF++?CRLF,
    S9++ApacheHttpMethod:getResponseBody().

%% @spec resolveURL(S,URLinfo,S1) -> NewURL
%% where
%% S = string()
%% URLinfo = string()
%% S1 = string()
%% NewURL = string()
%% @doc Parse redirect URI.
resolveURL(S,URLinfo,S1) ->
    {Scheme, _, Host, Port, _, _} = http_uri:parse(URLinfo),
    SS = string:strip(httputils:removeChars(S,"\n\r")),
    S2 = if
        Port=:=80 ->
            "";
        true ->
            ":"++integer_to_list(Port)
    end,
    I = string:str(SS,":"),
    if
        I=/=0 ->
            S3 = string:sub_string(SS,1,I-1),
            S4 = string:strip(S3),
            Flag = case httputils:onlyChars(S4,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") of
                true ->
                    false;
                _->
                    true
            end,
            Index = (not httputils:startsWith(SS,S3++"://")),
            Flag1 = if
                ((S4=:="http" orelse S4=:="https") and Index) ->
                    true;
                true ->
                    Flag
            end,
            SS1 = SS;
        true ->
            case httputils:startsWith(SS,"//") of
                true ->
                    Flag1 = false,
                    SS1 = atom_to_list(Scheme)++":"++SS;
                _->
                    Flag1 = true,
                    SS1 = SS
            end
    end,
    %% flag=flag1,s=ss1
    %%
    if
        Flag1 ->
            Index2 = httputils:startsWithIgnoreCase(SS1,"http:") orelse httputils:startsWithIgnoreCase(SS1,"https:"),
            SS2 = if
                Index2 ->
                    J=string:str(SS1,":"),
                    string:sub_string(SS1,J+1,string:len(SS1));
                true ->
                    SS1
            end,
            case httputils:startsWith(SS2,"/") of
                true ->
                    SS3 = if
                        S1=/="" ->
                            AS1 = case httputils:endsWith(S1,"/") of
                                false ->
                                    S1++"/";
                                _->
                                    S1
                            end,
                            {Scheme1, _, Host1, Port1, _, _} = http_uri:parse(AS1),
                            S5=if
                                Port1=:=80 ->
                                    "";
                                true ->
                                    ":"++integer_to_list(Port1)
                            end,
                            atom_to_list(Scheme1)++"://"++Host1++S5++SS2;
                        true ->
                            atom_to_list(Scheme)++"://"++Host++S2++SS2
                    end;
                _->
                    if
                        S1=:="" ->
                            NewS1 = getFile(URLinfo),
                            K = string:rstr(NewS1,"/"),
                            L = string:str(NewS1,"?"),
                            if
                                ((L=/=0) and (K>L)) ->
                                    K1 = string:rstr(string:sub_string(NewS1,L+1,string:len(NewS1)),"/");
                                true ->
                                    K1=K
                            end,
                            Len = string:len(NewS1),
                            String1 = if
                                ((K1=/=0) and (K1<Len)) ->
                                    string:sub_string(NewS1,1,K1);
                                true ->
                                    NewS1
                            end,
                            String2 = case httputils:endsWith(String1,"/") of
                                false ->
                                    String1++"/";
                                _->
                                    String1
                            end,
                            SS3 = atom_to_list(Scheme)++"://"++Host++S2++String2++SS2;
                        true ->
                            Sb1 = case httputils:endsWith(S1,"/") of
                                false ->
                                    S1++"/";
                                _->
                                    S1
                            end,
                            SS3 = Sb1++SS2
                    end
            end;
        true ->
            SS3=SS1
    end,
    NS = stripDotDot(SS3),
    NS1 = stripDotSlash(NS),
    NS2 = string:strip(httputils:removeChars(NS1,"\n\r")),
    %%io:format("new url is:~p~n",[NS2]),
    NS2.

%% @spec stripDotDot(S) -> NewURL
%% where
%% S = string()
%% NewURL = string()
%% @doc Strip ".." in URI.
stripDotDot(S) ->
    I = string:rstr(S,"?"),
    J = string:str(S,".."),
    if
        ((J=:=0) or (I=/=0) and (J>I)) ->
            S;
        true ->
            L = J-2,
            K = for5(S,L,0),
            if
                K=:=0 ->
                    S;
                true ->
                    S1 = case httputils:endsWith(string:sub_string(S,1,K),":/") of
                        true ->
                            string:sub_string(S,1,J-1)++string:sub_string(S,J+3,string:len(S));
                        _->
                            string:sub_string(S,1,K-1)++string:sub_string(J+2)
                    end,
                    stripDotDot(S1)
            end
    end.

%% @spec stripDotSlash(S) -> NewURL
%% where
%% S = string()
%% NewURL = string()
%% @doc Strip "./" in URI.
stripDotSlash(S)->
    I = string:str(S,"//"),
    if
        I=:=0 ->
            S;
        true ->
            J = httputils:indexOf(S,"/",I+2),
            if
                J=:=0 ->
                    S;
                true ->
                    for6(S,J)
            end
    end.

%% @spec getRequest(HTTPRequestSettings,StringBuffer) -> {Apachehttpmethod,StateString}
%% where
%% HTTPRequestSettings = module()
%% StringBuffer = string()
%% Apachehttpmethod = module()
%% StateString = string
%% @doc Do get method.
getRequest(HTTPRequestSettings,StringBuffer)->
    {_T,RequestString,StateString,R} = processRequest(get,"",HTTPRequestSettings,StringBuffer),
    {apachehttpmethod:new(R),RequestString,StateString}.

%% @spec postPairs(HTTPRequestSettings,Vector,StringBuffer) -> {Apachehttpmethod,StateString}
%% where
%% HTTPRequestSettings = module()
%% Vector = list()
%% StringBuffer = string()
%% Apachehttpmethod = module()
%% StateString = string
%% @doc Do post method.
postPairs(HTTPRequestSettings,Vector,StringBuffer) ->
    %%EncodePostData = HTTPRequestSettings:getEncodePostData(),
    %%Body = if
        %%EncodePostData ->
            %%ibrowse_lib:url_encode(reformatPostData(Vector));
        %%true ->
    Body = reformatPostData(Vector),
    %%end,
    {_,RequestString,StateString,R} = processRequest(post,Body,HTTPRequestSettings,StringBuffer),
    {apachehttpmethod:new(R),RequestString,StateString}.

reformatPostData(Post)->
    {S,Left} = parseStringPost(Post,"",Post),
    Post1 = if
        S=/="" ->
            S;
        true ->
            ""
    end,
    Post2 = if
        Left=/=[] ->
            buildNameValuePair(Left);
        true ->
            Post1
    end,
    Post2.
    
parseStringPost([],S,Left)->{S,Left};
parseStringPost([F|R],S,Left)->
    case is_list(F) of
        true ->
            parseStringPost(R,S++F,lists:delete(F,Left));
        _->
            parseStringPost(R,S,Left)
    end.

buildNameValuePair([])->"";
buildNameValuePair([{K,V}|R])->
    Dot = if
        R=/=[] ->
            "&";
        true ->
            ""
    end,
    K++"="++V++Dot++buildNameValuePair(R);
buildNameValuePair([_|R])->
    buildNameValuePair(R).
    
            

%% @spec processRequest(Method,Body,HTTPRequestSettings,StringBuffer) -> Response
%% where
%% Method = atom()
%% Body = string()
%% HTTPRequestSettings = module()
%% StringBuffer = string()
%% Response = httpresponse()
%% @doc Do http request.
processRequest(Method,Body,HTTPRequestSettings,StringBuffer)->
    {ok,{_,Profile}} = THIS:get_attribute(profile),
    inets:start(httpc, [{profile, Profile}]),
    HTTP11 = case HTTPRequestSettings:getHttp11() of
        true ->
            "HTTP/1.1";
        _ ->
            "HTTP/1.0"
    end,
    Proxy = HTTPRequestSettings:getProxy(),
    SetOption = if
        ((Proxy=/=null) and (Proxy=/="")) ->
            ProxyHost = HTTPRequestSettings:getProxyHost(),
            ProxyPort = list_to_integer(HTTPRequestSettings:getProxyPort()),
            [{proxy,{{ProxyHost,ProxyPort},[]}}];
        true ->
            []
    end,
    httpc:set_options(SetOption,Profile),
    ProxyUserName = HTTPRequestSettings:getProxyUserName(),
    ProxyAuth = if
        ((ProxyUserName=/=null) and (ProxyUserName=/=""))->
            [proxy_auth,{ProxyUserName,HTTPRequestSettings:getProxyPassword()}];
        true ->
            []
    end,
    Cookie = HTTPRequestSettings:getCookies(),
    Timeout = HTTPRequestSettings:getRequestTimeoutMS(),
    AuthUserName = HTTPRequestSettings:getAuthUserName(),
    AuthPassword = HTTPRequestSettings:getAuthPassword(),
    Headers = HTTPRequestSettings:getHeaders()++[{"Cookie",Cookie}],
    HTTPOptions = [{timeout,Timeout},{autoredirect,false},{relaxed, true},{version,HTTP11}]++ProxyAuth,
    Option = [],
    URL = HTTPRequestSettings:getUrl(),
    Scheme = HTTPRequestSettings:getScheme(),
    if
        Scheme=:=https ->
            application:start(ssl);
        true ->
            ok
    end,
    Response = try retry(Headers,HTTPOptions,Option,URL,Profile,AuthUserName,AuthPassword,Method,Body,SetOption) of
        {1,RequestString,R}->
            {ok,{{_,Status,StatusLine},_,_}} = R,
            if
                Status=:=200 ->
                    Flag = true,
                    StateString = StringBuffer;
                true ->
                    Flag = false,
                    StateString = StringBuffer++" ApacheHttpUtils: Request failed. URL: "++URL++", status code: "++integer_to_list(Status)++" : , status string: "++StatusLine
            end,
            {Flag,RequestString,StateString,R};
        {_,RequestString,R}->
            ErrorStatus = case R of
                {error,Error} ->
                    inetErrorCodeStatus(Error);
                _ ->
                    ""
            end,
            {false,RequestString,StringBuffer++ErrorStatus,R}
    catch
        error:X->X,
        %%io:format("error reason is:~p~n",[X]),
        {false,""," ApacheHttpUtils: Failed to process request",null}
    end,
    Response.
    
retry(_,_,_,"http://",_,_,_,_,_,_) ->  %%针对http在访问http://时出现长时间等待的问题
    {-1,"",{error,"http://"}};
retry(Headers,HTTPOptions,Option,URL,Profile,AuthUserName,AuthPassword,Method,Body,SetOption) ->
    RR = if
        Method==get ->
            httpc:request(Method,{URL,Headers},HTTPOptions,Option,Profile);
        true ->
            httpc:request(Method,{URL,Headers,"application/x-www-form-urlencoded",Body},HTTPOptions,Option,Profile)
    end,	
    case RR of
        {ok,{{_,401,_},Responsehead,_}}->
            AuthResponse = string:strip(proplists:get_value("www-authenticate",Responsehead,"")),
            AuthRequest = case httputils:startsWithIgnoreCase(AuthResponse,"basic") of
                0 ->
                    url_auth:digest_auth(AuthUserName,AuthPassword,Responsehead,URL,Method);   
                _->
                    url_auth:basic_auth(AuthUserName,AuthPassword)
            end,
            RequestString = getRequestString(Method,URL,Headers++AuthRequest,HTTPOptions,Option,Body,SetOption),
            {Sta,NR} = doauth(AuthRequest,Method,URL,Headers,HTTPOptions,Option,Profile,Body),
            {Sta,RequestString,NR};
        {error,Error}->
            RequestString = getRequestString(Method,URL,Headers,HTTPOptions,Option,Body,SetOption),
            {-1,RequestString,RR};
        _->
			RequestString = getRequestString(Method,URL,Headers,HTTPOptions,Option,Body,SetOption),
            {1,RequestString,RR}
   end.

doauth(AuthRequest,Method,URL,Headers,HTTPOptions,Option,Profile,Body)->
    RR = if
        Method==get ->
            httpc:request(Method,{URL,Headers++AuthRequest},HTTPOptions,Option,Profile);
        true ->
            httpc:request(Method,{URL,Headers++AuthRequest,"application/x-www-form-urlencoded",Body},HTTPOptions,Option,Profile)
    end,
    case RR of
        {error,_}->
            {-1,RR};
        _->
            {1,RR}
    end.
    
getRequestString(Method,URL,Headers,HTTPOptions,Option,Body,SetOption)->
    Version = proplists:get_value(version,HTTPOptions,"HTTP/1.1"),
    case http_uri:parse(URL) of
        {error,_}->
            Host = "",
            Path = "",
            Query = "";
        {_, _, Host, _, Path, Query} ->
            ok
    end,
    {NewHeaders, Uri} = case proplists:get_value(proxy,SetOption) of
        undefined ->
            {Headers,Path ++ Query};
        _->
			case proplists:get_value(proxy_auth,HTTPOptions) of
                {User, Password} ->
                    UserPasswd = base64:encode_to_string(User ++ ":" ++ Password),
                    {Headers++[{"proxy-authorization","Basic " ++ UserPasswd}],Path ++ Query};
				_ -> {Headers,Path ++ Query}
            end
    end,
    ContentLength = length(Body),
    NewHeaders1 = NewHeaders++[{"content-length",integer_to_list(ContentLength)},{"host",Host}],
    FinalHeaders = http_headers(NewHeaders1,[]),
    Message = [method(Method), " ", Uri, " ", 
	       version(Version), ?CRLF, headers(FinalHeaders, Version), ?CRLF, Body],
   lists:append(Message).

http_headers([], Headers) ->
    Headers;
http_headers([{Key,Value} | Rest], Headers) ->
    Header = Key ++ ": " ++ Value ++ ?CRLF,
    http_headers(Rest, Headers++Header).
    
method(Method) ->
    http_util:to_upper(atom_to_list(Method)).

version("HTTP/0.9") ->
    "";
version(Version) ->
    Version.
    
headers(_, "HTTP/0.9") ->
    "";
%% HTTP 1.1 headers not present in HTTP 1.0 should be
%% consider as unknown extension headers that should be
%% ignored. 
headers(Headers, _) ->
    Headers.

%% @spec finalHTTPClientlRequestPreparation(URLContext,HTTPRequestSettings,PostData,Timeout) -> Vector
%% where
%% URLContext = module()
%% HTTPRequestSettings = module()
%% PostData = list()
%% Timeout = integer()
%% Vector = list()
%% @doc Built request head and cookie.
finalHTTPClientlRequestPreparation(URLContext,HTTPRequestSettings,PostData,Timeout)->
    AuthUserName = HTTPRequestSettings:getAuthUserName(),
    if
        AuthUserName=:=null ->
            HTTPRequestSettings:setAuthUserName("");
        true ->
            ok
    end,
    AuthenticationOnFirstRequest = HTTPRequestSettings:getAuthenticationOnFirstRequest(),
    if
        AuthenticationOnFirstRequest=:=null ->
            HTTPRequestSettings:setAuthenticationOnFirstRequest(authOnFirstRequest("authOnFirst"));
        true ->
            ok
    end,
	{CurrentTime,_} = statistics(wall_clock),
	L1 = Timeout - CurrentTime,
    if
        L1=<0 ->
            throw("time out");
        true ->
            ok
    end,
    HTTPRequestSettings:setConnectionTimeoutMS(L1),
    HTTPRequestSettings:setRequestTimeoutMS(L1),
    S2 = "",
    S3 = HTTPRequestSettings:getHost(),
    Proxy = HTTPRequestSettings:getProxy(),
    IsProxyExcluded = isProxyExcluded(S2,S3),
    F =  ((httputils:len(Proxy)>0) and IsProxyExcluded),
    if
        F ->
            HTTPRequestSettings:setProxy("");
        true ->
            ok
    end,
    THIS:set_attribute(headers,[]),
	S4 = case getUserAgent(PostData) of
		""->
			"Mozilla/4.0 (compatible; MSIE 4.01; Windows NT)";
		Ua ->
			Ua
	end,
	{ok,{_,H1}} = THIS:get_attribute(headers),
	THIS:set_attribute(headers,H1++[{"User-Agent",S4}]),
	for(PostData),
    Vector1 = URLContext:getCookieHeader(HTTPRequestSettings:getUrl()),
    if
        (Vector1=/=null)->
            %%io:format("URLMonitor.checkURLRetrieveDoneHere: COOKIE ADD TO HEADERS: "++"~p~n",[Vector1]),
            HTTPRequestSettings:setCookies(Vector1);
        true ->
            %%io:format("URLMonitor.checkURLRetrieveDoneHere: COOKIE ADD TO HEADERS: NULL - NONE TO ADD ~n"),
            ok
    end,
	{ok,{_,H2}} = THIS:get_attribute(headers),
	S6 = URLContext:getStreamEncoding(),
	if
        S6=/="" ->
			THIS:set_attribute(headers,H2++[{"Accept-charset",S6}]);
        true ->
            ok
	end,
    {ok,{_,H3}} = THIS:get_attribute(headers),
    THIS:set_attribute(headers,H3++[{"Accept","*/*"}]),
	V2 = prepareParametersForApache(PostData,[]),
	if
		V2=/=[] ->
			S8 = case S6 of
				""->
					"";
				_->
					"; charset=" ++ S6
			end,
			S9 = getContentType(PostData),
			{ok,{_,H4}} = THIS:get_attribute(headers),
			THIS:set_attribute(headers,H4++[{"Content-Type",S9++S8}]);
		true ->
			ok
	end,
    HTTPRequestSettings:setAcceptAllUntrustedCerts(true),
    {ok,{_,H5}} = THIS:get_attribute(headers),
    HTTPRequestSettings:setHeaders(H5),
    S10 = URLContext:getEncodePostData(),
    F1 = ((S10 =:= "contentTypeUrlencoded") orelse (S10 =:= "")),
    F2 = S10 =:= "forceEncode",
    F3 = S10 =:= "forceNoEncode",
    if
        F1 ->
            Vector3 = HTTPRequestSettings:getHeaders(),
            HTTPRequestSettings:setEncodePostData(false),
            for1(Vector3,HTTPRequestSettings);
        F2 ->
            HTTPRequestSettings:setEncodePostData(true);
        F3 ->
            HTTPRequestSettings:setEncodePostData(false);
        true ->
            ok
    end,
    setClientSideCertSettings(HTTPRequestSettings,URLContext),
    V2.

%%暂时没有建立ssl 证书的文件夹
setClientSideCertSettings(_,_)->ok.

%% @spec isProxyExcluded(S,S1) -> Flag
%% where
%% S = string()
%% S1 = string()
%% Flag = bool()
%% @doc Check proxy.
isProxyExcluded(S,S1) ->
    if
        S=/="" ->
            As = string:tokens(S,","),
            for2(As,S1);
        true ->
            false
    end.

%% @spec getHTMLEncoding(Head) -> Encode
%% where
%% Head = list()
%% Encode = string()
%% @doc Get HTML response encode.
getHTMLEncoding(Head) ->
    getHTMLEncoding(Head,"").
    
getHTMLEncoding([],Body)->getPageEncoding(Body);
getHTMLEncoding([{"content-type",V}|R],Body)->
    I = string:str(V,"charset="),
    if
        I=/=0 ->
            string:sub_string(V,I+string:len("charset="),string:len(V));
        true ->
            getHTMLEncoding(R,Body)
    end;
getHTMLEncoding([_|R],Body)->
    getHTMLEncoding(R,Body).
    
%如果http头中没有关于编码的内容需要到网页中寻找
getPageEncoding(Body) ->
    Tree = htmltagparser:process(Body),
    Metas = htmltagparser:findTags(Tree,meta),
    find_encode(Metas).
    
find_encode([]) ->"GBK";
find_encode([F|R]) ->
    Con = string:to_lower(htmltagparser:getValue(F,content)),
    Index = string:str(Con,"charset="),
    if
        Index/=0 ->
            string:sub_string(Con,Index+string:len("charset="));
        true ->
            find_encode(R)
    end.

%% @spec getContentType(Post) -> ContentType
%% where
%% Post = list()
%% ContentType = string()
%% @doc Get ContentType in post data.
getContentType(Array) ->
    case for4(Array) of
        ?CONTENT_TYPE_DEFAULT ->
            ?CONTENT_TYPE_DEFAULT;
        Others ->
            string:sub_string(Others,string:len(?CONTENT_TYPE_HEADER)+1,string:len(Others))
    end.

%% @spec getUserAgent(Post) -> UserAgent
%% where
%% Post = list()
%% UserAgent = string()
%% @doc Get UserAgent in post data.
getUserAgent([]) ->"";
getUserAgent([F|R]) ->
	case httputils:startsWithIgnoreCase(F,"User-Agent: ") of
		true ->
			string:sub_string(F,string:len("User-Agent: ")+1,string:len(F));
		_->
			getUserAgent(R)
	end.

%% @spec add_urlOtherHeadersTo_postData(S,Post) -> NewPost
%% where
%% S = string()
%% Post = list()
%% NewPost = list()
%% @doc Add S into post data.
add_urlOtherHeadersTo_postData("",Array) ->Array;
add_urlOtherHeadersTo_postData(S,Array) ->
	A = string:tokens(S,?CRLF),
	Array1 = for3(A),
	Array++Array1.

getFile(URL) ->
	I = string:str(URL,":"),
	J = string:len(":"),
	if
		I=/=0 ->
			Per = string:substr(URL,I+1,2),
			J1 = if
				Per=:="//" ->
					3;
				true ->
					J
			end,
			S = string:sub_string(URL,I+J1,string:len(URL)),
			if
				J1=:=3 ->
					K = string:str(S,"/"),
					if
						K=/=0 ->
							string:sub_string(S,K,string:len(S));
						true ->
							"/"
					end;
				true ->
					S
			end;
		true ->
			""
	end.

for([])->ok;
for([F|R]) ->
	case httputils:startsWithIgnoreCase(F,?CUSTOM_HEADER) of
		true ->
			S = string:sub_string(F,string:len(?CUSTOM_HEADER)+1,string:len(F)),
			N = string:str(S,":"),
            {ok,{_,H}} = THIS:get_attribute(headers),
			if
				N=:=0->
					THIS:set_attribute(headers,H++[{S,""}]);
				true ->
					THIS:set_attribute(headers,H++[{string:sub_string(S,1,N-1),string:sub_string(S,N+1,string:len(S))}])
			end;
		_->
			ok
	end,
    for(R).

for1([],_)->ok;
for1([{K,V}|R],HTTPRequestSettings) ->
    F1 = string:to_lower(K)=:=string:to_lower("Content-Type"),
    F2 = string:str(string:to_lower(V),"urlencoded"),
    if
        ((not F1) or (F2)) ->
            for1(R,HTTPRequestSettings);
        true ->
            HTTPRequestSettings:setEncodePostData(true)
    end.
            
for2([],_)->false;
for2([F|R],S1) ->
    Flag1 = string:to_lower(F) =:= string:to_lower(S1),
    Flag2 = httputils:match(S1,F),
    if
        Flag1 orelse Flag2 ->
            true;
        true->
            for2(R,S1)
    end.

for3([])->[];
for3([F|R]) ->
	Index1 = getHeaderType(F),
	if
		Index1<0 ->
			S1 = "Custom-Header: ";
		true->
			S1 = ""
	end,
	Index2 = ((string:str(F,"&"))=/=0),
	Index3 = (not (string:str(F,"Referer: ")=:=1)),
	if
		((Index2) and (Index3)) ->
			S2 = string:sub_string(F,1,(string:str(F,"&")-1));
		true->
			S2 = F
	end,
	[S1++S2]++for3(R).

for4([])->?CONTENT_TYPE_DEFAULT;
for4([F|R]) ->
    SS1 = F,
    case httputils:startsWithIgnoreCase(SS1,?CONTENT_TYPE_HEADER) of
        false->
            for4(R);
        _->
            SS1
    end.
    
for5(_,1,K)->K;
for5(S,N,K)->
    case lists:nth(N,S) of
        $/ ->
            N;
        _ ->
            for5(S,N-1,K)
    end.
    
for6(S,J)->
    K = string:rstr(S,"?"),
    L = httputils:indexOf(S,"./",J+1),
    if
        ((L=/=0) and ((K=:=0) or (L=<K))) ->
            S1 = string:sub_string(S,1,L-1)++string:sub_string(S,L+2,string:len(S)),
            for6(S1,J);
        true ->
            S
    end.

prepareParametersForApache([],Vector)->
    Vector;
prepareParametersForApache([F|R],Vector) ->
	F1 = case httputils:isSubstituteExpression(F) of
		true ->
			httputils:substitute(F);
		_->
			F
	end,
	I = getHeaderType(F1),
	Flag1 = string:str(F1,"<")=:=1,
	V = if
		I =:= ?CUSTOM_CONTENT_TYPE ->
			[string:sub_string(F1,string:len(?CUSTOM_CONTENT)+1,string:len(F1))++?CRLF];
		Flag1 ->
			[F1];
		I=<0 ->
			J = string:str(F1,"="),
			if
				J=/=0 ->
					S1 = string:sub_string(F1,1,J-1),
					S2 = httputils:replaceAll(S1,"\\eq.","="),
					S3 = string:sub_string(F1,J+1,string:len(F1)),
					[{S2,S3}];
				true ->
					[]
			end;
		true ->
			[]
	end,
	prepareParametersForApache(R,Vector++V).

%% @spec getHeaderType(S) -> Type
%% where
%% S = string()
%% Type = integer()
%% @doc Get the header type.
getHeaderType(S) ->
    F1 = httputils:startsWithIgnoreCase(S,?CUSTOM_CONTENT),
    F2 = httputils:startsWithIgnoreCase(S,?CUSTOM_HEADER),
    F3 = httputils:startsWithIgnoreCase(S,?CONTENT_TYPE_HEADER),
    F4 = httputils:startsWithIgnoreCase(S,?HOST_HEADER),
    F5 = httputils:startsWithIgnoreCase(S,?USER_AGENT_HEADER),
    F6 = httputils:startsWithIgnoreCase(S,?SET_COOKIE_HEADER),
    F7 = httputils:startsWithIgnoreCase(S,?METHOD_HEADER),
    F8 = httputils:startsWithIgnoreCase(S,?REQUEST_PROTOCOL_HEADER),
    F9 = httputils:startsWithIgnoreCase(S,?ACTION_HEADER),
    if
        F1 ->
            ?CUSTOM_CONTENT_TYPE;
        F2 ->
            ?CUSTOM_HEADER_TYPE;
        F3 ->
            ?CONTENT_TYPE_HEADER_TYPE;
        F4 ->
            ?HOST_HEADER_TYPE;
        F5 ->
            ?USER_AGENT_HEADER_TYPE;
        F6 ->
            ?SET_COOKIE_HEADER_TYPE;
        F7 ->
            ?METHOD_HEADER_TYPE;
        F8 ->
            ?REQUEST_PROTOCOL_HEADER_TYPE;
        F9 ->
            ?ACTION_HEADER_TYPE;
        true ->
            -1
    end.

inetErrorCodeStatus(Error) ->
    case Error of
        "http://" ->
            "no status in reply from server";
        nxdomain ->
            "the hostname or domain name could not be found";
        etimedout ->
            "connection timed out";
        etime ->
            "timer expired";
        esocktnosupport ->
            "socket type not supported";
        eshutdown ->
            "cannot send after socket shutdown";
        eprototype ->
            "protocol wrong type for socket";
        Other ->
            if
                is_atom(Other) ->
                    atom_to_list(Error);
                true ->
                    ""
            end
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
		whenToAuthenticate->
			[{"Use Global Preference","Use Global Preference"},{"Authenticate first request","authOnFirst"},{"Authenticate if requested","authOnSecond"}];
		encodePostData->
			[{"Use content-type:","contentTypeUrlencoded"},{"force url encoding","forceEncode"},{"force NO url encoding","forceNoEncode"}];
		checkContent->
			[{"no content checking","no content checking"},{"compare to last contents","on"},{"compare to saved contents","baseline"},{"reset saved contents","reset"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.    
get_classifier(error)->
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{status,'!=',200}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,10-length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',200}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,10-length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',200}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,10-length(Cls)));
		true ->
			Cls
	end.

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++
	  [
		#property{name=url,title="URL",type=text,description="the URL to be verified. If the URL. (example: http://demo.siteview.com)",order=1},
		#property{name=contentMatch,title="Match Content",type=text,description="optional, match against content of URL, using a string or a regular expression or XML names."},
		#property{name=timeout,title="Timeout",type=numeric,description="the time out, in seconds, to wait for the response",advance=true,optional=true,order=3,default=60,baselinable=true},
		#property{name=proxy,title="HTTP Proxy",type=text,description="optional list of proxy servers to use including port (example: proxy.siteview.com:8080)",advance=true,optional=true,order=4},
		#property{name=errorContent,title="Error If Match",type=text,description="optionally generate an error if the content of the URL contains this text",advance=true,optional=true,order=7},
		#property{name=checkContent,title="Check for Content Changes",type=scalar,description="generate error if the content of the URL changes - resetting the saved contents updates the contents checked against during the next monitor run",advance=true,optional=true,order=8},
		#property{name=userName,title="Authorization User Name",type=text,description="optional user name if the URL requires authorization.",advance=true,optional=true,order=9},
		#property{name=authorizationpassword,title="Authorization Password",type=password,description="optional password if the URL requires authorization",advance=true,optional=true,order=10},
		#property{name=domain,title="Authorization NTLM Domain",type=text,description="optional domain if the URL requires for NTLM authorization",advance=true,optional=true,order=11},
		#property{name=whenToAuthenticate,title="Preemptive Authorization",type=scalar,description="when to authorization",advance=true,optional=true,order=12},
		#property{name=proxyUserName,title="Proxy Server User Name",type=text,description="optional user name if the proxy server requires authorization",advance=true,optional=true,order=13},
		#property{name=proxyPassword,title="Proxy Server Password",type=password,description="optional password if the proxy server requires authorization",advance=true,optional=true,order=14},
		#property{name=postData,title="POST Date",type=text,multiple=true,description="optional name=value variables,to send with a POST request",advance=true,optional=true,order=15},
		#property{name=errorOnRedirect,title="No Redirected",type=bool,multiple=true,description="optional when select will not use autoredirect",advance=true,optional=true,order=15},
		%%#property{name=measureDetails,title="Show Detailed Measurements",type=bool,description="when selected, detailed measurement times are displayed for DNS lookup, connecting, server response, and downloading.",advance=true,optional=true,order=16},
		#property{name=urlEncoding,title="Encoding Character Set",type=text,description="Enter code page (ie Cp1252 or Shift_JIS or EUC-JP)",advance=true,optional=true,order=17},
		#property{name=monitorRunCount,title="Baseline Interval",type=text,description="The number of monitor runs to be averaged",advance=true,optional=true,order=18},
		#property{name=httpVersion10,title="HTTP Version",type=bool,description="when unselected, use HTTP Version 1.1 in the request header; when selected, use 1.0",advance=true,optional=true,order=19},
		#property{name=retries,title="Retries",type=numeric,description="The number of times (0-10) to retry the request on recoverable errors, if monitor times out retries are cut short.",advance=true,optional=true,order=20},
		#property{name=acceptAllUntrustedCerts,title="Accept Untrusted Certs for HTTPS",type=bool,description="Accept certificates that are untrusted in the cert chain.",advance=true,optional=true,order=21},
		#property{name=acceptInvalidCerts,title="Accept Invalid Certs for HTTPS",type=bool,description="Accept certificates even if todays date in not in the date ranges in the cert chain.",advance=true,optional=true,order=22},
		#property{name=encodePostData,title="When to Encode Post Data",type=scalar,description="By default if Content-Type: urlencoded found, then encode, otherwise force according to the selected option.",advance=true,optional=true,order=23},
		#property{name=status,title="Status",type=numeric,state=true,configurable=false},
		#property{name=roundTripTime,title="round trip time",type=numeric,state=true,configurable=false,baselinable=true},
		#property{name=size,title="size",type=numeric,state=true,configurable=false},
        #property{name=matchValue,title="MatchValue",type=text,state=true,configurable=false}
	  ].