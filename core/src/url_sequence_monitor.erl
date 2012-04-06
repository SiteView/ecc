%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc URL Sequence Monitor.
%% 
%% Description: URL Sequence Monitor simulates a user's access across several pages,
%% This is particularly useful for monitoring and testing multi-page e-commerce transactions and other interactive online applications
%%大致思想如下：
%%访问之前的refrence，并解析它的body，与页面上下一步所选择的refrencetype比对找出接下来访问所需所有参数，根据这些参数循环向下访问
%%注意在生成页面时同样需要解析之前的requestbody，但页面并不记录你所需要的参数
%%注意在这个过程中与url monitor产生的调用关系可能出现缺少property的情况.
-module(url_sequence_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([defaultTitle/1,update/0,new/0,allocateStepProperties/1,checkURLSequence/28,getParamsFromConfig/16,getLastIndex/2,checkReferer/3,appendContentBuffer/8,putproperty_to_list/1,getCostInLicensePoints/0,getNumberOfSteps/0,getScalarValues/2,get_classifier/1,get_template_property/0]).

-record(linkReference,{link,index}).

-define(DEFAULT_MILLISECOND_PRECISION,2).
-define(STEP_PROPERTIES,12).
-define(ELEMENTS_FOR_TOTAL,7).
-define(URL_RESULT_INDEX,0).
-define(TOTAL_DURATION_INDEX,1).
-define(TOTAL_BYTES_INDEX,2).
-define(TOTAL_FRAMES_INDEX,3).
-define(TOTAL_FRAME_ERRORS_INDEX,4).
-define(TOTAL_IMAGES_INDEX,5).
-define(TOTAL_IMAGE_ERRORS_INDEX,6).
-define(ELEMENTS_PER_STEP,6).
-define(DURATION_STEP_OFFSET,0).
-define(DNSTIME_STEP_OFFSET,1).
-define(CONNECT_TIME_STEP_OFFSET,2).
-define(RESPONSE_TIME_STEP_OFFSET,3).
-define(DOWNLOAD_TIME_STEP_OFFSET,4).
-define(HTML_TRUNCATED_IF_NONZERO_STEP_OFFSET,5).
-define(refererStartToken,"Referer: ").
-define(refererEndToken,"endReferer").
-define(JAVASCRIPT,"[javascript]").
-define(replaceToken,"Replace: ").
%%max step number
-define(numberOfSteps,20).
%%
-define(DEFAULT_TIMEOUT,60000).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = atomic_monitor:new(),
	Obj:set_attribute(referenceType,[]),
	Obj:set_attribute(reference,[]),
    Obj:set_attribute(contentMatch,[]),
    Obj:set_attribute(errorContent,[]),
    Obj:set_attribute(postData,[]),
	Obj:set_attribute(userName,[]),
	Obj:set_attribute(password,[]),
	Obj:set_attribute(domain,[]),
	Obj:set_attribute(whenToAuthenticate,[]),
	Obj:set_attribute(stepDelay,[]),
	Obj:set_attribute(stepName,[]),
    Obj:set_attribute(encoding,[]),
	Obj:set_attribute(encodePostData,[]),
    Obj:set_attribute(stepURLs,[]),
    Obj:set_attribute(stepStatus,[]),
    Obj:set_attribute(progressString,""),
	{?MODULE,Obj}.

%% @spec defaultTitle(Params) -> Title
%% where
%% Params = [{key,Vale}]
%% Key = string()
%% Value = string()
%% Title = string()
%% @doc Give monitor a default title.
defaultTitle(Params)->
	URL = proplists:get_value(reference1,Params),
	if
		length(URL)>0->
			BASE:defaultTitle(Params) ++":" ++ URL;
		true ->
			BASE:defaultTitle(Params)
	end.

%% @spec allocateStepProperties(N) -> Properties
%% where
%% N = integer()
%% Properties = list()
%% @doc Extented specified properties.
allocateStepProperties(N)->
	allocateStepProperties(0,N).
	
%%referenceType,reference,encoding,contentMatch,errorContent,userName,password,domain,whenToAuthenticate,stepDelay,stepName,postData,encodePostData
allocateStepProperties(N1,N2) ->
	I1 = N1+1,
	J1 = 1,
	if
		N1<N2 ->
			[#property{name=list_to_atom(atom_to_list(referenceType)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Type",description="the type of item being referred to in the reference (the next field) - this is used to determine which parts of the HTML will be scanned for text matches.",type=scalar,optional=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(reference)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Reference",description="the URL, link, or submit button to be followed at this step",type=text,optional=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(encoding)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Encoding Character Set:",description="Enter code page (ie Cp1252 or Shift_JIS or EUC-JP)",type=text,optional=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(contentMatch)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Match Content",description="optional, match against content for this step, using a string or a regular expression",type=text,optional=true,advance=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(errorContent)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Error If Match",description="optional, generate an error if the content of the URL for this step contains this text",type=text,optional=true,advance=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(userName)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" User Name",description="optional, user name if the URL for this step requires authorization",type=text,advance=true,optional=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(password)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Password",description="optional, password if the URL for this step requires authorization",type=password,advance=true,optional=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(domain)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Authorization NTLM Domain",description="optional domain if the URL requires for NTLM authorization",type=text,advance=true,optional=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(whenToAuthenticate)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Preemptive Authorization",description="when to authenticate",type=scalar,advance=true,optional=true,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(stepDelay)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Delay",description="optional, number of seconds to wait before performing the next step",type=numeric,advance=true,optional=true,default=0,order=N1*12+J1+1}]++
			[#property{name=list_to_atom(atom_to_list(stepName)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" Title",description="optional, title for this step used in alerts and reports",type=text,advance=true,optional=true,order=N1*12+J1+1}]++
            [#property{name=list_to_atom(atom_to_list(postData)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" POST Data",description="optional, enter name=value variables, one per line, to send with a POST request for this step",type=text,multiple=true,advance=true,optional=true,order=N1*12+J1+1}]++
            [#property{name=list_to_atom(atom_to_list(encodePostData)++integer_to_list(I1)),title="Step "++integer_to_list(I1)++" When to Encode Post Data",description="By default if Content-Type: urlencoded found, then encode, otherwise force according to the selected option.",type=scalar,advance=true,optional=true,order=N1*12+J1+1}]++
			allocateStepProperties(N1+1,N2);
		true->
			[]
	end.
	
%% @spec new() -> ok
%% @doc Start the monitor instance.
update() ->
	update1("","").
	
update1(_,_)->
    %%清掉之前的property,防止之前存入的attribute影响现在的步骤
    clearAttribute(),
	StepURLs = lists:duplicate(?numberOfSteps,[]),
	StepStatus = lists:duplicate(?numberOfSteps,[]),
	{ok,{_,Proxy}} = THIS:get_property(proxy),
	{ok,{_,ProxyPassword}} = THIS:get_property(proxyPassword),
	{ok,{_,ProxyUserName}} = THIS:get_property(proxyUserName),
	{ok,{_,TimeoutPerStep}} = THIS:get_property(timeoutPerStep),
	StartTime = httputils:timeMillis(),
	LastIndex = getNumberOfSteps(),
	putproperty_to_list(LastIndex),
	{ok,{_,ReferenceType}} = THIS:get_attribute(referenceType),
	{ok,{_,Reference}} = THIS:get_attribute(reference),
	{ok,{_,Encoding}} = THIS:get_attribute(encoding),
	{ok,{_,PostData}} = THIS:get_attribute(postData),
	{ok,{_,ContentMatch}} = THIS:get_attribute(contentMatch),
	{ok,{_,ErrorContent}} = THIS:get_attribute(errorContent),
	{ok,{_,UserName}} = THIS:get_attribute(userName),
	{ok,{_,Password}} = THIS:get_attribute(password),
	{ok,{_,Domain}} = THIS:get_attribute(domain),
	{ok,{_,WhenToAuthenticate}} = THIS:get_attribute(whenToAuthenticate),
	{ok,{_,StepDelay}} = THIS:get_attribute(stepDelay),
	{ok,{_,StepName}} = THIS:get_attribute(stepName),
	{ok,{_,EncodePostData}} = THIS:get_attribute(encodePostData),
    {ok,{_,Timeout}} = THIS:get_property(timeout),
    {ok,{_,Class}} = THIS:get_property(?CLASS),
	{ok,{_,Id}} = THIS:get_property(?ID),
    Profile = list_to_atom(atom_to_list(Class) ++ "-" ++ atom_to_list(Id)),
    THIS:set_attribute(profile,Profile),
    {FinalSequenceResult,LastStepResult} = checkURLSequence(ReferenceType,Reference,Encoding,ContentMatch,ErrorContent,Proxy,ProxyUserName,ProxyPassword,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData,Timeout,TimeoutPerStep,"",THIS,null,null,StepURLs,StepStatus,"","","",false),
    UrlResult = lists:nth(1,FinalSequenceResult),
    CostTime = lists:nth(2,FinalSequenceResult),
    TotalDuration = if
        CostTime=:=0 ->
            httputils:timeMillis() - StartTime;
        true ->
            CostTime
    end,
    TotalBytes = lists:nth(3,FinalSequenceResult),
    THIS:set_attribute(status,UrlResult),
    THIS:set_property(url,LastStepResult#urlresults.redirectBuffer),
    if
        UrlResult=:=200 ->
            Duration = httputils:floatToString(TotalDuration/1000,2)++" sec",
            StateString = Duration++", "++integer_to_list(LastIndex)++" steps, "++httputils:bytesToString(TotalBytes,2)++" total",
            THIS:set_attribute(?STATE_STRING,StateString),
            THIS:set_attribute(roundTripTime,TotalDuration);
        true ->
            StateString = httputils:lookupStatus(UrlResult),
            THIS:set_attribute(?STATE_STRING,StateString),
            THIS:set_attribute(roundTripTime,"n/a")
    end.
    
    
setStateValue(Len,Len,_,_)->ok;
setStateValue(I,Len,Monitor,Result)->
    Index = I*6+7,
    Value = lists:nth(Index,Result),
    if
        Value>=0 ->
            Monitor:set_attribute(stepRoundTripTime,lists:nth(Index,Result));
        true ->
            Monitor:set_attribute(stepRoundTripTime,lists:nth(Index,"n/a"))
    end,
    setStateValue(I+1,Len,Monitor,Result).
    
%% @spec checkURLSequence(ReferenceType,Reference,Encoding,Match,ErrorContent,Proxy,ProxyUserName,ProxyPassword,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData,Timeout,TimeoutPerStep,ErrorBuffer,Monitor,TraceStream,SequenceBuffer,StepURLs,StepStatus,RedirectBuffer,ContentBuffer,SessionBuffer,AcceptButton) -> {FinalSequenceResult,LastStepResult}
%% where
%% ReferenceType = list()
%% Reference = list()
%% Encoding = list()
%% Match = list()
%% ErrorContent = list()
%% Proxy = string()
%% ProxyUserName = string()
%% ProxyPassword = string()
%% PostData = list()
%% UserName = string()
%% Password = string()
%% Domain = string()
%% WhenToAuthenticate = string()
%% StepDelay = list()
%% StepName = list()
%% EncodePostData = list()
%% Timeout = integer()
%% TimeoutPerStep = integer()
%% ErrorBuffer = string()
%% Monitor = instance()
%% TraceStream = string()
%% SequenceBuffer = string()
%% StepURLs = list()
%% StepStatus = list()
%% RedirectBuffer = string()
%% ContentBuffer = string()
%% SessionBuffer = string()
%% AcceptButton = bool()
%% FinalSequenceResult = list()
%% LastStepResult = record()
%% @doc The core function in url sequence monitor,send request from first step,parse the response ,compaire with page state of next step,get the params from the response,use these params loop the request
checkURLSequence(ReferenceType,Reference,Encoding,Match,ErrorContent,Proxy,ProxyUserName,ProxyPassword,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData,Timeout,TimeoutPerStep,ErrorBuffer,Monitor,TraceStream,SequenceBuffer,StepURLs,StepStatus,RedirectBuffer,ContentBuffer,SessionBuffer,AcceptButton) ->
    Profile = case Monitor:get_attribute(profile) of
        {ok,{_,Pro}} ->
            Pro;
        _->
            httputils:createProfile(url_sequence_monitor)
    end,
    Monitor:set_attribute(profile,Profile),
    Monitor:set_attribute(stepURLs,StepURLs),
    Monitor:set_attribute(stepStatus,StepStatus),
    Monitor:set_attribute(progressString,""),
    Monitor:set_attribute(redirectBuffer,""),
    Monitor:set_attribute(contentBuffer,""),
    Monitor:set_attribute(statusBuffer,""),
    Monitor:set_attribute(sessionBuffer,""),
    Monitor:set_attribute(errorStepName,""),
    URL = case httputils:isSubstituteExpression(lists:nth(1,Reference)) of
		true ->
			httputils:substitute(lists:nth(1,Reference));
		_->
			lists:nth(1,Reference)
	end,
    Len = length(ReferenceType),
    %%这里使用reference的长度来标识总共的步骤
    %%Len = length(Reference),
    
    Monitor:set_attribute(lastURL,""),
    %%6的含义是访问过程中的每一步的时间参数【duration，dnsTime，connectTime，responseTime，downloadTime，htmlTruncatedIfNonZero】
    %%7的含义是当前第一步的结果【status，totalDuration，totalBytes，frameImageResult*4】
    THIS:set_attribute(sequenceResult,lists:duplicate(?numberOfSteps*6+7,-1)),
    Session = urlcontext:new(Monitor),
    Post = lists:nth(1,PostData),
    %%io:format("post is:~p~n",[Post]),
    Monitor:set_attribute(parentURL,""),
    Monitor:set_attribute(matchValues,[]),
    Monitor:set_attribute(matchValueList,lists:duplicate(?numberOfSteps,[])),
    Monitor:set_attribute(totalDuration,0),
    Monitor:set_attribute(totalBytes,0),
    Monitor:set_attribute(status,200),
    TimeoutDuration = case Timeout of
        0 ->
            60000;
        _->
            Timeout*1000
    end,
    Monitor:set_attribute(timeoutDuration,TimeoutDuration),
    Monitor:set_attribute(oldStatus,200),
    {ok,{_,Selected}} = Monitor:get_property(resumeStep),
    {ok,{_,ResumeRemainingSteps}} = Monitor:get_property(resumeRemainingSteps),
    RStep = if
        Selected=/="" ->
            RS = list_to_integer(Selected),
            if
                RS>=Len ->
                    -1;
                true ->
                    RS
            end;
        true ->
            -1
    end,
    Monitor:set_attribute(virginURL,""),
    Monitor:set_attribute(saveBuffer,null),
    retriveStepURL(1,Len+1,Monitor,TraceStream,URL,Post,Session,Encoding,Match,ErrorContent,Proxy,ProxyUserName,ProxyPassword,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData,Timeout,TimeoutPerStep,TimeoutDuration,ErrorBuffer,SequenceBuffer,RStep,ResumeRemainingSteps,ReferenceType,Reference,AcceptButton),
    {ok,{_,LastStepBody}} = Monitor:get_attribute(body),
    {ok,{_,LastStepHead}} = Monitor:get_attribute(head),
    %%contentBuffer,sessionBuffer,redirectBuffer
    {ok,{_,LastStepContentBuffer}} = Monitor:get_attribute(contentBuffer),
    {ok,{_,LastStepSessionBuffer}} = Monitor:get_attribute(sessionBuffer),
    {ok,{_,LastStepRedirectBuffer}} = Monitor:get_attribute(redirectBuffer),
    {ok,{_,LastStepErrorMessage}} = Monitor:get_attribute(errorBuffer),
    LastStepResult = #urlresults{
                    body = LastStepBody,
                    head = LastStepHead,
                    contentBuffer = LastStepContentBuffer,
                    sessionBuffer = LastStepSessionBuffer,
                    redirectBuffer = LastStepRedirectBuffer,
                    errorMessage = LastStepErrorMessage
                    },
    {ok,{_,OldStatus}} = Monitor:get_attribute(oldStatus),
    {ok,{_,Status}} = Monitor:get_attribute(status),
    if
        ((OldStatus=/=?kURLok) and (OldStatus=/=Status)) ->
            Monitor:set_attribute(status,OldStatus);
        true ->
            ok
    end,
    if
        Status=/=?kURLok ->
            io:format("Error="++integer_to_list(Status)++", Transaction Sequence=~n");
        true ->
            %%io:format("DONE WITH SEQUENCE~n")
            ok
    end,
    {ok,{_,MatchValueList}} = Monitor:get_attribute(matchValueList),
    Monitor:set_attribute(value,null),
    updateMatchValueFinal(MatchValueList,1,Monitor),
    {ok,{_,NewValue}} = Monitor:get_attribute(value),
    if
        NewValue=/=null ->
            Monitor:set_attribute(matchValue,NewValue);
        true ->
            ok
    end,
    {ok,{_,TotalDuration}} = Monitor:get_attribute(totalDuration),
    {ok,{_,TotalBytes}} = Monitor:get_attribute(totalBytes),
    {ok,{_,SequenceResult}} = Monitor:get_attribute(sequenceResult),
    SequenceResult1 = httputils:shiftNthElementInList(1,Status,SequenceResult),
    SequenceResult2 = httputils:shiftNthElementInList(2,TotalDuration,SequenceResult1),
    FinalSequenceResult = httputils:shiftNthElementInList(3,TotalBytes,SequenceResult2),
    {FinalSequenceResult,LastStepResult}.
    
checkURLSequence(Config,ErrorBuffer,TraceStream,SequenceBuffer,ContentBuffer,SessionBuffer,Monitor)->
    checkURLSequence(Config,ErrorBuffer,TraceStream,SequenceBuffer,ContentBuffer,SessionBuffer,Monitor,false).
    
checkURLSequence(Config,ErrorBuffer,TraceStream,SequenceBuffer,ContentBuffer,SessionBuffer,Monitor,AcceptButton) ->
    LastIndex = getLastIndex(Config,0),
    %%io:format("last index is:~p~n",[LastIndex]),
    {ReferenceType,Reference,Encoding,Match,ErrorContent,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData} = getParamsFromConfig(1,LastIndex,Config,[],[],[],[],[],[],[],[],[],[],[],[],[]),
    %%io:format("get params from config:~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p~n",[ReferenceType,Reference,Encoding,Match,ErrorContent,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData]),
    checkURLSequence(ReferenceType,Reference,Encoding,Match,ErrorContent,proplists:get_value(proxy,Config,""),proplists:get_value(proxyUserName,Config,""),proplists:get_value(proxyPassword,Config,""),PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData,proplists:get_value(timeout,Config,60),proplists:get_value(timeoutPerStep,Config,0),ErrorBuffer,Monitor,TraceStream,SequenceBuffer,null,null,"",ContentBuffer,SessionBuffer,AcceptButton).

%% @spec getParamsFromConfig(J,LastIndex,Config,ReferenceType,Reference,Encoding,ContentMatch,ErrorContent,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData) -> {ReferenceType,Reference,Encoding,ContentMatch,ErrorContent,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData}
%% where
%% J = integer()
%% LastIndex = integer()
%% config = list()
%% ReferenceType = list()
%% Reference = list()
%% Encoding = list()
%% ContentMatch = list()
%% ErrorContent = list()
%% PostData = list()
%% UserName = list()
%% Password = list()
%% Domain = string()
%% WhenToAuthenticate = list()
%% StepDelay = list()
%% StepName = list()
%% EncodePostData = list()
%% @doc Get params from config,params' number is depends by step number
getParamsFromConfig(J,LastIndex,Config,ReferenceType,Reference,Encoding,ContentMatch,ErrorContent,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData)->
    if
        J=<LastIndex ->
            getParamsFromConfig(J+1,LastIndex,Config,
                            ReferenceType++[proplists:get_value(list_to_atom("referenceType"++integer_to_list(J)),Config)],
                            Reference++[proplists:get_value(list_to_atom("reference"++integer_to_list(J)),Config)],
                            Encoding++[proplists:get_value(list_to_atom("encoding"++integer_to_list(J)),Config)],
                            ContentMatch++[proplists:get_value(list_to_atom("contentMatch"++integer_to_list(J)),Config)],
                            ErrorContent++[proplists:get_value(list_to_atom("errorContent"++integer_to_list(J)),Config)],
                            PostData++[[string:strip(X)||X<-string:tokens(proplists:get_value(list_to_atom("postData"++integer_to_list(J)),Config),"\n")]],
                            UserName++[proplists:get_value(list_to_atom("userName"++integer_to_list(J)),Config)],
                            Password++[proplists:get_value(list_to_atom("password"++integer_to_list(J)),Config)],
                            Domain++[proplists:get_value(list_to_atom("domain"++integer_to_list(J)),Config)],
                            WhenToAuthenticate++[proplists:get_value(list_to_atom("whenToAuthenticate"++integer_to_list(J)),Config)],
                            StepDelay++[proplists:get_value(list_to_atom("stepDelay"++integer_to_list(J)),Config,0)],
                            StepName++[proplists:get_value(list_to_atom("stepName"++integer_to_list(J)),Config,"")],
                            EncodePostData++[proplists:get_value(list_to_atom("encodePostData"++integer_to_list(J)),Config)]);
        true ->
            {ReferenceType,Reference,Encoding,ContentMatch,ErrorContent,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData}
    end.

%% @spec getLastIndex(Config,Index) -> StepNum
%% where
%% Config = list()
%% Index = integer()
%% StepNum = integer()
%% @doc Get the last step number.
getLastIndex(Config,Index)->
    Value = proplists:get_value(list_to_atom("reference"++integer_to_list(Index+1)),Config),
    if
        ((Value =/= "") and (Value =/= undefined)) ->
            getLastIndex(Config,Index+1);
        true ->
            Index
    end.
    
    
updateMatchValueFinal([],_,_)->ok;
updateMatchValueFinal([Vals|R],I,Monitor)->
    NewI = eachMatchValue(Vals,I,Monitor),
    updateMatchValueFinal(R,NewI+1,Monitor).
    
eachMatchValue([],I,_)->I;
eachMatchValue([Value|R],I,Monitor)->
    Monitor:set_attribute(value,Value),
    Monitor:set_attribute(list_to_atom("matchValue"++integer_to_list(I)),Value),
    eachMatchValue(R,I+1,Monitor).
    

%%函数的作用是请求当前的url,解析其内容,与页面上下一步所选的referer进行比对,并得到所需要的参数如此循环
retriveStepURL(Len,Len,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->ok;
retriveStepURL(I,Len,Monitor,TraceStream,URL,Post,Session,Encoding,Match,ErrorContent,Proxy,ProxyUserName,ProxyPassword,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData,Timeout,TimeoutPerStep,TimeoutDuration,ErrorBuffer,SequenceBuffer,RStep,ResumeRemainingSteps,ReferenceType,Reference,AcceptButton) ->
    StartTime = httputils:timeMillis(),
    Monitor:set_attribute(i,I),
    Monitor:set_attribute(break,false),
    ExtraHeaders = "Pragma: No-Cache",
    Monitor:set_attribute(errorAppended,false),
    TimeoutDuration1 = if
        TimeoutPerStep ->
            if
                Timeout=:=0 ->
                    60000;
                true ->
                    Timeout*1000
            end;
        true ->
            TimeoutDuration
    end,
    Monitor:set_attribute(timeoutDuration,TimeoutDuration1),
    %%省略了替换的地方
    {ok,{_,PS}} = Monitor:get_attribute(progressString),
    Monitor:set_attribute(progressString,PS++"Step "++integer_to_list(I)++": checking URL "++URL++"\n"),
    if
        TraceStream=/=null ->
            %%io:format("Step "++integer_to_list(I)++": checking URL "++URL);
            ok;
        true ->
            ok
    end,
    %%判断是否用户输入了referer,没有的话使用lasturl,有的话查看是否含有[none],若含有则去掉当前这个post项
    {NewPost,AddReferrerHeader} = if
        ((Post=/=[]) and (Post=/=null)) ->
            checkReferer(Post,Post,true);
        true ->
            {Post,true}
    end,
    {ok,{_,LastURL}} = Monitor:get_attribute(lastURL),
    %%io:format("lastURL is:~p~n",[LastURL]),
    ExtraHeaders1 = if
        ((AddReferrerHeader) and (length(LastURL)>0)) ->
            EH = ExtraHeaders++"\r\n",
            EH++?refererStartToken++LastURL++"&"++?refererEndToken;
        true ->
            ExtraHeaders
    end,
    %%io:format("ExtraHeaders1 is:~p~n",[ExtraHeaders1]),
    {ok,{_,StepURLs}} = Monitor:get_attribute(stepURLs),
    if
        StepURLs=/=null ->
            Monitor:set_attribute(stepURLs,httputils:shiftNthElementInList(I,URL,StepURLs));
        true ->
            ok
    end,
    %%准备请求当前的url,准备本次的参数等
    %%每次访问之前会清空当前这几个buffer值
    Monitor:set_attribute(redirectBuffer,URL),
    Monitor:set_attribute(contentBuffer,""),
    Monitor:set_attribute(sessionBuffer,""),
    Encode = lists:nth(I,Encoding),
    OKMatch = lists:nth(I,Match),
    ErrorMatch = lists:nth(I,ErrorContent),
    StepDelaySeconds = lists:nth(I,StepDelay),
    Retries = 0,
    ContentMaxBytes = 50000,
    Session:setEncodePostData(lists:nth(I,EncodePostData)),
    Session:setDomain(lists:nth(I,Domain)),
    Session:setStreamEncoding(Encode),
    {ok,{_,ContentBuffer}} = Monitor:get_attribute(contentBuffer),
    {ok,{_,RedirectBuffer}} = Monitor:get_attribute(redirectBuffer),
    {ok,{_,SessionBuffer}} = Monitor:get_attribute(sessionBuffer),
    Settings = httprequestsettings:new(URL,lists:nth(I,UserName),lists:nth(I,Password),lists:nth(I,Domain),lists:nth(I,WhenToAuthenticate),Proxy,ProxyUserName,ProxyPassword,null,Retries,0,0),
    Settings:init(),
    M = url_monitor:new(),
    {ok,{_,Profile}} = Monitor:get_attribute(profile),
    M:set_attribute(profile,Profile),
    URLResults = M:checkURL(Settings,Session,OKMatch,ErrorMatch,NewPost,ContentBuffer,ContentMaxBytes,ExtraHeaders1,TimeoutDuration1,RedirectBuffer,SessionBuffer),
    Head = URLResults#urlresults.head,
    HTMLEncode = M:getHTMLEncoding(Head,URLResults#urlresults.body),
    io:format("encode:~p~n",[HTMLEncode]),
    Body = iconv:convert(HTMLEncode,httputils:pageEncode(),URLResults#urlresults.body),
    Monitor:set_attribute(body,Body),
    Monitor:set_attribute(head,Head),
    M:delete(),
    Status = URLResults#urlresults.status,
    Monitor:set_attribute(status,Status),
    ErrorMessage = URLResults#urlresults.errorMessage,
    NewContentBuffer = iconv:convert(HTMLEncode,httputils:pageEncode(),URLResults#urlresults.contentBuffer),
    NewRedirectBuffer = iconv:convert(HTMLEncode,httputils:pageEncode(),URLResults#urlresults.redirectBuffer),
    NewSessionBuffer = iconv:convert(HTMLEncode,httputils:pageEncode(),URLResults#urlresults.sessionBuffer),
    %%file:write_file("e:/content.txt",NewContentBuffer),
    Monitor:set_attribute(contentBuffer,NewContentBuffer),
    Monitor:set_attribute(redirectBuffer,NewRedirectBuffer),
    Monitor:set_attribute(sessionBuffer,NewSessionBuffer),
    Duration = URLResults#urlresults.totalDuration,
    BytesTransferred = URLResults#urlresults.totalBytes,
    {ok,{_,StepStatus}} = Monitor:get_attribute(stepStatus),
    
    %%io:format("retrive URL result is :~p~p~p~p~p~n",[Status,NewRedirectBuffer,Duration,BytesTransferred,Head]),
    
    if
        StepStatus=/=null ->
            Monitor:set_attribute(stepStatus,httputils:shiftNthElementInList(I,ErrorMessage,StepStatus));
        true ->
            ok
    end,
    NewDuration = if
        Duration=:=0 ->
            httputils:timeMillis() - StartTime;
        Status=:=?kURLTimeoutError ->
            0;
        true ->
            Duration
    end,
    {ok,{_,TotalBytes}} = Monitor:get_attribute(totalBytes),
    Monitor:set_attribute(totalBytes,TotalBytes+BytesTransferred),
    {ok,{_,TotalDuration}} = Monitor:get_attribute(totalDuration),
    Monitor:set_attribute(totalDuration,TotalDuration+NewDuration),
    {ok,{_,TimeoutDuration2}} = Monitor:get_attribute(timeoutDuration),
    Monitor:set_attribute(timeoutDuration,TimeoutDuration2-NewDuration),
    {ok,{_,SequenceResult}} = Monitor:get_attribute(sequenceResult),
    ResultIndex = (I-1)*6+7,
    %%注意这里要在ResultIndex的基础上加1避免覆盖上面的第七个参数
    Monitor:set_attribute(sequenceResult,httputils:shiftNthElementInList(ResultIndex+1,NewDuration,SequenceResult)),
    %%剩下的5个参数暂时无法取到
    {ok,{_,ProgressString}} = Monitor:get_attribute(progressString),
    PS1 = if
        Status=:=?URLok ->
            ProgressString++httputils:lookupStatus(Status)++" "++"in "++httputils:floatToString(Duration/1000,2)++" sec";
        true ->
            ProgressString++httputils:lookupStatus(Status)++" "
    end,
    Monitor:set_attribute(progressString,PS1++"\n"),
    if
        TraceStream=/=null ->
            %%io:format("Step "++integer_to_list(I)++"\nSTATUS="++integer_to_list(Status)++"\nDURATION="++integer_to_list(Duration));
            ok;
        true ->
            ok
    end,
    if
        SequenceBuffer=/=null ->
            {ok,{_,RedirectBuffer1}} = Monitor:get_attribute(redirectBuffer),
            {ok,{_,SessionBuffer1}} = Monitor:get_attribute(sessionBuffer),
            SequenceBuffer1 = appendContentBuffer(URL,RedirectBuffer1,SequenceBuffer,SessionBuffer1,Body,Head,I,(I=/=Len)),
            Monitor:set_attribute(sequenceBuffer,SequenceBuffer1);
        true ->
            ok
    end,
    Monitor:set_attribute(matchValues,[]),
    Monitor:set_attribute(errorBuffer,ErrorBuffer),
    ErrorBuffer2 = case httputils:isValueExpression(OKMatch) of
        true ->
            {MatchValues1,ErrorBuffer1} = updateMatchValue(ErrorBuffer,OKMatch,{Body,Head},"Content Matched: "),
            Monitor:set_attribute(matchValues,MatchValues1),
            ErrorBuffer1;
        _->
            ErrorBuffer
    end,
    case httputils:isValueExpression(ErrorContent) of
        true ->
            {TempMatchValues,TmpString} = updateMatchValue("",ErrorContent,{Body,Head},"Error Matched: "),
            if
                TempMatchValues=/=[] ->
                    Monitor:set_attribute(matchValues,TempMatchValues);
                true ->
                    ok
            end,
            NewStatusBuffer = if
                ErrorBuffer2=/="" ->
                    ErrorBuffer2++", "++TmpString;
                true ->
                    TmpString
            end,
            Monitor:set_attribute(errorBuffer,NewStatusBuffer);
        _->
            ok
    end,
    {ok,{_,MVL}} = Monitor:get_attribute(matchValueList),
    {ok,{_,MV}} = Monitor:get_attribute(matchValues),
    Monitor:set_attribute(matchValueList,httputils:shiftNthElementInList(I,MV,MVL)),
    {ok,{_,Source}} = Monitor:get_attribute(redirectBuffer),
    %%io:format("source is:~p~n",[Source]),
    %%省略掉取images和frame的过程
    %%取到下一步的referer,并解析本次请求得到的页面
    NextStep = if
        ((Status=/=?kURLok) and (I<RStep)) ->
            RStep;
        true ->
            I+1
    end,
    Monitor:set_attribute(nextStep,NextStep),
    CanResume = I<RStep,
    {ok,{_,OldStatus}} = THIS:get_attribute(oldStatus),
    Resumed = OldStatus=/=?kURLok,
    Monitor:set_attribute(url,URL),
    Monitor:set_attribute(post,Post),
    
    %%io:format("flag params is:~p,~p,~p,~p,~p~n",[Status,NextStep,RStep,URL,OldStatus]),
    %%io:format("parse nextstep flag is:~p~n",[(((Status=:=?kURLok) and (((not Resumed) or (ResumeRemainingSteps)))) or ((Status=/=?kURLok) and (CanResume)))]),
    
    if
        (((Status=:=?kURLok) and (((not Resumed) or (ResumeRemainingSteps)))) or ((Status=/=?kURLok) and (CanResume))) ->
            if
                I<(Len-1) ->
                    Monitor:set_attribute(formVariables,null),
                    MatchReference = lists:nth(NextStep,Reference),
                    NextReferenceType = lists:nth(NextStep,ReferenceType),
                    Monitor:set_attribute(nextReferenceType,NextReferenceType),
                    Monitor:set_attribute(formMethod,"POST"),
                    RefererURL = Session:getRefererURL(),
                    %%io:format("RefererURL:~p~n",[RefererURL]),
                    if
                        ((RefererURL=/=null) and (RefererURL=/="") and (Status=:=?kURLok)) ->
                            Monitor:set_attribute(lastURL,RefererURL);
                        true ->
                            Monitor:set_attribute(lastURL,URL)
                    end,
                    Session:setRefererURL(null),
                    Monitor:set_attribute(url,""),
                    {ok,{_,LastURL1}} = Monitor:get_attribute(lastURL),
                    Session:updateCookies(Head,LastURL1),
                    MatchReference1 = case httputils:startsWith(MatchReference,"[javascript]") of
                        true ->
                            string:sub_string(MatchReference,string:len("[javascript]")+1,string:len(MatchReference));
                        _ ->
                            MatchReference
                    end,
                    HTMLTree = htmltagparser:process(Body),
                    %%io:format("MATCH REFERENCE="++MatchReference1++" NEXT REFERENCE TYPE="++NextReferenceType++"~n"),
                    %%分析上次的reference和reference type,找出下次请求需要的所有参数
                    if
                        NextReferenceType=:="link" ->
                            MatchReference2 = string:strip(MatchReference1),
                            %%LinkRef = extractLinkRef(MatchReference2),
                            %%MatchReference3 = LinkRef#linkReference.link,
                            MatchReference3 = MatchReference2,
                            Links = htmltagparser:findTags(HTMLTree,a),
                            %%URL_a = getUrlFromtags_a(Links,MatchReference3,0,LinkRef),
                            URL_a = getUrlFromtags_a_strict(Links,MatchReference3),
                            Monitor:set_attribute(url,URL_a),
                            if
                                URL_a=:="" ->
                                    Areas = htmltagparser:findTags(HTMLTree,area),
                                    URL_area = getUrlFromtags_area(Areas,MatchReference3),
                                    Monitor:set_attribute(url,URL_area);
                                true ->
                                    ok
                            end,
                            {ok,{_,URL1}} = Monitor:get_attribute(url),
                            IsValueExpression = httputils:isValueExpression(MatchReference3),
                            if
                                ((URL1=:="") and IsValueExpression) ->
                                    {_,_,MatchResults} = httputils:matchExpression(Body,MatchReference3,[],""),
                                    URL_append = appendMatchPartToURL(MatchResults,URL1),
                                    Monitor:set_attribute(url,URL_append);
                                true ->
                                    ok
                            end,
                            {ok,{_,URL2}} = Monitor:get_attribute(url),
                            %%此处区别于上面的部分在于可能link的contents为空,在形成页面时把reference的值设成link的url本身,将这种情况下的url找到
                            if
                                URL2=:="" ->
                                    URL_a1 = getUrlFromtags_a(htmltagparser:findTags(HTMLTree,a),MatchReference3),
                                    Monitor:set_attribute(url,URL_a1);
                                true ->
                                    ok
                            end,
                            {ok,{_,URL3}} = Monitor:get_attribute(url),
                            if
                                URL3=:="" ->
                                    {ok,{_,ErrorBuffer3}} = Monitor:get_attribute(errorBuffer),
                                    {ok,{_,LastURL2}} = Monitor:get_attribute(lastURL),
                                    Monitor:set_attribute(errorBuffer,ErrorBuffer3++"missing link \""++MatchReference3++"\" on "++lists:nth(I,StepName)++" ("++LastURL2++")"),
                                    Monitor:set_attribute(errorAppended,true),
                                    if
                                        I<RStep ->
                                            Monitor:set_attribute(url,lists:nth(RStep,Reference)),
                                            Monitor:set_attribute(nextStep,RStep),
                                            Monitor:set_attribute(nextReferenceType,lists:nth(RStep,ReferenceType)),
                                            Monitor:set_attribute(status,-990);
                                        true ->
                                            ok
                                    end;
                                true ->
                                    %%io:format("Found link "++MatchReference3++"~n"),
                                    {ok,{_,URL4}} = Monitor:get_attribute(url),
                                    Monitor:set_attribute(url,httputils:unescapeHTML(URL4))
                            end;
                        NextReferenceType=:="form" ->
                            IEnd = string:rstr(MatchReference1,"]"),
                            IStart = string:rstr(MatchReference1,"["),
                            if
                                ((IStart=/=0) and (IEnd=/=0) and (IStart<IEnd)) ->
                                    WhichForm = list_to_integer(string:sub_string(MatchReference1,IStart+1,IEnd-1)),
                                    MatchReference2 = string:sub_string(MatchReference1,1,IStart-1),
                                    IEnd1 = string:rstr(MatchReference2,"]"),
                                    IStart1 = string:rstr(MatchReference2,"["),
                                    if
                                        ((IStart1=/=0) and (IEnd1=/=0) and (IStart1<IEnd1)) ->
                                            WhichButton = list_to_integer(string:sub_string(MatchReference2,IStart1+1,IEnd1-1)),
                                            MatchReference3 = string:sub_string(MatchReference2,1,IStart1-1);
                                        true ->
                                            WhichButton = -1,
                                            MatchReference3 = MatchReference2
                                    end;
                                true ->
                                    WhichForm = -1,
                                    WhichButton = -1,
                                    MatchReference3 = MatchReference1
                            end,
                            Forms = htmltagparser:findTags(HTMLTree,form),
                            getDataFromtags_form(Forms,0,WhichForm,Monitor,NextStep,PostData,MatchReference3,HTMLTree,WhichButton,AcceptButton),
                            {ok,{_,URL5}} = Monitor:get_attribute(url),
                            if
                                URL5=:="" ->
                                    ButtonTitle = if
                                        WhichButton=/=-1 ->
                                            "["++integer_to_list(WhichButton)++"]";
                                        true ->
                                            "\""++MatchReference3++"\""
                                    end,
                                    FormIndex = if
                                        WhichForm=/=-1 ->
                                            "["++integer_to_list(WhichForm)++"]";
                                        true ->
                                            ""
                                    end,
                                    {ok,{_,ErrorBuffer4}} = Monitor:get_attribute(errorBuffer),
                                    {ok,{_,LastURL3}} = Monitor:get_attribute(lastURL),
                                    Monitor:set_attribute(errorBuffer,ErrorBuffer4++"missing form"++FormIndex++" button "++ButtonTitle++" on "++lists:nth(I,StepName)++" ("++LastURL3++")"),
                                    Monitor:set_attribute(errorAppended,true),
                                    if
                                        I<RStep ->
                                            Monitor:set_attribute(url,lists:nth(RStep,Reference)),
                                            Monitor:set_attribute(nextStep,RStep),
                                            Monitor:set_attribute(nextReferenceType,lists:nth(RStep,ReferenceType)),
                                            Monitor:set_attribute(status,-990);
                                        true ->
                                            ok
                                    end;
                                true ->
                                    %%io:format("Found submit button "++MatchReference3)
                                    ok
                            end;
                        NextReferenceType=:="frame" ->
                            FlagMatch = httputils:match("_parent",MatchReference1),
                            if
                                FlagMatch ->
                                    {ok,{_,ParentURL}} = Monitor:get_attribute(parentURL),
                                    Monitor:set_attribute(url,ParentURL),
                                    Monitor:set_attribute(parentURL,null);
                                true ->
                                    Frames = htmltagparser:findTags(HTMLTree,frame),
                                    IFrames = htmltagparser:findTags(HTMLTree,iframe),
                                    getUrlFromtags_frame(Frames,MatchReference1,Monitor),
                                    {ok,{_,URL6}} = Monitor:get_attribute(url),
                                    if
                                        ((URL6=:="") and (IFrames=/=[]))->
                                            getUrlFromtags_iframe(IFrames,MatchReference1,Monitor);
                                        true ->
                                            ok
                                    end,
                                    {ok,{_,URL7}} = Monitor:get_attribute(url),
                                    if
                                        URL7=:="" ->
                                            {ok,{_,ErrorBuffer5}} = Monitor:get_attribute(errorBuffer),
                                            {ok,{_,LastURL4}} = Monitor:get_attribute(lastURL),
                                            Monitor:set_attribute(errorBuffer,ErrorBuffer5++"missing frame \""++MatchReference1++"\" on "++lists:nth(I,StepName)++" ("++LastURL4++")"),
                                            Monitor:set_attribute(errorAppended,true),
                                            if
                                                I<RStep ->
                                                    Monitor:set_attribute(url,lists:nth(RStep,Reference)),
                                                    Monitor:set_attribute(nextStep,RStep),
                                                    Monitor:set_attribute(nextReferenceType,lists:nth(RStep,ReferenceType)),
                                                    Monitor:set_attribute(status,-990);
                                                true ->
                                                    ok
                                            end;
                                        true ->
                                            %%io:format("Found frame "++MatchReference1)
                                            ok
                                    end
                            end;
                        NextReferenceType=:="refresh" ->
                            Metas = htmltagparser:findTags(HTMLTree,meta),
                            getUrlFromtags_meta(Metas,MatchReference1,Monitor),
                            {ok,{_,URL8}} = Monitor:get_attribute(url),
                            if
                                (URL8=:="" and (I<RStep)) ->
                                    Monitor:set_attribute(url,lists:nth(RStep,Reference)),
                                    Monitor:set_attribute(nextStep,RStep),
                                    Monitor:set_attribute(nextReferenceType,lists:nth(RStep,ReferenceType)),
                                    Monitor:set_attribute(status,-990);
                                true ->
                                    ok
                            end;
                        NextReferenceType=:="url" ->
                            URL_sub = case httputils:isSubstituteExpression(MatchReference1) of
                                true ->
                                    httputils:substitute(MatchReference1);
                                _->
                                    MatchReference1
                            end,
                            Monitor:set_attribute(url,URL_sub);
                        true ->
                            io:format("!!!!!!!!!!error type~n")
                    end,
                    Monitor:set_attribute(containsPrivate,false),
                    {ok,{_,URL9}} = Monitor:get_attribute(url),
                    if
                        URL9=:="" ->
                            Monitor:set_attribute(status,-990);
                        true ->
                            Monitor:set_attribute(virginURL,URL9),
                            Bases = htmltagparser:findTags(HTMLTree,base),
                            Base = if
                                Bases=/=[] ->
                                    htmltagparser:getValue(lists:nth(1,Bases),href);
                                true ->
                                    []
                            end,
                            URLMonitor = url_monitor:new(),
                            {ok,{_,URL10}} = Monitor:get_attribute(url),
                            URL11 = URLMonitor:resolveURL(string:strip(URL10),Source,Base),
                            %%io:format("final url is:~p~n",[URL11]),
                            URLMonitor:delete(),
                            Monitor:set_attribute(url,URL11),
                            Monitor:set_attribute(virginURL,URL11),
                            {ok,{_,NewNextStep}} = Monitor:get_attribute(nextStep),
                            Post1 = lists:nth(NewNextStep,PostData),
                            {ok,{_,FormVariables}} = Monitor:get_attribute(formVariables),
                            if
                                FormVariables=/=null ->
                                    NewFormVariables = mergeForEachPost(Post1,FormVariables),
                                    Monitor:set_attribute(formVariables,NewFormVariables),
                                    %%io:format("FORM VARIABLES LIST="++NewFormVariables++"~n"),
                                    {ok,{_,Method}} = Monitor:get_attribute(formMethod),
                                    IsGetMethod = string:to_lower(Method)=:="get",
                                    FNewPost = if
                                        IsGetMethod ->
                                            Separator = case string:str(URL11,"?") of
                                                0 ->
                                                    "?";
                                                _->
                                                    "&"
                                            end,
                                            {ok,{_,URL12}} = Monitor:get_attribute(url),
                                            checkFormVariables(NewFormVariables,[],URL12,Separator,Encoding,NewNextStep,Monitor,[]);
                                        true ->
                                            NewFormVariables
                                    end;
                                true ->
                                    FNewPost = Post1
                            end,
                            %%io:format("final post is:~p~n",[FNewPost]),
                            Monitor:set_attribute(post,FNewPost)
                    end,
                    {ok,{_,ContainsPrivate}} = Monitor:get_attribute(containsPrivate),
                    {ok,{_,VirginURL1}} = Monitor:get_attribute(virginURL),
                    if
                        (not ContainsPrivate) ->
                            Monitor:set_attribute(virginURL,"");
                        true ->
                            Monitor:set_attribute(virginURL,VirginURL1++".... (contains private variables that aren't not shown)")
                    end;
                true ->
                    ok
            end;
        true ->
            %%io:format("!!!!!!!!!!!!!!!!!get in first step sequence~n"),
            {ok,{_,VirginURL1}} = Monitor:get_attribute(virginURL),
            {ok,{_,URL12}} = Monitor:get_attribute(url),
            if
                VirginURL1=/="" ->
                    Monitor:set_attribute(lastURL,VirginURL1);
                true ->
                    Monitor:set_attribute(lastURL,URL12)
            end
    end,
    {ok,{_,Status1}} = Monitor:get_attribute(status),
    %%io:format("final status is:~p~n",[Status1]),
    {ok,{_,ErrorAppended}} = Monitor:get_attribute(errorAppended),
    {ok,{_,NewOldStatus}} = Monitor:get_attribute(oldStatus),
    if
        ((Status1=/=?kURLok) and (((NewOldStatus=:=?kURLok) or (I=/=RStep)) and (not ErrorAppended))) ->
            {ok,{_,ErrorBuffer2}} = Monitor:get_attribute(errorBuffer),
            {ok,{_,NewLastURL}} = Monitor:get_attribute(lastURL),
            if
                ErrorBuffer2=/="" ->
                    Monitor:set_attribute(errorBuffer,ErrorBuffer2++", "++ErrorMessage++" on "++getStepName(I,StepName)++", "++NewLastURL);
                true ->
                    Monitor:set_attribute(errorBuffer,ErrorMessage++" on "++getStepName(I,StepName)++", "++NewLastURL)
            end;
        true ->
            ok
    end,
    if
        ((NewOldStatus=/=?kURLok) and (I>=RStep) and (not ResumeRemainingSteps)) ->
            Monitor:set_attribute(status,NewOldStatus),
            {ok,{_,SaveBuffer1}} = Monitor:get_attribute(saveBuffer),
            Monitor:set_attribute(contentBuffer,SaveBuffer1),
            Monitor:set_attribute(break,true);
        true ->
            {ok,{_,NewStatus}} = Monitor:get_attribute(status),
            if
                ((NewStatus=/=?kURLok) and (I<RStep)) ->
                    Monitor:set_attribute(i,RStep-1),
                    Monitor:set_attribute(oldStatus,NewStatus),
                    {ok,{_,NewContentBuffer}} = Monitor:get_attribute(contentBuffer),
                    Monitor:set_attribute(saveBuffer,NewContentBuffer);
                NewStatus=/=?kURLok ->
                    {ok,{_,StepN}} = Monitor:get_attribute(stepName),
                    Monitor:set_attribute(errorStepName,getStepName(I,StepN)),
                    Monitor:set_attribute(break,true);
                true ->
                    ok
            end
    end,
    {ok,{_,IsBreak}} = Monitor:get_attribute(break),
    if
        IsBreak ->
            break;
        true ->
            if
                StepDelaySeconds=/=0 ->
                    platform:sleep(StepDelaySeconds);
                true ->
                    ok
            end,
            {ok,{_,FinalIndex}} = Monitor:get_attribute(i),
            {ok,{_,FinalURL}} = Monitor:get_attribute(url),
            {ok,{_,FinalPost}} = Monitor:get_attribute(post),
            {ok,{_,FinalTimeoutDuration}} = Monitor:get_attribute(timeoutDuration),
            {ok,{_,FinalErrorBuffer}} = Monitor:get_attribute(errorBuffer),
            %%io:format("when leave retriveStepURL current is:~p~n",[FinalIndex+1]),
            retriveStepURL(FinalIndex+1,Len,Monitor,TraceStream,FinalURL,FinalPost,Session,Encoding,Match,ErrorContent,Proxy,ProxyUserName,ProxyPassword,PostData,UserName,Password,Domain,WhenToAuthenticate,StepDelay,StepName,EncodePostData,Timeout,TimeoutPerStep,FinalTimeoutDuration,FinalErrorBuffer,SequenceBuffer,RStep,ResumeRemainingSteps,ReferenceType,Reference,AcceptButton)
    end.
                    
                    
setMatchValue([],_,_)->ok;
setMatchValue([Vals|R],I,Monitor)->
    NewI = setEachMatch(Vals,I,Monitor),
    setMatchValue(R,NewI+1,Monitor).
    
setEachMatch([Value|R],I,Monitor)->
    NewI = I-1,
    if
        NewI=:=0 ->
            Monitor:set_attribute(list_to_atom("matchValue"),Value);
        true ->
            Monitor:set_attribute(list_to_atom("matchValue"++integer_to_list(NewI)),Value)
    end,
    setEachMatch(R,I+1,Monitor).
    
    
getUrlFromtags_a_strict([],_)->"";
getUrlFromtags_a_strict([F|R],MatchReference) ->
    Link = htmltagparser:getValue(F,href),
    Contents = htmltagparser:getValue(F,contents),
    Flag = httputils:match(Contents,MatchReference),
    if
        Flag ->
            Link;
        true ->
            getUrlFromtags_a_strict(R,MatchReference)
    end.
 

getUrlFromtags_a([],_)->"";
getUrlFromtags_a([F|R],MatchReference)->
    Link = htmltagparser:getValue(F,href),
    Flag = httputils:match(Link,MatchReference),
    if
        Flag ->
            Link;
        true ->
            getUrlFromtags_a(R,MatchReference)
    end.
    
getUrlFromtags_area([],_)->"";
getUrlFromtags_area([F|R],MatchReference)->
    Link = htmltagparser:getValue(F,href),
    Flag = httputils:match(Link,MatchReference),
    if
        Flag ->
            Link;
        true ->
            getUrlFromtags_area(R,MatchReference)
    end.
    
appendMatchPartToURL([],URL)->URL;
appendMatchPartToURL([F|R],URL)->
    appendMatchPartToURL(R,URL++F).

getDataFromtags_form([],_,_,_,_,_,_,_,_,_)->ok;
getDataFromtags_form([F|R],Form,WhichForm,Monitor,NextStep,PostData,MatchReference,HTMLTree,WhichButton,AcceptButton)->
    %%io:format("params in getDataFromtags_form:~p,~p,~p,~p,~p,~p~n",[WhichForm,NextStep,PostData,MatchReference,WhichButton,AcceptButton]),
    Form1 = Form+1,
    if
        %%选择的form num不一致时跳过
        ((WhichForm =/= -1) and (Form1 =/= WhichForm)) ->
            getDataFromtags_form(R,Form1,WhichForm,Monitor,NextStep,PostData,MatchReference,HTMLTree,WhichButton,AcceptButton);
        true ->
            Action = case htmltagparser:getValue(F,action) of
                "" ->
                    {ok,{_,LastURL}} = Monitor:get_attribute(lastURL),
                    LastURL;
                Others->
                    Others
            end,
            %%io:format("action is:~p~n",[Action]),
            Action1 = if
                length(PostData)>=NextStep ->
                    XE = lists:nth(NextStep,PostData),
                    getAction(XE,Action);
                true ->
                    Action
            end,
            %%io:format("check form params:~p,~p,~p,~p~n",[MatchReference,F,WhichButton,AcceptButton]),
            FormVariables = checkForm(MatchReference,F,HTMLTree,WhichButton,AcceptButton),
            Monitor:set_attribute(formVariables,FormVariables),
            if
                FormVariables=/=null ->
                    Monitor:set_attribute(url,Action1),
                    Method = htmltagparser:getValue(F,method),
                    if
                        Method=/="" ->
                            Monitor:set_attribute(formMethod,Method);
                        true ->
                            Monitor:set_attribute(formMethod,"get")
                    end;
                true ->
                    getDataFromtags_form(R,Form1,WhichForm,Monitor,NextStep,PostData,MatchReference,HTMLTree,WhichButton,AcceptButton)
            end
    end.
            

getAction([],Action)->Action;
getAction([F|R],Action)->
    M = url_monitor:new(),
    Flag = M:getHeaderType(F)=:=8,
    Action1 = if
        Flag->
            string:sub_string(F,string:len("Action: ")+1,string:len(F));
        true ->
            Action
    end,
    getAction(R,Action1).

checkForm(ButtonMatch,FormTag,Tree,WhichButton,AcceptButton)->
    Inputs = htmltagparser:findTags(FormTag,input),
    Button = htmltagparser:findTags(FormTag,button),
    InputsAndButtonsEnumeration = Inputs++Button,
    put(buttonName,null),
    put(buttonValue,""),
    put(foundImage,false),
    if
        ((WhichButton=:=-1) and (ButtonMatch=:="")) ->
            put(buttonName,"");
        true ->
            put(foundButton,null),
            parseInputAndButtonTags(InputsAndButtonsEnumeration,0,WhichButton,ButtonMatch),
            FoundButton = get(foundButton),
            if
                FoundButton=/=null ->
                    put(buttonName,htmltagparser:getValue(FoundButton,name)),
                    put(buttonValue,htmltagparser:getValue(FoundButton,value));
                true ->
                    ok
            end
    end,
    put(variables,null),
    ButtonName = get(buttonName),
    if
        (ButtonName=:=null and AcceptButton) ->
            put(buttonName,ButtonMatch),
            put(buttonValue,ButtonMatch);
        true ->
            ok
    end,
    ButtonName1 = get(buttonName),
    ButtonValue1 = get(buttonValue),
    %%io:format("ButtonName1:~p,ButtonValue1:~p~n",[ButtonName1,ButtonValue1]),
    if
        ButtonName1=/=null ->
            Variables = htmltagparser:getVariables1(FormTag,ButtonName1,ButtonValue1),
            %%io:format("Variables:~p~n",[Variables]),
            put(variables,Variables),
            FoundImage = get(foundImage),
            if
                FoundImage ->
                    PosName1 = if
                        ButtonName1=/="" ->
                            ButtonName1++"."++"x";
                        true ->
                            "x"
                    end,
                    Var = htmltagparser:findVar(Variables,PosName1),
                    if
                        Var =:= null ->
                            put(variables,Variables++[PosName1++"=1"]);
                        true ->
                            ok
                    end,
                    Variables1 = get(variables),
                    PosName2 = if
                        ButtonName1=/="" ->
                            ButtonName1++"."++"y";
                        true ->
                            "y"
                    end,
                    Var1 = htmltagparser:findVar(Variables1,PosName1),
                    if
                        Var1=:=null ->
                            put(variables,Variables1++[PosName2++"=1"]);
                        true ->
                            ok
                    end;
                true ->
                    ok
            end;
        true ->
            ok
    end,
    get(variables).

parseInputAndButtonTags([],_,_,_)->ok;
parseInputAndButtonTags([InputTag|R],Button,WhichButton,ButtonMatch) ->
    Type = string:to_lower(htmltagparser:getValue(InputTag,type)),
    %%io:format("input type is:~p~n",[Type]),
    put(move1,false),
    put(move2,false),
    if
        Type =:= "submit" ->
            %%io:format("get in submit~n"),
            Button1 = Button+1,
            if
                WhichButton=/=-1 ->
                    if
                        WhichButton=/=Button1 ->
                            parseInputAndButtonTags(R,Button1,WhichButton,ButtonMatch);
                        true ->
                            put(foundButton,InputTag)
                    end;
                true ->
                    SubmitValue = case htmltagparser:getValue(InputTag,value) of
                        "" ->
                            "Submit";
                        SV ->
                            SV
                    end,
                    Flag = httputils:match(SubmitValue,ButtonMatch),
                    if
                        Flag ->
                            put(foundButton,InputTag);
                        true ->
                            Flag1 = httputils:match(htmltagparser:getValue(InputTag,name),ButtonMatch),
                            if
                                Flag1 ->
                                    put(foundButton,InputTag),
                                    parseInputAndButtonTags(R,Button1,WhichButton,ButtonMatch);
                                true ->
                                    put(move1,true)
                            end
                    end
            end;
        Type=/="image" ->
            Button1 = Button,
            parseInputAndButtonTags(R,Button1,WhichButton,ButtonMatch);
        true ->
            Button1 = Button+1,
            if
                WhichButton=/=-1 ->
                    if
                        WhichButton=/=Button1 ->
                            parseInputAndButtonTags(R,Button1,WhichButton,ButtonMatch);
                        true ->
                            put(foundButton,InputTag),
                            put(foundImage,true)
                    end;
                true ->
                    Flag = httputils:match(htmltagparser:getValue(InputTag,name),ButtonMatch),
                    if
                        Flag ->
                            put(foundButton,InputTag),
                            put(foundImage,true);
                        true ->
                            Flag1 = httputils:match(htmltagparser:getValue(InputTag,alt),ButtonMatch),
                            if
                                Flag1->
                                    put(foundButton,InputTag),
                                    put(foundImage,true);
                                true ->
                                    Flag2 = httputils:match(htmltagparser:getValue(InputTag,value),ButtonMatch),
                                    if
                                        Flag2 ->
                                            put(foundButton,InputTag),
                                            put(foundImage,true);
                                        true ->
                                            Flag3 = httputils:match(htmltagparser:getValue(InputTag,src),ButtonMatch),
                                            if
                                                Flag3 ->
                                                    put(foundButton,InputTag),
                                                    put(foundImage,true);
                                                true ->
                                                    put(move2,true)
                                            end
                                    end
                            end
                    end
            end
    end,
    Move1 = get(move1),
    Move2 = get(move2),
    if
        (Move1 and Move2) ->
            parseInputAndButtonTags(R,Button1,WhichButton,ButtonMatch);
        true ->
            break
    end.
    
getUrlFromtags_frame([],_,_)->ok;
getUrlFromtags_frame([F|R],MatchReference,Monitor)->
    Name = case htmltagparser:getValue(F,name) of
        null ->
            "";
        Other->
            Other
    end,
    Flag = httputils:match(Name,MatchReference),
    if
        Flag ->
            Monitor:set_attribute(url,htmltagparser:getValue(F,src)),
            {ok,{_,RedirectBuffer}} = Monitor:get_attribute(redirectBuffer),
            Monitor:set_attribute(parentURL,RedirectBuffer);
        true ->
            getUrlFromtags_frame(R,MatchReference,Monitor)
    end.

getUrlFromtags_iframe([],_,_)->ok;
getUrlFromtags_iframe([F|R],MatchReference,Monitor)->
    Name = case htmltagparser:getValue(F,name) of
        null ->
            "";
        Other ->
            Other
    end,
    Flag = httputils:match(Name,MatchReference),
    if
        Flag ->
            Monitor:set_attribute(url,htmltagparser:getValue(F,src)),
            {ok,{_,RedirectBuffer}} = Monitor:get_attribute(redirectBuffer),
            Monitor:set_attribute(parentURL,RedirectBuffer);
        true ->
            getUrlFromtags_iframe(R,MatchReference,Monitor)
    end.

getUrlFromtags_meta([],_,_)->ok;
getUrlFromtags_meta([F|R],MatchReference,Monitor) ->
    Flag = string:to_lower(htmltagparser:getValue(F,'http-equiv'))=:="refresh",
    if
        Flag ->
            Content = htmltagparser:getValue(F,content),
            EqualsIndex = string:str(Content,"="),
            if
                EqualsIndex=/=0 ->
                    URL = string:strip(string:sub_string(Content,EqualsIndex+1,string:len(Content))),
                    Monitor:set_attribute(url,URL),
                    Flag1 = httputils:match(URL,MatchReference),
                    if
                        Flag1 ->
                            break;
                        true ->
                            getUrlFromtags_meta(R,MatchReference,Monitor)
                    end;
                true ->
                    getUrlFromtags_meta(R,MatchReference,Monitor)
            end;
        true ->
            getUrlFromtags_meta(R,MatchReference,Monitor)
    end.

mergeForEachPost(null,FormVariables)->FormVariables;
mergeForEachPost([],FormVariables)->FormVariables;
mergeForEachPost([F|R],FormVariables) ->
    NewFormVariables = mergeVariable(FormVariables,F),
    mergeForEachPost(R,NewFormVariables).

mergeVariable(FormVariables,NewVar) ->
    Key = httputils:readStringFromStart(NewVar,"="),
    F1 = httputils:startsWith(Key,"["),
    F2 = httputils:endsWith(Key,"]"),
    ThisVarNumber = if
        (F1 and F2) ->
            list_to_integer(string:sub_string(Key,2,string:len(Key)-1));
        true ->
            -1
    end,
    {Added,NewVar1,Result} = eachFormVariables(FormVariables,0,false,Key,ThisVarNumber,NewVar,[]),
    if
        (not Added) ->
            Result++[NewVar1];
        true ->
            Result
    end.
    
eachFormVariables([],_,Added,_,_,NewVar,Result)->{Added,NewVar,Result};
eachFormVariables([F|R],VarCount,Added,Key,ThisVarNumber,NewVar,Result)->
    VarCount1 = VarCount+1,
    OldVar = F,
    OldKey = httputils:readStringFromStart(OldVar,"="),
    Matched = Key=:=OldKey,
    if
        ThisVarNumber=:=VarCount1 ->
            Matched1 = true,
            EqualsIndex = string:str(NewVar,"="),
            NewValue = if
                EqualsIndex >=0 ->
                    string:sub_string(NewVar,EqualsIndex+1,string:len(NewVar));
                true ->
                    ""
            end,
            NewVar1 = OldKey++"="++NewValue,
            Added1 = false;
        true ->
            Matched1 = Matched,
            NewVar1 = NewVar,
            Added1 = Added
    end,
    Flag = httputils:endsWith(Key,"*"),
    if
        Flag ->
            NewVariableName = string:sub_string(Key,1,string:len(Key)-1),
            Flag1 = httputils:startsWith(OldKey,NewVariableName),
            if
                Flag1 ->
                    Matched2 = true,
                    EqualsIndex1 = string:str(NewVar1,"="),
                    NewValue1 = if
                        EqualsIndex1>=0 ->
                            string:sub_string(NewVar1,EqualsIndex1+1,string:len(NewVar1));
                        true ->
                            ""
                    end,
                    NewVar2 = OldKey++"="++NewValue1,
                    Added2 = false;
                true ->
                    Matched2 = Matched1,
                    Added2 = Added1,
                    NewVar2 = NewVar1
            end;
        true ->
            Matched2 = Matched1,
            Added2 = Added1,
            NewVar2 = NewVar1
    end,
    Flag2 = (not Matched2),
    Flag3 = (not Added2),
    NewResult = if
        Flag2 ->
            Added3 = Added2,
            Result++[OldVar];
        Flag3 ->
            Added3 = true,
            Result++[NewVar2];
        true ->
            Added3 = Added2,
            Result
    end,
    eachFormVariables(R,VarCount1,Added3,Key,ThisVarNumber,NewVar2,NewResult).

checkFormVariables([],NewPost,URL,_,_,_,Monitor,NewFormVariables) ->
    Monitor:set_attribute(url,URL),
    Monitor:set_attribute(formVariables,NewFormVariables),
    NewPost;
checkFormVariables([NameValue|R],NewPost,URL,Separator,Encoding,NextStep,Monitor,NewFormVariables)->
    M = url_monitor:new(),
    HeaderType = M:getHeaderType(NameValue),
    M:delete(),
    if
        HeaderType>0 ->
            Separator1 = Separator,
            NewPost1 = NewPost++[NameValue],
            NewFormVariables1 = NewFormVariables++[NameValue],
            URL2 = URL;
        true ->
            NameValue1 = if
                HeaderType=:=0 ->
                    string:sub_string(NameValue,string:len("Custom-Content: ")+1,string:len(NameValue));
                true ->
                    NameValue
            end,
            NewFormVariables1 = NewFormVariables++[NameValue1],
            EqualsIndex = string:str(NameValue1,"="),
            Key = if
                EqualsIndex=/=0 ->
                    string:sub_string(NameValue1,1,EqualsIndex-1);
                true ->
                    NameValue1
            end,
            URL1 = URL++Separator++iconv:convert("utf-8",lists:nth(NextStep,Encoding),Key),
            URL2 = if
                EqualsIndex=/=0 ->
                    Val = string:sub_string(NameValue1,EqualsIndex+1,string:len(NameValue1)),
                    URL1++"="++iconv:convert("utf-8",lists:nth(NextStep,Encoding),Val);
                true ->
                    URL1
            end,
            NewPost1 = NewPost,
            Separator1 = "&"
    end,
    checkFormVariables(R,NewPost1,URL2,Separator1,Encoding,NextStep,Monitor,NewFormVariables1).

extractLinkRef(MatchReference) ->
    LinkIndexSeparator = "\\link_sep/",
    SeparatorLocation = string:rstr(MatchReference,LinkIndexSeparator),
    Len = string:len(MatchReference),
    if
        ((SeparatorLocation=/=0) and (SeparatorLocation<Len)) ->
            case string:to_integer(string:sub_string(MatchReference,SeparatorLocation+string:len(LinkIndexSeparator),Len)) of
                {error,_} ->
                    MatchReference1 = MatchReference,
                    Index = 1;
                {N,_} ->
                    MatchReference1 = string:sub_string(MatchReference,1,SeparatorLocation-1),
                    Index = N
            end;
        true ->
            MatchReference1 = MatchReference,
            Index = 1
    end,
    #linkReference{link = MatchReference1,index = Index}.
            
%% @spec checkReferer(Post,Post,Flag) -> {NewPost,NewFlag}
%% where
%% Post = list()
%% Flag = bool()
%% NewPost = list()
%% NewFlag = bool()
%% @doc Get referer from postdata,if referer is exist set flag false.
checkReferer([],Post,Flag)->{Post,Flag};
checkReferer([F|R],Post,Flag)->
    case F of
        "Custom-Header: Referer:"++_ ->
            Index = string:str(F,"[none]"),
            NewArray = if
                Index=/=0 ->
                    lists:delete(F,Post);
                true ->
                    Post
            end,
            {NewArray,false};
        _->
            checkReferer(R,Post,Flag)
    end.

%% @spec appendContentBuffer(URL,Redirect,SequenceBuffer,ContentBuffer,Body,Head,Step,MoreStep) -> NewContentBuffer
%% where
%% Post = list()
%% Flag = bool()
%% NewPost = list()
%% NewFlag = bool()
%% @doc Create message for current step,the message include the request and response for the session.
appendContentBuffer(URL,Redirect,SequenceBuffer,ContentBuffer,Body,Head,Step,MoreStep)->
    HTMLTree = htmltagparser:process(Body),
    Titles = htmltagparser:findTags(HTMLTree,title),
    Title = getTitleContents(Titles,""),
    S1 = "<HR><B><A NAME=step"++integer_to_list(Step)++">Step "++integer_to_list(Step)++"</A></B><H3>",
    S2 = if
        length(Title)>0 ->
            S1++Title++" : ";
        true ->
            S1
    end,
    S3 = S2++URL++"</H3>",
    S5 = S3,
    S5++"<HR><P><PRE>"++Redirect++"\n\n"++ContentBuffer++"</PRE>\n".

getTitleContents([],Title)->Title;
getTitleContents([F|R],Title) ->
    NewTitle = htmltagparser:getValue(F,contents),
    getTitleContents(R,NewTitle).
    

getStepName(I,StepName)->
    if
        (I>=(length(StepName))) ->
            "step "++integer_to_list(I);
        true ->
            case lists:nth(I,StepName) of
                "" ->
                    "step "++integer_to_list(I);
                Name->
                    Name++" (step "++integer_to_list(I)++")"
            end
    end.
    
updateMatchValue(Result,Match,Content,Label)->
    {Body,Head} = Content,
    {Flag,MatchSummary,MatchValues} = httputils:matchExpression(Body,Match,[],"",Label),
    {_,MatchSummary1,MatchValues1} = if
        Flag=/=?kURLok ->
            M=url_monitor:new(),
            Encode = M:getHTMLEncoding(Head,Body),
            M:delete(),
            httputils:matchExpression(Body,iconv:convert("utf-8", Encode, Match),[],"",Label);
        true ->
            {Flag,MatchSummary,MatchValues}
    end,
    FinalStatus = if
        MatchSummary1=/="" ->
            MS = if
                Result=/="" ->
                    Result++", ";
                true ->
                    Result
            end,
            MS++MatchSummary1;
        true ->
            Result
    end,
    {MatchValues1,FinalStatus}.
    
    
%% @spec putproperty_to_list(I) -> ok
%% where
%% I = integer()
%% @doc Put the properties into attribute.
putproperty_to_list(I) ->
	putproperty_to_list(1,I).
	
%%referenceType,reference,encoding,encodePostData,postData,contentMatch,errorContent,userName,password,domain,whenToAuthenticate,stepDelay,stepName,stepName
putproperty_to_list(N1,N2) ->
	case N1=<N2 of
		true ->
			{ok,{_,X1}} = THIS:get_property(list_to_atom("referenceType"++integer_to_list(N1))),
			{ok,{_,Y1}} = THIS:get_attribute(referenceType),
			THIS:set_attribute(referenceType,Y1++[X1]),
			{ok,{_,X2}} = THIS:get_property(list_to_atom("reference"++integer_to_list(N1))),
			{ok,{_,Y2}} = THIS:get_attribute(reference),
			THIS:set_attribute(reference,Y2++[X2]),
			{ok,{_,X3}} = THIS:get_property(list_to_atom("encoding"++integer_to_list(N1))),
			{ok,{_,Y3}} = THIS:get_attribute(encoding),
			THIS:set_attribute(encoding,Y3++[X3]),
			{ok,{_,X4}} = THIS:get_property(list_to_atom("postData"++integer_to_list(N1))),
			{ok,{_,Y4}} = THIS:get_attribute(postData),
			THIS:set_attribute(postData,Y4++[string:tokens(X4,"\n")]),
			{ok,{_,X5}} = THIS:get_property(list_to_atom("contentMatch"++integer_to_list(N1))),
			{ok,{_,Y5}} = THIS:get_attribute(contentMatch),
			THIS:set_attribute(contentMatch,Y5++[X5]),
			{ok,{_,X6}} = THIS:get_property(list_to_atom("errorContent"++integer_to_list(N1))),
			{ok,{_,Y6}} = THIS:get_attribute(errorContent),
			THIS:set_attribute(errorContent,Y6++[X6]),
			{ok,{_,X7}} = THIS:get_property(list_to_atom("userName"++integer_to_list(N1))),
			{ok,{_,Y7}} = THIS:get_attribute(userName),
			THIS:set_attribute(userName,Y7++[X7]),
			{ok,{_,X8}} = THIS:get_property(list_to_atom("password"++integer_to_list(N1))),
			{ok,{_,Y8}} = THIS:get_attribute(password),
			THIS:set_attribute(password,Y8++[X8]),
			{ok,{_,X9}} = THIS:get_property(list_to_atom("domain"++integer_to_list(N1))),
			{ok,{_,Y9}} = THIS:get_attribute(domain),
			THIS:set_attribute(domain,Y9++[X9]),
			{ok,{_,X10}} = THIS:get_property(list_to_atom("whenToAuthenticate"++integer_to_list(N1))),
			{ok,{_,Y10}} = THIS:get_attribute(whenToAuthenticate),
			THIS:set_attribute(whenToAuthenticate,Y10++[X10]),
			{ok,{_,X11}} = THIS:get_property(list_to_atom("stepDelay"++integer_to_list(N1))),
			{ok,{_,Y11}} = THIS:get_attribute(stepDelay),
			THIS:set_attribute(stepDelay,Y11++[X11]),
			{ok,{_,X12}} = THIS:get_property(list_to_atom("stepName"++integer_to_list(N1))),
			{ok,{_,Y12}} = THIS:get_attribute(stepName),
			THIS:set_attribute(stepName,Y12++[X12]),
			{ok,{_,X13}} = THIS:get_property(list_to_atom("encodePostData"++integer_to_list(N1))),
			{ok,{_,Y13}} = THIS:get_attribute(encodePostData),
			THIS:set_attribute(encodePostData,Y13++[X13]),
			putproperty_to_list(N1+1,N2);
		_->
			ok
	end.
    
clearAttribute()->
    THIS:set_attribute(referenceType,[]),
    THIS:set_attribute(reference,[]),
    THIS:set_attribute(encoding,[]),
    THIS:set_attribute(postData,[]),
    THIS:set_attribute(errorContent,[]),
    THIS:set_attribute(userName,[]),
    THIS:set_attribute(password,[]),
    THIS:set_attribute(domain,[]),
    THIS:set_attribute(whenToAuthenticate,[]),
    THIS:set_attribute(stepDelay,[]),
    THIS:set_attribute(stepName,[]),
    THIS:set_attribute(encodePostData,[]).

%% @spec getNumberOfSteps() -> StepNum
%% where
%% StepNum = integer()
%% @doc Get the total step number.
getNumberOfSteps() ->
	getNumberOfSteps(0,1).
	
getNumberOfSteps(N1,N2) ->
	F1 = N2<?numberOfSteps,
	{ok,{_,Reference}} = THIS:get_property(list_to_atom("reference"++integer_to_list(N2))),
	F2 = Reference=/="",
	if
		((F1) and (F2)) ->
			getNumberOfSteps(N1+1,N2+1);
		true ->
			N1
	end.

%% @spec getCostInLicensePoints() -> Points
%% where
%% Points = integer()
%% @doc Each step cost 1 point.
getCostInLicensePoints() ->
	getNumberOfSteps() * 1.
    
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
	case atom_to_list(Prop) of
		"whenToAuthenticate"++_ ->
			[{"Use Global Preference","Use Global Preference"},{"Authenticate first request","authOnFirst"},{"Authenticate if requested","authOnSecond"}];
		"encodePostData"++_ ->
			[{"Use content-type:","contentTypeUrlencoded"},{"force url encoding","forceEncode"},{"force NO url encoding","forceNoEncode"}];
		"referenceType"++_ ->
			[{"link","link"},{"form","form"},{"frame","frame"},{"refresh","refresh"}];
		"resumeStep"->
			[{"None","0"}];
			%%++buildscalar(getpagestep(Params,1));
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

getHostname()-> "url sequence monitor".

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++
	allocateStepProperties(?numberOfSteps) ++
	[
	#property{name=timeout,title="Timeout",description="the time out, in seconds, to wait for entire sequence to complete",type=numeric,advance=true,optional=true,default=60,order=249,baselinable=true},
	#property{name=timeoutPerStep,title="Timeout is Per Step",description="when selected, the timeout specified above is for each step, rather than the overall time",type=bool,advance=true,optional=true,order=250},
	#property{name=proxy,title="HTTP Proxy",description="optional list of proxy servers to use including port",type=text,advance=true,optional=true,order=251},
	%%#property{name=images,title="Retrieve Images",description="when selected, all of the graphics externally referenced in the page will also be retrieved and calculated in the total response time",type=bool,advance=true,optional=true,order=252},
	%%#property{name=frames,title="Retrieve Frames",description="when selected, all of the URLs referenced by frames in a frameset will be retrieved and calculated in the total response time",type=bool,advance=true,optional=true,order=253},
	#property{name=proxyUserName,title="Proxy Server User Name",description="optional user name if the proxy server requires authorization",type=text,advance=true,optional=true,order=254},
	#property{name=proxyPassword,title="Proxy Server Password",description="optional password if the proxy server requires authorization",type=password,advance=true,optional=true,order=255},
	#property{name=resumeStep,title="Resume at step, if error",description="If a step returns an error, you can specify where to resume the sequence.",type=scalar,advance=true,optional=true,order=256},
	#property{name=resumeRemainingSteps,title="Execute resume step and remaining steps",description="If resume step is executed, this selection tells the monitor to execute that step and continue executing to the end.",type=bool,advance=true,optional=true,order=257},
	%%#property{name=measureDetails,title="Show Detailed Measurements",description="when selected, detailed measurement times are displayed for DNS lookup, connecting, server response, and downloading.",type=bool,advance=true,optional=true,order=258},
	#property{name=httpVersion10,title="HTTP Version",description="when unselected, use HTTP Version 1.1 in the request header; when selected, use 1.0",type=bool,advance=true,optional=true,order=259},
	%%#property{name=retries,title="Retries",description="The number of times (0-10) to retry the request on recoverable errors, if monitor times out retries are cut short.",type=numeric,advance=true,optional=true,order=260},
	#property{name=acceptAllUntrustedCerts,title="Accept Untrusted Certs for HTTPS",description="Accept certificates that are untrusted in the cert chain.",type=bool,advance=true,optional=true,order=261},
	#property{name=acceptInvalidCerts,title="Accept Invalid Certs for HTTPS",description="Accept certificates even if todays date in not in the date ranges in the cert chain.",type=bool,advance=true,optional=true,order=262},
	#property{name=status,title="status",type=numeric,state=true,configurable=false},
    #property{name=roundTripTime,title="roundTripTime",type=numeric,state=true,configurable=false,baselinable=true},
    #property{name=matchValue,title="content match",type=numeric,state=true,configurable=false}
	].
