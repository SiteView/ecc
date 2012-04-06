%% @author kaiyang.cheng@dragonflow.com
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Oracle9i Application Server.
%%
%% Description: Oracle 9i Application Server monitor to monitor the server performance data for Oracle 9i servers.
%% %% Versions supported: Oracle 9i Application Server
%% Platform: All
%% Requirement:
%% a: Must enable Web caching on the Oracle 9i Application Server to use the Oracle 9i Application Server monitor.
%%
-module(oracle9i_monitor,[BASE]).
-compile(export_all).
-extends(urlcontent_base).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,verify/1,update/0,get_counter_name/1,set_counter_val/3,getTemplateFile/0,get_classifier/1,get_template_property/0]).

-define(TEMPLATE_FILE,"counters.oracle").
-define(DEFAULT_TIMEOUT,60000).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = urlcontent_base:new(),
	{?MODULE,Obj}.

%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
verify(Params)->
	Errs =
	case proplists:get_value(url,Params) of
		""->
			[{url,"URL is missing."}];
		URL->
			case string:str(URL," ") of
				0->
					[];
				_->
					[{url,"no spaces are allowed"}]
			end
	end ++
	case proplists:get_value(counters,Params) of
		[]->
			[{counters,"Counters is missing."}];
		_->
			[]
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

%% @spec update() -> ok
%% @doc Run the monitor.
%%
%% Description: Oracle9i Application Server Monitor get Oracle9i service counters value with url
update()->
    THIS:set_attribute(countersInError,0),
	{ok,{_,Url}} = THIS:get_property(url),
	{ok,{_,Proxy}} = THIS:get_property(proxy),
	{ok,{_,ProxyUserName}} = THIS:get_property(proxyUserName),
	{ok,{_,ProxyPassword}} = THIS:get_property(proxyPassword),
	{ok,{_,Username1}} = THIS:get_property(userName),
	{ok,{_,Password}} = THIS:get_property(password),
	{ok,{_,NTLM}} = THIS:get_property(ntlm),
	Username = if
		NTLM ->
			"[NT]"++Username1;
		true ->
			Username1
	end,
	{ok,{_,T}} = THIS:get_property(timeout),
	Timeout = if
		T=:=0 ->
			?DEFAULT_TIMEOUT;
		true ->
			T*1000
	end,
    %%Slightly different from other inherited urlcontent_base the monitor, you need to write all this is not a match this way counters
    RegExp = buildRegExp(""),
	%%io:format("Retrieving URL ~p~n",[Url]),
	{ok,{_,Class}} = THIS:get_property(?CLASS),
	{ok,{_,Id}} = THIS:get_property(?ID),
    Profile = list_to_atom(atom_to_list(Class) ++ "-" ++ atom_to_list(Id)),
	M = url_content_monitor:new(),
    M:set_attribute(profile,Profile),
	URLResults = M:checkURL(Url,"","",Proxy,ProxyUserName,ProxyPassword,[],Username,Password,"","",51200,"",0,Timeout,""),
	M:delete(),
    State = URLResults#urlresults.status,
    Head = URLResults#urlresults.head,
    Body = URLResults#urlresults.body,
    IsValueExpression = httputils:isValueExpression(RegExp),
    {ok,{_,Counters}} = THIS:get_property(counters),
    if
        IsValueExpression ->
            {ok,{_,PreliminaryRegex}} = THIS:get_attribute(string),
            OriginalState = element(1,httputils:matchExpression(Body,PreliminaryRegex)),
            Flag1 = ((length(PreliminaryRegex)=:=0) or (httputils:matchExpression(Body,PreliminaryRegex)=:=?kURLok)),
            if
                Flag1 ->
                    Ret = httputils:matchExpression(Body,RegExp,[],""),
                    MatchStatus = element(1,Ret),
                    if
                        MatchStatus=/=?kURLok ->
                            URLMonitor = url_monitor:new(),
                            HTMLEncode = URLMonitor:getHTMLEncoding(Head),
                            URLMonitor:delete(),
                            EncodeBody = iconv:convert(HTMLEncode,httputils:pageEncode(),Body),
                            NewRet = httputils:matchExpression(EncodeBody,RegExp,[],"");
                        true ->
                            NewRet = Ret
                    end;
                true ->
                    NewRet = {OriginalState,[],[]}
            end,
            case element(1,NewRet) of
                ?kURLok ->
                    MatchStateString = set_counter_val(Counters,element(3,NewRet),"");
                _ ->
                    MatchStateString = "",
                    set_error_val(Counters,"")
            end;
        true ->
            NewRet = {State,"",""},
            MatchStateString = ""
    end,
    Flag2 = ((element(1,NewRet)=:=200) and (length(MatchStateString)>0)),
    if
        Flag2 ->
            THIS:set_attribute(?STATE_STRING,MatchStateString),
            THIS:set_attribute(status,integer_to_list(element(1,NewRet)));
        true ->
            Error = case element(1,NewRet) of
                200 ->
                    THIS:set_attribute(status," no match "),
                    "invalid match expression ";
                _ ->
                    THIS:set_attribute(status,httputils:lookupStatus(element(1,NewRet))),
                    THIS:set_attribute(?NO_DATA, true),
                    httputils:lookupStatus(element(1,NewRet))
            end,
            THIS:set_attribute(?STATE_STRING,Error)
    end.

%% @spec set_error_val(Counters,ErrorString) -> NewErrorString
%% where
%% Counters = list()
%% ErrorString = string()
%% NewErrorString = string()
%% @doc Set error status for counters and build state string.
set_error_val([],ErrorString)->ErrorString;
set_error_val([C|T],ErrorString) ->
    S = case C of
        {K,V} ->
            THIS:set_attribute(K,"n/a"),
            THIS:inc_attribute(countersInError),
			V++" = "++"n/a"++"<br>";
		_ ->
			""
	end,
    set_error_val(T,ErrorString++S).

%% @spec set_counter_val(Counters,MatchValue,ErrorString) -> NewErrorString
%% where
%% Counters = list()
%% ErrorString = string()
%% NewErrorString = string()
%% @doc Set error status for counters and build state string.
set_counter_val([],_,MatchValueString)->MatchValueString;
set_counter_val([C|T],[F|R],MatchValueString)->
	S = case C of
		{K,V} ->
			THIS:set_attribute(V, list_to_integer(F)),
			V++" = "++F++"<br>";
		_ ->
			THIS:inc_attribute(countersInError),
			""
	end,
    set_counter_val(T,R,MatchValueString++S).
	
%%Need to make changes in the web page information
buildRegExp(S) ->
	THIS:set_attribute(flag,false),
	THIS:set_attribute(flag1,true),
	AllCounters = THIS:getDefaultCounters(),
	THIS:set_attribute(string,S++"/"),
	THIS:set_attribute(string1,"/Up\\/Down Time.*?"),
	filter(AllCounters).
	
filter([])->
	{ok,{_,S1}} = THIS:get_attribute(string1),
	{ok,{_,S}} = THIS:get_attribute(string),
	THIS:set_attribute(string,S++"/"),
	THIS:set_attribute(string,S1++"/"),
	S1++"/";
filter([F|R]) ->
	case F of
		{_,N} ->
			{ok,{_,F1}} = THIS:get_attribute(flag1),
			F2 = httputils:startsWith(N,"Apology"),
			F3 = case re:run(N,".*Up\\/Down Time.*") of
				{match,_}->
					true;
				_->
					false
			end,
			F4 = isCounterSelected(N),
			if
				(F2 and F1) ->
					THIS:set_attribute(flag1,false),
					{ok,{_,S1}} = THIS:get_attribute(string1),
					THIS:set_attribute(string1,S1++"Network Error.*?");
				true ->
					ok
			end,
			if
				F3 ->
					if
						F4 ->
							{ok,{_,SS1}} = THIS:get_attribute(string1),
							THIS:set_attribute(string1,SS1++"(UP|DOWN).*?"),
							{ok,{_,S}} = THIS:get_attribute(string),
							THIS:set_attribute(string1,S++"(UP|DOWN).*?");
						true ->
							{ok,{_,SS1}} = THIS:get_attribute(string1),
							THIS:set_attribute(string1,SS1++"[UD][PO]W?N?.*?"),
							{ok,{_,S}} = THIS:get_attribute(string),
							THIS:set_attribute(string1,S++"[UD][PO]W?N?.*?")
					end;
				true ->
					if
						F4 ->
							{ok,{_,SSS1}} = THIS:get_attribute(string1),
							THIS:set_attribute(string1,SSS1++">\\s*([-0-9]\\d*\\.?\\d*).*?");
						true ->
							{ok,{_,SSS1}} = THIS:get_attribute(string1),
							THIS:set_attribute(string1,SSS1++">\\s*[-0-9]\\d*\\.?\\d*.*?")
					end
			end;
		_->
			ok
	end,
	filter(R).

	
isCounterSelected(S) ->
	{ok,{_,Counters}} = THIS:get_property(counters),
	lists:keymember(S,2,Counters).


get_counter_name([]) ->[];
get_counter_name([F|R]) ->
	[atom_to_list(element(2,F))|get_counter_name(R)].

getCountersContent()->
	case THIS:get_property(counters) of
		{ok,{_,V}}->
			V;
		_->
			[]
	end.

getTemplateFile()->
	?TEMPLATE_FILE.

getDefaultCounters()->
	THIS:getURLContentCounters(THIS,"",false).

getCostInLicensePoints() ->
    I = THIS:getActiveCounters(THIS),
    1*I.

get_classifier(error)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'!=',0}]
	end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'!=',0}]
	end,

	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Count = THIS:getMaxCounters(),
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls) < Count ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,Count - length(Cls)));
		true ->
			Cls
	end.
	
getHostname()->
	case THIS:get_property(url) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=url,title="Host Name", description="the server administration URL (example: http://server:port/webcacheadmin?SCREEN_ID=CGA.Site.Stats&ACTION=Show)",type=text,editable=true,order=2},
	#property{name=timeout,title="Timeout",type=numeric,description="connect time out(second)",advance=true,optional=true,order=3,default=60,baselinable=true},
	#property{name=proxy,title="HTTP Proxy",type=text,description="optional list of proxy servers to use including port (example: proxy.siteview.com:8080)",advance=true,optional=true,order=5},
	#property{name=userName,title="Authorization User Name",type=text,description="optional user name if the URL requires authorization.",advance=true,optional=true,order=6},
	#property{name=password,title="Authorization Password",type=password,description="optional password if the URL requires authorization",advance=true,optional=true,order=7},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,default=THIS:getDefaultCounters()},
	#property{name=ntlm,title="NT Challenge Response",type=bool,description="when selected, use NT Challenge Response authorization",advance=true,optional=true,order=8},
	#property{name=proxyUserName,title="Proxy Server User Name",type=text,description="optional user name if the proxy server requires authorization",advance=true,optional=true,order=9},
	#property{name=proxyPassword,title="Proxy Server Password",type=password,description="optional password if the proxy server requires authorization",advance=true,optional=true,order=10},
	#property{name=status,title="status",type=text,state=true,configurable=false}
	].