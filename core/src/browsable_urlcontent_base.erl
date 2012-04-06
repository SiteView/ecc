%%
%%browsableURLContentBase
%%
%%

-module(browsable_urlcontent_base,[BASE]).
-extends(browsable_base).
-compile(export_all).
-include("monitor_template.hrl").

-define(nMaxCounters,120).
-define(defaultGetBrowseTreeTimeout,120).

verify(Params)->
	Errs =
	case proplists:get_value(browse,Params) of
		undefined->
			[{browse,"must select at least one browse counter"}];
		[]->
			[{browse,"must select at least one browse counter"}];
		_->
			[]
	end ++
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true ->
			{ok,""}
	end.

new()->
	Base = browsable_base:new(),
	Base:set_attribute(stringbuffer,""),
	Base:set_attribute(stringbuffer1,""),
	Base:set_attribute(latestprefix,""),
	Base:set_attribute(prefix,""),
	{?MODULE,Base}.

setPrecision() ->ok.

getBrowseTreeTimeoutParameterName() ->ok.

getMaxCounters() ->
	?nMaxCounters.
	
%%Just flat swing, do not use tree structure
getCounterByXml(S) ->
	XML=string:tokens(S,"\r\n"),
	C = findcounter(XML,[],[]),
    %%printcounter(C),
	for1(1,C).
    
printcounter([]) ->"";
printcounter([F|R])->
    F++"\r\n"++printcounter(R).
	
findcounter([],_,CountersName)->CountersName;
findcounter([F|R],Prefix,CountersName) ->
    Line = string:strip(F),
    case Line of
        "<object name="++Rest ->
            FromIndex = string:str(Rest,"\""),
            LastIndex = httputils:indexOf(Rest,"\"",FromIndex+1),
            Object = string:sub_string(Rest,FromIndex+1,LastIndex-1),
            %%io:format("prefix is:~p~n",[Prefix++[Object]]),
            findcounter(R,Prefix++[Object],CountersName++[addPrefixToCounter(Prefix++[Object])]);
        "<counter name="++Rest ->
            FromIndex = string:str(Rest,"\""),
            LastIndex = httputils:indexOf(Rest,"\"",FromIndex+1),
            Counter = string:sub_string(Rest,FromIndex+1,LastIndex-1),
            findcounter(R,Prefix,CountersName++[prefix(Prefix)++Counter]);
        "</object>"->
            findcounter(R,lists:sublist(Prefix,1,length(Prefix)-1),CountersName);
        _->
            findcounter(R,Prefix,CountersName)
    end.

prefix([])->"";
prefix([F|R])->
    F++"/"++prefix(R).
    
addPrefixToCounter([])->"";
addPrefixToCounter([F|R]) ->
    if
        R=/=[] ->
            F++"/"++addPrefixToCounter(R);
        true ->
            F++addPrefixToCounter(R)
    end.
	
%%为每个counter加一个different key
for1(_,[]) ->[];
for1(N,[F|R]) ->
	[{integer_to_list(N),F}]++for1(N+1,R).
	
	
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{countersInError,'==',0}]
	end.	

get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=proxy,title="HTTP Proxy", description="optional proxy server to use including port (example:proxy.siteview.com:8080)",type=text,order=8},
	#property{name=proxyUserName,title="Proxy Server User Name",description="optional user name if the proxy server requires authorization",type=text,order=9},
	#property{name=proxyPassword,title="Proxy Server Password",description="optional password if the proxy server requires authorization",type=password,order=10},
	#property{name=timeout,title="Timeout",description="the time out, in seconds, to wait for the response",type=numeric,advance=true,optional=true,default=60,order=11,baselinable=true}
	].