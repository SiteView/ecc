%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Netscope Server Monitor.
%% 
%% Description: For Netscope server version 4.1,6.0
-module(iplanet_server_monitor,[BASE]).
-extends(urlcontent_base).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,verify/1,update/0,process_body/1,get_counter_name/1,getTemplateFile/0,getDefaultCounters/0,get_classifier/1,get_template_property/0]).

-define(TEMPLATE_FILE,"counters.iplanet").
-define(DEFAULT_TIMEOUT,60000).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = urlcontent_base:new(),
	Obj:set_attribute(flag,false),
	Obj:set_attribute(s4,""),
	Obj:set_attribute(countersInError,0),
	Obj:set_attribute(server,""),
	Obj:set_attribute(value,""),
	Obj:set_attribute(error,""),
	{?MODULE,Obj}.
	
	
%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
%%
%% Description:check url,counters,timeout
verify(Params)->
	Errs =
	case proplists:get_value(url,Params) of
		""->
			[{url,"URL is missing"}];
		URL->
			case string:str(URL," ") of
				0 ->
					[];
				_->
					[{url,"no spaces are allowed"}]
			end
	end ++
	case proplists:get_value(counters,Params) of
		[] ->
			[{counters,"Counters is missing"}];
		_->
			[]
	end ++
	case proplists:get_value(timeout,Params) of
		""->
			[{time,"timeout missing."}];
		V->
			if
				not is_number(V) ->
					[{time,"timeout must be a number."}];
				true->
					[]
			end
	end ++
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
%%Description:before get counters value,check whether url is fit with counters name
%%Get counters value with url
update()->
    %%error protection,reset attribute
	THIS:set_attribute(flag,false),
	THIS:set_attribute(s4,""),
	THIS:set_attribute(server,""),
	THIS:set_attribute(value,""),
	THIS:set_attribute(error,""),
	THIS:set_attribute(countersInError,0),
    %%
	{ok,{_,Url}} = THIS:get_property(url),
	{ok,{_,Counters}} = THIS:get_property(counters),
	THIS:set_attribute(flag,false),
	Counter = [V||{_,V}<-Counters],
	CounterName = joinname(Counter),
	As = ["Other", "Of", "404", "Max", "Cache", "Keep", "Thread"],
	As1 = ["Other", "Max", "Of", "transfer", "requ", "xx"],
	As2 = ["transfer", "requ", "xxx", "Thread"],
	As3 = ["sitemon", ".perf", "Stats"],
	Flag1 = string:str(Url,lists:nth(1,As3))=/=0,
	Flag2 = string:str(Url,lists:nth(2,As3))=/=0,
	Flag3 = string:str(Url,lists:nth(3,As3))=/=0,
	{F1,SS1} = anyStringsFoundIn(CounterName,As,""),
	{F2,SS2} = anyStringsFoundIn(CounterName,As1,""),
	{F3,SS3} = anyStringsFoundIn(CounterName,As2,""),
    
	if
		Flag1 ->
			if
				F1 ->
					THIS:set_attribute(flag,false),
					S4 = "Your URL: contains 'sitemon', which is for Netscape version 4.x, but one of the counters you selected contain this counter string: \'" ++ SS1 ++ "\' counters cannot contain any of these: " ++ joinname(As) ++ " for this Netscape version. ",
					THIS:set_attribute(s4,S4);
				true ->
					THIS:set_attribute(flag,true),
					THIS:set_attribute(server,"sitemon")
			end;
		Flag2 ->
			if
				F2 ->
					THIS:set_attribute(flag,false),
					S4 = "Your URL: contains '.perf', which is for Netscape version 4.x perf dump, but one of the counters you selected contain this counter string: \'" ++ SS2 ++ "\' counters cannot contain any of these: " ++ joinname(As1) ++ " for this Netscape version. ",
					THIS:set_attribute(s4,S4);
				true ->
					THIS:set_attribute(flag,true),
					THIS:set_attribute(server,"perf")
			end;
		Flag3 ->
			if
				F3 ->
					THIS:set_attribute(flag,false),
					S4 = "Your URL: contains 'Stats', which is for Netscape version 6.0 stats, but one of the counters you selected contain this counter string: " ++ SS3 ++ " counters cannot contain any of these: " ++ joinname(As2) ++ " for this Netscape version. ",
					THIS:set_attribute(s4,S4);
				true ->
					THIS:set_attribute(flag,true),
					THIS:set_attribute(server,"Stats")
			end;
		true ->
			THIS:set_attribute(flag,false),
			S4 = "Your URL: must contain one of these strings: "++joinname(As3),
			THIS:set_attribute(s4,S4)
	end,
	if
		CounterName=:="" ->
			THIS:set_attribute(flag,false),
			THIS:set_attribute(s4,"Counters are empty, select counters and update monitor");
		true ->
			ok
	end,
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
	%%io:format("Retrieving URL ~p~n",[Url]),
    {ok,{_,Class}} = THIS:get_property(?CLASS),
	{ok,{_,Id}} = THIS:get_property(?ID),
    Profile = list_to_atom(atom_to_list(Class) ++ "-" ++ atom_to_list(Id)),
    M = url_content_monitor:new(),
    M:set_attribute(profile,Profile),
	URLResults = M:checkURL(Url,"","",Proxy,ProxyUserName,ProxyPassword,[],Username,Password,"","",51200,"",0,Timeout,""),
	M:delete(),
    Body = URLResults#urlresults.body,
    State = URLResults#urlresults.status,
	{ok,{_,FF}} = THIS:get_attribute(flag),
	if
		((State=:=200) and FF) ->
			Res = process_body(Body),
			{ok,{_,Value}} = THIS:get_attribute(value),
			{ok,{_,Error}} = THIS:get_attribute(error),
			if
				Error=:="" ->
                    THIS:set_attribute(status,"ok"),
					THIS:set_attribute(?STATE_STRING,set_counter_val(Res,Counters));
				true ->
                    THIS:set_attribute(status,"error"),
					THIS:set_attribute(countersInError,100),
					THIS:set_attribute(?STATE_STRING,Error),
					THIS:set_attribute(?CATEGORY, nodata),
					THIS:set_attribute(?NO_DATA, true)
			end;
		true ->
			if
				(not FF) ->
					{ok,{_,ES}} = THIS:get_attribute(s4),
					THIS:set_attribute(?STATE_STRING,ES);
				true ->
					THIS:set_attribute(?STATE_STRING,"connect error")
			end,
            THIS:set_attribute(status,"error"),
			THIS:set_attribute(countersInError,100),
			THIS:set_attribute(?CATEGORY, nodata),
			THIS:set_attribute(?NO_DATA, true)
	end.
	
	
%% @spec process_body(Response) -> [Result]
%% where
%% Response = string()
%% Result = {value,{Countername,Countervalue}}
%% Countername = string()
%% Countervalue = string()
%% @doc Get counters value from response,using regular expression.
process_body(R) ->
	{ok,{_,Counters}} = THIS:get_property(counters),
	filter(Counters,R),
	{ok,{_,L2}}=THIS:get_attribute(value),
	if
		L2=:=[] ->
			[];
		true ->
			[lists:keysearch(X1,1,L2)||X1<-get_counter_name(THIS:getCounters(THIS,THIS:getCountersContent()))]
	end.
	
filter([],_)->[];
filter([{_,F}|R],Response) ->
	{ok,{_,Server}} = THIS:get_attribute(server),
	Index1 = string:str(Server,"perf"),
	Index2 = string:str(F,"transfer"),
	Index3 = string:str(F,"Total requ"),
	Index4 = string:str(F,"of Process"),
	Index5 = string:str(F,"Number Of"),
	Index6 = string:str(F,"Max"),
	Index7 = string:str(F,"Alive Time"),
	Index8 = string:str(F,"outs"),
	Exp = if
		Index1=:=0 ->
			if
				((Index2=/=0) or (Index3=/=0) or (Index4=/=0)) ->
					".*?<TD>[\\w\\W]{0,64}"++F++":<\\/TD><TD>(\\d*)<\\/TD>";
				((Index5=/=0) or (Index6=/=0) or (Index7=/=0) and (Index8=:=0)) ->
					".*?<TD>[\\w\\W]{0,64}"++F++":.*?<\\/TD><TD>(\\d*)\\W?<\\/TD>";
				true ->
					".*<TD[\\w\\W]{20,96}"++F++"[\\w\\W]{5,17}<\\/TD>[\\w\\W]{96,128}center[\\w\\W]{48,64}3\">(\\d*)[\\w\\W]{0,8}<\\/TD>"
			end;
		true ->
			"[\\w\\W]*?"++F++"[\\s]{2,80}([\\/\\w\\d\\:]{0,80})"
	end,
	case re:run(Response,Exp) of
		{match,L} ->
			[L1|[L2|_]] = L,
			%%index from 0
			V=string:substr(Response,element(1,L2)+1,element(2,L2)),
			%%in erlang regexp \\s present include \n,so wo need throw it off
			V1 = string:strip(httputils:replaceAll(V,"\n","")),
			%%io:format("match value:~p~n",[V]),
			{ok,{_,Va}} = THIS:get_attribute(value),
			THIS:set_attribute(value,[{F,V1}|Va]);
		nomatch ->
			%%io:format("which one is bad:~p~n",[Exp]),
			THIS:set_attribute(error,"content match error")
	end,
	filter(R,Response);
filter([F|R],Response) ->
	filter(R,Response).


joinname([])->"";
joinname([F|R])->
	if
		R=/=[] ->
			F++","++joinname(R);
		true ->
			F++joinname(R)
	end.
	
anyStringsFoundIn(S,As,Sb) ->
	anyStringsFoundIn(S,As,Sb,false).
	
anyStringsFoundIn(_,_,SB,true) ->{true,SB};
anyStringsFoundIn(_,[],SB,Flag) ->{Flag,SB};
anyStringsFoundIn(S,[F|R],SB,Flag) ->
	case string:str(S,F) of
		0->
			anyStringsFoundIn(S,R,SB,Flag);
		_->
			anyStringsFoundIn(S,R,SB++F,true)
	end.

%% @spec get_counter_name(Counters) -> [Countersname]
%% where
%% Counters = [{Counternumber,Countername}]
%% Counternumber = integer()
%% Countername = string()
%% @doc Return counters name list.
get_counter_name([]) ->[];
get_counter_name([F|R]) ->
	[element(2,F)|get_counter_name(R)].
	
set_counter_val([],_)->[];
set_counter_val([C|T],Counters)->
	{ok,{_,Statestring}} = THIS:get_attribute(error),
	if
		Statestring=/="" ->
			THIS:set_attribute(?CATEGORY, nodata),
			THIS:set_attribute(?NO_DATA, true),
			THIS:set_attribute(countersInError,100),
			Statestring;
		true->
			case C of
				{value,{N,Val}} ->
					{value,Cou} = lists:keysearch(N,2,Counters),
					THIS:set_attribute(element(1,Cou),Val),
                    THIS:set_attribute(element(2,Cou),Val),
					%考虑设置参数的时候要把值转换成integer
					lists:flatten(io_lib:format("~p=~p<br>",[N,Val])) ++ set_counter_val(T,Counters);
				false ->
					THIS:inc_attribute(countersInError),
					set_counter_val(T,Counters);
				_ ->
					THIS:inc_attribute(countersInError),
					set_counter_val(T,Counters)
			end
	end.
    
errorCount() ->
    ErrorCount = 
        case THIS:get_attribute(countersInError) of
            {ok,{_,FF}} ->
                FF + 1;
            _ ->
                0
        end,
    THIS:set_attribute(countersInError,ErrorCount).
	
getCountersContent()->
	case THIS:get_property(counters) of
		{ok,{_,V}}->
			V;
		_->
			[]
	end.
	
%% @spec getTemplateFile() -> File
%% where
%% File = string()
%% @doc Appoint template file path.
getTemplateFile()->
	?TEMPLATE_FILE.

%% @spec getDefaultCounters() -> Counters
%% where
%% Counters = [{Counternumber,Countername}]
%% Counternumber = integer()
%% Countername = string()
%% @doc Get default counters.
getDefaultCounters()->
	THIS:getURLContentCounters(THIS,"",false).

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
			[{countersInError,'!=',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'!=',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{countersInError,'==',0}]
	end.

%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=url,title="Server URL", description="the server administration URL (example: http://server:adminport/https-SERVERINSTANCE/bin/sitemon?doit:)",type=text,editable=true,order=2},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,default=THIS:getDefaultCounters()},
	#property{name=timeout,title="Timeout",type=numeric,description="connect time out(second)",advance=true,optional=true,order=3,default=60,baselinable=true},
	#property{name=proxy,title="HTTP Proxy",type=text,description="optional list of proxy servers to use including port (example: proxy.siteview.com:8080)",advance=true,optional=true,order=5},
	#property{name=userName,title="Authorization User Name",type=text,description="optional user name if the URL requires authorization.",advance=false,optional=true,order=6},
	#property{name=password,title="Authorization Password",type=password,description="optional password if the URL requires authorization",advance=false,optional=true,order=7},
	#property{name=ntlm,title="NT Challenge Response",type=bool,description="when selected, use NT Challenge Response authorization",advance=true,optional=true,order=8},
	#property{name=proxyUserName,title="Proxy Server User Name",type=text,description="optional user name if the proxy server requires authorization",advance=true,optional=true,order=9},
	#property{name=proxyPassword,title="Proxy Server Password",type=password,description="optional password if the proxy server requires authorization",advance=true,optional=true,order=10},
	#property{name=status,title="status",type=numeric,state=true,configurable=false}
	].