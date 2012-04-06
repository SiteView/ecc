%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc Apache Server Monitor.
%%
%% Description: Monitor the content of server administration pages for Apache
%% Versions supported: 1.3.9, 1.3.12, 2.0.x
%% Platform: All
%% Requirement:
%% a: Configure the Apache server you want to monitor so that status reports (server-status) are enabled for the server
%% b: Modify httpd.conf which in Apache server,add lines(<Location /server-status>SetHandler server-status Order allow,deny Allow from all</Location>ExtendedStatus On),save the file
%% c: Test url(http：//IP/server-status) is worked
-module(apache_monitor,[BASE]).   
-extends(urlcontent_base).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([new/0,verify/1,update/0,get_counter_name/1,filter/3,set_counter_val/2,getTemplateFile/0,getScalarValues/2,get_classifier/1,get_template_property/0]).

-define(TEMPLATE_FILE,"counters.apache").
-define(DEFAULT_TIMEOUT,60000).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Obj = urlcontent_base:new(),
	Obj:set_attribute(countersInError,0),
	Obj:set_attribute(value,[]),
	Obj:set_attribute(servermatch,false),
	Obj:set_attribute(error,""),
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
	case proplists:get_value(timeout,Params) of
		""->
			[{index,"Timeout missing"}];
		Timeout when not is_number(Timeout)->
			[{index,"Timeout must be a number"}];
		X->
			case X > 0 of
			true ->
				[];
			_ ->
				[{index,"Timeout must be greater than 0"}]
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
%% Description: Apache monitor get apache service counters value with url
update()->
    %%error protection,reset attribute
	THIS:set_attribute(flag,false),
	THIS:set_attribute(server,""),
	THIS:set_attribute(value,""),
	THIS:set_attribute(error,""),
	THIS:set_attribute(countersInError,0),
    %%
	{ok,{_,Url}} = THIS:get_property(url),
	{ok,{_,Proxy}} = THIS:get_property(proxy),
	{ok,{_,ProxyUserName}} = THIS:get_property(proxyUserName),
	{ok,{_,ProxyPassword}} = THIS:get_property(proxyPassword),
	{ok,{_,Username1}} = THIS:get_property(userName),
	{ok,{_,Password}} = THIS:get_property(password),
	{ok,{_,NTLM}} = THIS:get_property(ntlm),
	{ok,{_,Counters}} = THIS:get_property(counters),
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
    State = URLResults#urlresults.status,
    Body1 = URLResults#urlresults.body,
    Body = iconv:convert("gbk","utf-8",Body1),
	if
		State=:=200 ->
			Res = process_body(Body,Url),
			{ok,{_,Value}} = THIS:get_attribute(value),
			{ok,{_,Error}} = THIS:get_attribute(error),
			if
				Error=:="" ->
					THIS:set_attribute(?STATE_STRING,set_counter_val(Res,Counters));
				true ->
					THIS:set_attribute(countersInError,100),
					THIS:set_attribute(?STATE_STRING,Error),
					THIS:set_attribute(?CATEGORY, nodata),
					THIS:set_attribute(?NO_DATA, true)
			end;
		true ->
			THIS:set_attribute(countersInError,100),
			THIS:set_attribute(?STATE_STRING,"connect error"),
			THIS:set_attribute(?CATEGORY, nodata),
			THIS:set_attribute(?NO_DATA, true)
	end.
	
	
%% @spec get_counter_name(Counters) -> Countervalue
%% where
%% Counters = [{Counternumber,Countervalue}]
%% Counternumber = integer()
%% Countervalue = string()
%% @doc Get counters name.
get_counter_name([]) ->[];
get_counter_name([F|R]) ->
	[element(2,F)|get_counter_name(R)].

	
%% @spec filter(Counters,URLResponse,URL) -> ok
%% where
%% Counters = [{Counternumber,Countervalue}]
%% Counternumber = integer()
%% Countervalue = string()
%% URLResponse = string()
%% URL = string()
%% @doc make regular expression for each counters and extract value from urlresponse.
%% Description: erlang version 5.7.2 and later have a new re module to replace regexp
%% use new re module to match
filter([],_,_)->[];
filter([{_,F}|R],Response,URL)->
	Index1 = string:str(F," "),
	Index2 = string:str(F,"/"),
	Index3 = string:str(F,"tal Acc"),
	Index4 = string:str(F,"tal K"),
	Index5 = string:str(F,"quest"),
	Index6 = string:str(F,"idle"),
	Index7 = string:str(F,"load"),
	Index8 = string:str(F,"Tot"),
	Index10 = string:str(URL,"auto"),
	Index11 = string:str(URL,"refresh"),
	Index12 = string:str(F,"al Acc"),
	Index13 = string:str(F,"al kBy"),
	Exp = if
		((Index10>=2) and (Index1=:=0) and (Index2=:=0) or (Index12=/=0) or (Index13=/=0)) ->
			THIS:set_attribute(servermatch,true),
			F1 = httputils:replaceAll(F,"Workers","[\\w]{1,8}"),
			F1++":\\s?([\\d\\.%]{0,28})";
		((Index11>=2) and ((Index1>=2) or (Index2>=1)) and (Index3=:=0) and (Index4=:=0)) ->
			THIS:set_attribute(servermatch,true),
			if
				((Index2=/=0) or (Index5=/=0) or (Index6=/=0) or (Index7=/=0)) ->
					F1 = httputils:replaceAll(F,"Workers","[\\w]{1,8}"),
					F2 = httputils:replaceAll(F1,"/","\\/"),
					"([\\d\\.%]{0,12})\\s?"++F2;
				Index8=/=0 ->
					F++":\\s?([\\s\\w\\d\\\\.]{1,52})";
				true ->
					F++":\\s?([\\s\\w\\d\\-:,\\/\\.)(]{1,96})"
			end;
		true ->
			THIS:set_attribute(servermatch,false),
			%%io:format("error counter is~p~n",[F]),
			""
	end,
	{ok,{_,ServerMatch}} = THIS:get_attribute(servermatch),
	if
		(not ServerMatch) ->
			THIS:set_attribute(error,"Server page and counter mismatch ");
		true ->
			case re:run(Response,Exp) of
				{match,L} ->
					[L1|[L2|_]] = L,
					%%index from 0
					V=string:substr(Response,element(1,L2)+1,element(2,L2)),
					%%in erlang regexp \\s present include \n,so wo need throw it off
                    V1 = if
                        ((F=="Restart Time") or (F=="Current Time")) ->
                            Colon = string:rstr(V,":"),
                            string:sub_string(V,1,Colon+2);
                        true ->
                            V
                    end,
                    V2 = string:strip(httputils:replaceAll(V1,"\n","")),
					%%io:format("match value:~p,final value is:~p~n",[V,V2]),
					{ok,{_,Va}} = THIS:get_attribute(value),
					THIS:set_attribute(value,[{F,V2}|Va]);
				nomatch ->
					THIS:set_attribute(error,"content match error")
			end
	end,
	filter(R,Response,URL);
filter([F|R],Response,URL) ->
	filter(R,Response,URL).
	
	
%% @spec set_counter_val(ChoseCounters,Counters) -> ok
%% where
%% ChoseCounters = [{value,{Countername,Countervalue}}]
%% Counters = [{Counternumber,Countervalue}]
%% Countername = string()
%% Countervalue = string()
%% Counternumber = integer()
%% @doc set counters value to attribute and create statestring.
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
                    io:format("cou:~p~n",[Cou]),
					THIS:set_attribute(element(2,Cou),Val),
					%Should consider setting the parameter value when converted to integer
					N++" = "++Val++"<br>" ++ set_counter_val(T,Counters);
				false ->
					THIS:inc_attribute(countersInError),
					set_counter_val(T,Counters);
				_ ->
					THIS:inc_attribute(countersInError),
					set_counter_val(T,Counters)
			end
	end.

process_body(R,URL) ->
	{ok,{_,Counters}} = THIS:get_property(counters),
	filter(Counters,R,URL),
	{ok,{_,L2}}=THIS:get_attribute(value),
	if
		L2=:=[] ->
			[];
		true ->
			[lists:keysearch(X1,1,L2)||X1<-get_counter_name(THIS:getCounters(THIS,THIS:getCountersContent()))]
	end.

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

getDefaultCounters()->
	THIS:getURLContentCounters(THIS,"",false).
    
getCostInLicensePoints() ->
    I = THIS:getActiveCounters(THIS),
    1*I.

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
		server->
			[{"Unix","unix"},{"Linux","linux"},{"NT","nt"}];
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
	#property{name=url,title="Host Name", description="the server administration URL (example: http://server:port/server-status?auto)",type=text,editable=true,order=2},
	#property{name=timeout,title="Timeout",type=numeric,description="connect time out(second)",advance=true,optional=true,order=3,default=60,baselinable=true},
	#property{name=server,title="Server OS",type=scalar,description="optional Operating System of server, default is Unix",advance=true,optional=true,order=4,default=unix},
	#property{name=proxy,title="HTTP Proxy",type=text,description="optional list of proxy servers to use including port (example: proxy.siteview.com:8080)",advance=true,optional=true,order=5},
	#property{name=userName,title="Authorization User Name",type=text,description="optional user name if the URL requires authorization.",advance=false,optional=true,order=6},
	#property{name=password,title="Authorization Password",type=password,description="optional password if the URL requires authorization",advance=false,optional=true,order=7},
	#property{name=counters,title="Counters", description="Current selection of counters.",type=counters,editable=true,default=THIS:getDefaultCounters()},
	#property{name=ntlm,title="NT Challenge Response",type=bool,description="when selected, use NT Challenge Response authorization",advance=true,optional=true,order=8},
	#property{name=proxyUserName,title="Proxy Server User Name",type=text,description="optional user name if the proxy server requires authorization",advance=true,optional=true,order=9},
	#property{name=proxyPassword,title="Proxy Server Password",type=password,description="optional password if the proxy server requires authorization",advance=true,optional=true,order=10},
	#property{name=status,title="status",type=numeric,state=true,configurable=false}
	].