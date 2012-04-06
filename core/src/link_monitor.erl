%% ---
%% Link Monitor
%%
%%---
-module(link_monitor, [BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").


new()->
	Obj = atomic_monitor:new(),
	ThisObj = {?MODULE,Obj},
	put(obj, ThisObj),
	ThisObj.


update() ->
	{ok, {_, Url}} = BASE:get_property(url),
	{ok, {_, Proxy}} = BASE:get_property(httpproxy),
	{ok, {_, ProxyUserName}} = BASE:get_property(proxyserveruser),
	{ok, {_, ProxyPassword}} = BASE:get_property(proxyserverpass),	
	
	{ok, {_, PostData}} = BASE:get_property(postdata),
	{ok, {_, UserName}} = BASE:get_property(authusername),
	{ok, {_, Password}} = BASE:get_property(authpassword),
	{ok, {_, Timeout}} = BASE:get_property(timeout),%60s
	{ok, {_, PauseMS}} = BASE:get_property(pause),%250milliseconds
	{ok, {_, MaxLinks}} = BASE:get_property(maximunlinks),	%800(int)	
	
	{ok, {_, ExternalLinks}} = BASE:get_property(searchextlinks),%bool
	{ok, {_, NtCR}} = BASE:get_property(ntchallengeresponse),
	{ok, {_, MaxHops}} = BASE:get_property(maximunhops),
	%io:format("MaxHops:~p~n", [MaxHops]),	
	
	MaxSearchDepth = case MaxHops of
					"other" ->
						{ok, {_, SpecHops}} = BASE:get_property(specifyhops),
						H = try list_to_integer(SpecHops)
							catch
								_ : _ -> 100
							end,
						case H<1 of
							true ->
								100;
							_ ->
								H
						end;		
					_ ->
						list_to_integer(MaxHops)
				end,		
		
	Array = string:tokens(PostData, "\n"),
	User = case NtCR of
		true ->
			"[NT]"++UserName;
		_ ->
			UserName
	end,
	case PauseMS < 250 of
		true ->
			Pause = 250;
		_ ->
			Pause = PauseMS
	end,		
	case get(obj) of
		undefined ->
			Monitor = null;
		Obj ->
			Monitor = Obj
	end,
	LF = linkfinder:new(Url, Proxy, ProxyUserName, ProxyPassword, Array, UserName, Password, Timeout*1000, Pause, MaxLinks, MaxSearchDepth, "", Monitor),
	LF:search(ExternalLinks),
	
	updateProperties("", LF:getTotalBroken(), LF:getTotalPages(), LF:getTotalGraphics(), LF:getTotalTime(), LF:getBrokenSummary(), LF:getBrokenLinks()).
	

updateProperties(Url, TotalBroken, TotalPages, TotalGraphics, TotalTime, BrokenSummary, _BrokenLinks) ->
	io:format("updateProperties--url:~p~n", [Url]),
	case (TotalPages-TotalBroken) /= 0 of
		true ->
			Average = TotalTime div (1000*(TotalPages-TotalBroken));
		false ->
			Average = 0, %ms,
			THIS:set_attribute(?CATEGORY, nodata),
			THIS:set_attribute(?NO_DATA, true)
	end,
	THIS:set_attribute(linkerrors, TotalBroken),%integer
	THIS:set_attribute(totallinks, TotalPages),
	THIS:set_attribute(totalgraphics, TotalGraphics),
	THIS:set_attribute(average, Average),
	Pre = case Url /= "" of
		true ->
			"IN PROGRESS, " ++ Url ++ ", ";
		_ ->
			""
	end,			
	Str = case TotalBroken of
		0 ->
			Pre ++ integer_to_list(TotalPages) ++ " links, " ++ integer_to_list(TotalGraphics) ++ " graphics, average " ++ integer_to_list(Average) ++ " ms";
		_ ->
			Pre ++ integer_to_list(TotalPages) ++ " links, " ++ integer_to_list(TotalGraphics) ++ " graphics, average " ++ integer_to_list(Average) ++ " ms, " ++ BrokenSummary
	end,
	THIS:set_attribute(?STATE_STRING, Str).
	
	
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{linkerrors, '>', 0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{none, '>=',  0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{linkerrors, '==', 0}]
	end.

getHostname()->
	case THIS:get_property(url) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.
	
get_template_property()->
	K = #property{name=?FREQUENCY,title="Update every",type=frequency,editable=true,default=600,description="amount of time between checks of a monitor"},
	lists:delete(K, BASE:get_template_property()) ++
	%BASE:get_template_property() ++	
	[
	#property{name=url, title="URL", type=text, editable=true, order=1, description="the URL to start link checking from (example: http://demo.siteview.com)"},
	#property{name=searchextlinks, title="Search External Links", type=bool, editable=true, order=2, description="when selected, search all links on each page, not just links with the original base URL."},
	#property{name=?FREQUENCY,title="Update every",type=frequency,editable=true,default=86400, order=3, description="amount of time between checks of a monitor"},
	#property{name=pause, title="Pause", default=250, type=numeric, editable=true, advance=true, order=4, description="delay between checks, in milliseconds"},
	#property{name=maximunlinks, title="Maximum Links", default=800, type=numeric, editable=true, advance=true, order=5, description="the maximum number of links to check"},
	#property{name=maximunhops, title="Maximum Hops", type=scalar, advance=true, order=6},
	#property{name=specifyhops, title="", type=text, advance=true, order=7, description="the maximum \"hops\" from the start URL to check"},	
	#property{name=timeout, title="Timeout", type=numeric, default=60, advance=true, order=8, description="the time out, in seconds, to wait for the response",baselinable=true},
	#property{name=httpproxy, title="HTTP Proxy", default="", advance=true, order=9, description="optional proxy server to use including port (example: proxy.siteview.com:8080)"},
	
	#property{name=ntchallengeresponse, title="NT Challenge Response", type=bool, advance=true, order=10, description="when selected, use NT Challenge Response authorization"},
	#property{name=authusername, title="Authorization User Name", advance=true, order=11, description="optional user name if the URL requires authorization"},
	#property{name=authpassword, title="Authorization Password", advance=true, order=12, description="optional password if the URL requires authorization"},
	#property{name=proxyserveruser, title="Proxy Server User Name", advance=true, order=13, description="optional user name if the proxy server requires authorization"},
	#property{name=proxyserverpass, title="Proxy Server Password", advance=true, order=14, description="optional password if the proxy server requires authorization"},
	#property{name=postdata, title="POST Data", type=text, multiple=true, advance=true, order=15, description="optional name=value variables, one per line, to send with a POST request to the first URL checked"},
	
	#property{name=linkerrors, title="link errors(errors)", type=numeric, order=16, configurable=false, state=true},
	#property{name=totallinks, title="total links(links)", type=numeric, order=17, configurable=false, state=true},	
	#property{name=totalgraphics, title="total graphics(links)", type=numeric, order=18, configurable=false, state=true},
	#property{name=average, title="average(milliseconds)", type=numeric, order=19, configurable=false, state=true,baselinable=true}	,
	#property{name=none, title="none", type=numeric, order=20, configurable=false, state=true}	
	].

getScalarValues(maximunhops, _Params)->	
	[{"no limit", "100"}, {"main page links", "1"}, {"3", "3"}, {"5", "5"}, {"10", "10"}, {"Other", "other"}];
getScalarValues(Prop, Params) ->
	BASE:getScalarValues(Prop, Params).
