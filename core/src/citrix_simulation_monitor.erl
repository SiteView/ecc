%% Author: bin.liu
%% Created: 2011.5
%% Description: TODO: Add description to citrix_simulation_monitor
-module(citrix_simulation_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").


new()->
	Base = atomic_monitor:new(),
	Base:set_attribute(countersInError,0),
	{?MODULE,Base}.

defaultTitle(Params)->
	Appname = proplists:get_value(appname,Params),
	if
		length(Appname)>0->
			BASE:defaultTitle(Params) ++":" ++ Appname;
		true ->
			BASE:defaultTitle(Params)
	end.

getBrowseData(Params)->
[{"simulation","simulation"},
 {"connection status","simulation/connection status"},
 {"connection times(ms)","simulation/connection times(ms)"},
 {"logon status","simulation/logon status"},
 {"logon times(ms)","simulation/logon times(ms)"},
 {"global channel count","simulation/global channel count"},
 {"client address count","simulation/client address count"},
 {"error","simulation/error"}]
.
perfexCommand()->
	
	{ok, {_, Appname}} = THIS:get_property(appname),
	{ok, {_, Address}} = THIS:get_property(address),
	{ok, {_, Domain}} = THIS:get_property(domain),
	{ok, {_, Username}} = THIS:get_property(username),
	{ok, {_, Password}} = THIS:get_property(password),
	
%% 	init
	THIS:set_attribute("simulation/connection status","fail"),
	THIS:set_attribute("simulation/connection times(ms)","n/a"),
	THIS:set_attribute("simulation/logon status","fail"),
	THIS:set_attribute("simulation/logon times(ms)","n/a"),
	THIS:set_attribute("simulation/global channel count","n/a"),
	THIS:set_attribute("simulation/client address count","n/a"),
	THIS:set_attribute("simulation/error","n/a"),
%% 	"siteview/192.168.6.198:1494/citrix/administrator/kenny" "\"" + filePath + "\""
	Cmd="tools\\citrix\\citrix.exe "++ "\""++iconv:convert("utf-8","gbk", Appname)++"/"++Address++"/"++Domain++"/"++Username++"/"++Password++"\"",
%%     io:format("cmd:~p~n", [Cmd]),   
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd),
	THIS:set_attribute(?STATE_STRING,iconv:convert("gbk", "utf-8", util:replace(Rets,"|",","))),
	Result = string:tokens(Rets,"|"),
	buildResult(Result,[]).
buildResult([],R)->
	R;
buildResult([H|E],R)->
	KV= string:tokens(H, "="), 
	Tem=[{lists:nth(1, KV),lists:nth(2, KV)}],
	buildResult(E,R++Tem).
update()->
	Ret=perfexCommand(),
	updateValues(Ret,0),
	ok.
updateValues([],N)->
	THIS:set_attribute(countersInError,N),
	ok;
updateValues([H|E],N)->
	{K,V}=H,
	TN=case V of
		   "n/a" ->
			   N+1;
		   "fail"->
			   N+1;
		   _ ->
			   N
	   end,
	VV=case string:to_integer(V) of
		   {error,_}->
			   iconv:convert("gbk", "utf-8",V);
		   {NN,_} ->
			   NN
	   end,	
	THIS:set_attribute(K,VV),
	THIS:set_attribute("simulation/"++K,VV),
	updateValues(E,TN).
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
%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
%% Description: check counters
verify(Params)->
	Errs =
	case proplists:get_value(appname,Params) of
		[]->
			[{appname,"appname is missing"}];
		_->
			[]
	end++
		case proplists:get_value(address,Params) of
		[]->
			[{address,"address is missing"}];
		_->
			[]
	end++
		case proplists:get_value(domain,Params) of
		[]->
			[{domain,"domain is missing"}];
		_->
			[]
	end++
		case proplists:get_value(username,Params) of
		[]->
			[{username,"username is missing"}];
		_->
			[]
	end++
		case proplists:get_value(password,Params) of
		[]->
			[{password,"password is missing"}];
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
			[{contersInError,'>',0}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{contersInError,'>',0}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{contersInError,'==',0}]
	end.

getCostInLicensePoints()->
	{ok,{_,Counters}} = THIS:get_property(browse),
	length(Counters).

getLogProperties(This)->
	{ok,{_,Counters}} = THIS:get_property(browse),
	Temp = This:get_template_property(),
	[X#property.name || X<-Temp,X#property.state=:=true] ++ [X||{_,X}<- Counters].

getStatePropertyObjects()->
	[
	 #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}  
	].
%% @spec get_template_property() -> PropertyList
%% where
%% PropertyList = [PropertyRecord]
%% PropertyRecord = record()
%% @doc Add properties to template property list.
get_template_property()->
	BASE:get_template_property() ++ 
	[
	  #property{name=appname,title="Application Name",type=text,configurable=true,editable=true,state=false,description="the Application Name of citrix published Applications", order=1},
	  #property{name=address,title="Citrix Address",type=text,configurable=true,editable=true,state=false,description="Citrix Address 127.0.0.1:1494",order=2},
	  #property{name=domain,title="Domain",type=text,configurable=true,editable=true,state=false,description="the Domain of the Citrix",order=3},
	  #property{name=username,title="User Name",type=text,configurable=true,editable=true,state=false,description="the user name of Citrix",order=4},
	  #property{name=password,title="PassWord",type=password,configurable=true,editable=true,state=false,description="the password of Citrix",order=5},
	  #property{name=browse,title="Counters", description="Current selection of counters.",type=browsable,editable=true,order=100},
	  #property{name=countersInError,title="counters in error",type=numeric,editable=true,configurable=false,state=true,order=100}
	].
