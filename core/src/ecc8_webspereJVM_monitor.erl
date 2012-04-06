%% Author: Administrator
%% Created: 2011-11-22
%% Description: TODO: Add description to ecc8_webspereJVM_monitor
-module(ecc8_webspereJVM_monitor,[BASE]).
-extends(atomic_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").
new()->
	Base = atomic_monitor:new(),
	{?MODULE,Base}.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->    
	THIS:set_attribute(totalMemory,"n/a"),
	THIS:set_attribute(freeMemory,"n/a"),
	THIS:set_attribute(usedMemory,"n/a"),
	THIS:set_attribute(percentMemory,"n/a"),
       
	{ok,{_,Server}}=THIS:get_property(server), 
    {ok,{_,Url}}=THIS:get_property(url), 
	Paras="_Server="++Server++"$_Url="++Url,
    RL= monitorc2erlang:getrefreshedmonitor(Paras, "ecc8monitor/websphere.dll", "JVM"),
	LL=string:tokens(RL, "$"),
	case length(LL)>1 of
		true ->
			updatevalues(LL,"");
		_ ->
			THIS:set_attribute(?STATE_STRING,RL)
%% 			THIS:set_attribute(?STATE_STRING,iconv:convert("utf-8","gbk",RL))
	end
   . 
updatevalues([],S)->
	THIS:set_attribute(?STATE_STRING,S),
	ok;
%%
%%"freeMemory=0.00$usedMemory=111.44$totalMemory=117.98$percentMemory=94.46$"
%% 
updatevalues([H|E],S)->
	TVset=string:tokens(H, "="),
	case length(TVset) of
		2 ->
		  	[K,V]=TVset;
		_ ->
			[K]=TVset,
			V=""
	end,
	V1=try
		   list_to_integer(V) 
	   catch _:_ ->
				 try
					 list_to_float(V)
				 catch _:_ ->
						   V
				 end
	   end,   
	Tem=case K of
		"freeMemory" ->
			THIS:set_attribute(freeMemory,V1),
			S++"Free Memory(MB)="++V++",";
		 "usedMemory" ->
			THIS:set_attribute(usedMemory,V1),
			S++"used Memory(MB)="++V++",";
		 "totalMemory" ->
			THIS:set_attribute(totalMemory,V1),
			S++"total Memory(MB)="++V++",";	
		 "percentMemory" ->
			 THIS:set_attribute(percentMemory,V1),
			 S++"percent Memory(%)="++V
		end,		
	updatevalues(E,Tem).
	
verify(Params)->
	Errs =
	case proplists:get_value(server,Params) of
		undefined->
			[{server,"machineName is null"}];
		[]->
			[{server,"machineName is null"}];
		_->
			[]
	end ++
	case proplists:get_value(url,Params) of
		undefined->
			[{url,"url is null"}];
		[]->
			[{url,"url is null"}];
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
	
	
%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{percentMemory,'>',90}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{percentMemory,'>',80}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{percentMemory,'>',0}]
	end.
%% @spec getLogProperties(This)->list()
%% @doc get properties need to log
%%
getLogProperties(This)->
	Temp = This:get_template_property(),
    [X#property.name || X<-Temp,X#property.state=:=true].
%%
%% 
get_template_property()->
	BASE:get_template_property() ++ 
	[
	#property{name=server,title="Machine Name",type=text,editable=true,order=1,description="Machine Name"},
	#property{name=url,title="perfservlet url",type=text,editable=true,order=3,description="perfservlet url"},
	#property{name=totalMemory,title="total Memory",type=numeric,order=99,configurable=false,state=true,baselinable=true},
	#property{name=freeMemory,title="free Memory",type=numeric,order=100,configurable=false,state=true,baselinable=true},
	#property{name=usedMemory,title="used Memory",type=numeric,order=101,configurable=false,state=true,baselinable=true},
	#property{name=percentMemory,title="percent Memory",type=numeric,order=102,configurable=false,state=true,baselinable=true}
	].


