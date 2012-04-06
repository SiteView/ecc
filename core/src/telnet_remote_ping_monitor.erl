-module(telnet_remote_ping_monitor,[BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").

new() ->
    Base = browsable_base:new(),
    {?MODULE,Base}.

update() ->
    %%{ok,{_,PingDeviceIp}} = THIS:get_property(ping_device),
    {ok,{_,Template}} = THIS:get_property(template),
    %%io:format("~p ~p~n",[PingDeviceIp,RemoteDeviceIp]),    
    %%Params = network_flow:g_params({list_to_atom(RemoteDeviceId),null}),    
    case THIS:get_property(browse) of
	{ok,{_,Browse}} ->
	    Config = [{Param,g_property(Param)} || Param <- [ip,port,user,passwd,superUser,superPasswd,userPrompt,passwdPrompt,prompt,superPrompt,timeout,to]],
	    %io:format("Config:~p~n",[Config]),
	    Counters = [Counter || {Counter,_} <- Browse ],	    
	    Data = network:telnet_ping(Counters,Config,Template),
	    ErrorData = [ Counter || {Counter,[]} <- Data],
	    THIS:set_attribute(countersInError,length(ErrorData)),
	    [THIS:set_attribute(atom_to_list(Counter),Value) || {Counter,Value} <- Data],
	    Infos = network:get_remote_ping_desc(Data),
	    THIS:set_attribute(?STATE_STRING,Infos);
	_ ->
	    []
    end.

g_property(Param)->
    case THIS:get_property(Param) of
		{ok,{_,Value}}->
	    	Value;
		_ -> 
			[]
    end.			 

getBrowseData(_Params)->
    [{"max_rtt","Max round trip times"},{"avg_rtt","Avg round trip times"},{"min_rtt","Min round trip times"}].

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

verify(Params)->
    Errs=case BASE:verify(Params) of
		{error,Be}->
		 	Be;
		_->
			[]
	end,
    if	length(Errs)>0 
		->
	    	{error,Errs};
		true ->
	    	{ok,""}
    end.

get_template_property()->
    BASE:get_template_property() ++
	[ 
	 #property{name=to,title="Ping Device",type=text,editable=true,configurable=true,state=false,order=1,description="the name of the server"},
	 #property{name=ip,title="Remote Device",type=text,editable=true,configurable=true,state=false,order=2,description="the name of the remote server"},
	 #property{name=port,title="Port",type=numeric,editable=true,configurable=true,state=false,order=3,default=23,description="port number on server - default is 23"},
	 #property{name=user,title="User",type=text,editable=true,configurable=true,state=false,order=4,description="User"},
	 #property{name=passwd,title="Password",type=password,editable=true,configurable=true,state=false,order=5,description="Password"},
	 #property{name=superUser,title="SuperUser",type=text,editable=true,configurable=true,state=false,order=6,description="SuperUser"},
	 #property{name=superPasswd,title="Super Passwd",type=password,editable=true,configurable=true,state=false,order=7,description="Super user Passwd"},
	 #property{name=userPrompt,title="User Prompt",type=text,editable=true,configurable=true,state=false,order=8,default="",description="User Prompt"},
	 #property{name=prompt,title="Prompt",type=text,editable=true,configurable=true,state=false,order=9,default=">",description="Prompt"},
	 #property{name=superPrompt,title="Super Prompt",type=text,editable=true,configurable=true,state=false,order=10,default="#",description="Super Prompt"},
	 #property{name=timeout,title="Timeout",type=numeric,editable=true,configurable=true,state=false,order=11,default=2000,description="the time out, in seconds, to wait for the response"},
	 #property{name=template,title="Template",type=text,editable=true,configurable=true,state=false,order=12,description="Template"}
	].


