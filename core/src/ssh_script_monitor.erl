%% Author: Administrator
%% Created: 2011-10-12
%% Description: TODO: Add description to ssh_script_monitor
-module(ssh_script_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTER,20).

%% @spec new() -> ok
%% @doc Start the monitor instance.
new()->
	Base = server_monitor:new(),
	{?MODULE,Base}.

getLinuxMachine(Host)->
	dbcs_machine:get_machine_match("my.host=" ++ Host).

%% @spec getHostname(This) -> string()
%% where
%% This = instance()
%% @doc Get Machine.
getHostname(This)->
	{ok,{_,Machine}}=This:get_property(machine),
	Machine.
%% @spec getMaxCounters() -> integer()
%% @doc Get Max counters number.
getMaxCounter()->?MAX_COUNTER.
getmhost(Host)->
	string:strip(Host, left, $\\).
update()->

%% 	{ok,{_,Host}}=THIS:get_property(host),
%% 	{ok,{_,Port}}=THIS:get_property(port),
%% 	{ok,{_,User}}=THIS:get_property(username),
%% 	{ok,{_,Passwd}}=THIS:get_property(password),
	{ok,{_,SshCmd}}=THIS:get_property(sshCmd),
%% 	THIS:getCounterValues(Host,User,Passwd,Port,SshCmd).
	{ok,{_,Machine}}=THIS:get_property(machine),
	case Machine of
		""->
			Host="127.0.0.1",
			User=" ",
			Passwd=" ",
	         THIS:getCounterValues(Host,User,Passwd,22,SshCmd);  
		_->
			case THIS:getLinuxMachine(Machine) of
				[]->
					THIS:set_attribute(?NO_DATA,true),
					THIS:set_attribute("status","error"),
				    THIS:set_attribute(?CATEGORY,?NO_DATA),
					THIS:set_attribute(?STATE_STRING,"connect target error!"),
					[];
				[M|_]->
                    THIS:getCounterValues(getmhost(M#machine.host),M#machine.login,M#machine.passwd,M#machine.sshport,SshCmd)
			end
  end.
getCounterValues(Host,User,Passwd,Port,Cmd) ->
    THIS:set_attribute(returnvalue,"n/a"),
    THIS:set_attribute(status,"error"),
    Ret=ssh_command:exec(Host, Port, User, Passwd, Cmd),
	platform:sleep(1000),
	case Ret of
		{ok,RR}->
			THIS:set_attribute(status,"ok"),
			[Name,V1]=string:tokens(RR, "="),
			[V]=string:tokens(V1,"\r\n"),
			Value=try
					  list_to_float(V)
				  catch _:_ ->
							try
								list_to_integer(V)
							catch _:_ ->
									  V
							end
				  end,
			THIS:set_attribute(returnvalue,Value),
			THIS:set_attribute(?STATE_STRING,RR)
			;
		_error ->
		    THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,?NO_DATA),
		    THIS:set_attribute(?STATE_STRING,Ret)
	end.     


%% @spec verify(Params) -> {ok, []} | {error, Reason}
%%  Params = [term()]
%%  Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params) ->
    Errs = 
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,   
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.
getLogProperties(This)->
	Temp = This:get_template_property(),
	[X#property.name|| X<-Temp,X#property.state=:=true].
getCostInLicensePoints()->
	1.
get_classifier(error)->
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{status,'!=',"ok"}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->

	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'!=',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',"ok"}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.
get_template_property()->
	BASE:get_template_property() ++  
	[
%% 	  #property{name=host,title="Host Name",type=text,configurable=true,editable=true,state=false,description="the name of the server",order=1},
%% 	  #property{name=port,title="Port",default=22,type=numeric,configurable=true,editable=true,state=false,description="the port of the server",order=2},
	  #property{name=sshCmd,title="sshCmd",type=text,configurable=true,editable=true,state=false,description="shell cmd",order=3},
%% 	  #property{name=username,title="User Name",type=text,configurable=true,editable=true,state=false,description="the user name of server",order=4},
%% 	  #property{name=password,title="PassWord",type=password,configurable=true,editable=true,state=false,description="the password of server",order=5},
	  #property{name=returnvalue,title="Return Value",type=numeric,configurable=false,state=true,upIsBad=false,baselinable=true},
	  #property{name=status,title="status",configurable=false,state=true}
    ].