%%
%% Device Config Monitor
%%Monitoring equipment configuration changes

%% @doc device config monitor
%% @version{0.1}
%% @copyright 2011 dragonflow.com

-module(device_config_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(PORT,23).
-define(TIMEOUT,5000).
-define(UserPrompt,"ogin:").
-define(PasswdPrompt,"assword:").

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for server monitor
new()->
	Base = server_monitor:new(),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% Params = [term()]
%% List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	BASE:defaultTitle(Params).

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the  diskspace monitor
update()->
    {ok,{machine,Machine}} = THIS:get_property(machine),
    {ok,{command,Command}} = THIS:get_property(command),
    io:format("~n THIS:get_properties():~p",[THIS:get_properties()]),
    case machine:getMachine(Machine) of
    []->
        THIS:set_attribute(statues,"error"),
        THIS:set_attribute(?STATE_STRING,"get machine fail");
    [M|_]->
        Other = M#machine.other,
        IP = M#machine.host,
       TelnetParams = proplists:get_value(telnetParam,Other,[]),
        Oid = proplists:get_value(objectid,Other,""),
        User = proplists:get_value(user,TelnetParams,""),
        Passwd = proplists:get_value(passwd,TelnetParams,""),
        SuperUser = proplists:get_value(superUser,TelnetParams,""),
        SuperPasswd = proplists:get_value(superPasswd,TelnetParams,""),
        UserPrompt = proplists:get_value(userPrompt,TelnetParams,?UserPrompt),
        PasswdPrompt = proplists:get_value(passwdPrompt,TelnetParams,?PasswdPrompt),
        Prompt = proplists:get_value(prompt,TelnetParams,">"),
        SuperPrompt = proplists:get_value(superPrompt,TelnetParams,"#"),
        Timeout = proplists:get_value(timeout,TelnetParams,?TIMEOUT),
        Port = proplists:get_value(port,TelnetParams,?PORT),
        Params = [{ip,IP},{port,Port}, {user,User}, {passwd,Passwd}, {superUser,SuperUser}, {superPasswd,SuperPasswd}, 
        {userPrompt,UserPrompt}, {passwdPrompt,PasswdPrompt}, {prompt,Prompt}, {superPrompt,SuperPrompt}, {timeout,Timeout}],
        {ok,{_,MonitorID}} = THIS:get_property(?ID),
        case THIS:get_property(originalconfig) of
         {ok,{_,OriginalConfig}}->
            case api_nnm:telnet_connect(Params) of
            {ok,Pid}-> 
                    case api_nnm:telnet_send(Pid,Command ++ "\r\n") of
                    {ok,NConfigString} ->
                        api_nnm:telnet_close(Pid),
                        %%with Compared to the original configuration
                        CompareResultO = device_config_monitor_utils:compare_Config(OriginalConfig,NConfigString),
                        CompareResultN = device_config_monitor_utils:compare_Config(NConfigString,OriginalConfig),
                        case CompareResultO=:=[] andalso CompareResultN=:=[] of
                        true->
                            THIS:set_attribute(statues,"good"),
                            THIS:set_attribute(?STATE_STRING,"no change");
                        _->
                            %%Matching error.
                            THIS:set_attribute(statues,"error"),
                            THIS:set_attribute(?STATE_STRING,"config chanaged: "++ string:join(CompareResultO," , ")++string:join(CompareResultN," , ")),
                            dbcs_device_config:create([{time,sv_datetime:now()},{monitorid,MonitorID},{deviceip,M#machine.host},{deviceid,M#machine.id},{nconfig,NConfigString},{oconfig,OriginalConfig}])
                        end;
                    _->
                        %%Command error
                        THIS:set_attribute(statues,"error"), 
                        THIS:set_attribute(?STATE_STRING,"telnet get config command error")
                    end;
                _->
                    THIS:set_attribute(statues,"error"),
                    THIS:set_attribute(?STATE_STRING,"telnet connect fail")
            end;
         _->
            case api_nnm:telnet_connect(Params) of
            {ok,Pid}-> 
                case api_nnm:telnet_send(Pid,Command ++ "\r\n") of
                {ok,ConfigString} ->
                    THIS:set_property(originalconfig,ConfigString),
                    BASE:save_monitor(),
                    api_nnm:telnet_close(Pid),
                    THIS:set_attribute(statues,"good"),
                    THIS:set_attribute(?STATE_STRING,"no change");
                _->
                %%Command error
                    THIS:set_attribute(statues,"error"), 
                    THIS:set_attribute(?STATE_STRING,"telnet get config command error")
                end;
            _->
                THIS:set_attribute(statues,"error"),
                THIS:set_attribute(?STATE_STRING,"telnet connect fail")
            end
        end
    end.  

get_base()->
    BASE.

get_orginalConfig()->
    THIS:get_property(originalconfig).

getCommand()->
	case THIS:get_property(command) of
		{ok,{_,[]}}->
			"";
		{ok,{_,Command}}->
			Command;
		_->
			""
	end.

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%%  Params = [term()]
%%  Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params)->
	Errs = 
    case proplists:get_value(command,Params) of
    ""->
		[{command,"Show Config Command  missing"}];
    _->
	    []
	end ++
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

%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{statues,'!=',"good"}]
	end;
get_classifier(warning)->
    case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{statues,'!=',"good"}]
    end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{statues,'==',"good"}]
	end.

%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property()->
	BASE:get_template_property()++
    [
        #property{name=command,title="show config command",type=text,editable=true,order=2,description="show the config command"},
        #property{name=statues,title="status",type=text,order=1,configurable=false,state=true,baselinable=true,description="the name of the server"}
    ].


