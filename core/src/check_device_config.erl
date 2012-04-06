%% Check device configuration monitor
-module(check_device_config).
-compile(export_all).
-include("monitor.hrl").

-define(PORT,23).
-define(TIMEOUT,5000).
-define(UserPrompt,"ogin:").
-define(PasswdPrompt,"assword:").

%%Access to equipment list
config_device_display()->
    Monitors = api_monitor:browse('device_config_monitor',[]),
    [{proplists:get_value(id,M),proplists:get_value(name,M),lists:nth(1,machine:getMachine(proplists:get_value(machine,M)))} || M<-Monitors,proplists:get_value(host,M,[])=:=[]].

%%Access to equipment history, alarm configuration
get_device_config(Host,Index, Count, Sort, SortType)->
WhereCondition=
        #query_condition_where{
                where=[{"my.deviceip","=",""++Host++"","&"}]
            },
    Query_Condition=
        #query_condition1{
            where=WhereCondition,
            index=Index,
            count=Count,
            sort=Sort,
            sortType=SortType
        },
        dbcs_device_config:get_Where(Query_Condition).

%%Read the original configuration
get_originalconfig(Monitorid)->
    case siteview_object_table:get_object(Monitorid) of
    [Monitor|_]->
        case Monitor:get_orginalConfig() of
            {ok,{_,OriginalConfig}}->
                {ok,OriginalConfig};
            _->{ok,""}
        end;
    _->
       {error,"get monitor error"}
    end.

%%Reset the original configuration
reset_originalconfig(Monitorid)when is_list(Monitorid)->
    reset_originalconfig(list_to_atom(Monitorid));
reset_originalconfig(Monitorid)->
    case siteview_object_table:get_object(Monitorid) of
    [Monitor|_]->
        Host=Monitor:getHostname(),
        case machine:getMachine(Host) of
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
             case api_nnm:telnet_connect(Params) of
                {ok,Pid}->
                    case api_nnm:telnet_send(Pid,Monitor:getCommand()++"\r\n") of
                        {ok,NConfigString} ->
                            Monitor:set_property(originalconfig,NConfigString),
                            BASE = Monitor:get_base(),
                            BASE:save_monitor(),
                            Monitor:set_attribute(statues,"good"),
                            Monitor:set_attribute(?STATE_STRING,"no change"),
                            api_nnm:telnet_close(Pid),
                            Monitor:update(),
                            {ok,"reset sucess"};
                        _->
                            api_nnm:telnet_close(Pid),
                            {error,"telnet send message error"}
                        end;
                 _->
                    {error,"telnet connect error"}
                 end;
        _->
            {error,"get Machine error"}
        end;
    _->
        {error,"get Monitor error"}
    end.
    

