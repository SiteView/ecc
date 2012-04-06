%% 
%% @doc api of group operation
%% @version{1.0}
%% @copyright 2011 dragonflow.com
%% @author Hu Hengyi <hengyi.hu@dragonflow.com>


-module(api_device_config_manage).
-compile(export_all).
-include("monitor.hrl").

-export([get_Devices/0,get_Alart_config/5,get_originalconfig/1,reset_originalconfig/1]).

%% @spec get_Devices()-> Result
%% where
%%		Result = list()
%% @docBack to the list of monitoring equipment，Result={Monitorid,MonitorName,{#Machine{}}, #Machine{}={ id, name,host, login, passwd, trace,os, status, method, prompt, loginprom,passwdprom,secondprom,secondresp, initshell,remoteencoding, sshcommand, sshclient,sshport, disableconncaching, connlimit, version, keyfile, sshauthmethod,label, total, type,other,pwdmode}
%% release
get_Devices()->
    check_device_config:config_device_display().

%% @spec get_Alart_config(Host,Index, Count, Sort, SortType)-> Result | {error,Reason}
%% where
%%		Host = list()
%%		Index = integer()
%%		Count = integer()
%%		Sort = string()
%%     SortType=string()
%%     Result = list()
%% @docBack to the list of monitoring equipment,Result=[{time,Time},{monitorid,MonitorID},{deviceip,IP},{deviceid,MachineId},{nconfig,NConfigString},{oconfig,OriginalConfig}],NConfigString:最新配置，OriginalConfig，原始配置
%% release
get_Alart_config(Host,Index, Count, Sort, SortType)->
    check_device_config:get_device_config(Host,Index, Count, Sort, SortType).


%% @spec get_originalconfig(Monitorid)-> {ok,OrginalConfig} | {error,Reason}
%% where
%%		Monitorid = atom()
%%     OrginalConfig = string()
%%     Reason=string()
%% @doc The original reading device configuration information
%% release
 get_originalconfig(Monitorid)->
    check_device_config:get_originalconfig(Monitorid).

%% @spec reset_originalconfig(Monitorid)-> {ok,Reason} | {error,Reason}
%% where
%%		Monitorid = atom()
%%      Reason=string()
%% @doc Reset the device configuration information of the original
%% release
reset_originalconfig(Monitorid)->
    check_device_config:reset_originalconfig(Monitorid).
    
    