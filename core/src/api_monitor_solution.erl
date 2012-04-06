%% 
%% @doc api function of monitor set
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_monitor_solution).
%%-extends(api_siteview).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-export([get_monitorset_list/0,get_monitorset/1,configure_monitorset/2,get_applications/2]).

%% @spec get_monitorset_list()-> [{File,Name,Descripition}]
%% where
%%	File = string()
%%	Name = string()
%%	Descripition = string()
%% @doc get monitor set list,return data is a tuple,
%%	File is the monitor set file name 
get_monitorset_list()->
	monitor_solution_template:get_template_list().

%% @spec get_monitorset(File)->({ok,MonitorSet} | {error,Err})
%% where
%%	File = string()
%%	MonitorSet = #monitor_set{}
%%	Err = string()
%% @doc get detail information of a monitor set.
%% File is the file name of monitor set,TemplateData is the Data of the monitor set,is a monitor_set record
get_monitorset(File) when is_list(File)->
	monitor_solution_template:get_template(File);
get_monitorset(_)->{error,parameter_error}.

%% @spec configure_monitorset(File,Params)->({ok,MonitorSet} | {error,Err})
%% where
%%	File = string()
%%	Params = [{Key,Value}]
%%	Key = string()
%%	Value = string()
%%	MonitorSet = #monitor_set{}
%% @doc configure monitor set with input value
configure_monitorset(File,Params)->
	monitor_solution_template:configure_template(File,Params).
	


%% @spec create_monitor_from_monitorset(Parent,Data)-> ({ok,Result} | {error,Reason})
%% where
%%	Parent = atom()
%%	Data = [{atom(),term()}]
%%	Result = atom()
%%	Reason = atom()
%% @doc create monitor from monitor set's monitor data
%% Data is a member of #monitor_set.monitors
get_applications(Type,Params)->

	monitor_solution_template:get_applications(Type,Params).