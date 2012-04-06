%% @doc nmap_scan module
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(nmap_scan).
-compile(export_all).

%% @spec fast_scan(Target)->->list()
%% where
%%	Target = string()
%% @doc fast scan a target,it only found whether a host state is up.
fast_scan(Target)->
	nmap:scan([Target,"-n","-T5","-sP"]).

%% @spec os_scan(Target)->list()
%% where
%%	Target = string()
%% @doc scan a target's os information,it is a slowly scan 
os_scan(Target)->
	nmap:scan([Target,"-n","-sU","-sT","-O","-F","-T4"]).

%% @spec scan(Target,Params)->list()
%% where
%%	Target = string()
%%	Params = list()
%% @doc scan a target with custom parameters
scan(Target,Params)->
	nmap:scan([Target] ++ Params).
