%% 
%% @doc api of backup
%% @version{1.0}
%% @copyright 2010 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(api_backup).

-export([backup/0,delete/1,backup_list/0,restore/1]).

%% @spec backup()-> ({ok,File} | {error,Reason})
%% where
%%	File = string()
%%	Reason = term()
%% @doc create backup file of system
backup()->
	svecc_backup:backup().
	
%% @spec delete(File)-> ( ok | {error,Reason})
%% where
%%	File = string()
%%	Reason = term()
%% @doc delete a backup file
delete(File)->
	svecc_backup:delete(File).

%% @spec backup_list()-> ({ok,Filelist} | {error,Reason})
%% where
%%	Filelist = [string()]
%%	Reason = term()
%% @doc return list of backup files	
backup_list()->
	svecc_backup:backup_list().
	
restore(File)->
	svecc_backup:restore(File).
