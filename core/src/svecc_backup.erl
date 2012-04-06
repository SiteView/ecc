-module(svecc_backup).
-export([backup/0,restore/1,backup_list/0,delete/1]).
-include("dbcs_common.hrl").
-include_lib("kernel/include/file.hrl").

-define(BACKUP_DIR,"backup").

backup()->
	case api_preferences:get_prefs(log,monitorLogBackupPath) of
		{ok,[]}->
			{error,no_backupdir};
		{ok,[{_,BackupPath}|_]}->
			{{Y,M,D},{HH,MM,SS}} = erlang:localtime(),
			File =filename:join(BackupPath,lists:flatten(io_lib:format("~4.4.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w.zip",[Y,M,D,HH,MM,SS]))),
			case zip:create(File,get_files("contentstore/db") ++ get_files("logs")) of
				{ok,_}->
					{ok,filename:basename(File)};
				Else->
					Else
			end
	end.
	
restore(File)->
	case api_preferences:get_prefs(log,monitorLogBackupPath) of
		{ok,[]}->
			{error,no_backupdir};
		{ok,[{_,BackupPath}|_]}->
			try
				rpc_proxy:call(?DBName, content_store, stop,[]),
				case zip:extract(filename:join(BackupPath,File)) of
					{ok,_}->
						ok;
					Else->
						io:format("retore fail:~p~n",[Else]),
						Else
				end,
				rpc_proxy:call(?DBName, content_store, start,[]),
				application:stop(svecc),
				application:start(svecc),
				ok
			catch
				E1:E2->
					{E1,E2}
			end;
		Else->
			Else
	end.
	
backup_list()->
	case api_preferences:get_prefs(log,monitorLogBackupPath) of
		{ok,[]}->
			{error,no_backupdir};
		{ok,[{_,BackupPath}|_]}->
			
			case file:list_dir(BackupPath) of
				{ok,Files}->
					Fl = 
					lists:foldl(fun(X,R)->
						case file:read_file_info(filename:join(BackupPath ,X)) of
							{ok,Fi}->
								R ++ [{X,Fi#file_info.size,sv_datetime:tostr(Fi#file_info.mtime)}];
							_->
								R ++ [{X,0,0}]
						end
					end,[], Files),
					{ok,Fl};
				Else->
					Else
			end;
		Err->
			Err
	end.
	
delete(File)->
	case api_preferences:get_prefs(log,monitorLogBackupPath) of
		{ok,[]}->
			{error,no_backupdir};
		{ok,[{_,BackupPath}|_]}->
			file:delete(filename:join(BackupPath ,File));
		Err->
			Err
	end.
	
get_files(Dir)->
	lists:foldl(fun(X,R)->
		case filelib:is_regular(X) of
			true->
				R ++ [X];
			_->
				R
		end
	end,[], filelib:wildcard(Dir++"/*")++filelib:wildcard(Dir++"/*/*")).
	
copy_dir(Src,Des)->
	io:format("copy dir:~p,~p~n",[Src,Des]),
	case file:list_dir(Src) of
		{ok,Files}->
			copy_files(Files,Src,Des);
		Else->
			Else
	end.
											
copy_files([],_,_)->ok;
copy_files([F|T],Src,Des)->
	case filelib:is_dir(F) of
		false->
			io:format("copy file:~p,~p~n",[Src ++ "/" ++ F,Des ++ "/" ++ F]),
			case file:copy(Src ++ "/" ++ F,Des ++ "/" ++ F) of
				{ok,_}->
					copy_files(T,Src,Des);
				{error,eisdir}->
					copy_files(T,Src,Des);
				Else->
					Else
			end;
		_->
			copy_files(T,Src,Des)
	end.