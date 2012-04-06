-module(file_sync).

-export([dir_sync/2,file_sync/2]).

-include_lib("kernel/include/file.hrl").
-include("log.hrl").



-define(MAX_FILE_SIZE,10000000).

file_sync(Node,FileName)->
	case rpc:call(Node,file,read_file_info,[FileName]) of
		{ok,Info}->
			if
				Info#file_info.size > ?MAX_FILE_SIZE->
					{error,big_file};
				Info#file_info.type == directory ->
					{error,is_directory};
				true->
					case file:read_file_info(FileName) of
						{ok,LcIf}->
							if
								LcIf#file_info.mtime< Info#file_info.mtime ->
									file_copy(Node,FileName);
								true ->
									{ok,sync}
							end;
						_->
							file_copy(Node,FileName)
					end
			end;
		Else->
			?ERROR_LOG2("file sync error:~p",[Else]),
			Else
	end.
	
file_copy(Node,FileName)->
	case rpc:call(Node,file,read_file,[FileName]) of
		{ok,Data}->
			filelib:ensure_dir(FileName),
			file:write_file(FileName,Data);
		Err->
			?ERROR_LOG2("file copy error:~p",[Err]),
			Err
	end.
	
dir_sync(Node,Dir)->
	case rpc:call(Node,file,list_dir,[Dir]) of
		{ok,SubDirs}->
			F = fun(X)->
				FN = Dir++"/" ++ X,
				case rpc:call(Node,filelib,is_dir,[FN]) of
					true->
						dir_sync(Node,FN);
					false->
						file_sync(Node,FN);
					_->
						pass
				end
			end,
			lists:map(F,SubDirs);
		Else->
			?ERROR_LOG2("dir sync error:~p",[Else]),
			Else
	end.