%% 
%% @doc dialy file logger
%%
-module(dialy_file_logger,[BASE,FileName,MaxSize,MaxDays,BufferDuration,BufferSize]).
-extends(base_file_logger).

-compile(export_all).

-include("log.hrl").

-define(LOGDIR,"logs").

new(FileName,MaxSize,MaxDays,BufferDuration,BufferSize)->
	This = {?MODULE,base_file_logger:new(BufferDuration,BufferSize),FileName,MaxSize,MaxDays,BufferDuration,BufferSize},

	Ext = filename:extension(FileName),
	BaseName = filename:basename(FileName,Ext),
	Files = filelib:wildcard(?LOGDIR ++ "/" ++ BaseName ++ "_*" ++ Ext),
	if 
		MaxDays > 0 ->
			RemDay = string:substr(sv_datetime:now2str(sv_datetime:now() - MaxDays * 24 * 60 * 60 * 1000),1,10),
			F = fun(X)->
				Bn2 = string:substr(filename:basename(X,Ext),length(BaseName)+2),
				if
					Bn2==""->
						case file:rename(X,X++".old") of
							ok->
								ok;
							_->
								?ERROR_LOG2("ERROR:Could not rename old log file:~p",[X])
						end;
					Bn2 < RemDay ->
						case file:delete(X)of
							ok->
								ok;
							_->
								?ERROR_LOG2("ERROR:Could not remove old log file:~p",[X])
						end;
					true ->
						pass
				end
			end,
			lists:foreach(F,Files);
		true ->
			pass
	end,
	FilesDes = lists:reverse(lists:sort(Files)),
	This:keep_file_size(FilesDes,0,MaxSize),
	This.

log(Mod,Line,Date,Format,Msg)->
	Ext = filename:extension(FileName),
	BaseName = filename:basename(FileName,Ext),
	case file:open(?LOGDIR ++ "/" ++ BaseName ++ "_" ++ Date ++ Ext , [append]) of
		{ok,File}->
			io:format(File,"~s,~p(line ~p),"++Format++"~n",[Date,Mod,Line] ++ Msg),
			file:close(File);
		_->
			pass
	end.

keep_files_size([],Size,_)->Size;
keep_files_size([F|T],Size,Max)->
	NewSize = filelib:file_size(F) + Size,
	if
		NewSize =< Max ->
			keep_files_size(T,NewSize,Max);
		true ->
			if
				F == FileName->
					case file:rename(F,F++".old") of
						ok->
							ok;
						_->
							?ERROR_LOG2("ERROR:Could not rename old log file:~p",[F])
					end;
				true ->
					case file:delete(F) of
						ok->
							ok;
						_->
							?ERROR_LOG2("ERROR:Could not remove old log file:~p",[F])
					end,
					keep_files_size(T,Size,Max)
			end
	end.