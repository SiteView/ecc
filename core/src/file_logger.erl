%% 
%% @doc file logger
%%
-module(file_logger,[BASE,FileName,MaxSize,MaxDays,BufferDuration,BufferSize]).
-extends(base_file_logger).

%-export([log/5,log/4,new/5]).
-compile(export_all).

-define(LOGDIR,"logs").

-include("log.hrl").
-include_lib("kernel/include/file.hrl").

new(FileName,MaxSize,MaxDays,BufferDuration,BufferSize)->
	This = {?MODULE,base_file_logger:new(BufferDuration,BufferSize),FileName,MaxSize,MaxDays,BufferDuration,BufferSize},

	Path1 = ?LOGDIR ++ "/" ++ FileName,
	Path2 = ?LOGDIR ++ "/" ++ FileName ++ ".old",
	
	case file:read_file_info(Path1) of
		{ok,Fi}->
			if
				Fi#file_info.size > 0 andalso MaxDays > 0 ->
					file:delete(Path2),
					case file:rename(Path1,Path2) of
						ok->
							Date = string:substr(sv_datetime:now2str(sv_datetime:now() - MaxDays * 24 * 60 * 60 * 1000),1,10),
							This:copy_log(Path2,Path1,Date);
						_->
							io:format("rename error~n")
					end;
				Fi#file_info.size > 0 andalso MaxSize > 0 andalso Fi#file_info.size > MaxSize ->
					file:delete(Path2),
					case file:rename(Path1,Path2) of
						ok->
							ok;
						_->
							io:format("rename error~n")
					end;
				true ->
					pass
			end;
		_->
			io:format("read file info error~n")
	end,
	This.

log(Mod,Line,Date,Format,Msg)->
	case file:open(?LOGDIR ++ "/" ++ FileName, [append]) of
		{ok,File}->
			io:format(File,"~s,~p(line ~p),"++Format++"~n",[Date,Mod,Line] ++ Msg),
			file:close(File);
		_->
			pass
	end.

copy_log(Src,Dst,Date)->
	{ok,F1} = file:open(Src,[read]),
	{ok,F2} = file:open(Dst,[read,write]),
	copy_log_line(F1,F2,Date),
	file:close(F1),
	file:close(F2).

copy_log_line(F1,F2,Date)->
	case io:get_line(F1,'') of
		eof->
			done;
		L->
			V1 = string:substr(L,1,10),
			case is_date(V1) of
				true ->
					if
						V1>=Date ->
							io:format(F2,"~s",[L]),
							copy_log_line(F1,F2,Date);
						true ->
							copy_log_line(F1,F2,Date)
					end;
				_->
					copy_log_line(F1,F2,Date)
			end
	end.
	
is_date(Str)->
	case string:tokens(Str,"-") of
		[N1,N2,N3]->
			true;
		_->
			false
	end.