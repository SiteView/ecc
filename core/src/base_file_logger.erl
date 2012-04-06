%% 
%% @doc base file logger
%%
-module(base_file_logger,[BASE,BufferDuration,BufferSize]).
-extends(logger).

-compile(export_all).

new(BufferDuration,BufferSize)->
	{?MODULE,logger:new(BufferDuration),BufferDuration,BufferSize}.

log(Mod,Line,Date,Msg)->
	io:format("~s,~p(line ~p),~p~n",[Date,Mod,Line,Msg]).

log(Mod,Line,Date,Format,Msg)->
	io:format("~s,~p(line ~p),"++Format++"~n",[Date,Mod,Line] ++ Msg).
