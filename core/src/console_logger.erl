%% 
%% @doc logger
%%
-module(console_logger,[BASE]).
-extends(logger).
-compile(export_all).

new()->
	{?MODULE,logger:new(0)}.


log(Mod,Line,Date,Format,Msg)->
	io:format("~s,~p(line ~p),"++Format++"~n",[Date,Mod,Line] ++ Msg).
