%% ---
%% rule
%%
%%---
-module(log).
-define(LOGDIR,"logs").
-export([trace/3,trace/4,error/3,error/4]).

get_datetime()->
	case erlang:localtime() of
		{{Y,M,D},{HH,MM,SS}}->
			lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",[Y,M,D,HH,MM,SS]));
		_->
			""
	end.

trace(Module,Line,Msg)->
	case file:open(?LOGDIR ++ "/debug.log", [append]) of
		{ok,File}->
			io:format(File,"~s,~p(~p),~s~n",[get_datetime(),Module,Line,Msg]),
			file:close(File);
		_->
			ok
	end.

trace(Module,Line,Format,Params)->
	case file:open(?LOGDIR ++ "/debug.log", [append]) of
		{ok,File}->
			io:format(File,"~s,~p(~p),"++Format++"~n",[get_datetime(),Module,Line]++Params),
			file:close(File);
		_->
			ok
	end.	

error(Module,Line,Msg)->
	case file:open(?LOGDIR ++ "/error.log", [append]) of
		{ok,File}->
			io:format(File,"~s,~p(~p),~s~n",[get_datetime(),Module,Line,Msg]),
			file:close(File);
		_->
			ok
	end.

error(Module,Line,Format,Params)->
	case file:open(?LOGDIR ++ "/error.log", [append]) of
		{ok,File}->
			io:format(File,"~s,~p(~p),"++Format++"~n",[get_datetime(),Module,Line]++Params),
			file:close(File);
		_->
			ok
	end.