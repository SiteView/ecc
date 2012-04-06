%% 
%% @doc logger
%%
-module(logger,[Duration]).

-compile(export_all).

get_datetime()->
	case erlang:localtime() of
		{{Y,M,D},{HH,MM,SS}}->
			lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",[Y,M,D,HH,MM,SS]));
		_->
			""
	end.

log(Mod,Line,Monitor)->
	Props = Monitor:getLogProperties(),
	S = get_log_str(Monitor,Props),
	log(Mod,Line,get_datetime(),"~s",S).

get_log_str(_,[])->"";
get_log_str(Monitor,[P|T])->
	case Monitor:get_attribute(P) of
		{ok,{_,V}}->
			io:format("~p:~p ",[P,V]) ++ get_log_str(Monitor,T);
		_->
			get_log_str(Monitor,T)
	end.

log(_,_,_,_,_)->
	ok.

customLogName()->"".