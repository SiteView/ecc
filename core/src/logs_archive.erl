%% 
%% @doc archive monitor log
%% @version{1.0}
%% @copyright 2010 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(logs_archive).
-compile(export_all).

-define(SERVER,'logs_archive').

start()->
	case whereis(?SERVER) of
		undefined->
			SId = spawn(fun()-> loop(?SERVER) end),
			register(?SERVER,SId),
			{ok,SId};
		_->
			{error,already_started}
	end.

stop()->
	?SERVER ! {self(),stop},
	receive
		Msg->
			Msg
	after 5000->
			{error,timeout}
	end.

loop(Name)->
	receive
		{P,stop}->
			P ! {stopped,self()}
	after 1200000->
			check_logs(),
			loop(Name)
	end.

check_logs()->
	case preferences:get(log,monitorLogKeepDays) of
		{ok,[]}->
			ok;
		{ok,[{_,V}]} ->
			case string:to_integer(V) of
				{N,[]}->
					trim_logs(N);
				_->
					ok
			end;
		
		_->
			ok
	end.

trim_logs(N)->
	Files = filelib:wildcard("logs/*/*.log"),
	F = fun(Path,Pref,File)->
		try
		case lists:prefix(Pref,File) of
			true->
				DateStr = string:sub_string(File,string:len(Pref)+1),
				Date =
				case string:tokens(DateStr,"-") of
					[Y,M,D]->
						{list_to_integer(Y),list_to_integer(M),list_to_integer(D)};
					_->
						date()
				end,
				NowDays = calendar:date_to_gregorian_days(date()),
				FileDays = calendar:date_to_gregorian_days(Date),
				if
					NowDays-FileDays>N->
						file:delete(Path);
					true->
						pass
				end;
			_->
				pass
		end
		catch
			E1:E2->
				io:format("~p:~p~n",[E1,E2])
		end
	end,
	lists:map(fun(X)->
		FileName = filename:basename(X,".log"),
		F(X,"siteview_",FileName),
		F(X,"alert_",FileName)
	end,Files),

	ok.
			