%%
%% perfchartfile
%%
%%
-module(perfchartfile).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").


get_settings(FileName)->
	case file:read_file(?TEMPLATES_PERFMON ++ "/" ++ FileName) of
		{ok,Binary}->
			Data = binary_to_list(Binary),
			%%io:format("get_settings:~p~n",[Data]),
			%%io:format("perfchartfile:~p~n",[lists:map(fun(X)->lists:nth(X,Signature) end,lists:seq(1,40,2))]),
			Sign = lists:map(fun(X)->lists:nth(X,Data) end,lists:seq(1,40,2)),
			Type = case regexp:match(Sign,"PERF CHART") of
						{match,_,_}->
							1;
						_->
							case regexp:match(Sign,"PERF WORKSPACE") of 
								{match,_,_}->
									2;
								_->
									0
							end
					end,
			J = byte_to_dword(lists:sublist(Data,41,4)),
			K = byte_to_dword(lists:sublist(Data,45,4)),
			%%io:format("J:~p,K:~p~n",[J,K]),
			if
				(Type=:=1 andalso J=:= 1 andalso K=:=3) or (Type=:=2 andalso J=:=1 andalso K=:=6) ->
					Strat = if
								Type =:= 2 ->
									Temp1 = byte_to_dword(lists:sublist(Data,153,4)),
									Temp2 = Temp1 - 156,
									if
										length(Data) < 153+Temp2 ->
											throw({error,error_file_size});
										true ->
											253+8+Temp2
									end;
								true ->
									253
							end,
					Ret = get_line(Data,Strat),
					{ok,Ret};
					%%io:format("get_settings:~p~n",[Ret]);
				true->
					{error,file_format_error}
			end;
		_->
			{error,open_file_error}
	end.

byte_to_dword([])->0;
byte_to_dword([N|T])->
	N bor (byte_to_dword(T) bsl 8).

get_line(Data,Start) when Start > length(Data) -> [];
get_line(Data,Start)->
	LineSign = lists:sublist(Data,Start,4),
	%%io:format("get_line:~p~n",[LineSign]),
	B0 = lists:nth(1,LineSign),
	B1 = lists:nth(3,LineSign),
	if 
		B0 =:= 76 andalso B1 =:= 105 ->
			
			LineLen = byte_to_dword(lists:sublist(Data,Start+4,4)),
			Line = lists:sublist(Data,Start+8,LineLen),

			I = byte_to_dword(lists:sublist(Line,5,4)),
			J = byte_to_dword(lists:sublist(Line,9,4)),

			SysName = lists:map(fun(X)->lists:nth(X,Line) end,make_seq(J+5,I*2+J+4,2)),
			
			I1= byte_to_dword(lists:sublist(Line,13,4)),
			J1 = byte_to_dword(lists:sublist(Line,17,4)),

			ObjName = lists:map(fun(X)->lists:nth(X,Line) end,make_seq(J1+13,I1*2+J1+12,2)),
			
			I2= byte_to_dword(lists:sublist(Line,21,4)),
			J2 = byte_to_dword(lists:sublist(Line,25,4)),
			CounterName = lists:map(fun(X)->lists:nth(X,Line) end,make_seq(J2+21,I2*2+J2+20,2)),
			
			I3= byte_to_dword(lists:sublist(Line,29,4)),
			J3 = byte_to_dword(lists:sublist(Line,33,4)),
			InstanceName = lists:map(fun(X)->lists:nth(X,Line) end,make_seq(J3+29,I3*2+J3+28,2)),
			
			ScaleIndex = byte_to_dword(lists:sublist(Line,93,4)),
			%%io:format("get_line,SysName:~p,ObjName:~p,CounterName:~p,InstanceName:~p,ScaleIndex:~p~n",
			%%	[SysName,ObjName,CounterName,InstanceName,ScaleIndex]),
			
			[#perf_counter{object=ObjName,counterName=CounterName,instance=InstanceName}] ++ get_line(Data,Start+ 8 + LineLen);
		true ->
			get_line(Data,Start+ 4)
	end.


make_seq(S,E,_) when E < S -> [];
make_seq(S,E,I)-> lists:seq(S,E,I).