%%
%% ntcounter_util
%%
%%
-module(ntcounter_util).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

getIDCacheForCounters(Host,PerfCounters)->
	Hmap = [{string:to_upper(X#perf_counter.counterName),X#perf_counter.counterName}||X<-PerfCounters] ++
			[{string:to_upper(X#perf_counter.object),X#perf_counter.object}||X<-PerfCounters],
	
	Cmd = platform:perfexCommand(Host) ++ " -map",
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd),
	FF = fun(X)->
			case regexp:match(X,"id:") of
				nomatch->
					{"",""};
				_->
				Pos1 = string:rstr(X,"id:"),
				Pos2 = string:rstr(X,"name:"),
				{string:strip(string:sub_string(X,Pos1+3,Pos2-1)),string:strip(string:sub_string(X,Pos2+5,length(X)))}
			end
		end,
	Lines= lists:map(FF,string:tokens(Rets,"\r\n")),
	F = fun(X)->
			case X of
				{_,P2}->
					case lists:keymember(string:to_upper(P2),1,Hmap) of
						true ->
							true;
						_->
							false
					end;
				_->
					false
			end
		end,
		lists:filter(F,Lines).
			
getPerfData(Host,Counters,Monitor,[])->
	%%io:format("Counters:~p~n",[Counters]),
	I = length(Counters),
	Objects = filter_counters(1,[],Counters,I),
	Cmds = platform:perfexCommand(Host) ++" " ++  make_params4(Objects),
	%%io:format("getPerfData:~p~n",[Cmds]),
	CmdLine = command_line:new(),
	Ret = CmdLine:exec(Cmds),
	RetLines =[list_to_tuple(string:tokens(X,":")) || X<- string:tokens(Ret,"\r\n")],
	%io:format("getPerfData lines:~p~n",[RetLines]),
	V = parse_line(RetLines,[],Counters),
	%io:format("getPerfData1 Result:~p~n",[V]),
	V;
getPerfData(Host,Counters,Monitor,Map)->
	%%io:format("Counters2:~p~n",[Counters]),
	I = length(Counters),
	Objects = filter_counters(1,[],Counters,I),
	FMaps = filter_map(1,[],Map,length(Map)),
	Params = make_params(Objects,Counters,Map,FMaps),
	%%io:format("getPerfData~nObjects:~p~nMaps:~p~nParams:~p~n",[Objects,FMaps,Params]),
	Cmds = platform:perfexCommand(Host) ++" " ++  Params,
	CmdLine = command_line:new(),
	Ret = CmdLine:exec(Cmds),
	%%io:format("getPerfData Result:~p",[Ret]),
	RetLines =[list_to_tuple(string:tokens(X,":")) || X<- string:tokens(Ret,"\r\n")],
	
	%%V = parse_line(RetLines,Map,Counters),
	{Types,Values,Units,Others} = parse_line2(RetLines,Map,Counters,"","",false,list_to_tuple(lists:duplicate(I,"n/a")),list_to_tuple(lists:duplicate(I,"n/a")),list_to_tuple(lists:duplicate(I,"n/a")),[],1),
	%%io:format("Types:~p,Values:~p,Units:~p~n",[Types,Values,Units]),
	V = Others ++ make_result(Counters,Types,Values,Units,1),
	%%io:format("getPerfData2 Result:~p~n",[V]),
	V
	.

make_result([],_,_,_,_)->[];
make_result([C|T],Types,Values,Units,I)->
	case C#perf_counter.instance of
		""->
			[{C#perf_counter.counterName,{element(I,Values),element(I,Units),element(I,Types)}}] ++
			make_result(T,Types,Values,Units,I+1);
		_->
			[{C#perf_counter.counterName ++ ":" ++ C#perf_counter.instance,{element(I,Values),element(I,Units),element(I,Types)}}] ++
			make_result(T,Types,Values,Units,I+1)
	end.

filter_counters(I,Ret,_,Max) when I > Max ->Ret;
filter_counters(I,Ret,Counters,Max)->
	C = lists:nth(I,Counters),
	%%io:format("filter_counters:~p,~p~n",[I,C]),
	case lists:keysearch(C#perf_counter.object,1,Ret) of
		{value,{_,Cs}} ->
			filter_counters(I+1,lists:keyreplace(C#perf_counter.object,1,Ret,{C#perf_counter.object,Cs++[C]}),Counters,Max);
		_->
			filter_counters(I+1,Ret++[{C#perf_counter.object,[C]}],Counters,Max)
	end.


filter_map(I,Ret,_,Max) when I > Max ->Ret;
filter_map(I,Ret,Maps,Max)->
	C = lists:nth(I,Maps),
	case lists:keysearch(element(2,C),1,Ret) of
		{value,{_,Mids}} ->
			filter_map(I+1,lists:keyreplace(element(2,C),1,Ret,{element(2,C),Mids ++ "," ++ element(1,C)}),Maps,Max);
		false ->
			filter_map(I+1,Ret++[{element(2,C),element(1,C)}],Maps,Max)
	end.


make_params([],_,_,_)->"";
make_params([{Obj,_}|T],Counters,Map,MapId)->
	case lists:keysearch(Obj,2,Map) of
		{value,{Id,Name}}->
			"-oic \"" ++ Id ++ "/" ++ make_params2(Counters,Name,Map) ++ "/" ++ make_params3(Counters,Name,MapId) ++
			"\" " ++ make_params(T,Counters,Map,MapId);
		_->
			make_params(T,Counters,Map,MapId)
	end.


make_params2([],_,_)->"";
make_params2([C=#perf_counter{instance=undefined}|T],Name,Map)->
	make_params2(T,Name,Map);
make_params2([C=#perf_counter{instance=""}|T],Name,Map)->
	make_params2(T,Name,Map);
make_params2([C|T],Name,Map)->
	case C#perf_counter.object of
		Name->
			case lists:keysearch(C#perf_counter.instance,2,Map) of
				{value,{Id,_}}->
					if
						length(T) > 0 ->
							Id ++ "," ++ make_params2(T,Name,Map);
						true ->
							Id
					end;
				_->
					make_params2(T,Name,Map)
			end;
		_->
			make_params2(T,Name,Map)
	end.


make_params3([],_,_)->"";
make_params3([C=#perf_counter{counterName=undefined}|T],Name,Map)->
	make_params3(T,Name,Map);
make_params3([C=#perf_counter{counterName=""}|T],Name,Map)->
	make_params3(T,Name,Map);
make_params3([C|T],Name,Map)->
	case C#perf_counter.object of
		Name ->
			case lists:keysearch(C#perf_counter.counterName,1,Map) of
				{value,{_,Id}}->
					if
						length(T) > 0 ->
							Id ++ "," ++ make_params3(T,Name,Map);
						true ->
							Id
					end;
				_->
					make_params3(T,Name,Map)
			end;
		_->
			make_params3(T,Name,Map)
	end.


make_params4([])->"";
make_params4([{Obj,_}|T])->
	"-o " ++ Obj ++ " "++ make_params4(T).

parse_line([],_,_)->[];
parse_line([{"PerfFreq",V}|T],Map,Counters)->
	[{"PerfFreq",string:strip(V)}] ++ parse_line(T,Map,Counters);
parse_line([{"PerfTime100nSec",V}|T],Map,Counters)->
	[{"PerfTime100nSec",string:strip(V)}] ++ parse_line(T,Map,Counters);
parse_line([{"PerfTime",V}|T],Map,Counters)->
	[{"PerfTime",string:strip(V)}] ++ parse_line(T,Map,Counters);
parse_line([{K,V}|T],[],Counters)->
	case find_counter(K,Counters) of
		true ->
			VV = [string:strip(X)||X<-string:tokens(V," ")],
			if 
				length(VV) == 1->
					parse_line(T,[],Counters);
				length(VV) == 2->
					[V1,V2] = VV,
					[{K,list_to_tuple([V1,"",V2])}] ++parse_line(T,[],Counters);
				length(VV) > 2 ->
					[{K,list_to_tuple(VV)}]++parse_line(T,[],Counters);
				true ->
					parse_line(T,[],Counters)
			end;
		_->
			parse_line(T,[],Counters)
	end;
parse_line([{K,V}|T],Map,Counters)->
	case lists:keysearch(K,1,Map) of
		{value,{_,Name}}->
			VV = [string:strip(X)||X<-string:tokens(V," ")],
			if 
				length(VV) == 1->
					parse_line(T,Map,Counters);
				length(VV) == 2->
					[V1,V2] = VV,
					[{Name,list_to_tuple([V1,"",V2])}] ++parse_line(T,Map,Counters);
				length(VV) > 2 ->
					[{Name,list_to_tuple(VV)}]++parse_line(T,Map,Counters);
				true ->
					parse_line(T,Map,Counters)
			end;
		_->
			parse_line(T,Map,Counters)
	end;
parse_line([_|T],Map,Counters)->
	parse_line(T,Map,Counters).


find_counter(_,[])->false;
find_counter(Name,[C|T])->
	case C#perf_counter.counterName of
		Name ->
			true;
		_->
			find_counter(Name,T)
	end.

find_counter_by_object(_,[],_)->undefined;
find_counter_by_object(Object,[C|T],I)->
	case C#perf_counter.object of
		Object->
			{I,C};
		_->
			find_counter_by_object(Object,T,I+1)
	end.

parse_line2([],_,_,_,_,_,Types,Values,Units,Others,_)->{Types,Values,Units,Others};
parse_line2([{"PerfFreq",V}|T],Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others,Index)->
	 parse_line2(T,Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others ++ [{"PerfFreq",string:strip(V)}],Index);
parse_line2([{"PerfTime100nSec",V}|T],Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others,Index)->
	 parse_line2(T,Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others++[{"PerfTime100nSec",string:strip(V)}],Index);
parse_line2([{"PerfTime",V}|T],Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others,Index)->
	parse_line2(T,Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others++[{"PerfTime",string:strip(V)}],Index);
parse_line2([{"name",V}|T],Map,Counters,_,Obj,BInstance,Types,Values,Units,Others,Index)->
	parse_line2(T,Map,Counters,string:strip(V),Obj,BInstance,Types,Values,Units,Others,Index);
parse_line2([{"object",V}|T],Map,Counters,Name,_,BInstance,Types,Values,Units,Others,Index)->
	[Object|_] = string:tokens(V," "),
	case lists:keysearch(Object,1,Map) of
		{value,{_,Obb}}->
			parse_line2(T,Map,Counters,Name,Obb,BInstance,Types,Values,Units,Others,Index);
		_->
			parse_line2(T,Map,Counters,Name,Object,BInstance,Types,Values,Units,Others,Index)
	end;
parse_line2([{K,V}|T],Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others,Index)->
	if
		BInstance == true ->
			Start = length(V)-5,
			case regexp:match(V,"_BASE") of
				{match,_,_} ->
					Temps = string:tokens(V," "),
					%%io:format("parse_line2:~p,~p~n",[K,lists:nth(1,Temps)]),
					TU =setelement(Index,Units,lists:nth(1,Temps)),
					parse_line2(T,Map,Counters,Name,Obj,BInstance,Types,Values,TU,Others,Index+1);
				_->
				 {NIns,NTypes,NValues,NNUnits,NIndex} = parse_counters(Counters,Obj,Name,{K,V},T,Map,false,Types,Values,Units,Units,1,Index),
				 parse_line2(T,Map,Counters,Name,Obj,NIns,NTypes,NValues,NNUnits,Others,NIndex)
			end;
		true ->
			{NIns,NTypes,NValues,NNUnits,NIndex} = parse_counters(Counters,Obj,Name,{K,V},T,Map,BInstance,Types,Values,Units,Units,1,Index),
			parse_line2(T,Map,Counters,Name,Obj,NIns,NTypes,NValues,NNUnits,Others,NIndex)
	end;
	%%io:format("result1:~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p~n",[Counters,Obj,Name,K,V,T,Map,BInsta,Types,Values,NUnits,NUnits,Index]),
	%%{NIns,NTypes,NValues,NNUnits,NIndex} = parse_counters(Counters,Obj,Name,{K,V},T,Map,BInsta,Types,Values,NUnits,NUnits,1,Index),
	%%parse_line2(T,Map,Counters,Name,Obj,NIns,NTypes,NValues,NNUnits,Others,NIndex);

parse_line2([_|T],Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others,Index)->
	parse_line2(T,Map,Counters,Name,Obj,BInstance,Types,Values,Units,Others,Index).

parse_counters([],_,_,_,_,_,BInstance,Types,Values,Units,_,_,Index)->{BInstance,Types,Values,Units,Index};
parse_counters([C|T],Object,Name,{K,V},TT,Map,BInstance,Types,Values,Units,NUnits,I,Index)->
	if
		(C#perf_counter.object == Object) ->
			PerfCounterName = C#perf_counter.counterName,
			%ÕÒcounterµÄÃû×Ö
			CName = case lists:keysearch(K,1,Map) of
						{value,{_,CCName}}->
							CCName;
						_->
							K
					end,
			%%io:format("parse_counters:~p,~p~n",[PerfCounterName,CName]),
			IsCounter = case lists:nthtail(length(PerfCounterName)-1,PerfCounterName) of
					"*"->
						case regexp:match(CName,string:substr(PerfCounterName,length(PerfCounterName)-1)) of
							nomatch->
								false;
							_->
								true
						end;
					_->
						if
							CName == PerfCounterName ->
								true;
							true ->
								false
						end
				end,
			if
				IsCounter == true ->
					BTotal = C#perf_counter.instance == "*total*",
					
					BInstanceAV = (C#perf_counter.instance =="") or (C#perf_counter.instance==Name) or BTotal,
					
					if
						BInstanceAV == true ->
							Tokens = string:tokens(V," "),
							%%io:format("setelement1:~p,~p,~p~n",[I,Types,Tokens]),
							NTypes = if
										length(Tokens) == 2 ->
											setelement(I,Types,lists:nth(2,Tokens));
										length(Tokens)==3->
											setelement(I,Types,lists:nth(3,Tokens));
										true ->
											Types
									end,
							%%io:format("setelement2:~p,~p,~p~n",[I,Values,Tokens]),
							NValues	= if
										 BTotal== true ->
												case element(I,Values) of
													"n/a"->
														setelement(I,Values,lists:nth(1,Tokens));
													Val2->
														setelement(I,Values,integer_to_list(list_to_integer(Val2) + list_to_integer(lists:nth(1,Tokens))))
												end;
											true ->
												setelement(I,Values,lists:nth(1,Tokens))
									end,
							%%io:format("setelement3:~p,~p,~p~n",[I,Units,Tokens]),
							NNUnits = setelement(I,Units,""),
							parse_counters(T,Object,Name,{K,V},TT,Map,true,NTypes,NValues,NNUnits,NUnits,I+1,I);
							%%parse_line2(T,Map,Counters,Name,Obj,BInstance,NTypes,NValues,NNUnits,Others,I)
						true ->
							%%parse_line2(T,Map,Counters,Name,Obj,BInstance,NTypes,NValues,NNUnits,Others,I)
							parse_counters(T,Object,Name,{K,V},TT,Map,BInstance,Types,Values,Units,NUnits,I+1,Index)
					end;
				true ->
					%%parse_line2(T,Map,Counters,Name,Obj,BInstance,Types,Values,NUnits,Others,Index)
					parse_counters(T,Object,Name,{K,V},TT,Map,BInstance,Types,Values,Units,NUnits,I+1,Index)
			end;
		true ->
			parse_counters(T,Object,Name,{K,V},TT,Map,BInstance,Types,Values,Units,NUnits,I+1,Index)
	end.