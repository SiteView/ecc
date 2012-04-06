-module(wmi).

-export([cpu/4, memory/4, disk/4, disk/5, service/4, service/5, process/4, process/5, network/4, network/5, directory/7]).

replace(List, Find, Replace)->	replace(List, Find, Replace, []).

replace_contact(Acc, [H|R])-> replace_contact([H|Acc],R);
replace_contact(Acc, [])-> Acc.

replace_compare([C|L], [C|R], Acc)-> replace_compare(L, R, [C|Acc]);
replace_compare(L, [], _)-> {true, L, []};
replace_compare(L, _, Acc)-> {false, L, lists:reverse(Acc)}.

replace([C|L], [C|R] = Find, Replace, Acc)->
	case replace_compare(L, R, [C]) of
		{true, Rest1, _} ->
			replace(Rest1, Find, Replace, replace_contact(Acc, Replace));
		{_, Rest2, Result2}->
			replace(Rest2,  Find, Replace,replace_contact(Acc, Result2))
	end;
replace([C|L], Find, Replace, Acc)-> replace(L, Find, Replace, [C|Acc]);
replace([], _, _, Acc)-> lists:reverse(Acc).

subdirectory(Host,User,Password,Directory) ->
    try trysubdirectory(Host,User,Password,Directory) of
       {error,ErrorResult} -> {error,ErrorResult}; 
	Result -> {ok,Result}			   
    catch
        error:X -> {error,X}
    end.
    
trysubdirectory(Host,User,Password,Directory) ->
   Directorylist = subdirectorylist(Host,User,Password,[Directory],[Directory]),
   case Directorylist of
          {error,ErrorResult} -> {error,ErrorResult}; 
	  _ ->
	  io:format("Directorylist:~p~n",[Directorylist]),
	  filelist(Host,User,Password,Directorylist,[])
   end.
   
   
filelist(Host,User,Password,[Directory|R],Acc) ->
    try directory(Host,User,Password,Directory) of
	{ok,Result} -> filelist(Host,User,Password,R,Result++Acc);	 
	_ -> filelist(Host,User,Password,R,Acc)
    catch
        _:_ -> filelist(Host,User,Password,R,Acc)
    end;
filelist(_,_,_,[],Acc) -> Acc.  
    
subdirectorylist(Host,User,Password,[Directory|R],Acc) ->
   case string:str(Directory,":") of	 
	 2 when length(Directory) =:= 2 ->	  
	    Wql = lists:flatten(["SELECT Name FROM Win32_Directory where Drive= '",Directory,"' and Path = '\\\\'"]), 
	    Directorys = wmidirectorylist(Host,User,Password,Wql),
	    case Directorys of
	          {error,ErrorResult} -> {error,ErrorResult};  
	         _ ->
		    Sublist = subdirectorylist(Host,User,Password,Directorys,[]),
		    subdirectorylist(Host,User,Password,R,Acc++Directorys++Sublist)
	    end;
	 2 ->
	    [Driver,Path] = string:tokens(Directory,":"),
	    Wql = case  lists:suffix("\\",Path) of
	          true -> lists:flatten(["SELECT Name FROM Win32_Directory where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"'"]);
		  _ -> lists:flatten(["SELECT Name FROM Win32_Directory where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"\\\\'"])
	    end,
	    Directorys = wmidirectorylist(Host,User,Password,Wql),
	    case Directorys of
	          {error,ErrorResult} -> {error,ErrorResult};  
	         _ ->
		    Sublist = subdirectorylist(Host,User,Password,Directorys,[]),
		    subdirectorylist(Host,User,Password,R,Acc++Directorys++Sublist)
	    end;	  
	 _ -> subdirectorylist(Host,User,Password,R,Acc)
   end;
subdirectorylist(_,_,_,[],Acc) -> Acc.   

wmidirectorylist(Host,User,Password,Wql) ->   
  %% io:format("service:~p~n",[Wql]),
   case  wmic:wmic(Host,User,Password,Wql) of
	 {ok,empty} -> [];
         {ok,WmiData} ->
	       
	        case  lists:keysearch(Wql, 1, WmiData) of
		  {value,{_,{ok,empty}}} -> [];
		  {value,{_,{ok,Values}}} ->		          
			  Fun = fun(X) -> 
				Lists = tuple_to_list(X),
				{value,{_,Name}} =  lists:keysearch('Name', 1, Lists),
				binary_to_list(Name)
				end,			
			 lists:map(Fun,Values); 
		  _ -> []
		end;
         {error,ErrorResult} -> {error,ErrorResult};      		
	 _ -> []
   end.   

directory(Host,User,Password,Directory) ->
   case string:str(Directory,":") of	 
	 2 when length(Directory) =:= 2 ->	  
	    Wql = lists:flatten(["SELECT CreationDate, Drive, Extension, FileName, FileSize, LastAccessed, LastModified, Name FROM CIM_DataFile where Drive= '",Directory,"' and Path = '\\\\'"]), 
	    wmidirectory(Host,User,Password,Wql);
	 2 -> 
	    [Driver,Path] = string:tokens(Directory,":"),
	    Wql = case  lists:suffix("\\",Path) of
	          true -> lists:flatten(["SELECT CreationDate, Drive, Extension, FileName, FileSize, LastAccessed, LastModified, Name FROM CIM_DataFile where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"'"]);
		  _ -> lists:flatten(["SELECT CreationDate, Drive, Extension, FileName, FileSize, LastAccessed, LastModified, Name FROM CIM_DataFile where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"\\\\'"])
	    end,
	    wmidirectory(Host,User,Password,Wql);
	 _ -> {error,"Directory param is error!"}
   end.
   
wmidirectory(Host,User,Password,Wql) ->   
   %%io:format("service:~p~n",[Wql]),
   case  wmic:wmic(Host,User,Password,Wql) of
         {ok,WmiData} ->
	        case  lists:keysearch(Wql, 1, WmiData) of
		  {value,{_,{ok,empty}}} -> {error,[]};
		  {value,{_,{ok,Values}}} ->
			  %%io:format("service:~p~n",[Values]),
			  try analysisdirectory(Values) of
				Result -> {ok,Result}			   
			  catch
			     error:X-> {error,X}
			  end; 
		  ErrorResult -> {error,ErrorResult}
		end;
	 ErrorResult -> ErrorResult
   end.
    
analysisdirectory(Values) ->
	Fun = fun(X) -> 
		Lists = tuple_to_list(X),
		%%io:format("service:~p~n",[Lists]),
		{value,{_,Name}} =  lists:keysearch('Name', 1, Lists),
		{value,{_,FileName}} =  lists:keysearch('FileName', 1, Lists),
		{value,{_,Extension}} =  lists:keysearch('Extension', 1, Lists),
		{value,{_,Drive}} =  lists:keysearch('Drive', 1, Lists),		
		{value,{_,FileSize}} =  lists:keysearch('FileSize', 1, Lists),				
		{value,{_,CreationDate}} =  lists:keysearch('CreationDate', 1, Lists),
		{value,{_,LastAccessed}} =  lists:keysearch('LastAccessed', 1, Lists),
		{value,{_,LastModified}} =  lists:keysearch('LastModified', 1, Lists),
		
		{Name,[{'FileName',FileName},
			   {'Extension',Extension},
			   {'FileSize',FileSize},
			   {'Drive',Drive},				   
			   {'CreationDate',CreationDate},	
			   {'LastAccessed',LastAccessed},	
			   {'LastModified',LastModified}]}
		end,
        lists:map(Fun,Values).
   
sleep(Time) ->
    receive 
	after Time ->
            ok
    end.
   

cpu(OS, Host, User, Password)->
	Wql = "SELECT DeviceID, LoadPercentage FROM Win32_Processor",
	Result = wmic:wmic(Host,User,Password,Wql),
	case Result of
		{ok, Tables}->
			case hd(Tables) of
				{_, {ok, Rows}}->
					DetailUtilization = string:strip(lists:append(lists:flatmap(fun({{_, V1}, {_, V2}})->[binary_to_list(V1), ":", integer_to_list(V2), "-"] end, Rows)), right, $-),
					{Count, Acc} = lists:foldl(fun({_, {_, V}}, {CountIn, AccIn})-> {CountIn + 1, AccIn + V} end, {0, 0}, Rows),
					Utilization = integer_to_list(Acc div Count),
					Return = lists:append(["detailutilization=", DetailUtilization ,";utilization=", Utilization]),
					{ok, Return};
				{_, {_, Error2}} ->
					{error, Error2}
			end;
		{_, Error1}->
			{error, Error1}
	end.

memory_process(1, Tuple)->
	case size(Tuple) of
		4 ->
			FreePhyMem = list_to_integer(binary_to_list(element(2, element(1, Tuple)))) / 1024,
			FreeVirMem = list_to_integer(binary_to_list(element(2, element(2, Tuple)))) / 1024,
			TotalVirMem = list_to_integer(binary_to_list(element(2, element(3, Tuple)))) / 1024,
			TotalPhyMem = list_to_integer(binary_to_list(element(2, element(4, Tuple)))) / 1024;
		_ ->
			FreePhyMem = list_to_integer(binary_to_list(element(2, element(1, Tuple)))) / 1024,
			FreeVirMem = list_to_integer(binary_to_list(element(2, element(2, Tuple)))) / 1024,
			TotalVirMem = list_to_integer(binary_to_list(element(2, element(4, Tuple)))) / 1024,
			TotalPhyMem = list_to_integer(binary_to_list(element(2, element(5, Tuple)))) / 1024
	end,
	lists:flatten(io_lib:format("TotalPhyMem=~.2f$TotalVirMemory=~.2f$FreePhyMem=~.2f$FreeVirMem=~.2f$PhyMemUsage=~.2f$PercentUsed=~.2f$", 
							[TotalPhyMem, TotalVirMem, FreePhyMem, FreeVirMem, (TotalPhyMem-FreePhyMem)/TotalPhyMem*100, (TotalVirMem-FreeVirMem)/TotalVirMem*100]));
memory_process( _, Tuple)->
	Frequency_PerfTime = list_to_integer(binary_to_list(element(2, element(1, Tuple)))),
	PagesPersec = element(2, element(2, Tuple)),
	Timestamp_PerfTime = list_to_integer(binary_to_list(element(2, element(3, Tuple)))),
	{Frequency_PerfTime, PagesPersec, Timestamp_PerfTime}.

memory(OS, Host, User, Password)->
	Wql1 = ["SELECT FreePhysicalMemory, FreeVirtualMemory, TotalVirtualMemorySize, TotalVisibleMemorySize FROM CIM_OperatingSystem", "SELECT Frequency_PerfTime, PagesPersec, Timestamp_PerfTime FROM Win32_PerfRawData_PerfOS_Memory"],
	Result1 = wmic:wmic(Host,User,Password,Wql1),
	case Result1 of
		{ok, [Table1, Table2]}->
			case Table1 of
				{_, {ok, [Row1]}}->
					case Table2 of
						{_, {ok, [Row2]}}->
							Wql2 = "SELECT Frequency_PerfTime, PagesPersec, Timestamp_PerfTime FROM Win32_PerfRawData_PerfOS_Memory",
							sleep(1000),
							Result2 = wmic:wmic(Host,User,Password,Wql2),
							case Result2 of
								{ok, [Table3]} ->
									case Table3 of
										{_, {ok, [Row3]}}->
											Memory = memory_process(1, Row1),
											{_, PagesPersec1, Timestamp_PerfTime1} = memory_process(2, Row2),
											{Frequency_PerfTime, PagesPersec2, Timestamp_PerfTime2} = memory_process(2, Row3),
											PagesPersec = (PagesPersec2-PagesPersec1)/((Timestamp_PerfTime2-Timestamp_PerfTime1)/Frequency_PerfTime),
											{ok, Memory ++ lists:flatten(io_lib:format("PagesPerSec=~.2f$", [PagesPersec]))};
										{_, {_, Error5}}->
											{error, Error5}
									end;
								{_, Error4}->
									{error, Error4}
							end;					
						{_, {_, Error3}}->
							{error, Error3}
					end;
				{_, {_, Error2}}->
					{error, Error2}
			end;
		{_, Error1}->
			{error, Error1}
	end.
	
	
filterCount(R) -> 
	case R of
		{_,[{_,{_,empty}}]} -> 0;
		{_,[{_,{_,RList}}]} -> length(RList);
		_ -> 0
	end.
filterEventLog(Host, User, Password, EventType, EventID) ->
	{YY, MM, DD} = date(),
	DATE = integer_to_list(YY) ++ integer_to_list(MM) ++ integer_to_list(DD) ++ "000000.000000+480",
	TWql = "SELECT Category FROM Win32_NTLogEvent where Logfile='System' and EventType=" ++ integer_to_list(EventType) ++ 
	" and TimeWritten>='"++ DATE ++ "'",
	IWql = "SELECT Category FROM Win32_NTLogEvent where Logfile='System' and EventType=" ++ integer_to_list(EventType) ++ 
	" and TimeWritten>='"++ DATE ++ "' and EventCode=" ++ integer_to_list(EventID),
	io:format("~s~n",[TWql]),
	io:format("~s~n",[IWql]),
	Result = wmic:wmic(Host,User,Password,TWql),
	Result1 = wmic:wmic(Host,User,Password,IWql),
	io:format("_____________________result wmi:~p~n",[Result]),
	io:format("_____________________result1 wmi:~p~n",[Result1]),
	TT = filterCount(Result),
	II = filterCount(Result1),
	io:format("_____________________result wmi:TT:~p II:~p~n",[TT,II]),
	{ok, {TT, II}}.

disk(OS, Host, User, Password)->
	Wql = "SELECT DeviceID FROM Win32_LogicalDisk WHERE MediaType=12",
	Result = wmic:wmic(Host,User,Password,Wql),
	case Result of
		{ok, Tables}->
			case hd(Tables) of
				{_, {ok, Rows}}->
					{ok, lists:foldl(fun({{_, Drive}}, Acc)-> Acc ++ binary_to_list(Drive) ++ "$" end, [], Rows)};
				{_, {_, Error2}} ->
					{error, Error2}
			end;
		{_, Error1}->
			{error, Error1}
	end.

disk(OS, Host, User, Password, Disk)->
	Wql = "SELECT FreeSpace, Size FROM Win32_LogicalDisk WHERE MediaType=12 and DeviceID='" ++ Disk ++ "'",
	Result = wmic:wmic(Host,User,Password,Wql),
	case Result of
		{ok, Tables}->
			case hd(Tables) of
				{_, {ok, Rows}}->
					case Rows of
						empty->
							{error, empty};
						[Row]->
							FreeSize = list_to_integer(binary_to_list(element(2, element(2, Row)))),
							Size = list_to_integer(binary_to_list(element(2, element(3, Row)))),
							String = lists:flatten(io_lib:format("percentFull=~.2f$Mbfree=~.2f$TotalSize=~.2f$",
							[(Size - FreeSize) / Size * 100 , FreeSize/1048576, Size/1048576])),
							{ok, String}
						end;
				{_, {_, Error2}} ->
					{error, Error2}
			end;
		{_, Error1}->
			{error, Error1}
	end.

service(OS, Host, User, Password)->
	Wql = "SELECT DisplayName FROM Win32_Service",
	Result = wmic:wmic(Host,User,Password,Wql),
	case Result of
		{ok, [Table]}->
			case Table of
				{_, {ok, Rows}}->
					case Rows of
						empty->
							{error, empty};
						_ ->
							{ok, lists:append(lists:flatmap(fun({{_, X}, _})-> [binary_to_list(X), "$"] end, Rows))}
						end;
				{_, {_, Error1}}->
					{error, Error1}
			end;
		{_, Error2}->
			{error, Error2}
	end.
	
service(OS, Host, User, Password, Service)->
	Wql1 = "SELECT ProcessId, State, Status FROM Win32_Service WHERE DisplayName='" ++ Service ++ "'",
	Result1 = wmic:wmic(Host,User,Password,Wql1),
	case Result1 of
		{ok, [Table1]} ->
			case Table1 of
				{_, {ok, Rows1}}->
					case Rows1 of
						empty->
							{error, empty};
						[Row1]->
							ProcessId = integer_to_list(element(2, element(2, Row1))),
							State = binary_to_list(element(2, element(3, Row1))),
							Status = binary_to_list(element(2, element(4, Row1))),
							case ProcessId of
								"0"->
									{ok, lists:append(["Processes=0$Started=0$ProcessName=NA$", "State=", State, "$", "Status=", Status ,"$"])};
								_ ->
									Wql2 = "SELECT Name FROM Win32_PerfRawData_PerfProc_Process where IDProcess=" ++ ProcessId,
									Result2 = wmic:wmic(Host,User,Password,Wql2),
									case Result2 of
										{ok, [Table2]}->
											case Table2 of
												{_, {ok, Rows2}}->
													case Rows2 of
														empty->
															{error, empty};
														[Row2]->
															ProcessName = binary_to_list(element(2, element(1, Row2))),
															{ok, lists:append(["Processes=1$Started=1$ProcessName=", ProcessName,"$", "State=", State, "$", "Status=", Status ,"$"])}
													end;
												{_, {_, Error4}}->
													{error, Error4}
											end;
										{_, Error3}->
											{error, Error3}
									end
							end
					end;
				{_, {_, Error2}}->
					{error, Error2}
			end;
		{_, Error1}->
			{error, Error1}
	end.
	
process(OS, Host, User, Password)->
	Wql = "SELECT Name FROM Win32_PerfRawData_PerfProc_Process WHERE Name<>'_Total'",
	Result = wmic:wmic(Host,User,Password,Wql),
	case Result of
		{ok, [Table]}->
			case Table of
				{_, {ok, Rows}}->
					{ok, tl(lists:foldl(fun({{_, Process}}, Acc)->Acc ++ "$" ++ binary_to_list(Process) end, [], Rows))};
				{_, {_, Error2}}->
					{error, Error2}
			end;
		{_, Error1}->
			{error, Error1}
	end.

process_process(1, List)->
	lists:foldl(fun(X, Acc)-> Acc + list_to_integer(binary_to_list(element(2, element(2, X)))) end, 0, List);
process_process(2, List)->
	PercentProcessorTime = lists:foldl(fun(X, Acc)-> Acc + list_to_integer(binary_to_list(element(2, element(2, X)))) end, 0, List),
	PrivateBytes = lists:foldl(fun(X, Acc)-> Acc + list_to_integer(binary_to_list(element(2, element(3, X)))) end, 0, List),
	ThreadCount = lists:foldl(fun(X, Acc)-> Acc + element(2, element(4, X)) end, 0, List),
	WorkingSet = lists:foldl(fun(X, Acc)-> Acc + list_to_integer(binary_to_list(element(2, element(5, X)))) end, 0, List),
	Count = length(List),
	{Count, PercentProcessorTime, PrivateBytes, ThreadCount, WorkingSet};
process_process(_, Tuple)->
	Size=size(Tuple),
	case Size of
		2 ->
			TotalVirMem = list_to_integer(binary_to_list(element(2, element(1, Tuple)))),
	        TotalPhyMem = list_to_integer(binary_to_list(element(2, element(2, Tuple))));
		_ ->
			TotalVirMem = list_to_integer(binary_to_list(element(2, element(2, Tuple)))),
			TotalPhyMem = list_to_integer(binary_to_list(element(2, element(3, Tuple))))
	end,
	{TotalVirMem, TotalPhyMem}.

process(OS, Host, User, Password, Process)->
	Wql1 = "SELECT PercentProcessorTime, PrivateBytes, ThreadCount, WorkingSet FROM Win32_PerfRawData_PerfProc_Process WHERE Name='" ++ Process ++ "'",
	Result1 = wmic:wmic(Host,User,Password,Wql1),
	case Result1 of
		{ok, [Table1]} ->
			case Table1 of
				{_, {ok, Rows1}}->
					case Rows1 of
						empty->
							{error, empty};
						_ ->
							sleep(1000),
							Result2 = wmic:wmic(Host,User,Password,Wql1),
							case Result2 of
								{ok, [Table2]} ->
									case Table2 of
										{_, {ok, Rows2}}->
											case Rows2 of
												empty->
													{error, empty};
												_ ->
													sleep(1000),
													Wql2 = "SELECT TotalVirtualMemorySize, TotalVisibleMemorySize FROM CIM_OperatingSystem",
													Result3 = wmic:wmic(Host,User,Password,Wql2),
													case Result3 of
														{ok, [Table3]} ->
															case Table3 of
																{_, {ok, [Row3]}}->
																	PercentProcessorTime1 = process_process(1, Rows1),
																	{Count, PercentProcessorTime2, PrivateBytes, ThreadCount, WorkingSet} = process_process(2, Rows2),
																	{TotalVirMem, TotalPhyMem} = process_process(3, Row3),
																	Temp = PercentProcessorTime2 - PercentProcessorTime1,
																	PercentProcessorTime = if 
																							Temp < 0 -> 0;
																							Temp > 100 -> 100;
																							true -> Temp
																						end,
																	Return = lists:flatten(io_lib:format("ProcessCount=~p$ThreadCount=~p$PercentProcessorTime=~.2f$WorkingSet=~.2f$MemUtilization=~.2f$PrivateBytes=~.2f$VirUtilization=~.2f$",
																										[Count, ThreadCount, PercentProcessorTime / 1, WorkingSet / 1 , WorkingSet/TotalPhyMem*100, PrivateBytes / 1, PrivateBytes/TotalVirMem*100])),
																	{ok, Return};
																{_, {_, Error6}}->
																	{error, Error6}
															end;
														{_, Error5}->
															{error, Error5}
													end
											end;
										{_, {_, Error4}}->
											{error, Error4}
									end;
								{_, Error3}->
									{error, Error3}
							end
					end;
				{_, {_, Error2}}->
					{error, Error2}
			end;
		{_, Error1}->
			{error, Error1}
	end.
	
network(OS, Host, User, Password)->
	Wql = "SELECT Name FROM Win32_PerfRawData_Tcpip_NetworkInterface",
	wmic:wmic(Host,User,Password,Wql).
	
network(OS, Host, User, Password, Network)->
	Wql = "SELECT BytesReceivedPerSec, BytesSentPerSec, Frequency_PerfTime, PacketsOutboundErrors, PacketsReceivedErrors, Timestamp_PerfTime FROM Win32_PerfRawData_Tcpip_NetworkInterface WHERE Name='" ++ Network ++ "'",
	Result1 = wmic:wmic(Host,User,Password,Wql),
	sleep(1000),
	Result2 = wmic:wmic(Host,User,Password,Wql),
	[Result1, Result2].
	

lower([C|R], Acc)-> 
	lower(R, [string:to_lower(C)|Acc]);
lower([], Acc)->Acc.

subMatch(_, _, [])->
    true;
subMatch([$\\|_], _, _)->
	false;
subMatch([C|LRest], Match, [C|RRest])->
	subMatch(LRest, Match, RRest);
subMatch([_|LRest], Match, [_|_])->
    match(LRest, Match);
subMatch( _, _, _)->
    false.

match([$\\|_], _)->
	false;
match([C|LRest], [C|RRest] = Match)->
    subMatch(LRest, Match, RRest);
match([_|LRest], Match)->
    match(LRest, Match);
match([], _)->
    false.

parseFile(File, Match, Count, Size, Min, Max)->
	case Match of
		[]->
			parseFileWithoutMatch(File, Count, Size, Min, Max);
		_ ->
			parseFileWithMatch(File, Match, Count, Size, Min, Max)
	end.

parseFileWithoutMatch({FileSize, LastModified, _}, Count, Size, Min, Max)->
	Time = lists:sublist(LastModified, 1, 14),
	NewMax = if
				Time > Max->Time;
				true->Max
			end,
	NewMin = if
				Time < Min->Time;
				true->Min
			end,
	NewSize = Size + list_to_integer(FileSize),
	{Count+1, NewSize, NewMin, NewMax}.

parseFileWithMatch({FileSize, LastModified, Name}, Match, Count, Size, Min, Max)->
	case match(lists:reverse(Name), Match) of
		true->
			Time = lists:sublist(LastModified, 1, 14),
			NewMax = if
						Time > Max->Time;
						true->Max
					end,
			NewMin = if
						Time < Min->Time;
						true->Min
					end,
			NewSize = Size + list_to_integer(FileSize),
			{Count+1, NewSize, NewMin, NewMax};
		_ ->
			{Count, Size, Min, Max}
	end.
	
count(Files, Match)->
	count(Files, lower(Match, []), 0, 0, "21000101000000", "0").

count([], _, Count, Size, Min, Max)->
	String = lists:flatten(io_lib:format("count=~p$size=~p$min=~s$max=~s",[Count, Size, Min, Max])),
	{ok, String};
count([File|Rest], Match, Count, Size, Min, Max)->
	FileName = binary_to_list(element(1, File)),
	KVList = element(2, File),
	{_, FileSize} = lists:keyfind('FileSize', 1, KVList),
	{_, LastModified} = lists:keyfind('LastModified', 1, KVList),
	NewFile = {binary_to_list(FileSize), binary_to_list(LastModified), FileName},
	{NewCount, NewSize, NewMin, NewMax} = parseFile(NewFile, Match, Count, Size, Min, Max),
	count(Rest, Match, NewCount, NewSize, NewMin, NewMax).

directory(OS, Host, User, Password, Path, Recursive, Match)->
	Result = case Recursive of
		"true"->
			subdirectory(Host, User, Password, Path);
		_ ->
			directory(Host, User, Password, Path)
	end,
	case Result of
		{ok, Files}->
			count(Files, Match);
		{_, Error}->
			{error, Error}
	end.
