-module(erlwmi).
-compile(export_all).


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

subdirectory(Server,UserName,PassWord,Directory) ->
    try trysubdirectory(Server,UserName,PassWord,Directory) of
	Result -> {ok,Result}			   
    catch
        error:X -> {error,X}
    end.
    
trysubdirectory(Server,UserName,PassWord,Directory) ->
   Directorylist = subdirectorylist(Server,UserName,PassWord,[Directory],[Directory]),
   io:format("Directorylist:~p~n",[Directorylist]),
   filelist(Server,UserName,PassWord,Directorylist,[]).
   
filelist(Server,UserName,PassWord,[Directory|R],Acc) ->
    try directory(Server,UserName,PassWord,Directory) of
	{ok,Result} -> filelist(Server,UserName,PassWord,R,Result++Acc);	 
	_ -> filelist(Server,UserName,PassWord,R,Acc)
    catch
        _:_ -> filelist(Server,UserName,PassWord,R,Acc)
    end;
filelist(_,_,_,[],Acc) -> Acc.  
    
subdirectorylist(Server,UserName,PassWord,[Directory|R],Acc) ->
   case string:str(Directory,":") of	 
	 2 when length(Directory) =:= 2 ->	  
	    Wql = lists:flatten(["SELECT * FROM Win32_Directory where Drive= '",Directory,"' and Path = '\\\\'"]), 
	    Directorys = wmidirectorylist(Server,UserName,PassWord,Wql),
	    Sublist = subdirectorylist(Server,UserName,PassWord,Directorys,[]),
	    subdirectorylist(Server,UserName,PassWord,R,Acc++Directorys++Sublist); 
	 2 -> 
	    [Driver,Path] = string:tokens(Directory,":"),
	    Wql = case  lists:suffix("\\",Path) of
	          true -> lists:flatten(["SELECT * FROM Win32_Directory where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"'"]);
		  _ -> lists:flatten(["SELECT * FROM Win32_Directory where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"\\\\'"])
	    end,
	    Directorys = wmidirectorylist(Server,UserName,PassWord,Wql),
	    Sublist = subdirectorylist(Server,UserName,PassWord,Directorys,[]),
	    subdirectorylist(Server,UserName,PassWord,R,Acc++Directorys++Sublist); 
	 _ -> subdirectorylist(Server,UserName,PassWord,R,Acc)
   end;
subdirectorylist(_,_,_,[],Acc) -> Acc.   

wmidirectorylist(Server,UserName,PassWord,Wql) ->   
  %% io:format("service:~p~n",[Wql]),
   case  wmic:wmic(Server,UserName,PassWord,Wql) of
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
	 _ -> []
   end.   

directory(Server,UserName,PassWord,Directory) ->
   case string:str(Directory,":") of	 
	 2 when length(Directory) =:= 2 ->	  
	    Wql = lists:flatten(["SELECT * FROM CIM_DataFile where Drive= '",Directory,"' and Path = '\\\\'"]), 
	    wmidirectory(Server,UserName,PassWord,Wql);
	 2 -> 
	    [Driver,Path] = string:tokens(Directory,":"),
	    Wql = case  lists:suffix("\\",Path) of
	          true -> lists:flatten(["SELECT * FROM CIM_DataFile where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"'"]);
		  _ -> lists:flatten(["SELECT * FROM CIM_DataFile where Drive= '",Driver,":' and Path = '",replace(Path,"\\","\\\\"),"\\\\'"])
	    end,
	    wmidirectory(Server,UserName,PassWord,Wql);
	 _ -> {error,"Directory param is error!"}
   end.
   
wmidirectory(Server,UserName,PassWord,Wql) ->   
   %%io:format("service:~p~n",[Wql]),
   case  wmic:wmic(Server,UserName,PassWord,Wql) of
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

service(Server,UserName,PassWord) ->
   CpuWql = "SELECT * FROM Win32_Service",
   case  wmic:wmic(Server,UserName,PassWord,CpuWql) of
         {ok,WmiData} ->
	        case  lists:keysearch(CpuWql, 1, WmiData) of
		  {value,{_,{ok,empty}}} -> {error,[]};
		  {value,{_,{ok,Values}}} ->
			  %%io:format("service:~p~n",[Values]),
			  try analysisservice(Values) of
				Result -> {ok,Result}			   
			  catch
			     error:X-> {error,X}
			  end; 
		  ErrorResult -> {error,ErrorResult}
		end;
	 ErrorResult -> ErrorResult
   end.
    
analysisservice(Values) ->
	Fun = fun(X) -> 
		Lists = tuple_to_list(X),
		%%io:format("service:~p~n",[Lists]),
		
		{value,{_,DisplayName}} =  lists:keysearch('DisplayName', 1, Lists),
		{value,{_,ProcessId}} =  lists:keysearch('ProcessId', 1, Lists),		
		{value,{_,PathName}} =  lists:keysearch('PathName', 1, Lists),
		{value,{_,State}} =  lists:keysearch('State', 1, Lists),		
		{value,{_,StartMode}} =  lists:keysearch('StartMode', 1, Lists),
		
		{DisplayName,[{'ProcessId',ProcessId},
			   {'PathName',PathName},
			   {'State',State},			 		  
			   {'StartMode',StartMode}]}
		end,
        lists:map(Fun,Values).
   
process(Server,UserName,PassWord) ->
   CpuWql = "SELECT * FROM Win32_PerfRawData_PerfProc_Process where Name<>'_Total'",
   case  wmic:wmic(Server,UserName,PassWord,CpuWql) of
         {ok,WmiData} ->
	        case  lists:keysearch(CpuWql, 1, WmiData) of
		  {value,{_,{ok,empty}}} -> {error,[]};
		  {value,{_,{ok,Values}}} ->
			  %%io:format("process:~p~n",[Values]),
			  try analysisprocess(Values) of
				Result -> {ok,Result}			   
			  catch
			     error:X-> {error,X}
			  end; 
		  ErrorResult -> {error,ErrorResult}
		end;
	 ErrorResult -> ErrorResult
   end.
    
analysisprocess(Values) ->
	Fun = fun(X) -> 
		Lists = tuple_to_list(X),
		
		{value,{_,Name}} =  lists:keysearch('Name', 1, Lists),
		{value,{_,IDProcess}} =  lists:keysearch('IDProcess', 1, Lists),		
		{value,{_,ThreadCount}} =  lists:keysearch('ThreadCount', 1, Lists),
		{value,{_,HandleCount}} =  lists:keysearch('HandleCount', 1, Lists),
		{value,{_,CreatingProcessID}} =  lists:keysearch('CreatingProcessID', 1, Lists),
		{value,{_,WorkingSet}} =  lists:keysearch('WorkingSet', 1, Lists),
		{value,{_,WorkingSetPeak}} =  lists:keysearch('WorkingSetPeak', 1, Lists),
		{value,{_,IOWriteBytesPersec}} =  lists:keysearch('IOWriteBytesPersec', 1, Lists),
		{value,{_,IOReadBytesPersec}} =  lists:keysearch('IOReadBytesPersec', 1, Lists),
		
		{Name,[{'IDProcess',IDProcess},
			   {'ThreadCount',ThreadCount},
			   {'HandleCount',HandleCount},
			   {'CreatingProcessID',CreatingProcessID},
			   {'WorkingSet',list_to_integer(binary_to_list(WorkingSet))},
			   {'WorkingSetPeak',list_to_integer(binary_to_list(WorkingSetPeak))},
			   {'IOWriteBytesPersec',list_to_integer(binary_to_list(IOWriteBytesPersec))},
			   {'IOReadBytesPersec',list_to_integer(binary_to_list(IOReadBytesPersec))}]}
		end,
        lists:map(Fun,Values).
	
memory(Server,UserName,PassWord) ->
   CpuWql = "SELECT * FROM CIM_OperatingSystem",
   case  wmic:wmic(Server,UserName,PassWord,CpuWql) of
         {ok,WmiData} ->
	        case  lists:keysearch(CpuWql, 1, WmiData) of
		  {value,{_,{ok,empty}}} -> {error,[]};
		  {value,{_,{ok,Values}}} ->
			  %%io:format("memory:~p~n",[Values]),
			  try analysismemory(Values) of
				Result -> {ok,Result}			   
			  catch
			     error:X-> {error,X}
			  end; 
		  ErrorResult -> {error,ErrorResult}
		end;
	 ErrorResult -> ErrorResult
   end.
   
analysismemory(Values) -> 
	Lists = tuple_to_list(hd(Values)),
	{value,{_,TotalVisibleMemorySize}} =  lists:keysearch('TotalVisibleMemorySize', 1, Lists),		
	{value,{_,TotalVirtualMemorySize}} =  lists:keysearch('TotalVirtualMemorySize', 1, Lists),
	{value,{_,FreePhysicalMemorySize}} =  lists:keysearch('FreePhysicalMemory', 1, Lists),
	{value,{_,FreeVirtualMemorySize}} =  lists:keysearch('FreeVirtualMemory', 1, Lists),
	
	TotalVisibleMemory  = list_to_integer(binary_to_list(TotalVisibleMemorySize)),
	TotalVirtualMemory  = list_to_integer(binary_to_list(TotalVirtualMemorySize)),
	FreePhysicalMemory  = list_to_integer(binary_to_list(FreePhysicalMemorySize)),
	FreeVirtualMemory  = list_to_integer(binary_to_list(FreeVirtualMemorySize)),
	
	Used = makefloat(100-calculation(FreeVirtualMemory,TotalVirtualMemory),2),
		
	[{'used',Used},
	{'totalphysicalmemory',TotalVisibleMemory},
	{'totalvirtualmemory',TotalVirtualMemory},
	{'freephysicalmemory',FreePhysicalMemory},
	{'freevirtualmemory',FreeVirtualMemory}].
   
disk(Server,UserName,PassWord) ->
   CpuWql = "SELECT * FROM Win32_LogicalDisk where MediaType=12",
   case  wmic:wmic(Server,UserName,PassWord,CpuWql) of
         {ok,WmiData} ->
	        case  lists:keysearch(CpuWql, 1, WmiData) of
		  {value,{_,{ok,empty}}} -> {error,[]};
		  {value,{_,{ok,Values}}} ->
			  %%io:format("enumdisksinfo:~p~n",[Values]),
			  try analysisdisks(Values) of
				Result -> {ok,Result}			   
			  catch
			     error:X-> {error,X}
			  end; 
		  ErrorResult -> {error,ErrorResult}
		end;
	 ErrorResult -> ErrorResult
   end.
   
analysisdisks(Values) ->
	Fun = fun(X) -> 
		Lists = tuple_to_list(X),
		{value,{_,DeviceID}} =  lists:keysearch('DeviceID', 1, Lists),		
		{value,{_,Size}} =  lists:keysearch('Size', 1, Lists),
		{value,{_,FreeSpace}} =  lists:keysearch('FreeSpace', 1, Lists),
		DiskSize  = makefloat(list_to_integer(binary_to_list(Size))/(1024*1024*1024),3),
		DiskFreeSpace = makefloat(list_to_integer(binary_to_list(FreeSpace))/(1024*1024*1024),3),
		Percent = makefloat(100-calculation(DiskFreeSpace,DiskSize),2),
		{DeviceID,[{'used',Percent},
			   {'size',DiskSize},
			   {'freespace',DiskFreeSpace}]}
		end,
        lists:map(Fun,Values).
	
	
	
cpu(Server,UserName,PassWord) ->
     try getcpu(Server,UserName,PassWord) of
	 CpuData1 ->	     
	     sleep(2000),
	      try getcpu(Server,UserName,PassWord) of
		  CpuData2 ->	
			try analysiscpu(CpuData1,CpuData2,[]) of
			     Result ->	{ok,Result}			   
		        catch
			     error:X-> {error,X}
		        end   
	      catch
		 error:X->  {error,X}
	     end
     catch
         error:X-> {error,X}
     end.
     
     
calculation(_,Y) when Y =:= 0 -> 100;
calculation(X,Y) -> 
    case X * 100 / Y of
         Result when Result < 0 -> 0;
	 Result when Result > 100 -> 100;
	 Result -> Result
    end.


analysiscpu([{Name,PercentProcessorTime1,PercentPrivilegedTime1,PercentInterruptTime1,PercentDPCTime1,Timestamp_Sys100NS1}|R],CpuData2,Acc) when is_binary(PercentProcessorTime1) andalso is_binary(Timestamp_Sys100NS1) -> 
	{value,{_,PercentProcessorTime2,PercentPrivilegedTime2,PercentInterruptTime2,PercentDPCTime2,Timestamp_Sys100NS2}} =  lists:keysearch(Name, 1, CpuData2),
	U = list_to_integer(binary_to_list(PercentProcessorTime2)) - list_to_integer(binary_to_list(PercentProcessorTime1)),
	P = list_to_integer(binary_to_list(PercentPrivilegedTime2)) - list_to_integer(binary_to_list(PercentPrivilegedTime1)),
	I = list_to_integer(binary_to_list(PercentInterruptTime2)) - list_to_integer(binary_to_list(PercentInterruptTime1)),
	Dpc = list_to_integer(binary_to_list(PercentDPCTime2)) - list_to_integer(binary_to_list(PercentDPCTime1)),
	D = list_to_integer(binary_to_list(Timestamp_Sys100NS2)) - list_to_integer(binary_to_list(Timestamp_Sys100NS1)),
	%%PercentProcessorTime = 100 - ( U * 100 / D) ,
	PercentProcessorTime = 100 - calculation(U,D),	
	PercentPrivilegedTime =  calculation(P,D),
	PercentInterruptTime =  calculation(I,D),
	PercentDPCTime =  calculation(Dpc,D),
	analysiscpu(R,CpuData2,[{Name,[{'PercentProcessorTime',makefloat(PercentProcessorTime,2)},
				       {'PercentPrivilegedTime',makefloat(PercentPrivilegedTime,2)},
				       {'PercentInterruptTime',makefloat(PercentInterruptTime,2)},
				       {'PercentDPCTime',makefloat(PercentDPCTime,2)}]}|Acc]);
	
analysiscpu([_|R],CpuData2,Acc)  -> analysiscpu(R,CpuData2,Acc);

analysiscpu([],_,Acc)  -> Acc.    


getcpu(Server,UserName,PassWord) ->
   CpuWql = "SELECT * FROM Win32_PerfRawData_PerfOS_Processor",
   {ok,WmiData}  = wmic:wmic(Server,UserName,PassWord,CpuWql),
   {value,{_,{ok,Values}}} =  lists:keysearch(CpuWql, 1, WmiData),
   Fun = fun(X) -> 
	Lists = tuple_to_list(X),
	{value,{_,Name}} =  lists:keysearch('Name', 1, Lists),
	{value,{_,PercentProcessorTime}} =  lists:keysearch('PercentProcessorTime', 1, Lists),
	{value,{_,PercentPrivilegedTime}} =  lists:keysearch('PercentPrivilegedTime', 1, Lists),
	{value,{_,PercentInterruptTime}} =  lists:keysearch('PercentInterruptTime', 1, Lists),
	%%{value,{_,PercentIdleTime}} =  lists:keysearch('PercentIdleTime', 1, Lists),
	{value,{_,PercentDPCTime}} =  lists:keysearch('PercentDPCTime', 1, Lists),
	{value,{_,Timestamp_Sys100NS}} =  lists:keysearch('Timestamp_Sys100NS', 1, Lists),	

	{Name,PercentProcessorTime,PercentPrivilegedTime,PercentInterruptTime,PercentDPCTime,Timestamp_Sys100NS}
	end,
   lists:map(Fun,Values).
   
makefloat(Number, X) ->
    N = math:pow(10,X),
    round(Number*N)/N. 
   
sleep(Time) ->
    receive 
	after Time ->
            ok
    end.
   


	