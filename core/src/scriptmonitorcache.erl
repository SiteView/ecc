%
%scriptmonitorcache.erl
%author:lei.lin@dragonflow.com
%

-module(scriptmonitorcache,[ScriptServerName,Script,CacheLifeTime,Tid]).
-compile(export_all).
-include("monitor.hrl").
-include_lib("kernel/include/file.hrl").

start() ->
    scriptMonitorCache(ScriptServerName,Script,CacheLifeTime).

scriptMonitorCache(ScriptServerName,Script,CacheLifeTime) -> 
    case platform:isWindows() of
	true ->
        scriptMonitorCache(ScriptServerName,Script,"cache" ++ "\\" ++ "scripts",CacheLifeTime);
	_ ->
        scriptMonitorCache(ScriptServerName,Script,"cache" ++ "/" ++ "scripts",CacheLifeTime)
    end.

scriptMonitorCache(ScriptServerName,Script,CacheFilePathPrefix,CacheLifeTime) ->
	case  platform:isWindows() of
	true ->
        CacheFilePath = CacheFilePathPrefix ++ "\\" ++ ScriptServerName ++ "." ++ Script;
	_ ->
        CacheFilePath = CacheFilePathPrefix ++ "/" ++ ScriptServerName ++ "." ++ Script
    end,
    %{FileStatu,IoDev} = file:open(CacheFilePath,[]),
    ets:insert_new(Tid,{f,CacheFilePath}),
    ets:insert_new(Tid,{exitValue,-1}), 
    ets:insert_new(Tid,{output,[]}),
    ets:insert_new(Tid,{cacheLifeTime,CacheLifeTime}), 
    init().    
	
init() ->
    load().



isFresh() ->
    [{_,CacheFilePath}] = ets:lookup(Tid,f),
    [{_,CacheLifeTime}] = ets:lookup(Tid,cacheLifeTime),     
    {FileState,FileInfo} = file:read_file_info(CacheFilePath),
    case FileState of
    ok ->
        io:format("erlang:localtime():~p~n",[erlang:localtime()]),
        io:format("FileInfo#file_info.mtime:~p~n",[FileInfo#file_info.mtime]),
         io:format("CacheLifeTime:~p~n",[CacheLifeTime]),          
        timer:seconds(calendar:datetime_to_gregorian_seconds(erlang:localtime()))  < timer:seconds(calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime)) + CacheLifeTime *1000;    
    _ ->
        false 
    end.

load() ->
    ets:insert_new(Tid,{output,[]}),
    [{_,CacheFilePath}] = ets:lookup(Tid,f),
    {State,FileInfo} = file:read_file_info(CacheFilePath),
    case State of
    ok ->
        if FileInfo#file_info.size > 0 ->
            FileDate = file:read_file(CacheFilePath),             
            case FileDate of
            {ok,Date} ->
                FileString = binary_to_list(Date),
                FileList = string:tokens(FileString,"\r\n");
            _ ->
                FileList = null
            end;
        true  ->
            FileList = null
        end;
    _ ->
        FileList = null
    end,
    if FileList == null->        
        [];
    true ->
        if length(FileList) == 0 ->
            [];
        true ->        
            [FileListValue] = lists:sublist(FileList,1,1),        
            ExitValue = list_to_integer(FileListValue),
            ets:insert_new(Tid,{exitValue,ExitValue}),      
            ets:insert_new(Tid,{output,FileList}),
            FileList
        end            
    end.
            

update(Count,FileList) ->
    OutputHeadLine = integer_to_list(Count)  ++ "\n",
    if FileList == null ->
        nothing;
    true ->
        StringBuff = OutputHeadLine ++ update_util(FileList),
        [{_,CacheFilePath}] = ets:lookup(Tid,f),
        io:format("cache CacheFilePath:~p~n",[CacheFilePath]),
        case filelib:ensure_dir(CacheFilePath) of
        ok ->
            is_lock(CacheFilePath),
            {State,Fid} = file:open(CacheFilePath,[write]),
            file:write(Fid,list_to_binary(StringBuff)),
            file:close(Fid),            
            release_lock(CacheFilePath),        
            ets:insert_new(Tid,{exitValue,Count}),
            ets:insert_new(Tid,{output,FileList});
        _ ->
            nothing 
        end 
    end.
    	
update_util(List) ->
    update_util_t(List,length(List),"").
update_util_t(_List,0,R) -> R;
update_util_t(Li,Num,Re) ->
    [A|B] = Li,
	update_util_t(B,Num-1,string:concat(Re,A ++ "\n")).
    	
getLastModDate() ->
    [{_,CacheFilePath}] = ets:lookup(Tid,f),    
    {FileState,FileInfo} = file:read_file_info(CacheFilePath),
    case FileState of
    ok ->
        timer:seconds(calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime));
    _ ->
        timer:seconds(calendar:datetime_to_gregorian_seconds(erlang:localtime()))  
    end.

getCacheLifeTime() ->
    [{_,CacheLifeTime}] = ets:lookup(Tid,cacheLifeTime),
    CacheLifeTime.

getExitValue() ->
    [{_,ExitValue}] = ets:lookup(Tid,exitValue),
    ExitValue.

getOutput() ->
    [{_,Output}] = ets:lookup(Tid,output),
    Output.

is_lock(Fidid) ->
	global:set_lock({Fidid, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock(Fidid) ->	
	global:del_lock({Fidid, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).
          	