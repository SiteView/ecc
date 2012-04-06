-module(df_log).

-compile(export_all).


start() ->
    Pid = openLog(),
    S = "helleworld",
    S1 = "my name is ~s~n",
    writeLog(Pid,S),
    writeLog(Pid,S1,["luoyu"]),
    closeLog(Pid).
    
    
openLog() ->
    {ok,F} = file:open("log/scanlog.txt",write),
    spawn_link(fun() -> writeLog(F) end).

writeLog(F) ->
    receive
        {ok,S,N} -> 
            io:format(F,S,N),
            writeLog(F);
        {ok,S}   ->
            io:format(F,"~s~n",[S]),
            writeLog(F);
        log_over   -> file:close(F);
        _      -> writeLog(F)
    end.

writeLog(Pid,S) ->
    Pid!{ok,S}.
    
writeLog(Pid,S,N) ->
    Pid!{ok,S,N}.
    
closeLog(Pid) ->
    Pid!log_over.




























