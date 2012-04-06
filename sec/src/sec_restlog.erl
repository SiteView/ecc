-module(sec_restlog).
-compile(export_all).

-include("sec_log.hrl").

%{_,{_,F,_}} = process_info(self(),current_function))

writelog(Log,_module,_line,Message) ->
    case file:open(Log, [append]) of
        {ok, File} ->
            io:format(File, "~p[~p]:~p~n", [_module,_line,Message]),
            file:close(File);
         _ -> ok
    end.

writelog(Log) ->
    case file:open("run.log", [append]) of
        {ok, File} ->
            io:format(File, "~s~n", [Log]),
            file:close(File);
         _ -> ok
    end.	
	
	
log(X,_module,_line)->
    case application:get_env(esyslog, log) of
         {ok,true} ->
            _msg = "["++atom_to_list(_module)++"]("++integer_to_list(_line)++")=>",
            case X of        
                {Format, Parametes}->
                    io:format(_msg++Format, Parametes),
                    write_log(getcurrenttime()++_msg++Format, Parametes);
                _ -> 
                    io:format(_msg++"~p~n", [X]),
                    write_log(getcurrenttime()++_msg++"~p~n", [X])
             end;
        undefined -> 
             _msg = "["++atom_to_list(_module)++"]("++integer_to_list(_line)++")=>",
             case X of        
                {Format, Parametes}->
                    io:format(_msg++Format, Parametes);
                _ -> 
                    io:format(_msg++"~p~n", [X])
             end;
        _ -> ok
    end.
     
log(Format,Parametes,_module,_line)->
    case application:get_env(esyslog, log) of
         {ok,true} ->
            _msg = "["++atom_to_list(_module)++"]("++integer_to_list(_line)++")=>",
            io:format(_msg++Format, Parametes),
            write_log(getcurrenttime()++_msg++Format, Parametes);
           undefined -> 
            _msg = "["++atom_to_list(_module)++"]("++integer_to_list(_line)++")=>",
            io:format(_msg++Format, Parametes);
         _ -> ok
    end.

write_log(Format,Parametes) ->
   case file:open("run.log", [append]) of
        {ok, File} ->
            io:format(File, Format, Parametes),
            file:close(File);
         _ -> ok
    end.
getcurrenttime() ->    
   case calendar:local_time() of
       {{_,_,_},{H,S,M}} -> lists:flatten(io_lib:format("[~2..0w:~2..0w:~2..0w]",[H, M, S]));
       _ -> []
   end.   