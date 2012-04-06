-module(log).
-compile(export_all).

-include("log.hrl").

%{_,{_,F,_}} = process_info(self(),current_function))

writelog(Log) ->
    case file:open("run.log", [append]) of
        {ok, File} ->
            io:format(File, "~s~n", [Log]),
            file:close(File);
         _ -> ok
    end.	
	
	
log(X,_module,_line)->
    _msg = "["++atom_to_list(_module)++"]("++integer_to_list(_line)++")=>",
    case X of        
        {Format, Parametes}->
            io:format(_msg++Format, Parametes),
            write_log(getcurrenttime()++_msg++Format, Parametes);
        _ -> 
            io:format(_msg++"~p~n", [X]),
            write_log(getcurrenttime()++_msg++"~p~n", [X])
     end.
     
log(Format,Parametes,_module,_line)->
    _msg = "["++atom_to_list(_module)++"]("++integer_to_list(_line)++")=>",
    io:format(_msg++Format, Parametes),
    write_log(getcurrenttime()++_msg++Format, Parametes).


write_log(Format,Parametes) ->
   case file:open("run.log", [append]) of
        {ok, File} ->
            io:format(File, Format, Parametes),
            file:close(File);
         _ -> ok
    end.
getcurrenttime() ->    
   case calendar:local_time() of
       {{_,_,_},{H,M,S}} -> lists:flatten(io_lib:format("[~2..0w:~2..0w:~2..0w]",[H, M, S]));
       _ -> []
   end.   