-module(siteview_commandline).
-compile(export_all).
-include("monitor.hrl").

exec(Host,Command) ->
    Localhost = dbcs_base:get_app(),  
   % io:format("Localhost:~p~n",[Localhost]),
    {CmdSta,CmdObj} = commandLine:get(Host,Command,Localhost),        
    %io:format("&&&&&CmdSta:~p~nCmdObj:~p~n",[CmdSta,CmdObj]),
    case CmdSta of
    ok ->                                                                            
        {Status,OutputListTemp} = CmdObj:exec(),
        commandLine:remove(Host),
        case Status of
        ok ->
            {ok,OutputListTemp};  
        _ ->
            {error,OutputListTemp}            
        end;
    _ ->
        {error,CmdObj}                                  
   end.   


