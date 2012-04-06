%
%sshcommandline.erl
%author:lei.lin@dragonflow.com
%

-module(sshcommandline).
-compile(export_all).
-include("monitor.hrl").

getMethodName() ->
    "ssh".
	
getMethodDisplayName() ->
    "SSH".

	
%return string or error	
exec(Cmd,Machine) ->
    exec(Cmd,Machine,true).

exec(Cmd,_Machine,Flag) ->
    SSHCmd = getsshcommand(_Machine),
	os:cmd(SSHCmd ++ " " ++ Cmd).

	
	

getsshcommand(Machine) ->
    if Machine /= "" ->
        case  platform:isWindows() of
	        true ->
                   case Machine#machine.keyfile of
                    "" ->
                        "tools\\plink.exe -ssh " ++ Machine#machine.login++"@"++Machine#machine.host++" -P "++integer_to_list(Machine#machine.sshport) ++ " -pw " ++ Machine#machine.passwd;				
                     _ ->
				    %"tools\\plink -ssh " ++ Machine#machine.login++"@"++Machine#machine.host++" -P "++integer_to_list(Machine#machine.sshport) ++ " -pw " ++ Machine#machine.passwd ++" -i "++Machine#machine.keyfile
                        "tools\\plink.exe -ssh " ++ Machine#machine.login++"@"++Machine#machine.host++" -P "++integer_to_list(Machine#machine.sshport) ++ " -pw " ++ Machine#machine.passwd            
		           end;
             _ ->
                 "ssh  " ++ Machine#machine.login++"@"++Machine#machine.host
        end;
    true ->
        ""    
    end.
    
    
getExitValue() ->
    0.


