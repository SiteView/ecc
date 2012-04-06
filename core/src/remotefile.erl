%
%remotefile.erl
%author:lei.lin@dragonflow.com
%

-module(remotefile).
-compile(export_all).
-include("monitor.hrl").

remotefile(Host,Path) ->
    Bool = machine:isNTSSH(Host),
    if Bool ->
        "";
    true ->
        MachineT = machine:getMachine(Host),
        if MachineT /= [] ->
            [Machine] = MachineT, 
            Osnum = machine:getOS(Host),
            Bool1 = (Osnum == 1),
            if Bool1 ->
                Bool2 = string:str(Path,":\\"),
                if Bool2 ->
                    Fullpath = Path;
                true ->
                    Fullpath = ""
                end;
            true ->
                Bool3 = (Osnum /= 1),
                Bool4 = (string:str(Path,"/") == 1),
                if Bool3,Bool4 ->
                    Fullpath = Path;
                true ->
                    Fullpath = ""
                end               
            end,
            FullpathLen = length(Fullpath),
            if FullpathLen == 0 ->
                CuDirCmd = platform:currentDirectoryCommand(Host),
                case Machine#machine.os of
                "nt" ->
                    "";
                _ ->            
                    %Res = sshcommandline:exec(CuDirCmd,Machine,true),
                    case commandLine:get(Host,CuDirCmd) of
                    {ok,CmdObj} ->                   
                        {Status,Res} = CmdObj:exec(),
                        commandLine:remove(Host),                   
                        case Status of
                        ok ->
                            if Res /= [] ->      
                                [PwdT] = findStartOfUnixPwdResultDirectoryPath(Machine,lists:sublist(Res,1,1),Osnum),
                                Bool5 = (string:rstr(PwdT,platform:pathSeparator(Osnum)) == length(PwdT)),
                                if Bool5 ->
                                    Pwd = PwdT;
                                true ->
                                    Pwd = PwdT ++ platform:pathSeparator(Osnum)
                                end,
                                FullPa = Pwd ++ Path;
                            true ->
                                FullPa =  []
                            end;
                        _ ->
                            ""  
                        end; 
                    _ ->
                        "" 
                    end 
                end;                    
            true ->
                FullPa =  Fullpath
            end;            
        true ->
            ""
        end    
    end.                                                       

getFullPath(Host,Path) -> 
   remotefile(Host,Path).


findStartOfUnixPwdResultDirectoryPath(Machie,Pwd,Osnum) ->
    Int = string:str(Pwd," /"),
    Bool = (Osnum /= 1), 
    PwdLen = length(Pwd),
    if Bool , Int > 0 ,Int < PwdLen ->
        string:substr(Pwd,Int+1);
    true ->
        Pwd
    end.        

%return List 
listFiles(Host,Path,Osnum) ->
    case Host of
    "" ->
        [];
    _ ->    
        FullPa = remotefile(Host,Path),
        Dir = platform:dirCommand(Osnum,2),
        FullPathT = Dir ++ " " ++ FullPa,
        Bool1 = machine:isNTSSH(Host),
        if Bool1 ->
            % I have no idea
            %FullPath = "directory.bat",
            %Machine = machine:getNTMachine(Host),
            [];
        true ->
            [Machine] = machine:getMachine(Host),
            %Res = sshcommandline:exec(FullPathT,Machine,true),
            case commandLine:get(Host,FullPathT) of
                {ok,CmdObj} ->                   
                    {Status,Res} = CmdObj:exec(),
                    commandLine:remove(Host),                   
                    case Status of
                    ok ->                  
                        listFiles_util(Res);
                    _ ->
                        []
                    end;
                 _ ->
                    []                  
                end                     
        end
    end.        
        


listFiles_util(List) ->
    listFiles_util_t(List,length(List),[]).
listFiles_util_t(_L,0,R) -> R;
listFiles_util_t(L,N,Re) ->
    [A|B] = L,
    Bool = (string:rstr(A,"*") == length(A)),
    if Bool ->
        ResS = string:substr(A,1,length(A)-1);
    true ->
        ResS = A
    end,
    Bool2 = (string:rstr(ResS,"/") /= length(A)),
    if Bool2 ->
        listFiles_util_t(B,N-1,lists:append(Re,[ResS]));
    true ->
        listFiles_util_t(B,N-1,Re)
    end.    
    
    