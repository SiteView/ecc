%%
%% Directory Monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2009 siteview
%% @version 1.0
%% @doc directory monitor

-module(directory_monitor,[BASE]).
-compile(export_all).
-extends(server_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("kernel/include/file.hrl").

%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for directory monitor
new() ->
    Base = server_monitor:new(),
    {?MODULE,Base}.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->
    THIS:set_attribute(age, "n/a"),
    THIS:set_attribute(size, "n/a"),
    THIS:set_attribute(fileCount, "n/a"),
    THIS:set_attribute(permitted, "n/a"),
    THIS:set_attribute(exists, "n/a"),
    {ok,{_,Host}} = THIS:get_property(machine),
    {ok,{_,DirPath}} = THIS:get_property(path),
    {ok,{_,NoRecurse}} = THIS:get_property(noRecurse), 
    {ok,{_,Match}} = THIS:get_property(match),
    LocalCode = platform:getLocalCode(),
    if Host /= [] ->
        [Machine|_] = machine:getMachine(Host),
        RemoteCode = Machine#machine.remoteencoding;
    true ->
        Machine = "" ,
        RemoteCode = LocalCode 
    end, 
    OsNum = machine:getOS(Host),
    if OsNum == 1 ->
	Path = iconv:convert("utf-8", RemoteCode, DirPath),  
	{Flag, Flag2,Tcount, Tsize, Tage, First} = if Machine == "" ->   
		proxy:directory(OsNum, "127.0.0.1", " ", " ", Path,NoRecurse,Match);
        true ->
		proxy:directory(OsNum, Machine#machine.host, Machine#machine.login, Machine#machine.passwd, Path,NoRecurse,Match)
        end,
        
        %if Host == "" ->               
        %    TPath = DirPath;
        %true ->
        %    TPath =  Host ++ "\\" ++  DirPath
        %end,
        %Path = iconv:convert("utf-8",LocalCode,TPath),      
	%io:format("**********Path->~p~n", [Path]),
        %{Status,_Reu} = file:list_dir(Path),     
        %case Status of
        %ok ->
        %    Flag = true;
        %_ ->
        %    Flag = false 
        %end,                      
        %if Flag ->               
        %    {Flag2,Tage,Tsize,Tcount,First} = countFiles(Path,NoRecurse,Match,-1),
	%    io:format("**********{Flag2,Tage,Tsize,Tcount,First}->~p~n", [{Flag2,Tage,Tsize,Tcount,First}]),
	%     {Flag2,Tage,Tsize,Tcount,First};
        %true ->
         %   Flag2 = true,
         %   Tage = 0,
         %   First = 0,
         %   Tsize = 0,
         %   Tcount = 0            
        %end,
		%io:format("{Flag, Flag2,Tcount, Tsize, Tage, First}->~p~n", [{Flag, Flag2,Tcount, Tsize, Tage, First}]),
        if Flag,Flag2 ->
            THIS:set_attribute(lastmofified,Tage),
            THIS:set_attribute(firstmofified,First),
            THIS:set_attribute(exists,"exists"),
            THIS:set_attribute(permitted,"permitted"), 
            THIS:set_attribute(size,Tsize),
            THIS:set_attribute(fileCount,Tcount),
			THIS:set_attribute(?STATE_STRING,integer_to_list(round(Tcount)) ++ " files, " ++ "size:" ++integer_to_list(round(Tsize / 1024)) ++"K"++",last time modified: " ++ Tage ++ ",first time modified: " ++First);
%%             THIS:set_attribute(?STATE_STRING,integer_to_list(round(Tcount)) ++ " files, " ++ "size:" ++integer_to_list(round(Tsize / 1024)) ++"K"++",last time since modified: " ++ integer_to_list(round(Tage)) ++ " min," ++ "first time since modified: " ++ integer_to_list(round(First)) ++ " min.");
        true ->        
            if Flag == false ->
                THIS:set_attribute(exists,"missing"),
                THIS:set_attribute(permitted,"n/a"), 
                THIS:set_attribute(?STATE_STRING,"directory not found"),
                THIS:set_attribute(?CATEGORY,?NO_DATA),
                THIS:set_attribute(?NO_DATA,true);
            true ->
                if Flag2 == false ->
					%io:format("****************************~n"),
                    THIS:set_attribute(exists,"denied"),
                    THIS:set_attribute(permitted,"denied"), 
                    THIS:set_attribute(?STATE_STRING,"access denied"),
                    THIS:set_attribute(?CATEGORY,?NO_DATA),
                    THIS:set_attribute(?NO_DATA,true); 
                true ->
                    THIS:set_attribute(exists,"file"),
                    THIS:set_attribute(?STATE_STRING,"directory was a file")
                end
            end,    
            THIS:set_attribute(lastmofified,"n/a"),
            THIS:set_attribute(firstmofified,"n/a"),
            THIS:set_attribute(fileCount,0),
            THIS:set_attribute(size,0)
        end;
    true ->
        Exists = isUnixDirectory(Host,DirPath),
        if NoRecurse ->
	        Cmd = case machine:getCommandString(fileListExtend,Host,[{"directory",DirPath}]) of
                {ok,Val} ->
				    Val;
			    _->
				    null
		       end;
        true ->
	        Cmd = case machine:getCommandString(fileListExtendRecursive,Host,[{"directory",DirPath}]) of
                {ok,Val} ->
				    Val;
			    _->
				    null
		       end
        end,
	    YearCmd = case machine:getCommandString(systemTime,Host) of
            {ok,Val1} ->
				Val1;
			_->
				null
		    end,
        {ok,Obj} = commandLine:get(Host,YearCmd),
        case Obj:exec() of
        {ok,List} ->
            io:format("dir List!!!:~p~nYearCmd:~p~n",[List,YearCmd]), 
            [Da] = lists:sublist(List,3,1), 
            [Y,T] = string:tokens(Da," "),
            [_Mon,_Day,TYear] = string:tokens(Y,"/"),
            [Ho,Min,Se] = string:tokens(T,":"),
            NowTime = {{erlang:list_to_integer(TYear),erlang:list_to_integer(_Mon),erlang:list_to_integer(_Day)},{erlang:list_to_integer(Ho),erlang:list_to_integer(Min),erlang:list_to_integer(Se)}},
            Year = erlang:list_to_integer(TYear);
        _ ->
            {{Year,_,_},_} = NowTime= calendar:local_time() 
        end, 
        commandLine:remove(Host),
        
        {ok,RegexpComp} = re:compile("^((([0-9]+)-([0-9]+))-([0-9]+))|((([0-9]+) ([0-9]+)) ([0-9]+))$|((([0-9]+)/([0-9]+))/([0-9]+))"),
        {ok,RegexpYear} = re:compile("^(?:19|20)[0-9][0-9]$"),
        {ok,RegexpTime} = re:compile("^([0-5][0-9]):([0-5][0-9]):([0-9]|[0-9][0-9]|[0-9][0-9][0-9])$"),
        {ok,CmdObj} = commandLine:get(Host,Cmd),        
        {Status,Result} = CmdObj:exec(),
        commandLine:remove(Host),         
        case Status of
        ok ->
            if length(Match) > 0 ->                
                {Size,LastMod,FirstMod,FileCount} = dealFileInfoMatch(RemoteCode,RegexpTime,NowTime,RegexpYear,RegexpComp,LocalCode,Result,Year,Match,Machine#machine.remoteencoding),
                THIS:set_attribute(lastmofified,LastMod/60),
                THIS:set_attribute(firstmofified,FirstMod/60),
                THIS:set_attribute(exists,"exists"),
                THIS:set_attribute(permitted,"permitted"), 
                THIS:set_attribute(size,Size/1024),
                THIS:set_attribute(fileCount,FileCount),
                THIS:set_attribute(?STATE_STRING,integer_to_list(FileCount) ++ " files, " ++ "size:" ++integer_to_list(round(Size / 1024)) ++"K"++",last time since modified: " ++ integer_to_list(round(LastMod/60)) ++ " min,"++"first time since modified: " ++ integer_to_list(round(FirstMod/60)) ++ " min.");                 
            true ->
                {Size,LastMod,FirstMod,FileCount} = dealFileInfo(RemoteCode,RegexpTime,NowTime,RegexpYear,RegexpComp,LocalCode,Result,Year),
                THIS:set_attribute(lastmofified,LastMod),
                THIS:set_attribute(firstmofified,FirstMod),
                THIS:set_attribute(exists,"exists"),
                THIS:set_attribute(permitted,"permitted"), 
                THIS:set_attribute(size,Size),
                THIS:set_attribute(fileCount,FileCount),
                THIS:set_attribute(?STATE_STRING,integer_to_list(FileCount) ++ " files, " ++ "size:" ++integer_to_list(round(Size / 1024)) ++"K"++",last time since modified: " ++ integer_to_list(round(LastMod/60)) ++ " min,"++"first time since modified: " ++ integer_to_list(round(FirstMod/60)) ++ " min.")                                
            end;
        _ ->
            THIS:set_attribute(lastmofified,"n/a"),
            THIS:set_attribute(firstmofified,"n/a"),
            THIS:set_attribute(fileCount,0),
            THIS:set_attribute(size,0),
            THIS:set_attribute(exists,"n/a"),
            THIS:set_attribute(permitted,"n/a"), 
            THIS:set_attribute(?STATE_STRING,Result)                    
        end  
    end.    

%NowYear  is  Integer
dealFileInfo(RemoteCode,RegexpTime,NowTime,RegexpYear,RegexpComp,LocalCode,FileList,NowYear) ->
    %io:format("FileList:~p~n",[FileList]), 
    dealFileInfo_t(RemoteCode,RegexpTime,NowTime,RegexpYear,RegexpComp,LocalCode,FileList,length(FileList),NowYear,0,-1,-1,0).
dealFileInfo_t(_RemoteC,_RegexpT,_Nt,_Ry,_Rc,_LC,_L,0,_N,Size,LastMod,FirstMod,FileCount) -> {Size,LastMod,FirstMod,FileCount};
dealFileInfo_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,[A|B],Num,Now,Si,Lm,Fm,Fc) ->
 if A /= [] ->
    DeniedBool =  ((string:str(A,"Permission denied") >0) or (string:str(A,"ls:") >0) or (string:str(A,iconv:convert(LocalCod,RemoteC,"!"))  == 0)or (string:str(A,iconv:convert(LocalCod,RemoteC,","))  > 0)), 
    if DeniedBool ->
        dealFileInfo_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Si,Lm,Fm,Fc) ;
    true ->        
        List = string:tokens(A,iconv:convert(LocalCod,"utf-8","!")),    
        [TimeTemp] = lists:sublist(List,2,1), 
        IsYMDTimeType =textutils:isDate(TimeTemp,RegexpC),
        %IsFileLink = lists:any(fun(X) -> X == iconv:convert(LocalCod,"utf-8","->") end,List),      
        if IsYMDTimeType ->
            [Date] = lists:sublist(List,2,1), 
            [Size] = lists:sublist(List,1,1),
            [Time] = lists:sublist(List,3,1),
            [Y,M,D] = string:tokens(Date,iconv:convert(LocalCod,"utf-8","-")),
            [H,Mi,S] = string:tokens(Time,iconv:convert(LocalCod,"utf-8",":")),
            %io:format("NowT:~p~n",[NowT]),
            %io:format("Time:~p~n",[{{erlang:list_to_integer(Y),erlang:list_to_integer(M),erlang:list_to_integer(D)},{list_to_integer(H),list_to_integer(Mi),round(list_to_float(S))}}]),        
            Second = calendar:datetime_to_gregorian_seconds({{erlang:list_to_integer(Y),erlang:list_to_integer(M),erlang:list_to_integer(D)},{list_to_integer(H),list_to_integer(Mi),round(list_to_float(S))}}),
            Live = calendar:datetime_to_gregorian_seconds(NowT) - Second,  
            if Lm == -1 ->
                Tlm = Live,
                if Live > Fm ->
                    Tfm = Live;
                true ->
                    Tfm = Fm  
                end;
            true ->            
                if Live > Lm ->
                    Tlm = Lm;
                true ->
                    Tlm = Live 
                end, 
                if Live > Fm ->
                    Tfm = Live;
                true ->
                    Tfm = Fm  
                end
            end,    
            dealFileInfo_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Si+erlang:list_to_integer(Size),Tlm,Tfm,Fc+1);            
        true -> 
            [Size] = lists:sublist(List,1,1),
            [Mon] =  lists:sublist(List,2,1),
            [Day] =   lists:sublist(List,3,1),
            [Time] =   lists:sublist(List,4,1),
            YearBool = textutils:isYear(Time,RegexpY), 
            TimeBool = textutils:isYear(Time,RegexpT), 
            if YearBool -> 
                FileYear = list_to_integer(Time),    
                H = "0",
                M = "0";
            true ->
                if TimeBool ->
                    [Temp] = lists:sublist(List,5,1), 
                    FileYear  = list_to_integer(Temp),
                    [H,M,_] = string:tokens(Time,":");              
                true -> 
                    FileYear = Now,        
                    [H,M] = string:tokens(Time,":")
                end    
            end,              
            Tm = dealMonth(Mon), 
            if  length(Tm) == 0 ->
                Month = month_to_number(Mon);
            true ->
                Month = list_to_integer(Tm) 
            end,
            %io:format("NowT:~p~n",[NowT]),
            %io:format("Time:~p~n",[{{Now,Month,erlang:list_to_integer(Day)},{erlang:list_to_integer(H),erlang:list_to_integer(M),0}}]),
            Second = calendar:datetime_to_gregorian_seconds({{FileYear,Month,erlang:list_to_integer(Day)},{erlang:list_to_integer(H),erlang:list_to_integer(M),0}}),
            Live = calendar:datetime_to_gregorian_seconds(NowT) - Second,   
            if Lm == -1 ->
                Tlm = Live,
                if Live > Fm ->
                    Tfm = Live;
                true ->
                    Tfm = Fm 
                end;            
            true ->    
                if Live > Lm ->
                    Tlm = Lm;
                true ->
                    Tlm = Live 
                end, 
                if Live > Fm ->
                    Tfm = Live;
                true ->
                    Tfm = Fm 
                end
            end,    
            dealFileInfo_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Si+erlang:list_to_integer(Size),Tlm,Tfm,Fc+1)              
        end
    end;    
true ->
    dealFileInfo_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Si,Lm,Fm,Fc)    
end.
 

%NowYear  is  Integer
dealFileInfoMatch(RemoteCode,RegexpTime,NowTime,RegexpYear,RegexpComp,LocalCode,FileList,NowYear,Match,Cod) ->
    dealFileInfoMatch_t(RemoteCode,RegexpTime,NowTime,RegexpYear,RegexpComp,LocalCode,FileList,length(FileList),NowYear,Match,Cod,0,-1,-1,0).
dealFileInfoMatch_t(_RemoteC,_RegexpT,_Nt,_Ry,_Rc,_LC,_L,0,_N,_M,_C,Size,LastMod,FirstMod,FileCount) -> {Size,LastMod,FirstMod,FileCount};
dealFileInfoMatch_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,[A|B],Num,Now,Mat,Coding,Si,Lm,Fm,Fc) ->
if A /= [] ->     
    DeniedBool =  ((string:str(A,"Permission denied") >0) or ( string:str(A,"ls:") >0) or (string:str(A,iconv:convert(LocalCod,RemoteC,"!"))  == 0) or (string:str(A,iconv:convert(LocalCod,RemoteC,","))  > 0)), 
    if DeniedBool ->
        dealFileInfo_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Si,Lm,Fm,Fc) ;
    true ->        
        List = string:tokens(A,iconv:convert(LocalCod,"utf-8","!")), 
        [TimeTemp] = lists:sublist(List,2,1), 
        IsYMDTimeType = textutils:isDate(TimeTemp,RegexpC),
        if IsYMDTimeType ->  
            TFileName = makeFilename(lists:sublist(List,4,length(List))),
            FileName = iconv:convert(Coding,"utf-8",TFileName),
            case re:run(FileName,Mat) of
            {match,_} ->
                [Date] = lists:sublist(List,2,1), 
                [Size] = lists:sublist(List,1,1),
                [Time] = lists:sublist(List,3,1),
                [H,Mi,S] = string:tokens(Time,iconv:convert(LocalCod,"utf-8",":")),
                [Y,M,D] = string:tokens(Date,iconv:convert(LocalCod,"utf-8","-")),
                Second = calendar:datetime_to_gregorian_seconds({{erlang:list_to_integer(Y),erlang:list_to_integer(M),erlang:list_to_integer(D)},{list_to_integer(H),list_to_integer(Mi),round(list_to_float(S))}}),
                Live = calendar:datetime_to_gregorian_seconds(NowT) - Second,  
                if Lm == -1 ->
                    Tlm = Live,
                    if Live > Fm ->
                        Tfm = Live;
                    true ->
                        Tfm = Fm 
                    end;
                true ->                
                    if Live > Lm ->
                        Tlm = Lm;
                    true ->
                        Tlm = Live 
                    end, 
                    if Live > Fm ->
                        Tfm = Live;
                    true ->
                        Tfm = Fm 
                    end
                end, 
                dealFileInfoMatch_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Mat,Coding,Si+erlang:list_to_integer(Size),Tlm,Tfm,Fc+1);                    
            _ ->
                dealFileInfoMatch_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Mat,Coding,Si,Lm,Fm,Fc)
            end;
        true ->
            [TFileName] = lists:sublist(List,length(List),1),
            FileName = iconv:convert(Coding,"utf-8",TFileName),
            case re:run(FileName,Mat) of
            {match,_} ->         
                [Size] = lists:sublist(List,1,1),
                [Mon] =  lists:sublist(List,2,1),
                [Day] =   lists:sublist(List,3,1),
                [Time] =   lists:sublist(List,4,1),
                YearBool = textutils:isYear(Time,RegexpY), 
                TimeBool = textutils:isYear(Time,RegexpT),            
                if YearBool -> 
                    FileYear = erlang:list_to_integer(Time),  
                    H = "0",
                    M = "0";
                true -> 
                    if TimeBool ->
                        [Temp] = lists:sublist(List,5,1), 
                        FileYear  = list_to_integer(Temp),
                        [H,M,_] = string:tokens(Time,":");
                    true -> 
                        FileYear = Now,            
                        [H,M] = string:tokens(Time,":")
                    end    
                end,                      
                Tm = dealMonth(Mon), 
                if  length(Tm) == 0 ->
                    Month = month_to_number(Mon);
                true ->
                    Month = list_to_integer(Tm) 
                end,
                Second = calendar:datetime_to_gregorian_seconds({{FileYear,Month,erlang:list_to_integer(Day)},{erlang:list_to_integer(H),erlang:list_to_integer(M),0}}),
                Live = calendar:datetime_to_gregorian_seconds(NowT) - Second,
                if Lm == -1 ->
                    Tlm = Live,
                    if Live > Fm ->
                        Tfm = Live;
                    true ->
                        Tfm = Fm 
                    end;
                true ->                
                    if Live > Lm ->
                        Tlm = Lm;
                    true ->
                        Tlm = Live 
                    end, 
                    if Live > Fm ->
                        Tfm = Live;
                    true ->
                        Tfm = Fm 
                    end
                end,
                dealFileInfoMatch_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Mat,Coding,Si+erlang:list_to_integer(Size),Tlm,Tfm,Fc+1);
            _ ->
                dealFileInfoMatch_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Mat,Coding,Si,Lm,Fm,Fc) 
            end 
        end
    end;   
true ->
    dealFileInfoMatch_t(RemoteC,RegexpT,NowT,RegexpY,RegexpC,LocalCod,B,Num-1,Now,Mat,Coding,Si,Lm,Fm,Fc)  
end.
                

makeFilename(List) ->
    makeFilename_t(List,length(List),"") .  
makeFilename_t(_L,0,Filename) -> Filename;
makeFilename_t([A|B],Num,FileN) ->
    makeFilename_t(B,Num-1,FileN++A). 

dealMonth(Str) ->
    dealMonth_t(Str,length(Str),"").
dealMonth_t(_S,0,M) -> M;
dealMonth_t(Str,Num,Mon) ->
    Char = string:substr(Str,1,1),
    Bool = textutils:isDigit(Char),
    if Bool ->
        dealMonth_t(string:substr(Str,2),Num-1,Mon ++ Char);
    true ->
        dealMonth_t(Str,0,Mon)
    end.    

isUnixDirectory(Host,Directory) ->
	Cmd = case machine:getCommandString(directoryExists,Host,[{"directory",Directory}]) of
            {ok,Val}->
				Val;
			_->
				null
		   end,       
    {ok,CmdObj} = commandLine:get(Host,Cmd),
    {Status,Result} = CmdObj:exec(),    
    commandLine:remove(Host),    
    case Status of
    ok ->
        if length(Result) > 0 ->
            false;
        true ->
            true 
        end; 
    _ ->
        false 
    end.


%% @spec fileIsDirectory(FilePath) -> true | false
%% @type FilePath = string()
%% @doc Check whether the FilePath is a directory
fileIsDirectory(FilePath) ->
    filelib:is_dir(FilePath).

%getHostname() ->

%% @spec countFiles(DirPath,NoRecurse,Machine,Ag) -> {true|false,Age,Size,Count}
%% @type DirPath -> string()
%% @type NoRecurse -> true | false
%% @type Machine -> record
%% @type Ag = integer()
%% @type Age = integer()
%% @type Size = integer()
%% @type Count = integer()
%% @doc  Access to the directory of the file last modification date, the total file size, the number of documents
countFiles(DirPath,NoRecurse,Machi,Ag) ->
    LocalOs = platform:getOs(),
    Separator = platform:pathSeparator(LocalOs),
    {ok,List} = file:list_dir(DirPath),
    DirList = make_path(List,DirPath,Separator),
    FilePathList = filelist_util(DirList,LocalOs,Separator),
    if NoRecurse ->   
        {Age,Size,Count,First} = noRecurse(DirPath,Separator,List,Machi),
        {true,Age,Size,Count,First};
    true -> 
       
        case length(FilePathList) of
        0 ->   
            {true,0,0,0,0};
         _ ->        
            {Eage,Esize,Ecount,First} = countFiles_util(FilePathList,Machi,Separator,Ag),
            if Eage == error ->
                {false,-1,-1,-1,-1};
            true ->
                {true,Eage,Esize,Ecount,First}
            end    
        end
    end.        

%% @spec noRecurse(DirPath,Separator,FileList,Machine) -> {Age,Size,Count}
%% @type DirPath -> string()
%% @type Separator -> "\\" | "/"
%% @type FileList -> list
%% @type Machine = record
%% @type Age = integer()
%% @type Size = integer()
%% @type Count = integer()
%% @doc  Access to top-level directory of the file last modification date, the total file size, the number of documents
noRecurse(DirPath,Separator,List,Machi) ->
    noRecurse_t(DirPath,Separator,List,length(List),Machi,0,-1,0,0).
noRecurse_t(_DirPath,_Separator,_List,0,_Machi,Size,Age,Count,FirstMod) -> {Age,Size,Count,FirstMod};
noRecurse_t(Dir,Sep,Li,Nu,Ma,Si,Ag,Coun,Fir) ->
    [A|B] = Li,
    {Status,Info} = file:read_file_info(Dir ++ Sep ++ A),   
    case Status of
    ok ->
        NowTime =  timer:seconds(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
        LastModified = timer:seconds(calendar:datetime_to_gregorian_seconds(Info#file_info.mtime)),
        AgeMilTime = textutils:min(textutils:max(0,LastModified),NowTime),
        Age = ((NowTime - AgeMilTime) / 1000) / 60,
        if Ag == -1 ->
                Eage =Age;
        true ->
            if Age > Ag ->
                Eage = Ag;
            true ->
                Eage = Age 
            end
        end,
        if  Age > Fir ->
            First = Age;
        true ->
            First = Fir
        end,       
        if length(Ma) /= 0 ->
            case re:run(A,Ma) of
            {match,_} ->                               
                if Info#file_info.type == regular  ->
                    Tsize = Si + Info#file_info.size,
                    noRecurse_t(Dir,Sep,B,Nu-1,Ma,Tsize,Eage,Coun+1,First);
                true ->                        
                    noRecurse_t(Dir,Sep,B,Nu-1,Ma,Si,Ag,Coun,Fir)
                end;
            _ ->
                %Tsize = Si + Info#file_info.size, 
                noRecurse_t(Dir,Sep,B,Nu-1,Ma,Si,Ag,Coun,Fir) 
            end;
        true ->
            Tsize = Si + Info#file_info.size,      
            noRecurse_t(Dir,Sep,B,Nu-1,Ma,Tsize,Eage,Coun+1,First)
        end;
    _ ->
        noRecurse_t(Dir,Sep,B,Nu-1,Ma,Si,Ag,Coun,Fir)   
    end.

countFiles_util(FileList,String,Separator,Age) ->
    countFiles_util_t(FileList,String,Separator,Age,length(FileList),0,0,0).
countFiles_util_t(_FiLi,_Str,_Sepa,_Age,0,Size,Count,FirstMod) ->  {_Age,Size,Count,FirstMod};
countFiles_util_t(FiLi,Str,Sepa,Tage,Num,TSize,TCount,Fir) ->
    [A|B] = FiLi,
    TempList = string:tokens(A,Sepa),
    [Filename] = lists:sublist(TempList,length(TempList),1),
    if length(Str) /= 0 ->
        case re:run(Filename,Str) of
        {match,_} ->
            F3 = true;
        _ ->
            F3 = false
        end,
        {_Status,Info} = file:read_file_info(A),
        case _Status of
        ok ->
            {file_info,Size,Type,Access,Atime,Mtime,Ctime,Mode,_,_,_,_,_,_} = Info, 
            TotalSize = TSize + Size,
            NowTime =  timer:seconds(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
            LastModified = timer:seconds(calendar:datetime_to_gregorian_seconds(Mtime)),
            AgeMilTime = textutils:min(textutils:max(0,LastModified),NowTime),
            Age = ((NowTime - AgeMilTime) / 1000) / 60,
            if Tage == -1 ->
                Eage =Age;
            true ->
                if Age > Tage ->
                    Eage = Tage;
                true ->
                    Eage = Age 
                end
            end,
            if  Age > Fir ->
                First = Age;
            true ->
                First = Fir
            end,            
            if F3  ->
                countFiles_util_t(B,Str,Sepa,Eage,Num-1,TotalSize,TCount+1,First);
            true ->
                countFiles_util_t(B,Str,Sepa,Tage,Num-1,TSize,TCount,Fir)       
            end;
        error ->
            countFiles_util_t(FiLi,Str,Sepa,error,0,error,Info,Fir) 
        end;
    true ->
        {_Status,Info} = file:read_file_info(A),
        case _Status of
        ok ->        
            {file_info,Size,Type,Access,Atime,Mtime,Ctime,Mode,_,_,_,_,_,_} = Info, 
            TotalSize = TSize + Size,
            NowTime =  timer:seconds(calendar:datetime_to_gregorian_seconds(erlang:localtime())),
            LastModified = timer:seconds(calendar:datetime_to_gregorian_seconds(Mtime)),
            AgeMilTime = textutils:min(textutils:max(0,LastModified),NowTime),
            Age = ((NowTime - AgeMilTime) / 1000) / 60,
            if Tage == -1 ->
                Eage =Age;
            true ->
                if Age > Tage ->
                    Eage = Tage;
                true ->
                    Eage = Age 
                end
            end,
            if  Age > Fir ->
                First = Age;
            true ->
                First = Fir
            end,            
            countFiles_util_t(B,Str,Sepa,Eage,Num-1,TotalSize,TCount+1,First);
        error ->
            countFiles_util_t(FiLi,Str,Sepa,error,0,error,Info,Fir) 
        end
    end.
        

filelist_util(DirPathList,LocalOsnum,Separator) ->
    filelist_util_t(DirPathList,LocalOsnum,Separator,length(DirPathList),[]).
filelist_util_t(_DPL,_Os,_Sep,0,R) -> R;
filelist_util_t(DirL,Osn,Sepa,Num,Re) ->
    [A|B] = DirL,
    case filelib:is_dir(A) of
    true ->
        {ok,FileList} = file:list_dir(A),        
        PathList = make_path(FileList,A,Sepa),
        DL = lists:append(B,PathList),
        filelist_util_t(DL,Osn,Sepa,length(DL),Re);
    _ ->
        %List = string:tokens(A,Sepa),
        filelist_util_t(B,Osn,Sepa,Num-1,lists:append(Re,[A]))
    end.
    
%% @spec noRecurse(FileList,DirPath,Separator) -> Path
%% @type FileList -> list()
%% @type DirPath -> string()
%% @type Separator -> "\\"|"/"
%% @type Path -> string()
%% @doc  Generate the absolute path of the file
make_path(List,Path,Separator) ->
    make_path_t(List,Path,Separator,length(List),[]).
make_path_t(_L,_P,_S,0,R) -> R;
make_path_t(Li,Pa,Separa,Num,Re) ->
    [A|B] = Li,
    
    String = Pa++Separa++A,
    make_path_t(B,Pa,Separa,Num-1,lists:append(Re,[String])).    

%% @spec verify(Params) -> {ok, []} | {error, Reason}
%% @type Params = [term()]
%% @type Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user
verify(Params)->
    Errs = 
	case proplists:get_value(path,Params) of
    ""->
	    [{path,"directory name missing."}];
    DirPath->
        [] 
	end ++
    case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,   
	if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.

%% @spec defaultTitle(Params) -> List
%% @type Params = [term()]
%% @type List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	Path = proplists:get_value(path,Params),
	if
		length(Path)>0->
			BASE:defaultTitle(Params) ++":" ++ Path;
		true ->
			BASE:defaultTitle(Params)
	end.


%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{exists,'!=',"exists"}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end.

%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
        #property{name=path,title="Directory Path",type=text,order=1,description="name of directory (example: seconddir)."},
        #property{name=noRecurse,title="No Subdirectories",default=false,type=bool,advance=true,order=1,description="check this if you <B>do not</B> want SiteView to count files in subdirectories."},        
        #property{name=match,title="File Name Match",type=text,order=2,advance=true,description="use a substring or a <a href=/SiteView/docs/regexp.htm>regular expression</a> to match - only file names that match will be counted."},      
        #property{name=size,title="total of file sizes",type=numeric,order=3,configurable=false,state=true},
        #property{name=exists,title="directory exists",type=text,order=4,configurable=false,state=true},       
        #property{name=lastmofified,title="last time modified",type=text,order=5,configurable=false,state=true},
        #property{name=firstmofified,title="first time modified",type=text,order=5,configurable=false,state=true},
        #property{name=fileCount,title="number of files",type=numeric,order=6,configurable=false,state=true},
        #property{name=permitted,title="access permitted",type=text,order=7,configurable=false,state=true}    
    ].    

     
 
month_to_number(MonStr) ->
    case MonStr of
    "Jan" ->
        1;
    "Feb" ->
        2;
    "Mar" ->
        3;
    "Apr" ->
        4;
    "May" ->
        5;
    "Jun" ->
        6;
    "Jul" ->
        7;
    "Aug" ->
        8;            
    "Sep" ->
        9;
    "Oct" ->
        10;
    "Nov" ->
        11;
    "Dec" ->
        12
    end.