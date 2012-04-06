%@author lei.lin
%@copyright  2009 dragonflow
%@version  0.1
 
-module(logMonitorBase,[BASE]).
-compile(export_all).
-extends(server_monitor).
-include("monitor.hrl").

%@type 
%@spec new() -> object()
new() ->
    Base = server_monitor:new(),
    Base:set_property(logFileName,"_logFile"),
    Base:set_property(logNameName,"logName"),
    Base:set_property(rulesFileName,"_rulesFile"),
    Base:set_property(noFileCheckExistName,"_noFileCheckExist"),
    Base:set_property(matchName,"_match"),
    Base:set_property(alertingName,"_alerting"),
    Base:set_property(lastAlertsPerMinuteName,"lastAlertsPerMinute"),
    Base:set_property(lastLinesPerMinuteName,"lastLinesPerMinute"),
    Base:set_property(matchesName,"matchCount"),   
    Base:set_property(linesName,"lineCount"),
    Base:set_property(lastFilePositionName,"lastFilePosition"),
    Base:set_property(startSearchPositionName,"startSearchPosition"),
    Base:set_property(lastModDateName,"lastModDate"),
    Base:set_property(lastMeasurementName,"lastMeasurement"),
    Base:set_property(messageName,"message"),
    Base:set_property(matchDetailsName,"matchDetails"),
    
    Base:set_attribute(pLastMeasurement,0),
    Base:set_attribute(pLastAlertsPerMinute,0),
    Base:set_attribute(pLastLinesPerMinute,0),
    Base:set_attribute(pLastFilePosition,-1),
    Base:set_attribute(pStartSearchPosition,-1),
    Base:set_attribute(pLines,"n/a"),
    %Base:set_attribute(pMatches,"n/a"),
    Base:set_attribute(pValue,"value"),
    Base:set_attribute(pValue2,"value2"),
    Base:set_attribute(pValue3,"value3"),
    Base:set_attribute(pValue4,"value4"),    
    Base:set_property(valueName,"value"),
    Base:set_property(value2Name,"value2"),
    Base:set_property(value3Name,"value3"),
    Base:set_property(value4Name,"value4"),
    Base:set_property(valueLabelsName,"_valeLabels"),
    Base:set_property(logFileNameDispalyText,"the pathname of the log file to monitor\n<p> In order to monitor remote Unix files choose the 'Choose Server' link above.  For NT, you must specify the UNC path to the file.  For example, \\\\machinename\\sharename\\filename.log.\n<p>Optionally, use a <a href=/SiteView/docs/regexp.htm>regular expression</a> to insert date and time variables <br>(e.g s/ex$shortYear$$0month$$0day$.log/ to match IIS log files)"),
    Base:set_property(alertingNameDisplayText,"How alerts for this monitor are triggered by the following options:  <TR><TD>(1) For <b>'for each log entry matched and report status'</b> the monitor triggers alerts for every matching entry found based on the <b>Error If</b> and <b>Warning If</b> thresholds defined for the monitor. </TD></TR>\n<TR><TD>(2) For <b>'once, after all log entries have been checked'</b>, the monitor counts up the number of matches and then triggers alerts based on the <b>Error If</b> and <b>Warning If</b> thresholds defined for the monitor. </TD></TR>\n"),
    Base:set_property(matchNameDisplayText,"enter the text to match in a log entry or a <a href=/SiteView/docs/regexp.htm>regular expression</a>. By default successful match makes monitor alert or error."),
    case platform:isWindows() of
    true ->
        String = "c:";
    _ ->
        String = "/usr"
    end, 
    Base:set_property(ruleFileDisplayText,"optional, pathname to a custom rules file, used instead of Content Match (for example" ++ String ++ "/SiteView/classes/CustomMonitor/examples/sample.rules"),    
    Base:set_property(noFileCheckExistDisplayText,"if the file is not found then don't error."),
    Base:set_property(valueLabelsDisplayText,"Labels for the values matched on the script output, separated by a \",\""),
    Base:set_property(logNameisplayText,"this is the path to the log file. However, it <b>should not be used</b>, because it is not loaded all of the time. It depends on the state of the monitor"),
    Base:set_property(labelsCache,null),
    Base:set_property(usingRules,false),
    Base:set_property(rules,[]),
    Base:set_attribute(lastRulesModDate,0),
    Base:set_property(hashmap,[]),
    {?MODULE,Base}.
    
getValueProperty() ->
     {ok,{_,ValueName}} =   THIS:get_property(valueName),
     ValueName.

getValue2Property() ->
     {ok,{_,Value2Name}} =   THIS:get_property(value2Name),
    Value2Name.
 
getValue3Property() ->
     {ok,{_,Value3Name}} =   THIS:get_property(value3Name),
    Value3Name.

getValue4Property() ->
     {ok,{_,Value4Name}} =   THIS:get_property(value4Name),
    Value4Name. 

getMessageProperty() ->
     {ok,{_,MessageName}} =   THIS:get_property(messageName),
    MessageName.     

getLabels() ->
    {ok,{_,LabelsCache}}= THIS:get_property(pValueLabels),
    {ok,{_,RulesFile}} = THIS:get_property(pLogFile),
    if LabelsCache == null ->
        {ok,{_,ValueLabels}} = THIS:get_property(pValueLabels),
        LabelsLen  = length(ValueLabels),
        if LabelsLen > 0 ->      
            ValueList = string:tokens(ValueLabels,","),
            Len = length(ValueList),
            [Temp1] = lists:sublist(ValueList,1,1),
            Value1 = [{"value",textutils:trim(Temp1)}],
            if Len >= 2 ->
                [Temp2] = lists:sublist(ValueList,2,1),
                Value2= [{"value1",textutils:trim(Temp2)}] ++ Value1,
                if Len >=3 ->
                    [Temp3] = lists:sublist(ValueList,3,1),
                    Value3 = [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1, 
                    if Len >= 4 ->
                        [Temp4] = lists:sublist(ValueList,4,1), 
                        Value4 = [{"value3",textutils:trim(Temp4)}] ++ [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1;
                    true ->
                        Value4= [{"value2",textutils:trim(Temp3)}] ++ [{"value1",textutils:trim(Temp2)}] ++ Value1                         
                    end;
                true ->
                    Value4 = [{"value1",textutils:trim(Temp2)}] ++ Value1
                end;
            true ->
                Value4 = Value1 
            end;
        true ->    
            Value4 = [{"value", "value"},{"value2", "value2"},{"value3", "value3"},{"value4", "value4"}] 
        end,
        %THIS:set_property(pValueLabels,Value4);
        Value4; 
    true ->
        []
    end.  

%getPropertyName(StringProperty) ->
    

%getProperty(StringProperty) ->
%    if  StringProperty == "" -> 
%        {ok,{_,Category}}= THIS:get_attribute(?CATEGORY),
%        if Gategory == good ->
                    
update() ->
    {ok,{_,LastMeasurement}} = THIS:get_attribute(pLastMeasurement),
    {ok,{_,LogFile}} = THIS:get_property(pLogFile),
    {ok,{_,NoFileCheckExist}} = THIS:get_property(pNoFileCheckExist),  
    %{ok,{_,L}} = THIS:get_attribute(pLastMeasurement),
    RulesString = readRules(), 
    if length(RulesString) == 0 ->     
        AL = checkLog([],1000),
        %Len = length(AL),
        if AL /= -1 ->
            [L1] = lists:sublist(AL,1,1),         
            [L2] = lists:sublist(AL,2,1), 
            [L3] = lists:sublist(AL,3,1),
            F2 = (L3 - LastMeasurement) / 1000,
            if F2 > 0 ->
                TF =  60 * (L1 / F2),
                if TF < 0 ->
                    F = 0;
                true ->
                    F = TF
                end,
                TF1 = 60 * (L2 / F2),
                if TF1 < 0 ->
                    F1 = 0;
                true ->                        
                    F1 = TF1
                end;
            true ->
                F = -1,
                F1 = -1 
            end,
            Bool1 = runOwnRules(),
            if Bool1 ->
                if length(RulesString) == 0 ->
                    THIS:set_attribute(?CATEGORY,?GOOD_CATEGORY);  
                true ->
                    THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY)
                end;
            true ->
                nothing
            end,            
            THIS:set_attribute(pLastAlertsPerMinute,F),
            THIS:set_attribute(pLastLinesPerMinute,F1),
            THIS:set_attribute(pLastMeasurement,L3),
            THIS:set_attribute(pMatches,L1),
            THIS:set_attribute(pLines,L2),
            THIS:set_attribute(pMeasurement,F1),
            %io:format("Matches:~p~n",[L1]),
            THIS:set_attribute(?STATE_STRING, integer_to_list(L1) ++ " matches, " ++ integer_to_list(round(F)) ++ " matches/min, " ++ integer_to_list(round(F1)) ++ " log entries/min"),    
            Bool = textutils:isSubstituteExpression(LogFile), 
            if Bool ->
                LF = testutils:substitute(LogFile);
            true ->
                LF = LogFile
            end,
            THIS:set_property(pLogFile,LF);
        true ->
            %[Al0] = lists:sublist(AL,1,1),
            if  AL /= -1 ->
                THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY),
                THIS:set_attribute(?STATE_STRING,textutils:lookupStatus(AL));
            true ->                             
                if NoFileCheckExist == false ->
                    String = "unable to read log file",
                    Bool = runOwnRules(),
                    if Bool ->
                        if length(RulesString) == 0 ->
                            THIS:set_attribute(?CATEGORY,?GOOD_CATEGORY);  
                        true ->
                            THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY)
                        end;
                    true ->
                        nothing
                    end,                    
                    THIS:set_attribute(?STATE_STRING,String),
                    THIS:set_attribute(pMeasurement,0), 
                    THIS:set_attribute(pLastAlertsPerMinute,"n/a"),
                    THIS:set_attribute(pLastLinesPerMinute,"n/a"),
                    THIS:set_attribute(pMatches,"n/a"),
                    THIS:set_attribute(pLines,"n/a"),
                    THIS:set_attribute(pLastMeasurement,0),
                    THIS:set_attribute(pLogFile,"n/a"),
                    THIS:set_attribute(?NO_DATA,true);
                true ->
                    L1 = 0,
                    L2 = 0,
                    L3 = 0,                
                    F2 = (L3 - LastMeasurement) / 1000,
                    if F2 > 0 ->
                        TF =  60 * (L2 / F2),
                        if TF < 0 ->
                            F = 0;
                        true ->
                            F = TF
                        end,
                        TF1 = 60 * (L2 / F2),
                        if TF1 < 0 ->
                            F1 = 0;
                        true ->                        
                            F1 = TF1
                        end;
                    true ->
                        F = 0,
                        F1 = 0 
                    end,
                    Bool1 = runOwnRules(),
                    if Bool1 ->
                        if length(RulesString) == 0 ->
                            THIS:set_attribute(?CATEGORY,?GOOD_CATEGORY);  
                        true ->
                            THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY)
                        end;
                    true ->
                        nothing
                    end,                    
                    THIS:set_attribute(pLastAlertsPerMinute,F),
                    THIS:set_attribute(pLastLinesPerMinute,F1),
                    THIS:set_attribute(pLastMeasurement,L3),
                    THIS:set_attribute(pMatches,L1),
                    THIS:set_attribute(pLines,L2),
                    THIS:set_attribute(pMeasurement,F1),
                    THIS:set_attribute(?STATE_STRING, integer_to_list(L1) ++ " matches, " ++ integer_to_list(round(F)) ++ " matches/min, " ++ integer_to_list(round(F1)) ++ " log entries/min"),    
                    Bool = textutils:isSubstituteExpression(LogFile), 
                    if Bool ->
                        LF = testutils:substitute(LogFile);
                    true ->
                        LF = LogFile
                    end,
                    THIS:set_property(pLogFile,LF)
                end                
            end           
        end;        
    true ->
        Bool = runOwnRules(),
        if Bool ->
            if length(RulesString) == 0 ->
                THIS:set_attribute(?CATEGORY,?GOOD_CATEGORY);  
            true ->
                THIS:set_attribute(?CATEGORY,?ERROR_CATEGORY)
            end;
        true ->
            nothing
        end,     
        THIS:set_attribute(?STATE_STRING,RulesString),
        THIS:set_attribute(pMeasurement,0), 
        THIS:set_attribute(pLastAlertsPerMinute,"n/a"),
        THIS:set_attribute(pLastLinesPerMinute,"n/a"),
        THIS:set_attribute(pMatches,"n/a"),
        THIS:set_attribute(pLines,"n/a"),
        THIS:set_attribute(pLastMeasurement,0),
        THIS:set_attribute(pLogFile,"n/a"),
        THIS:set_attribute(?NO_DATA,true)                
    end.
    

getRemoteLogFileSize(Host,LogFilePath) ->
    %{ok,{_,Host}} = THIS:get_property(machine),
	case  dbcs_machine:get_machine_match("my.host=" ++ Host)  of
    [] ->
	    io:format("Error, Could not get adapter for machine ~p~n",[Host]),
        -1;
	[Ma|_] ->	
        %Os_Name_List = string:tokens(Ma#machine.os,"_"),
        OsaI = machine:getAdapter(Host),
        %{ok,{_,Hash}} = THIS:get_property(hashmap),
        %HashMap = lists:append(Hash,[{"file",LogFilePath}]),
        {_CmdStatu,Command} = machine:getCommandString(fileExists,Host,[{"file",LogFilePath}]),
        %Res = sshcommandline:exec(Command,Ma,true),
        %ResList = string:tokens(Res,"\n"),
       case  siteview_commandline:exec(Ma#machine.host,Command) of
        {ok,ResList} ->
            if ResList == ["0"] ->         
                Tid2 = platform:init_lineReader(),
                {ok,FileSizeCommand} = machine:getCommandString(filesize,Host,[{"file",LogFilePath}]),
                %FileSizeRes = sshcommandline:exec(FileSizeCommand,Ma,true),
                %FileSizeResList = string:tokens(FileSizeRes,"\n"),
                {ok,FileSizeResList}= siteview_commandline:exec(Ma#machine.host,FileSizeCommand),
                {_SizeStatu,SizeN} = OsaI:getCommandSetting(filesize,size),        
                LineR2 = lineReader:new(FileSizeResList,textutils:delete_bar(Ma#machine.os),Tid2,filesize),
                [IntFlag2] = getRemoteLogFileSize_util_1(SizeN,LineR2),
                ets:delete(Tid2),            
                list_to_integer(IntFlag2);
            true ->
                -1    
            end;            
        _ ->
            -1 
        end  
    end.    
 



getRemoteLogFileSize_util(Match,LineR) ->
    getRemoteLogFileSize_util_t(Match,LineR,1,1).
getRemoteLogFileSize_util_t(_Mat,_Line,0,R) -> R;
getRemoteLogFileSize_util_t(Mat,Line,Num,Re) ->
    Flag= Line:processLine(),
    if Flag == false ->
        getRemoteLogFileSize_util_t(Mat,Line,0,Re);
    true ->
        case Line:skipLine() of
        true ->
            getRemoteLogFileSize_util_t(Mat,Line,1,Re);
        _ ->
            TCurrentLine = Line:getCurrentLine(),
            CurrentLine = textutils:trim(TCurrentLine),
            Index = string:str(CurrentLine,Mat), 
            if Index /= 1 ->
                getRemoteLogFileSize_util_t(Mat,Line,0,-1);
            true ->
                getRemoteLogFileSize_util_t(Mat,Line,0,Re)           
            end              
        end
    end.

getRemoteLogFileSize_util_1(SizeN,LineR) ->
    Flag1 = LineR:processLine(), 
    getRemoteLogFileSize_util_1_t(SizeN,LineR,Flag1,1,1).
getRemoteLogFileSize_util_1_t(_SizeN,_Line,_Flag,0,R) -> R;
getRemoteLogFileSize_util_1_t(SizeN,Line,Flag,Num,Re) ->   
    if Flag == false ->
        getRemoteLogFileSize_util_1_t(SizeN,Line,Flag,0,Re);
    true ->
        case Line:skipLine() of
        true ->
            TFlag= Line:processLine(), 
            getRemoteLogFileSize_util_1_t(SizeN,Line,TFlag,1,Re);
        _ ->
            Size = Line:readColumn(SizeN,size),
            Len = length(Size),
            if Len > 0 ->
                TFlag= Line:processLine(), 
                getRemoteLogFileSize_util_1_t(SizeN,Line,TFlag,0,Size);
            true ->
                TFlag= Line:processLine(),
                getRemoteLogFileSize_util_1_t(SizeN,Line,TFlag,0,-1)  
            end
        end
    end.
    
readRemoteLogFile(LogFileName,CurrentPos) ->
    {ok,{_,Host}} = THIS:get_property(machine),
    case  dbcs_machine:get_machine_match("my.host=" ++ Host)  of  
    [] ->
	    io:format("Error, Could not get adapter for machine ~p~n",[Host]),
        null;            
    [Machine|_] ->
        %Os_Name_List = string:tokens(Machine#machine.os,"_"),
        %OsaI = osAdapter:new(list_to_atom(Machine#machine.os)),
        %OsaI:initEts(), 
        {ok,{_,LogName}} = THIS:get_property(pLogFile),
        List= [{"file",LogName},{"bytes",integer_to_list(CurrentPos)}],
        {_Statu,Command} = machine:getCommandString(tail,Host,List),        
        case siteview_commandline:exec(Host,Command) of
        {ok,Res} ->
            Res;
        _ ->
            null
        end             
    end.
 
%readAllFile() ->
getNextLine(Braf,List,Int,Flag) ->
    if Braf /= null ->
        String = Braf:readLine(Flag);
    true ->
        Len = length(List),        
        if Int < Len ->
           lists:sublist(List,Int,1);
        true ->
            null 
        end      
    end.
    

getEndPosition(LogFilePath,Host) ->
    Len = length(Host),
    Os = machine:getOS(Host),
    %io:format("Len:~p~nOs:~p~n",[Len,Os]),
    if (Len == 0)  or (Os == 1) ->
        LocalCode = platform:getLocalCode(), 
        case file:read_file_info( iconv:convert("utf-8",LocalCode,LogFilePath)) of
        {ok,{file_info,Size,_,_,_,_,_,_,_,_,_,_,_,_}} ->
            Size;
        _ ->
            -1
        end;
    true ->
        getRemoteLogFileSize(Host,LogFilePath)
    end.
   
getStartPosition(EndSearchPosition,LogFile) ->
    {ok,{_,ResetFile}} = THIS:get_property(pResetFile),
    if ResetFile == "Never" ->
        getStartPosition1(EndSearchPosition,LogFile);
    true ->
        if  ResetFile == "First Time Only" ->
            THIS:set_property(pResetFile,"Never"),
            0;
        true ->
            0
        end     
    end.            

%getStartPosition(EndSearchPosition,LogFile)    
getStartPosition1(Long,String) ->
    {ok,{_,LastFilePosition}} = THIS:get_attribute(pLastFilePosition),
    {ok,{_,StartSearchPosition}} = THIS:get_attribute(pStartSearchPosition),
    {ok,{_,LastLinesPerMinute}} = THIS:get_attribute(pLastLinesPerMinute),
    if StartSearchPosition >= 0 , StartSearchPosition < LastFilePosition ->
        TL1 = StartSearchPosition;
    true ->
        TL1 = LastFilePosition                 
    end,
    if LastFilePosition == Long ->
        io:format("RunMonitor, getStartPosition(" ++  String ++ "): file size hasn't changed"),
        L1 = -1;
    true ->
        if TL1 == -1 , LastLinesPerMinute == "n/a" ->
            io:format("RunMonitor, getStartPosition(" ++ String ++ "): Previous run has no data, setting start position to start of log"),
            L1 = 0;
        true ->
            if TL1 == -1 ->
                io:format("RunMonitor, getStartPosition(" ++ String ++ "): Appears to be a new monitor, setting start position to end of log"),
                L1 = Long;
            true ->
                if TL1 > Long ->
                    io:format("RunMonitor, getStartPosition(" ++ String ++ "): This file smaller than previous, setting start position to beginning of log"),
                    L1 = 0;
                true ->
                    io:format("RunMonitor, getStartPosition(" ++ String ++ "): Log file has grown, setting start position to " ++ integer_to_list(TL1)),
                    L1 = TL1
                end
            end
        end
    end.       


getSetting(String) ->
    case platform:isSiteSeerServer() of
    true ->
        getSetting(s, true);
    _ ->
        getSetting(s, false)
    end.        
 

getSetting(String,Bool) ->
    ok.


forwardAlerts(LogFileOneLineString,String1,StringBuffer,Long) ->
    {ok,{_,UsingRules}} = THIS:get_property(usingRules), 
    {ok,{_,Match}} = THIS:get_property(pMatch), 
    if UsingRules == false ->
        {ok,{_,Match}} = THIS:get_property(pMatch), 
        case re:run(LogFileOneLineString,Match) of
        {match,_} ->
            IntFlag = 1,
            Stringbuffer3  = "";             
        _ ->
            IntFlag = 0, 
            Stringbuffer3  = ""  
        end,
        THIS:set_property(getMessageProperty(),LogFileOneLineString ++ Stringbuffer3),
        BoolFlag = runOwnRules(),
        if  BoolFlag ->      
            BASE:resetCategoryProperties(?ERROR_CATEGORY);
            %if                         
        true ->
            nothing              
        end;
    true ->
        nothing
    end.     
                          

 
checkLog(StringBuffer,Long) ->
    {ok,{_,Host}} = THIS:get_property(machine),
    {ok,{_,LogFile}} = THIS:get_property(pLogFile),
    THIS:set_attribute(pMessage,""),
    StartTime = platform:timeMillis(),
    EndSearchPosition = getEndPosition(LogFile,Host),
    if  EndSearchPosition < 0 ->
        EndSearchPosition;
    true ->
        StartSearchPosition = getStartPosition(EndSearchPosition,LogFile),
        %CurrentPos = StartSearchPosition,
        if  StartSearchPosition >= 0  , StartSearchPosition < EndSearchPosition ->
            Maleng = length(Host),
            Os =  machine:getOS(Host),
            if (Maleng > 0)  and (Os /= 1) ->
                Braf = null,
                CurrentPos = StartSearchPosition + 1,
                if CurrentPos =< EndSearchPosition ->
                    Rlf = readRemoteLogFile(LogFile,CurrentPos);
                true ->
                    Rlf = []
                end;                    
            true ->
                Rlf = null,
                CurrentPos = StartSearchPosition,
                LocalCode = platform:getLocalCode(),             
                case file:open( iconv:convert("utf-8",LocalCode,LogFile),[read]) of
                {ok,IoDevice} ->    
                    EtsTid = ets:new(braf,[]),                    
                    Braf = braf:new(THIS,IoDevice,CurrentPos,EtsTid),
                    Braf:fillBuffer();
                _ -> 
                    Braf = null
                end                
            end,            
            {ok,{_,Match}} = THIS:get_property(pMatch), 
            %MlRegEx = textutils:isMultiLineRegularExpression(Match),
            if Match /= [] -> 
                case re:compile(Match) of
                {ok,CompileR} ->
                    MlRegEx = true,
                    CompileReg = CompileR;
                _ ->
                    MlRegEx = false,
                    CompileReg = ""
                end;
            true ->                
                MlRegEx = false,
                CompileReg = ""                          
            end,
            LinesBuf = lists:duplicate(51,""),
            {CurrentPos2,LineLen,LineCount,MatchCount} = checkLog_while(CompileReg,Match,Braf,Rlf,MlRegEx,CurrentPos,LinesBuf,integer_to_list(Long)),
            Eol = StartSearchPosition + LineLen,  
                if EndSearchPosition  <  Eol ->
                    %THIS:set_property(pStartSearchPosition,Eol), 
                    TStartSearchPosition = Eol,
                    EndSearchPosition1 = Eol;
                true ->
                    %THIS:set_property(pStartSearchPosition,EndSearchPosition),
                    TStartSearchPosition = EndSearchPosition,
                    EndSearchPosition1 = EndSearchPosition                    
            end;
        true ->
            MatchCount = 0, 
            Braf = null, 
            LineCount = 0,
            EndSearchPosition1 = EndSearchPosition,
            TStartSearchPosition = StartSearchPosition,
            {ok,{_,Value}} = THIS:get_attribute(pValue),
            {ok,{_,Value2}} = THIS:get_attribute(pValue2),
            {ok,{_,Value3}} = THIS:get_attribute(pValue3),
            {ok,{_,Value4}} = THIS:get_attribute(pValue4),
            THIS:set_attribute(Value,""), 
            THIS:set_attribute(Value2,""),  
            THIS:set_attribute(Value3,""),
            THIS:set_attribute(Value4,"")
        end,
        %{ok,{_,LastFilePosition}} = THIS:get_property(pLastFilePosition),       
        THIS:set_attribute(pLastFilePosition,EndSearchPosition1),        
        if TStartSearchPosition >= 0 ->
            %{ok,{_,StartSearchPosition1}} = THIS:get_property(pStartSearchPosition),            
            THIS:set_attribute(pStartSearchPosition,TStartSearchPosition);
        true ->
            nothing  
        end,
        if Braf /= null ->
            Braf:close();
        true ->
            nothing
        end, 
        [MatchCount,LineCount,StartTime]        
        %Osa = machine:getAdapter(Host),
        %OsaCmd = Osa:getCommandSetting(fileExists, changeDirectory),                
    end.

checkLog_while(CompileReg,Match,Braf,Rlf,MlRegEx,CurrentPos,LinesBuf,L) ->
        %io:format("Braf:~p~nRlf:~p~nMlRegEx:~p~nCurrentPos:~p~nLinesBuf:~p~nL:~p~n",[Braf,Rlf,MlRegEx,CurrentPos,LinesBuf,L]), 
        checkLog_while_t(CompileReg,Match,Braf,Rlf,MlRegEx,CurrentPos,LinesBuf,L,0,0,0,true).
checkLog_while_t(_CompileR,_Matc,_Braf,_Rlf,_MlRegEx,CurrentPos2,_LinesBuf,_L,LineLen,LineCount,MatchCount,false) -> {CurrentPos2,LineLen,LineCount,MatchCount};
checkLog_while_t(CompileR,Match1,Braf1,Rlf3,MlRegEx1,CurrentPos1,LinesBuf1,L1,LineLen1,LineCount1,MatchCount1,Flag) ->
    if Braf1 /= null ->
        Rlf2 = [],        
        TmpLine = Braf1:readLine(Flag);
    true ->
        [Rlf1|Rlf2] = Rlf3,  
        TmpLine = Rlf1    
    end,          
    %TmpLine = getNextLine(Braf1,Rlf1,LineCount1,MlRegEx1),
    if  TmpLine == "" ->
        checkLog_while_t(CompileR,Match1,Braf1,Rlf2,MlRegEx1,CurrentPos1,LinesBuf1,L1,LineLen1,LineCount1,MatchCount1,false);
    true ->
        if Braf1 /= null -> 
            LineLen =  LineLen1 + Braf1:lastLineLength();
        true ->
            LineLen = LineLen1 + length(TmpLine)+1
        end,
        if MlRegEx1 ->
            %SubLB = lists:sublist(LinesBuf1,1,1),
            %io:format("SubLB:~p~n",[SubLB]),
            %LenLB = length(SubLB),
            %if LenLB == 0 ->
                %Len = 0;
            %true ->
                %[SLB] = SubLB,
                %Len = length(SLB)
            %end,
            case re:run(TmpLine,CompileR) of
            {match,_} ->
                TMatchCount = MatchCount1 +1;
            _ ->
                TMatchCount = MatchCount1  
            end, 
            CurrentPos = CurrentPos1 + LineLen;
            %LinesBuf = checkLog_while_1(50,LinesBuf1);
            %LinesBuf2 = lists:append(LinesBuf,TmpLine), 
            %ConcatLine2 = ConcatLine1 ++ L1;            
        true ->
            TMatchCount = MatchCount1, 
            CurrentPos = CurrentPos1 + LineLen
            %ConcatLine2 = TmpLine
        end,
        if (Rlf2 == []) and (Braf1 == null) ->                      
            checkLog_while_t(CompileR,Match1,Braf1,Rlf2,MlRegEx1,CurrentPos,LinesBuf1,L1,LineLen,LineCount1+1,TMatchCount,false); 
        true ->
            checkLog_while_t(CompileR,Match1,Braf1,Rlf2,MlRegEx1,CurrentPos,LinesBuf1,L1,LineLen,LineCount1+1,TMatchCount,Flag)
        end 
    end.
 
checkLog_while_1(LinesBufSize,LinesBuf) ->
    checkLog_while_1_t(LinesBufSize,LinesBuf,"","").
checkLog_while_1_t(0,_LinesB,LB,CL) ->{LB,CL};
checkLog_while_1_t(LinesBufSize1,LinesBuf1,LB1,CL1) ->
    LB2 = lists:append(LB1,lists:sublist(LinesBuf1,51-LinesBufSize1+1,1)), 
    LinesBuf = lists:sublist(LinesBuf1,51-LinesBufSize1+1,1),
    CL2 = CL1 ++ LinesBuf,    
    checkLog_while_1_t(LinesBufSize1-1,LinesBuf1,LB2,CL2).

readRules() ->
    {ok,{_,RulesFile}} = THIS:get_property(pRulesFile),
    {ok,{_,LastRulesModDate}} = THIS:get_attribute(lastRulesModDate),
    RFLen = length(RulesFile),
    if RFLen == 0 ->
        "";
    true->
        THIS:set_property(usingRules,true),
        LastModfied = calendar:datetime_to_gregorian_seconds(filelib:last_modified(RulesFile)), %Seconds from 1970       
        if  LastModfied == LastRulesModDate ->
            "";
        true ->
            THIS:set_property(lastRulesModDate,LastModfied),
            case  file:open(RulesFile,[read]) of
            {ok,IoDev} ->
                RulesList = readRules_util(IoDev),
                file:close(IoDev),
                THIS:set_property(rules,RulesList),
                "";
            _ ->
                "unable to read rule file"
            end         
        end            
    end.    
        
readRules_util(IoDevice) -> 
    readRules_util_t(IoDevice,0,0,[]).
readRules_util_t(Dev,_Location,eof,Rules) ->  Rules;
readRules_util_t(IoDev,Locat,End,Rul) ->
    {Line,E} = textutils:file_readline(IoDev,Locat),         
    if E == eof ->
        readRules_util_t(IoDev,Locat,eof,Rul);
    true ->
        Index1 = string:str(Line,"#"),
        Len1 = length(Line),
        if (Index1 /= 1) and (Len1 /= 0) ->
            Index2 = string:str(Line,"\t"),
            if Index2 /= 0 ->
                Char = "\t";
            true ->
                Char = ","
            end, 
            List = string:tokens(Line,Char),
            Len2 = length(List),
            if (Len2 < 4) or (Len2 > 5) ->
                {ok,{_,RulesFile}} = THIS:get_property(pRulesFile),
                io:format("Error,parsing rule, file: " ++ RulesFile ++ ", rule:" ++ Line ++", parts:" ++ integer_to_list(Len2)); 
            true ->
                readRules_util_t(IoDev,E,End,lists:append(Rul,List)) 
            end;
        true ->
            readRules_util_t(IoDev,E,End,Rul)
        end
    end.
        

runOwnRules() ->
    {ok,{_,Alerting}} = THIS:get_property(pAlerting),
    {ok,{_,RulesFile}} = THIS:get_property(pRulesFile),
    ((Alerting == "for each log entry matched") and (length(RulesFile) /= 0)).   

get_template_property()->
	BASE:get_template_property().

defaultTitle(Params)->
	Path1 = proplists:get_value(pLogFile,Params),
	if
		length(Path1)>0->
			BASE:defaultTitle(Params) ++":" ++ Path1;
		true ->
            BASE:defaultTitle(Params)  
	end.        
