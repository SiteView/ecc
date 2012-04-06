%
%script_monitor.erl
%author:lei.lin@dragonflow.com
%

-module(script_monitor,[BASE]).
-extends(server_monitor).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MAX,10).
-define(PERFEX_EXECSYNC_TIMEOUT,"ERROR: process timeout").
-define(PERFEX_EXECSYNC_LOGON_FAILURE,"ERROR: Logon failed").

new() ->
    Base = server_monitor:new(),
    Base:set_attribute(replacementChars, null),
    Base:set_attribute(replacementCharsInitialized, false),
    Base:set_attribute(labelsCache, null),
    Base:set_attribute(maxNumberOfMatches, 30),
    Base:set_attribute(scriptCommands, []),
    Base:set_attribute(useStatusAndRoundtrip,0),
    Base:set_attribute(pMatchValue,[]),
    {?MODULE,Base}.

init() ->
    Objects = [{'DEFAULT_TIMEOUT',"-1"},{'DEFAULT_SCRIPT_FOLDER',"scripts"},{'pLocalScriptLocation',""},{'pScript',""},{'pExpression',""},{'pParameters',""},{'pMaxMeasurement',""},{'pRemoteScript',""},{'pCacheLife',""},{'pTimeout',""},{'pRoundTripTime',""},{'pStatus',""},{'pStatusLabel',""},{'pScriptOutput',""},{'pValueLabels',""},{'pMatchValue',[]},{'maxNumberOfMatches',""},{'VALUE',"value"},{'replacementCharsTag',"_scriptMonitorReplacementChars"},{'replacementCharsDelimiter',""},{'scriptCommands',[]},{'NUM_TRYS_FOR_FILE_NOT_FOUND',10},{'useStatusAndRoundtrip',0},{'SCRIPT_MONITOR_USE_STATUS_AND_ROUNDTRIP',"_scriptMonitorUseStatusAndRoundtrip"}],
	case ets:info('script') of
    undefined ->
		ets:new('script',[public,named_table]),
		ets:insert('script',Objects);
    _ ->
        true
    end.	

update() ->
    Time1 = platform:timeMillis(),
	Int = -1,
    {ok,{_,MaxNumberOfMatches}} = THIS:get_attribute(maxNumberOfMatches),
    {ok,{_,Expression}} = THIS:get_property(pExpression),
	{ok,{_,MaxMeasurement}} = THIS:get_property(pMaxMeasurement),
	{ok,{_,CacheLife}} = THIS:get_property(pCacheLife),
    {ok,{_,ValueLabels}} = THIS:get_property(pValueLabels),   
    Flag = (CacheLife > 0),
	{ok,{_,Script}} = THIS:get_property(pScript),
	{ok,{_,RemoteScript}} = THIS:get_property(pRemoteScript),
	LocalScriptLocation = "scripts",
	%{ok,{_,LocalScriptLocation}} = THIS:get_property(pLocalScriptLocation),   
		{ok,{_,Host}}=THIS:get_property(machine),           
            if Host /= [] ->
                [Machine|_] = machine:getMachine(Host);
            true ->
                Machine = "" 
            end, 
		    Bool1 = machine:isNTSSH(Host),
		    Index3 = string:str(Script,"USE COMMAND"),
		    if Bool1,Index3 > 0 ->
		        failMonitorRun(Int, "Can not use this option with remote Windows ssh connection");			
		    true ->
                Bool2 = platform:isRemote(Host),
			    Bool3 = (dbcs_machine:get_machine_match("my.host=" ++ Host) == []),
			    if Bool2,Bool3->
				    failMonitorRun(Int, "Remote host unreachable"); 					
                true ->
                    Bool4 = platform:isNTRemote(Host),
                    Bool5 = (machine:isNTSSH(Host) /= true),	
                    if Bool4,Bool5 ->
				       failMonitorRun(-1, "NT host must be configured as NT ssh remote");							
                    true ->
				        Bool6 = (RemoteScript == "none"),
					    Bool7 = (Script == "USE COMMAND"),
				        if Bool6,Bool7 ->
				           failMonitorRun(-1, "Need to specify a script");   
				        true ->
                            Bool8 = (length(RemoteScript) > 0),
			                Bool9 = (RemoteScript /= "none"),
					        if Bool8 , Bool9 ->
							    case getCommandFromLocalFile(RemoteScript) of
                                {ok,Command} ->
                                    Path = Command;
                                _ ->
                                    Path = ""   
                                end; 
						    true ->
							    %Bool10 = platform:isRemote(Host),
                                Bool10 = (Host /= ""),
                                if Bool10 /= true ->
							        case platform:isWindows() of
							        true ->    
									    Path =  "scripts" ++ "\\" ++ Script;
								    _ ->
                                        Path = "scripts" ++ "/" ++ Script
                                    end;								
                                true ->
							        Path = "scripts" ++ machine:getMachinePathSeparator(Host) ++ Script,
                                    OsaI = machine:getAdapter(Host),
								    Command = OsaI:getCommandSetting(fileExists,changeDirectory),
								    case Command of
								    {ok,C} ->
								        S4 = C;
								    _ ->
								        S4 = "/usr/bin/cd"
								    end							
                                end							
					        end,
                            if Host /= [] ->
                                FullPath = remotefile:getFullPath(Host,Path);
                            true ->
                                FullPath = Path
                            end,
                            PathPar = FullPath ++ " " ++ getParameters(),
					 	    Index4 = string:str(Host,"\\\\"),
						    if Index4 == 1 ->
						        Mname = string:substr(Host,3);
                            true ->
                                Mname = Host						
                            end,
                            Machine2 = machine:getNTMachine(Mname),
			                Timeout = case THIS:get_property(pTimeout) of
						                {ok, {_, T}} ->
							                T*1000;
						                _ ->
							                600000
					                    end,
                            ScriptServerName = getScriptServerName(Host),								   
                            if Script /= "USE COMMAND" ->
						        Cmd = PathPar;
						    true ->
                                Cmd = Path
                            end,
                            Tid = ets:new(sctipt,[]),
                            if Script == "USE COMMAND" ->
                                CachePostfix =  RemoteScript;
                            true ->
                                CachePostfix = Script
                            end, 
                            Scriptmonitorcache = scriptmonitorcache:new(ScriptServerName,CachePostfix,CacheLife,Tid),
                            Scriptmonitorcache:start(),
                            IsFresh = Scriptmonitorcache:isFresh(),
                            ExitValueT = Scriptmonitorcache:getExitValue(),
                            if (CacheLife > 0) and (ExitValueT == 0) ->
                                OutputList = Scriptmonitorcache:getOutput(),
                                ExitValue = ExitValueT;
                            true ->
                                %OutputString = sshcommandline:exec(Cmd,Machine,true), 
                                if Host /= [] ->
                                    {CmdSta,CmdObj} = commandLine:get(Host,Cmd),        
                                    case CmdSta of
                                    ok ->                                                                            
                                        {Status,OutputListTemp} = CmdObj:exec(),
                                        commandLine:remove(Host),
                                        case Status of
                                        ok ->
                                      
                                            OutputList = OutputListTemp,
                                            ExitValue = 200;
                                        _ ->
  
                                            OutputList = [],
                                            ExitValue = -997                                          
                                        end;
                                    _ ->
                                        OutputList = [], 
                                        ExitValue = -996                                     
                                    end;
                                true ->
                                    case platform:getLocalPlatform() of
                                    1 ->
                                        io:format("Cmd:~p~n",[Cmd]), 
                                        OutputList = string:tokens(os:cmd(Cmd),"\r\n"),
                                        ExitValue = 200;
                                    _ ->
                                        OutputList = string:tokens(os:cmd("./"++Cmd),"\r\n"),
                                        ExitValue = 200                                         
                                    end 
                                end  
                           end, 
                            if (CacheLife>0) and IsFresh /= true ->
                                Scriptmonitorcache:update(ExitValue,OutputList);
                            true ->
                                nothing 
                            end,                        
                            if ExitValue < 0 ->
                                if ExitValue == -996 ->
                                    StateStr = "Script timed out";
                                true ->
                                    StateStr = "Failed to run script" 
                                end,
                                failMonitorRun(ExitValue,StateStr);
                            true ->
                                io:format("ExitValue:~p~n",[ExitValue]),   
                                {St,AS0} = update_utils(OutputList,Expression),                                
                                SBool = textutils:isSubstituteExpression(Expression),
                                if SBool ->
                                    Expre = textutils:substitute(Expression);
                                true ->
                                    Expre = Expression
                                end,
                                RoundTripTime = platform:timeMillis() - Time1,
                                ExBool = textutils:isRegularExpression(Expre),
                                if (length(Expre) > 0) , ExBool ->
                                    case  regexp:matches(St,Expre) of
                                    {match,MatList} ->
                                        String = setLabels(string:tokens(ValueLabels,","),MatList,St),                                     
                                        THIS:set_attribute(pScriptOutput,OutputList),                                  
                                        St6 = integer_to_list(round(RoundTripTime / 1000)) ++ " sec",
                                        THIS:set_attribute(pStatus,0),
                                        THIS:set_attribute(pRoundTripTime,RoundTripTime),
                                        THIS:set_attribute(pMeasurement,RoundTripTime), 
                                        THIS:set_attribute(?STATE_STRING,String);                                    
                                    _ ->
                                        failMonitorRun(-1, "Content Match Error")
                                    end; 
                                true ->  
                                        THIS:set_attribute(pScriptOutput,OutputList),  
                                        case update_util1(OutputList,Machine) of
                                        ok ->                                        
                                            St6 = integer_to_list(round(RoundTripTime /1000)) ++ " sec",
                                            THIS:set_attribute(pStatus,0),
                                            THIS:set_attribute(pRoundTripTime,RoundTripTime),
                                            THIS:set_attribute(pMeasurement,RoundTripTime), 
                                            THIS:set_attribute(?STATE_STRING,"exit:" ++ "0    " ++ St6);
                                        _ ->
                                            nothing 
                                        end  
                                end
                             end   
                        end					 
                    end				
                end		    
            end.

setLabels(ValueLabels,MatList,St) ->
    if ValueLabels == [] ->
        [{Start,End}|B] =  MatList,
        THIS:set_attribute(value,string:substr(St,Start,End)),         
        "value="++string:substr(St,Start,End);
    true ->
        setLabels_t(ValueLabels,MatList,St,"") 
    end.
setLabels_t(0,0,_S,E) ->  E;
setLabels_t(ValLabels,MList,S,En) ->
    if  ValLabels == [] ->
        setLabels_t(0,0,S,En);
    true ->
        if MList == [] -> 
            setLabels_t(0,0,S,En);
        true -> 
            [{Start,End}|B] = MList,
            [C|D] = ValLabels,             
            THIS:set_attribute(list_to_atom(C),string:substr(S,Start,End)), 
            setLabels_t(D,B,S,En++C ++"="++string:substr(S,Start,End) ++"<br>")            
        end                     
    end. 
    

update_util1(List,Machine) ->
    LocalCode = platform:getLocalCode(),
    if  Machine == [] ->
        update_util1_t(LocalCode,LocalCode,List,length(List),ok);
    true ->        
        update_util1_t(LocalCode,Machine#machine.remoteencoding,List,length(List),ok)
    end.    
update_util1_t(_Lc,_C,_L,0,E) -> E;
update_util1_t(LocalCod,Code,[A|B],Num,En) ->    
    Index = string:str(A,?PERFEX_EXECSYNC_TIMEOUT),
    Bool1 = (string:str(A,"not found") > 0),
    Bool2 = (string:str(A,"Not Found") > 0),
    Bool3 = (string:str(A,"denied") > 0),
    Bool4 = (string:str(A,"Denied") > 0),
    Bool5 = (string:str(A,"cannot execute") > 0),
    Bool6 = (string:str(A,"such file or directory") > 0),
    Bool7 = (string:str(iconv:convert(Code,LocalCod,A),"È¨ÏÞ²»¹»") > 0),
    if Index == 1 ->
        failMonitorRun(-1,"Error: Timeout"),
        update_util1_t(LocalCod,Code,B,0,en);
    true ->    
        if  Bool1 or Bool2 or Bool3 or Bool4 or Bool5 or Bool6  or Bool7 ->    
            THIS:set_attribute(pStatus,-1),
            THIS:set_attribute(?STATE_STRING,A),
            THIS:set_attribute(pRoundTripTime,"n/a"),
            THIS:set_attribute(pMeasurement,"n/a"),            
            update_util1_t(LocalCod,Code,B,0,en);
        true ->
            update_util1_t(LocalCod,Code,B,Num-1,En)
        end
    end.        
        
        
update_utils(List,Expre) ->
    update_utils_t(List,Expre,length(List),"","").
update_utils_t(L,Exp,0,R,As0) -> {R,As0};
update_utils_t(Li,Expr,N,Re,As) ->
    [A|B] = Li,
    Index = string:str(A,?PERFEX_EXECSYNC_TIMEOUT),
    Bool1 = (string:str(A,"not found") > 0),
    Bool2 = (string:str(A,"Not Found") > 0),
    Bool3 = (string:str(A,"denied") > 0),
    Bool4 = (string:str(A,"Denied") > 0),
    Bool5 = (string:str(A,"cannot execute") > 0),
    Bool6 = (string:str(A,"such file or directory") > 0),
    if Index == 1 ->
        failMonitorRun(-1,"Error: Timeout"),
        update_utils_t(B,Expr,0,Re,As);
    true ->
        if  Bool1 or Bool2 or Bool3 or Bool4 or Bool5 or Bool6 ->
            failMonitorRun(-1,A),
            update_utils_t(B,Expr,0,Re,As);
        true ->                                
            Bool7 = textutils:isRegularExpression(Expr),
            Bool8 = (string:str(A,Expr) /= 0),
            Bool9 = (length(Expr) > 0),
            if Bool7,Bool8,Bool9 ->
                L = textutils:findLong(A,"",""),
                if L /= -1 ->
                    update_utils_t(B,Expr,N-1,string:concat(Re,A ++ "\n"),L);
                true ->
                    update_utils_t(B,Expr,N-1,string:concat(Re,A ++ "\n"),As)
                end;
            true ->
                update_utils_t(B,Expr,N-1,string:concat(Re,A ++ "\n"),As)                                   
            end                                   
        end            
    end.      
    
    
failMonitorRun(Int,String) ->
    THIS:set_attribute(pStatus,Int),
    THIS:set_attribute(?STATE_STRING,String),
    {ok,{_,MatchValue}} = THIS:get_attribute(pMatchValue), 
    failMonitorRun_util(MatchValue),      
    THIS:set_attribute(pRoundTripTime,"n/a").

failMonitorRun_util(MatchValueList) ->
    failMonitorRun_util_t(MatchValueList,length(MatchValueList)).
failMonitorRun_util_t(_M,0) -> ok;
failMonitorRun_util_t(MatchValue,Num) ->
    [A|B] = MatchValue,
    THIS:set_property(A,"n/a"),
    failMonitorRun_util_t(B,Num-1).    

%%return {ok,Value} | {error,Reason}
getCommandFromLocalFile(CmdFileName) ->
    {ok,{_,ScriptCommands}} = THIS:get_attribute(scriptCommands),
    case lists:keysearch(CmdFileName,1,ScriptCommands) of
    false ->
	    Os =  platform:getOs(),
        case Os of
        1 ->
            Path = platform:getUsedDirectoryPath("scripts.remote","") ++ "\\" ++ CmdFileName;
        _ ->
            Path = platform:getUsedDirectoryPath("scripts.remote","") ++ "/" ++ CmdFileName	
        end,        
        case CommandFileContent = file:read_file(Path) of
        {ok,Binary} ->
            Command = binary_to_list(Binary),
            THIS:set_property(scriptCommands,[{CmdFileName,Command}|ScriptCommands]),
            {ok,Command};
        _ ->
            timer:sleep(100),
            case CommandFileContent = file:read_file(Path) of
            {ok,Binary} ->
                Command = binary_to_list(Binary),
                THIS:set_property(scriptCommands,[{CmdFileName,Command}|ScriptCommands]),
                {ok,Command};
            {error,Reason} ->              
                {error,Reason}    
            end
        end;              
    {value,{Key,Value}} ->
        {ok,Value}
    end.    
    
%	Os =  platform:getOs(),
%    case Os of
%    1 ->
%        Path = platform:getUsedDirectoryPath("scripts.remote","") ++ "\\" ++ CmdFileNmae;
%    _ ->
%        Path = platform:getUsedDirectoryPath("scripts.remote","") ++ "/" ++ CmdFileNmae	
%    end,			
%    Res = file:read_file(Path),
%    case Res of
%	{ok,Binary} ->
%		C = binary_to_list(Binary),
%		string:tokens(C,"\n");
%   {error,Reason} ->
%	    timer:sleep(1000),
%        Res2 = file:read_file(Path),
%        case Res2 of
%        {ok,Binary2} ->
%			C2 = binary_to_list(Binary2),
%		    string:tokens(C2,"\n");
%        {error,Rea} ->
%            [""]				
%        end					
%    end.	



getScriptServerName(Host) ->
    Len = length(Host),
	if Host == null ->
	    "";
	true ->
        if Len == 0 ->
            "loaclhost";
        true ->
            makeScriptServerName(Host) 						
        end		
    end.

makeScriptServerName(IpString) ->
    List = string:tokens(IpString,"."),
    makeScriptServerName_t(List,length(List),"").    
makeScriptServerName_t(_L,0,Str) -> Str;
makeScriptServerName_t([A|B],Num,S) ->
    makeScriptServerName_t(B,Num-1,S++"_"++ A). 

getParameters() ->
	{ok,{_,Parameters}} = THIS:get_property(pParameters),
	Bool = textutils:isSubstituteExpression(Parameters),
	if Bool ->
	    Subs = textutils:substitute(Parameters);
	true ->
        Subs = Parameters
    end,
    Bool1 = textutils:hasChars(Subs,"`;&|<>"),
    if Bool1 ->
        textutils:removeChars(Subs,"`;&|<>");
    true ->
        Subs
    end.		

getLabels() ->
    {ok,{_,LabelsCache}} = THIS:get_attribute(labelsCache), 
    if LabelsCache == null ->
        {ok,{_,ValueLabels}} = THIS:get_property(pValueLabels), 
        if length(ValueLabels) >0 ->
            ValueLabelList = string:tokens(ValueLabels,","), 
            ValueLabelTupleList  = getLabels_util1(ValueLabelList);
        true ->
            {ok,{_,MaxNumberOfMatches}} = THIS:get_attribute(maxNumberOfMatches),
            ValueLabelTupleList = getLabels_util2(MaxNumberOfMatches)           
        end,
        RecordList = BASE:getStateProperties(), 
        THIS:set_property(labelsCache,[getThresholdProperties(RecordList)|ValueLabelTupleList]),
        [getThresholdProperties(RecordList)|ValueLabelTupleList];
    true ->
        LabelsCache 
    end.

getThresholdProperties(RecordList) ->
    getThresholdProperties_t(RecordList,length(RecordList),[]).
getThresholdProperties_t(_R,0,E) -> E;
getThresholdProperties_t(Re,Num,En) ->
    [A|B] = Re,
    Bool = textutils:isThreshold(A),
    if Bool ->
        getThresholdProperties_t(B,Num-1,[{A#property.name,A#property.name}|En]);
    true ->
        getThresholdProperties_t(B,Num-1,En) 
    end.


getLabels_util1(List) ->
    getLabels_util1_t(List,length(List),0,[]).
getLabels_util1_t(_L,0,_N,E) -> E;
getLabels_util1_t(Li,Num,N,En) ->
    [A|B] = Li,
    getLabels_util1_t(B,Num-1,N+1,lists:append(En,[{getNameFromIndex(N),textutils:trim(A)}])).
    
getLabels_util2(MaxNumberOfMatches) ->
    getLabels_util2_t(MaxNumberOfMatches,[]).
getLabels_util2_t(0,E) -> E;
getLabels_util2_t(Num,En) ->
    Value = getNameFromIndex(Num),  
     getLabels_util2_t(Num-1,[{Value,Value}|En]).
 
getPropertyName(StringProperty) ->
    String = textutils:getValue(getLabels,StringProperty),
    if String == 0 ->
        StringProperty;
    true ->
        String
    end. 
     

getNameFromIndex(Integer) ->
    if Integer == 0 ->
        "value";
    true ->
        "value" ++ integer_to_list(Integer+1)  
    end.

cleanlist(ScriptList) ->
    cleanlist_t(ScriptList,length(ScriptList),[]).
cleanlist_t(_S,0,E) -> E;
cleanlist_t([A|B],Num,En) ->
    Index = string:str(A,"."), 
    if Index == 1 ->
        cleanlist_t(B,Num-1,En);
    true ->
        cleanlist_t(B,Num-1,[A|En]) 
    end.
    

getScalarValues(Prop,Params) ->
    case Prop of
	pScript  ->
                case lists:keysearch(machine,1,Params) of
				{value,{machine,Val}}->
                        io:format("!!!!!!!!!!!!Val:~p~n",[Val]),
                         
					    List = cleanlist(getScriptList(Val,"")),
                        
                        lists:append(make_tuple(List),[{"USE COMMAND","USE COMMAND"}]);
				 _->
					[]
				end;
    pRemoteScript ->
        Path = platform:getDirectoryPath("scripts.remote",""),    
        List = file:list_dir(Path),
        case List of
        {ok,L} ->
            [{"none","none"}] ++ make_tuple(cleanlist(L));
        _ ->
            []
        end;            
    _ ->
	    BASE:getScalarValues(Prop,Params)
	end.

make_tuple(List) ->
   make_tuple_t(List,length(List),[]).
make_tuple_t(_L,0,R) ->  R;
make_tuple_t(L,N,Re) ->
    [A|B] = L,
    make_tuple_t(B,N-1,lists:append(Re,[{A,A}])).


getScriptList(Host,LocalScriptLocation) ->
    if Host /= [] -> 
        Osnum = machine:getOS(Host),
        Len = length(LocalScriptLocation),
        if  Len == 0 ->
            ScriptLocation =  "scripts";
        true ->
            ScriptLocation = LocalScriptLocation
        end,  
        Bool1 = machine:isNTSSH(Host),
        if Bool1 ->
            FilesList = remotefile:listFiles(Host,"scripts",Osnum);
        true ->
            %Bool2 = platform:isCommandLineRemote(Host),
            FilesList = remotefile:listFiles(Host,"scripts",Osnum),
            io:format("!!!!!!FilesList:~p~n",[FilesList]),
            case FilesList of
            [] ->
                Path = platform:getDirectoryPath("scripts",""),
                case  file:list_dir(Path)  of
                {ok,FilesList} ->
                    FilesList;
                _ ->
                    []
                end;
            FlieL ->
                filt_file(FlieL,Osnum)         
            end    
        end;
    true ->
        Path = platform:getDirectoryPath("scripts",""),    
        case file:list_dir(Path) of
        {ok,L} ->
            L;
        _ ->
            []
        end            
    end.   
 
filt_file(FileList,Osnum) ->
   filt_file_t(Osnum,FileList,length(FileList),[]).
filt_file_t(_Osnum,_FileList,0,E) -> E;
filt_file_t(Os,[A|B],Num,En) -> 
    Index1 = string:str(A,".txt"),
    Bool1 = (length(A)-3 == Index1) and (Index1> 0),
    Bool2 = platform:isUnix(Os),
    Bool3 = (string:str(A,".") == 1),
    if (Bool1 /=  true)  and  ((Bool2 /= true) or  (Bool3 /= true)) ->
        filt_file_t(Os,B,Num-1,[A|En]);
    true ->
        filt_file_t(Os,B,Num-1,En) 
    end.

getLogProperties() ->
    Array = BASE:getLogProperties(),
    %io:format("getLogProperties:~p~n",[Array]),
    Bool = useStatusAndRoundtrip(),
    if Bool ->
        Properties = lists:append([pStatus,pRoundTripTime],Array);
    true ->
        Properties = Array   
    end,
    addValuesPropertiesToArray(Properties). 

addValuesPropertiesToArray(List) ->
    {ok,{_,Expression}} = THIS:get_property(pExpression),
    {ok,{_,MatchValue}} = THIS:get_property(pMatchValue),
    if length(Expression) > 0 ->     
        addValuesPropertiesToArray_util(List,MatchValue,Expression);
    true ->
        List 
    end. 
 
addValuesPropertiesToArray_util(List,MatchValue,Expression)  -> 
    addValuesPropertiesToArray_util_t(lists:append(List,lists:sublist(MatchValue,1,1)),MatchValue,length(MatchValue),getExtractedNumber(Expression),2,true).
addValuesPropertiesToArray_util_t(Li,_MatchV,_MatchLen,_ExpressionNumber,I,false) -> Li;
addValuesPropertiesToArray_util_t(List,MatchVa,MatchLen,ExpressionNumber,Int,Flag) ->
    if (Int < ExpressionNumber) and (Int < MatchLen) ->
        addValuesPropertiesToArray_util_t(lists:append(List,lists:sublist(MatchVa,Int,1)),MatchVa,MatchLen,ExpressionNumber,Int+1,Flag);
    true ->
        addValuesPropertiesToArray_util_t(List,MatchVa,MatchLen,ExpressionNumber,Int,false) 
    end. 
     
  
getExtractedNumber(String) ->
    getExtractedNumber_util(String). 
     
getExtractedNumber_util(String) ->
    getExtractedNumber_util_t(String,length(String),1,0).
getExtractedNumber_util_t(_S,0,_J,N) -> N;
getExtractedNumber_util_t(Str,Len,J,Num) ->
    Char = string:substr(Str,J,1),
    Prefix = string:substr(Str,J-1,1),
    if (Char == "(") and  (Num > 1) and (Prefix /= "\\")  ->
        getExtractedNumber_util_t(Str,Len-1,J+1,Num+1);
    true ->
        getExtractedNumber_util_t(Str,Len-1,J+1,Num)
    end.
    
useStatusAndRoundtrip() ->
    {ok,{_,UseStatusAndRoundtrip}} = THIS:get_property(useStatusAndRoundtrip),
    if  UseStatusAndRoundtrip == 0 ->
        case preferences:get(master_config,scriptMonitorUseStatusAndRoundtrip) of
        {ok,[{_,ScriptMonitorUseStatusAndRoundtrip}]} ->
            if ScriptMonitorUseStatusAndRoundtrip == false ->
                THIS:set_property(useStatusAndRoundtrip,-1),
                false;
            true ->
                THIS:set_property(useStatusAndRoundtrip,1), 
                true
            end;            
         _ ->   
                THIS:set_property(useStatusAndRoundtrip,-1),          
                false
        end;
    true ->
        UseStatusAndRoundtrip /= -1  
    end.

getProperty(StringProperty) ->
    if StringProperty == pDiagnosticText ->
        {ok,{_,MatchValue}} = THIS:get_property(pMatchValue),
        if length(MatchValue) > 0 ->
            [MatchValueNum] = lists:sublist(MatchValue,1,1),
            {ok,{_,MatchValueN}} = THIS:get_property(MatchValueNum),            
            if  MatchValueN /= "n/a" ->
                "";
            true ->                
                ScriptMonitorLinesToSave = 25,  
                {ok,{_,TempScriptOutput}} = THIS:get_property(pScriptOutput),
                ScriptOutput1 = textutils:replaceString(TempScriptOutput,"^","\n"),
                ScriptOutput = "Output of Script:\n" ++ ScriptOutput1 ++ "\n"
            end;
        true ->
            ""  
        end;
    true ->
        {ok,{_,StringProperty}} = BASE:get_property(StringProperty),
        StringProperty        
    end.
    
isMultiThreshold() ->
    true.
            
getTopazCounterLabel(StringProperty) ->
    getPropertyLabel(StringProperty,true).

getPropertyLabel(StringProperty,Flag) ->
    if StringProperty == pStatus ->
        {ok,{_,StatusLabel}} = THIS:get_property(pStatusLabel),
        StatusLabel;
    true ->
        String1 = textutils:getValue(getLabels(),StringProperty), 
        if length(String1) /= 0 ->
            String1;
        true -> 
            Index = string:str(String1,"value"),          
            if Flag and (Index /= 1) ->
                "";
            true ->
                String1
            end
        end            
    end.
    

verify(Params) ->
    Errs = 
	case proplists:get_value(pParameters,Params) of
    ""->
		[];
    Parameters->
	    case textutils:hasChars(Parameters,"`;&") of
		    false ->
			    [];
			_->
			    [{pParameters,"script parameters have illegal characters"}]
	    end
	end,  
    if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.    
    
get_template_property()->
	BASE:get_template_property() ++
	[	    
		#property{name=pScript, title="Script", type=scalar, order=2, description="the script from the scripts directory to run (example: scriptTest.bat), or choose <a href=\"\\SiteView\\docs\\ScriptMon.htm#script\" TARGET=Help> USE COMMAND </a>for the command file below."},
        #property{name=pParameters, title="Parameters", type=text, order=3,description="additional parameters to pass to the script. Optionally, use a regular expression to insert date and time variables into the parameters (e.g s/$month$ $day$ $year$/)"},	    
	    #property{name=pValueLabels, title="Match Value Labels", type=text, advance=true, order=1, description="Labels for the values matched on the script output"},		
		#property{name=pTimeout, title="Timeout", type=numeric, advance=true, order=2, description="The total time, in seconds, to wait for a successful script run. Default value is -1 (no timeout).",baselinable=true},
        #property{name=pRemoteScript, title="USE COMMAND Script File", default=1, type=scalar, advance=true, order=3, description="The file that contains a command to be run on the remote UNIX machine."},
		#property{name=pCacheLife, title="Cache Life", type=numeric,default=1,advance=true, order=4, description="The lifetime, in seconds, of the cache, each monitor run will check if the cache life has expired, if it has not then the cache data will be used, otherwise the script will be executed to update the cache."},
		#property{name=pExpression, title="Match Expression", type=text, advance=true, order=5, description="optional Perl regular expression to match against the output of the script or the command from the command file, to extract values (example: (\d*) File.*([\d,]*) bytes)."},
	    #property{name=pMaxMeasurement, title="Maximum for Measurement", type=numeric, advance=true, order=6, description="optional value to specify as maximum for gauge display in milleseconds (example: if the runtime of the script is 4 seconds and this value is set at 8 seconds (8000ms) than the gauge will show at 50%"},
	    #property{name=monitor_description, title="Monitor Description", type=text, advance=true, order=7, description="additional description of monitor that appears on Monitor Detail page (optional)"},
	    #property{name=list_order, title="List Order", type=scalar, advance=true, order=8, description="choose where this monitor appears in the list of monitors on the Monitor Detail page"},	   
	    #property{name=pStatus,title="status",type=numeric,order=9,configurable=false,state=true,description="Enter number or single quoted text"},
		#property{name=pRoundTripTime,title="round trip time",type=numeric,order=10,configurable=false,state=true,description="Enter number or single quoted text",baselinable=true}
		
	].
	
getStateProperties(This,Params) ->
    Temp = This:get_template_property(),
	T = [X || X<-Temp,X#property.state=:=true],
	case proplists:get_value(pExpression,Params) of
    undefined ->
        case  This:get_property(pExpression) of
         {ok,{_,Expression}} ->
            case proplists:get_value(pValueLabels,Params) of 
            undefined ->
                case  This:get_property(pValueLabels) of
                 {ok,{_,ValueLabels}} ->
                    if ValueLabels == [] ->
                        T ++ [#property{name=value,title="value",type=text,state=true,configurable=false}];
                    true ->
                        T ++ make_template(string:tokens(ValueLabels,",")) 
                    end;    
                _ ->                     
                    T ++ [#property{name=value,title="value",type=text,state=true,configurable=false}]
                 end;
             Lablels-> 
                T ++ make_template(string:tokens(Lablels,","))                
            end;  
         _ ->
            T 
        end; 
    _ ->     
        case proplists:get_value(pValueLabels,Params) of
        undefined ->        
            case  This:get_property(pValueLabels) of
            {ok,{_,ValueLabels}} ->
                T ++ make_template(string:tokens(ValueLabels,",")) ;  
            _ ->
                T ++ [#property{name=value,title="value",type=text,state=true,configurable=false}]
            end;
        Lablels->
            T ++ make_template(string:tokens(Lablels,","))         
        end
    end.        

make_template(List) ->
    make_template_t(List,length(List),[]).
make_template_t(_L,0,Template) -> Template; 
make_template_t([A|B],Num,Te) ->
   Temp = #property{name=list_to_atom(A),title=A,type=text,state=true,configurable=false}, 
   make_template_t(B,Num-1,[Temp|Te]).

get_classifier(error)->  
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{status,'>',1}]
			end,
	if      	
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?MAX - length(Cls)));
		true ->
			Cls
	end;
	
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',1}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?MAX - length(Cls)));
		true ->
			Cls
	end;
	
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{status,'==',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(X)->{'N/A','',''} end,lists:seq(1,?MAX - length(Cls)));
		true ->
			Cls
	end.	
       
getReplacementChars() ->
    {ok,{_,ReplacementCharsInitialized}} = THIS:get_attribute(replacementCharsInitialized),
    {ok,{_,ReplacementCharsTemp}} = THIS:get_attribute(replacementChars),
    if ReplacementCharsInitialized ->
        ReplacementChars = ReplacementCharsTemp;
    true ->
        %%Column to get the script from the configuration interface monitors replace characters in this version have been omitted, because the interface is not 6.5? Suspicion. . .
        ReplacementChars = ReplacementCharsTemp                    
   end,
   THIS:set_attribute(replacementCharsInitialized,true),
    ReplacementChars.
    
defaultTitle(Params)->
	Path1 = proplists:get_value(pScript,Params),
	if
		length(Path1)>0->
			BASE:defaultTitle(Params) ++":" ++ Path1;
		true ->
            Path2 = proplists:get_value(pRemoteScript,Params), 			
            if 
                length(Path2)>0->
                    BASE:defaultTitle(Params) ++":" ++ Path2;
                true ->   
                    BASE:defaultTitle(Params)
            end   
	end.        
     