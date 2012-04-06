%
%platform.erl
%author:lei.lin@dragonflow.com
%
-module(platform).
-compile(export_all).
-include("monitor.hrl").
-include("platform.hrl").
-include("config.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_machine.hrl").

start() ->
    init().
    	
init() ->
        case ets:info('platform') of
        undefined ->
		    ets:new('platform',[public,named_table]),
			Root = tryGetRoot(),
            Objects = [{'evalDays',10},{'totalDays',60},{'kWIN',1},{'kSUN',2},{'kSGI',3},{'kMac',4},{'kHP',5},{'kLinux',6},{'kMacOSX',7},{
'kOtherUnix',8},{'NO_PLATFORM',0},{'FIRST_PLATFORM',1},{'LAST_PLATFORM',8},{'exampleDomain',"siteview.com"},{'exampleURL',
"http://demo.siteview.com/test.htm"},{'exampleMailServer',"mail.this-company.com"},{'supportPhone',"0731-84318165"},{'homeURLPrefix',""},{
'urlOrderOptions',""},{'companyName',"DragonFlow"},{'supportEmail',"support@dragonflow.com"},{'siteseerSupportEmail',
"siteseersupport@dragonflow.com"},{'salesEmail',"siteviewsales@dragonflow.com"},{'salesPhone',"0731-84318165"},{'salesContactName',""},{
'salesContactEmail',""},{'salesContactPhone',""},{'licenseContactEmail',"siteviewlicense@dragonflow.com"},{'copyright',""},{'companyLogo',"<a 
href=\"http://www.dragonflow.com/\" target=\"web\"><img src=/SiteView/htdocs/artwork/DragonFlow2_Websafe_xsml.gif  border=\"\" alt=\"\"></a>"},{
'salesFooter',""},{'salesContact',3},{'isOsInitialized',false},{'os',1},{'portalDirectoryExists',false},{'productName',"SiteView"},{'qtpInstalled'
,-1},{'altInstalled',-1},{'kDebugCPU',8},{'kDebugCommand',4},{'kDebugSendModem',2},{'kDebugPerfex',1},{'kDebugNone',""},{'platformDebugTrace',""
},{'SERVICE_NAME_PREFIX',"username="},{'pid',0},{'userName',""},{'isRootInitialized',false},{'root',Root},{'isUserAccessAllowed',true},{
'userAccessAllowed',false},{'FILE_NEWLINE',"\n"},{'version',"0.1"},{'versionAsNumber',750},{'customConfigPath',""},{'SYSTEM_USER',"SYSTEM"},{
'serverName',""},{'versionString',null},{'isService',false},{'UNIX_DATE_FORMAT',"MM/dd/yyyy HH:mm:ss"},{'UNIX_DATE_FORMAT_WITH_ZONE',"MM/dd/yyyy 
HH:mm:ss 'GMT'Z"},{'charSetTag',""},{'timeZoneOffset',-1},{'PING_COMMAND_FAILED',-999},{'PING_SUCCESSES',1},{'PING_TOTAL_TIME',0},{'DIR_ALL_FILES'
,0},{'DIR_DIRECTORIES',1},{'DIR_FILES',2},{'timeZoneMap',null},{'currentCPUFile',1},{'cpuFile',"D:\\ccpu"}],

			ets:insert('platform', Objects);
        _ ->
            {error,exist}
			end.

init_lineReader() ->
    Tid = ets:new('linereader',[]),
	ets:insert(Tid,[{lineNumber,1},{startLine,1},{endLine,16#7fffffff},{headerLine,1},{columnNames,[]},{columnLabels,[]},{column,[]},{matchLine,""},{skipLineMatch,""},{startMatch,""},{endMatch,""},{reverseColumns,""},{traceStream,null},{traceDetail,null},{start,true},{lines,[]},{currentLine,""},{shouldSkipLine,false},{reading,true}]),
	Tid.		
			
%getRoot() ->
%    init(),
%    Data = ets:lookup('platform','root'),
%    case Data of
%    [{'isService',Val}] ->
%        Val;
%    _ ->
%        tryGetRoot()
%    end.	

getURLPath(String1,String2) ->
    "". 
			
isUserAccessAllowed() ->
    init(),
    Data = ets:lookup('platform','isUserAccessAllowed'),
    case Data of
    [{'isService',Val}] ->
        Val;
    _ ->
        false
    end.	    
		
setService(Flag) ->
    init(),
    ets:insert_new('platform',{'isService',Flag}).
		
isService() ->
    init(),
    Data = ets:lookup('platform','isService'),
    case Data of
    [{'isService',Val}] ->
        Val;
    _ ->
        false
    end.		
	
expiredName()->
	"expired".

isPortal()->
	false.

currentUser()->
	admin.

platformName()->
	case isUnix() of
		true->
			'unix';
		false->
			'nt'
	end.

%setupTimeZoneOffset() ->
%    case isWindows() of
%	    Cmd = perfexCommand("",false) ++ " -t ", 
%		Res = os:cmd(Cmd).
         
getVersion()->
	"0.1".

getLocalPlatform()->
	getOs().

isLocalPlatform(I)->
	getOs() =:= I.

getOs() ->
	case os:type() of %%A variety of environments not yet tested, nt correct
		{_,nt}->
			1;
		{_,solaris}->
			2;
		{_,irix}->
			3;
		{_,macos}->
			4;
		{_,'hp-ux'}->
			5;
		{_,linux}->
			6;
		{_,unix}->
			7
	end.

isWindows()->
	getOs()=:=1.
    
isWindows(OsNum)->
	OsNum=:=1.    

isSGI()->
	getOs()=:=3.

isUnix()->
	getOs()=:=5.

isUnix(Os) ->
    if Os == 1 ->
        false;
    true ->
         true
    end. 

isStandardAccount(String) ->
    Len = length(String),
    if String == "administrator" ->
        true;
    true ->
        if String == "user" ->
            true;
        true ->
            if String == "login" ->
                true;
            true ->
                if Len == 0 ->
                    true;
				true ->
                    false
                end
            end
        end
    end.		
					
	
isSiteSeerAccount(String) ->
    Bool = isStandardAccount(String),
	if Bool  ->
	    false;
	true ->
        true
    end.		

	
isSiteSeerServer() ->
    Res = masterconfig:getMasterConfig(),
    case Res of
	true ->
	    String = textutils:getValue_ets('masterconfig_content',"_siteseerServer"),
		Len = length(String),
		if Len /= 0 ,String == "true" ->
		    true;
		true ->
            false
        end;			
    null ->
	    false
	end.
	
chmodCommand() ->
    Osnum = getOs(),
	case Osnum of
	2 ->
	    Cmd = "chmod";
	3 ->
        Cmd = "chmod";
    5 ->
        Cmd = "/usr/bin/chmod";
	6 ->
        Cmd = "/bin/chmod";
    _ ->
        chmod
    end.		

%File is file path,string type.	
chmod(File,String) ->
    Bool1 = isUnix(),
	Value = file:read_file(File),
	case Value of
	{error,enoent} ->
        Bool2 = false;
    _ -> 
        Bool2 = true
    end,
    if Bool1,Bool2 ->
        R = masterConfig:getMasterConfig(),	
	    case R of
		true ->
		    SetUnixFilePermissions = ets:lookup('masterconfig_content',"_setUnixFilePermissions"),
            case SetUnixFilePermissions of
            [] ->
                nothing;
            [{"_setUnixFilePermissions",BOOL}] ->
                if BOOL == "false" ->
                    nothing;
                true ->
                    UnixFileMask  =  ets:lookup('masterconfig_content',"_unixFileMask"),
                    case UnixFileMask of
				    [] ->
				        nothing;
				    [{"_unixFileMask",Value}] ->
                        case Value of
                        "rwx" ->
                            I = "511";
                        "rw" ->
                            I = "438";                            						
                        _ ->
                            I = "292"
                        end,
                        Cmd = chmodCommand() ++ " " ++ 	I ++ " " ++ File,
                        os:cmd(Cmd)						
				    end
				end	
			end;
		_ ->
			{error,not_master_config}			
        end;
	true ->
	    nothing
	end.
	
ping(Ip,I,J,K)->
	ping(Ip,I,J,K).
	
ping(Ip,I,J,K,Monitor)->
	Cmd = pingCommand(Ip,I,J,K),
	Exec = command_line:new(),
	Ret = Exec:exec(Cmd,Monitor),
	Exec:delete(),
	Ret.

pingCommand(Ip,I,J,K)->
	case getOs() of
		1->
			L=J*(I/1000+1)+10,
			io_lib:format("ping.exe -timeout ~p -w ~p -l ~p ~p",[L,J,I,Ip]);
		6->
			io_lib:format("/bin/ping -c ~p -w ~p -s ~p ~p",[J,I/1000,K,Ip]);
		2->
			io_lib:format("/bin/ping -c ~p -w ~p -s ~p ~p",[J,I/1000,K,Ip]);
		5->
			io_lib:format("/user/sbin/ping -sn ~p ~p -n ~p",[Ip,K,J]);
		3->
			io_lib:format("/user/etc/ping -c ~p -s ~p ~p",[J,K,Ip])
	end.

traceRoute(Host) ->
    Command =  traceCommand(Host),
	string:tokens(os:cmd(Command),"\r\n").
     


%make tracert command
traceCommand(Host) ->
    Os = getOs(),
	case Os of
	1 ->
	    Cmd = "tracert  " ++ Host;
	2 ->
        Cmd = "/usr/local/bin/traceroute  " ++ Host;
    6 ->
        Cmd = "/usr/sbin/traceroute  " ++ Host;
    5 ->
        Cmd = "/usr/contrib/bin/traceroute  " ++ Host;
    3 ->
        Cmd = "/usr/etc/traceroute " ++ Host;
	_ ->
        Cmd = "tracert " ++ Host
    end.		
  
nslookupCommand() ->
    Os = getOs(),
	case Os of
	1 ->
	    Cmd = "nslookup";
	6 ->
        Cmd = "/usr/bin/nslookup";
    2 ->
        Cmd = "/usr/sbin/nslookup";
    5 ->
        Cmd = "/usr/bin/nslookup -norecurse";
    3 ->
        Cmd = "/usr/sbin/nslookup";
    _ ->
    	Cmd = {error,undefined}
	end.

dnsLookup(DNSserver,HostName,HostIp) ->
    dnsLookup(DNSserver,HostName,HostIp,null).
   
dnsLookup(DNSserver,HostName,HostIp,Monitor) ->
    Osnum = getOs(),
    Cmd = nslookupCommand() ++"  "++ HostName ++"  "++DNSserver,
	case Cmd of
	{error,Res} ->
	    {error,Res};
	_ ->
		{Duration,Value} = timer:tc(os,cmd,[Cmd]),
		List = string:tokens(Value,"\r\n"),
		Val = dnsUtil(List,Osnum,DNSserver),
        S4 = lists:keysearch(s4,1,Val), 
        case S4 of
	    {value,{s4,V}} ->
			Num = length(HostIp),
			if Num > 0 ->
                As1 = string:tokens(HostIp,","),
                TBool = dnsUtil3(As1,V),				
                if TBool ->
				    RS4 = V,
				    Status = 200;
				true ->
				    RS4 = V,
				    Status = -1001
				end;
            true ->
			    RS4 = V,
				Status = 200
            end;
        _ -> 			
            Flag  = lists:keysearch(flag,1,Val), 
            Flag1 = lists:keysearch(flag1,1,Val),
            case Flag of
            {value,{flag,_F}} ->
                case Flag1 of
                {value,{flag1,_F1}} ->
				    RS4 = "",
                    Status = -997;
                _ ->
				    RS4 = "",
                    Status = -997
                end;
            _ ->
                case Flag1 of
                {value,{flag1,_F1}} ->
				    RS4 = "",
                    Status = -997; %kURLBadHostNameError
                _ ->
				    RS4 = "",
                    Status = -998 %kURLNoConnectionError 
                end
            end
        end,
	Sta = integer_to_list(Status),
    lists:append(lists:append([Sta],[Duration]),[RS4])
    end.    		
		
dnsUtil(DnsList,Os,RemoteH) ->
    dnsUtil_t(Os,RemoteH,DnsList,length(DnsList),[]).
dnsUtil_t(_Os,_RH,_L,0,R) -> R;
dnsUtil_t(Osnum,ReHost,DL,Num,Rec) ->
    [A|B] = DL,
	if Osnum == 5 ->
	    Index = string:str(A,"Name Server:"),
	    if Index > 0 ->
		    [NameSer] = string:tokens(lists:sublist(A,13,length(A)-12)," "), 
	        [C|D] = B,
			[Ip] = string:tokens(lists:sublist(A,9,length(A)-8)," "),
            N1 = (NameSer /= ReHost),
            N2 = (Ip /= ReHost),
            if N1,N2 ->
                dnsUtil_t(Osnum,ReHost,DL,0,Rec); 
            true ->
                Flag = true,
				Value1  = lists:keysearch(flag,1,Rec),
				case Value1 of
				false ->
				    OutRe = lists:append(Rec,[{flag,Flag}]),
            	    dnsUtil_t(Osnum,ReHost,B,Num-1,OutRe);
				{value,_} ->
				    dnsUtil_t(Osnum,ReHost,B,Num-1,Rec)
                end				
            end;
        true ->	
           nothing
		end;
    true ->
        Bool1 = textutils:stringContainsSubstringFromArray(A,?DEFAULT_SERVER_TAGS),
            if Bool1 ->
                Index4 = string:rstr(A,"UnKnown"),
                if Index4 /= 1 ->
                    Flag = true,
				    Value1  = lists:keysearch(flag,1,Rec),
				    case Value1 of
				    false ->
				        OutRe1 = lists:append(Rec,[{flag,Flag}]),
            	        dnsUtil_t(Osnum,ReHost,B,Num-1,OutRe1);
				    {value,_} ->
				        dnsUtil_t(Osnum,ReHost,B,Num-1,Rec)
                    end;
                true ->
                    nothing
                end;
            true ->
                nothing
            end,
		    Bool2 = textutils:stringContainsSubstringFromArray(A,?DEFAULT_NAME_TAGS),
		    if Bool2 ->
		        [C|D] = B,
                Index2 =  string:rstr(C,"Address"),
                if Index2 == 1  ->
                    S4 = dnsUtil2(DL,C),
					OutRe2 = lists:append(Rec,[{s4,S4}]),
					dnsUtil_t(Osnum,ReHost,DL,0,OutRe2);
				true ->	
				    OutRe2 = Rec, 
                    dnsUtil_t(Osnum,ReHost,DL,0,Rec)
                end;
            true ->
                Index3 = string:rstr(A,"existent"),
                if Index3 == 1 ->
                    Flag1 = true,
				    Value2  = lists:keysearch(flag1,1,Rec),
				    case Value2 of
				    false ->
				        OutRe3 = lists:append(Rec,[{flag1,Flag1}]),
            	        dnsUtil_t(Osnum,ReHost,B,Num-1,OutRe3);
				    {value,_} ->
					    OutRe3 = Rec,
				        dnsUtil_t(Osnum,ReHost,B,Num-1,Rec)
                    end;
                true ->
				    dnsUtil_t(Osnum,ReHost,B,Num-1,Rec) 
                end				
            end	 
    end.		

	
dnsUtil2(List,Str) ->	
    dnsUtil2_t(List,length(List),Str).
dnsUtil2_t(_L,0,R) -> R;
dnsUtil2_t(L,N,Re) ->
    [A|B] = L,
    Num = length(A),
    Str = lists:sublist(A,1,1),	
	if Num == 0 ->
        if Str == " " ->
            dnsUtil2_t(L,0,Re);
        true ->
            dnsUtil2_t(L,0,Re)
        end;
    true ->
        if Str == " " ->
            dnsUtil2_t(L,0,Re);
        true ->
		    Res = Re ++ A,
            dnsUtil2_t(B,N-1,Res)
        end
    end.		

dnsUtil3(List,T_S4) ->
    dnsUtil3_t(List,T_S4,length(List),false).
dnsUtil3_t(_L,_T,0,R) -> R;
dnsUtil3_t(Li,T_s4,Num,Re) ->
    [A|B] = Li,
    [S8] = string:tokens(A," "),
    Bool = textutils:foundIPAddress(T_s4,S8),
	if Bool ->
	    dnsUtil3_t(Li,T_s4,0,true);
	true ->
        dnsUtil3_t(B,T_s4,Num-1,Re)
    end.		

    
isDemo()->
	false.

demoPath() ->	
	Osnum = getOs(),
	case Osnum of
	1 ->
	    "c:/ChangeLog.txt";
	_ ->
        "/ChangeLog.txt"
    end.

is_wmi(Host)->
	case machine:getNTMachine(Host) of
		[]->
			false;
		[Mach|_]->
			Mach#machine.method == "WMI"
	end.
    
getSnmpMemory(Host,LastPageFaults,LastMeasurement) ->
    case snmp_machine:getMemoryFull(Host) of
        {ok, MemValues} ->
            Frees =
            case lists:keysearch(?MEMORYFREE, 1, MemValues) of
                {value, {?MEMORYFREE, Free}} ->
                    Free;
                _ ->
                    -1
            end,
            Sizes =
            case lists:keysearch(?MEMORYSIZE, 1, MemValues) of
                {value, {?MEMORYSIZE, Size}} ->
                    Size;
                _ ->
                    -1
            end,
            Useds =
            case lists:keysearch(?MEMORYUSED, 1, MemValues) of
                {value, {?MEMORYUSED, Used}} ->
                    Used;
                _ ->
                    -1
            end,
			Untis = 
			case lists:keysearch(?MEMORYUNTIS, 1, MemValues) of
                {value, {?MEMORYUNTIS, Unti}} ->
					First = hd(Unti),
					First#varbind.value;
                _ ->
                    1
            end,
            if
                Frees =:= -1 ->
                    Szs = [X#varbind.value||X<-Sizes, erlang:is_record(X, varbind)],
                    AllSize = lists:sum(Szs),
                    Uds = [X#varbind.value||X<-Useds, erlang:is_record(X, varbind)],
                    AllUsed = lists:sum(Uds),
                    UsedPer = 
					case AllSize =/= 0 of
						true -> (AllUsed/AllSize)*100;
						_ -> 0
					end,
                    Fr = AllSize - AllUsed,
                    {ok, [UsedPer, AllSize, Fr*Untis, 0]};
                Sizes =:= -1 ->
                    Uds = [X#varbind.value||X<-Useds, erlang:is_record(X, varbind)],
                    AllUsed = lists:sum(Uds),
                    Frs = [X#varbind.value||X<-Frees, erlang:is_record(X, varbind)],
                    AllFree = lists:sum(Frs),
					UsedPer = 
					case AllUsed+AllFree =/= 0 of
						true -> (AllUsed/(AllUsed+AllFree))*100;
						_ -> 0
					end,
                    {ok, [UsedPer, (AllUsed+AllFree)*Untis, AllFree, 0]};
                Useds =:= -1 ->
                    Szs = [X#varbind.value||X<-Sizes, erlang:is_record(X, varbind)],
                    AllSize = lists:sum(Szs),
                    Frs = [X#varbind.value||X<-Frees, erlang:is_record(X, varbind)],
                    AllFree = lists:sum(Frs),
					UsedPer = 
					case AllSize =/= 0 of
						true -> ((AllSize-AllFree)/AllSize)*100;
						_ -> 0
					end,
                    {ok, [UsedPer, AllSize*Untis, AllFree*Untis, 0]};
                true ->
                    Szs = [X#varbind.value||X<-Sizes, erlang:is_record(X, varbind)],
                    AllSize = lists:sum(Szs),
                    Uds = [X#varbind.value||X<-Useds, erlang:is_record(X, varbind)],
                    AllUsed = lists:sum(Uds),
					UsedPer = 
					case AllSize =/= 0 of
						true -> (AllUsed/AllSize)*100;
						_ -> 0
					end,
                    Fr = AllSize - AllUsed,
                    {ok, [UsedPer, AllSize, Fr, 0]}
            end;
        _ ->
            {error,"no data"}
    end.
    
    
    
 getSnmpDisk(Host,Disk) ->
    case snmp_machine:getDiskFull(Host) of
	{ok, DiskInfoS} ->
	%~ io:format("DiskInfoS ........ ~p~n",[DiskInfoS]),
            DiskInfo = 
            case lists:keysearch(Disk, 1, DiskInfoS) of
                {value, {Disk, Value}} ->
                    Value;
                _ ->
                    -1
            end,
	    %~ io:format("diskinfo ........ ~p~n",[DiskInfo]),
    
            DiskFrees =
            case lists:keysearch(?DISKFREE, 1, DiskInfo) of
                {value, {?DISKFREE, Free}} ->
                    Free;
                _ ->
                    -1
            end,
	    %~ io:format("DiskFrees ........ ~p~n",[DiskFrees]),
	    
            DiskTotal =
            case lists:keysearch(?DISKTOTAL, 1, DiskInfo) of
                {value, {?DISKTOTAL, Size}} ->
                    Size;
                _ ->
                    -1
            end,
	    
	    %~ io:format("DiskTotal ........ ~p~n",[DiskTotal]),
	    
	    
            DiskUseds =
            case lists:keysearch(?DISKUSED, 1, DiskInfo) of
                {value, {?DISKUSED, Used}} ->
                    Used;
                _ ->
                    -1
            end,
	    %~ io:format("DiskUseds ........ ~p~n",[DiskUseds]),
	    
	    DiskUntis = 
	    case lists:keysearch(?DISKPERCENTUSED, 1, DiskInfo) of
		{value, {?DISKPERCENTUSED, Unti}} ->
					First = hd(Unti),
					First#varbind.value;
		_ ->
		    1
	    end,
	    %~ io:format("DiskUntis ........ ~p~n",[DiskUntis]),
	    
            if
                DiskFrees =:= -1 ->
                    Szs = [X#varbind.value||X<- DiskTotal, erlang:is_record(X, varbind)],
                    AllSize = lists:sum(Szs),
                    Uds = [X#varbind.value||X<- DiskUseds, erlang:is_record(X, varbind)],
                    AllUsed = lists:sum(Uds),
                    UsedPer = 
					case AllSize =/= 0 of
						true -> (AllUsed/AllSize)*100;
						_ -> 0
					end,
                    Fr = AllSize - AllUsed,
                    [UsedPer, AllUsed*DiskUntis, AllSize*DiskUntis];
                DiskTotal =:= -1 ->
                    Uds = [X#varbind.value||X<- DiskUseds, erlang:is_record(X, varbind)],
                    AllUsed = lists:sum(Uds),
                    Frs = [X#varbind.value||X<- DiskFrees, erlang:is_record(X, varbind)],
                    AllFree = lists:sum(Frs),
					UsedPer = 
					case AllUsed+AllFree =/= 0 of
						true -> (AllUsed/(AllUsed+AllFree))*100;
						_ -> 0
					end,
                    [UsedPer, AllUsed*DiskUntis, (AllUsed+AllFree)*DiskUntis];
                DiskUseds =:= -1 ->
                    Szs = [X#varbind.value||X<- DiskTotal, erlang:is_record(X, varbind)],
                    AllSize = lists:sum(Szs),
                    Frs = [X#varbind.value||X<- DiskFrees, erlang:is_record(X, varbind)],
		    
		    
                    AllFree = lists:sum(Frs),
		    
		    AllUsed =  AllSize - AllFree,
					UsedPer = 
					case AllSize =/= 0 of
						true -> ((AllSize-AllFree)/AllSize)*100;
						_ -> 0
					end,
                    [UsedPer, AllUsed*DiskUntis, AllSize*DiskUntis];
                true ->
                    Szs = [X#varbind.value||X<- DiskFrees, erlang:is_record(X, varbind)],
                    AllSize = lists:sum(Szs),
                    Uds = [X#varbind.value||X<- DiskUseds, erlang:is_record(X, varbind)],
                    AllUsed = lists:sum(Uds),
		    UsedPer = 
					case AllSize =/= 0 of
						true -> (AllUsed/AllSize)*100;
						_ -> 0
					end,
                    Fr = AllSize - AllUsed,
                    [UsedPer, AllUsed*DiskUntis, AllSize*DiskUntis]
            end;
        _ ->
            {error,"no data"}
    end.

    

%host is string,Ip or domain name,
getMemoryFull(Host,LastPageFaults,LastMeasurement) ->
    case machine:getSnmpMachine(Host) of
        {ok, Ma=#machine{}} ->
            getSnmpMemory(Host,LastPageFaults,LastMeasurement);
        _ ->
	%%  Host1 =  case Host of
	%%		"\\\\"++_HOST ->
	%%			Host;
	%%		_ ->
	%%			"\\\\"++Host
	%%    end,
            OsNum = machine:getOS(Host),
            WMI = case OsNum of
            1 ->
                is_wmi(Host);
            _ ->
                false
            end,
            case WMI of
            true->
                [Mach|_] = machine:getNTMachine(Host),
                proxy:memory(OsNum, Host, Mach#machine.login, Mach#machine.passwd);
            _ ->
		
                %get localhost type
                Cmd = machine:getCommandString(memory,Host),
                if Host == "" ->
                    TMachine = "";
                true ->    
                    TMachine = machine:getMachine(Host)
                end,
                if length(TMachine) > 0 ->
                    [Machine|_] = TMachine;
                true ->
                    Machine = ""
                end,
                %case Cmd of
                %{ok,Val} ->
                    %Command = Val,
                    %String = sshcommandline:exec(Command,Machine,true);
                %_ ->
                %Command = memoryCommand(Host,OsNum),
                %case Command of
                %undefined ->
                    %String = ""; 
                %_ ->
                    %if OsNum == 1 ->
                    %String = os:cmd(Command);
                    %true ->
                    %String = sshcommandline:exec(Command,Machine,true)
                    %end
                %end            
                %end,
                %List = string:tokens(String,"\r\n"),
                case Cmd of
                {ok,Command} ->
                    {Status,DateList} = siteview_commandline:exec(Host,Command);
                _ ->
                    {Status,DateList} = siteview_commandline:exec(Host,memoryCommand(Host,OsNum))  
                end,
                case  Status of
                    ok ->
                        % Tid = init_lineReader(),
                        if length(DateList) == 0 ->
                            {error,"no data"};  
                        true ->
                            Error = [X||X<-DateList,string:str(X,"ERROR:")>0],
                            if length(Error) > 0 ->
                            [Err|_] = Error,
                            {error,Err};
                            true ->
                            Tid = init_lineReader(),
                            Md = disposalMemoryData(Host,Tid,DateList,OsNum,LastPageFaults,LastMeasurement),
                            ets:delete(Tid),
                            {ok,Md}   
                            end
                        end;
                _ ->
                    {error,"no data"} 
                end
            end
    end.


%% @spec getEventLogFull(Host,EventType,EventID) -> {}
%% @doc Get EventLog 
%% only windows platform
%% author: qicheng.ai@dragonflow.com
getEventLogFull(Host,EventType,EventID) ->
	Host1 = case Host of
				"\\\\"++_HOST ->
					Host;
				_ ->
					"\\\\"++Host
					%%Host
			end,
io:format("---------------------------------------------Update Host1: ~s~n",[Host1]),
	%%OsNum = machine:getOS(Host1),
	OsNum = 1,
	WMI = case OsNum of
		1 ->
			is_wmi(Host1);
		_ ->
			false
		end,
	case WMI of
		true ->
			[Mach|_] = machine:getNTMachine(Host1),
			proxy:filterEventLog(OsNum, Host, Mach#machine.login, Mach#machine.passwd, EventType, EventID)
%%		proxy:filterEventLog(1, Host, "administrator", "aqugadqdn", EventType, EventID).
	end.
	
%Da is os:cmd() result,	
disposalMemoryData(Host,T,Da,Os,LastPageFaults,LastMeasurement) ->
	case Os of
		1 ->
			[Str_PerfTime] = [string:strip(element(2,X),both,32)||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "PerfTime"],
			%%%%io:format("Str_PerfTime:~p~n",[Str_PerfTime]),          
			[Str_PerfFreq] = [string:strip(element(2,X),both,32)||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "PerfFreq"], 
			%%%%io:format("Str_PerfFreq:~p~n",[Str_PerfFreq]),
			[Str_26] = [element(1,list_to_tuple(string:tokens(element(2,X)," ")))||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "26"],
			%%%%io:format("Str_26:~p~n",[Str_26]),
			[Str_40] = [element(1,list_to_tuple(string:tokens(element(2,X)," ")))||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "40"],
			%%%%io:format("Str_40:~p~n",[Str_40]),
			[Str_1406_FRACTION] = [element(1,list_to_tuple(string:tokens(element(2,X)," ")))||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "1406",string:str(element(2,X),"FRACTION")>0],
			%%%%io:format("Str_1406_FRACTION:~p~n",[Str_1406_FRACTION]),           
			[Str_1406_BASE] =  [element(1,list_to_tuple(string:tokens(element(2,X)," ")))||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "1406",string:str(element(2,X),"BASE")>0],
			%%%%io:format("Str_1406_BASE:~p~n",[Str_1406_BASE]), 
			if Str_1406_BASE > 0 ->
				L4 = (list_to_integer(Str_26) * list_to_integer(Str_1406_BASE)) / list_to_integer(Str_1406_FRACTION);
			true ->
				L4 = -1
			end,
			AL0 = (100 * list_to_integer(Str_26))  / L4,
			AL1 = list_to_integer(Str_26),
			AL2 = L4,            
			if LastPageFaults ==0 ->
				AL3 = -1;
			true ->
				AL3 = list_to_integer(Str_40) - LastPageFaults
			end, 
			if  LastMeasurement == 0 ->
				AL4 = 0;
			true ->
				AL4 = list_to_integer(Str_PerfTime)	- LastMeasurement
			end,
			AL5 = list_to_integer(Str_40),
			AL6 = list_to_integer(Str_PerfTime),
			AL7 = list_to_integer(Str_PerfFreq),
			[AL0,AL1,AL2,AL3,AL4,AL5,AL6,AL7];
		_ ->
			OsaI = osAdapter:new(list_to_atom(machine:osToString(Os))),
			OsaI:initEts(),
			{_Type1,TotalN} = OsaI:getCommandSettingAsInteger(memory,total,-1),
			%%%%io:format("TotalN:~p~n",[TotalN]),
			{_Type2,FreeN} = OsaI:getCommandSettingAsInteger(memory,free,-1),
			{_Type3,UsedN}  = OsaI:getCommandSettingAsInteger(memory,used,-1),
			{_Type4,UsedPercentageN} = OsaI:getCommandSettingAsInteger(memory, usedPercentage,-1),
			{_Type5,SwapUnitN} = OsaI:getCommandSettingAsInteger(memory, swapUnit,1),            
			{_Type6,TotalMatchN} = OsaI:getCommandSetting(memory, totalMatch),
			{_Type7,FreeMatchN} = OsaI:getCommandSetting(memory, freeMatch),
			{_Type8,UsedMatchN} = OsaI:getCommandSetting(memory, usedMatch),
			{_Type9,StartLineN} = OsaI:getCommandSettingAsInteger(memory, startLine,-1),
			if  _Type5 == error ->
				SwapUnit = 512;
			true ->
				SwapUnit =SwapUnitN
			end,
			if StartLineN > 0 ->
				Date = lists:sublist(Da,StartLineN,length(Da)-StartLineN+1);
			true ->
				Date = Da
			end,            
			LineRea = lineReader:new(Date,machine:osToString(Os),T,memory),
			Flag = LineRea:processLine(),
			{L3,L5,L7,L9} = disposalMemoryData_util(LineRea,TotalN,TotalMatchN,SwapUnitN,FreeN,FreeMatchN,UsedN,UsedMatchN,UsedPercentageN,Flag),			
			%%L3 is total,L5 is free,L7 is used,L9 is Percent
			%io:format("L3:~p~nL5:~p~nL7:~p~nL9:~p~n",[L3,L5,L7,L9]),
			if  UsedPercentageN >= 0 ->
				EL9 = L9,
				EL3 = L3,    
				if L9 >= 0 ,L3 >= 0 ->
					EL7 = (L3 * L9) /100,
					EL5 = (L3 * (100 - L9)) / 100;
				true ->
					EL7 = L7,
					EL5 = L5
				end;
			true ->	
				%EL7 = L7,               
				%EL5 = L5,
				if  UsedN >= 0 ->  
					EL7 = L7,                
					if L5 >= 0,L7 >= 0 ->
						EL3 = L5 + L7;
					true ->
						EL3 = L3
					end;
				true ->
					EL3 = L3,                 
					if L5 >= 0,L3 >= 0 -> 
						EL7 = L3 - L5;
					true ->
						EL7 = L7
					end
				end,

				if EL3 /= 0 ->
					EL9 = (100 * EL7) / EL3;
				true ->
					EL9 = L9
				end
			end,
			if EL3 /= -1 ->
				AL0 = EL9, %PercentFull
				AL1 = EL7, %used
				AL2 = EL3, %total
				L13 = timeMillis(),
				L14 = pageFaults(Host),
				{_CmdType,S11} = OsaI:getCommandSetting(pageFault,units,"pages"),
				%%%io:format("CmdType:~p~nS11:~p~n",[_CmdType,S11]),
				Index = string:rstr(S11,"/sec"),
				LenS = length(S11),
				if Index == LenS - 3 -> 
					AL3 = L14,
					AL4 = 1000,
					AL5 = 0,
					AL6 = 0,
					AL7 = 1;
				true ->
					if LastPageFaults == 0 ->				
						AL3 = -1;
					true ->	
						AL3 = L14 - LastPageFaults
					end,
					if LastMeasurement == 0 ->
						AL4 = 0;
					true ->
						AL4 = L13 -LastMeasurement
					end,
					AL5 = L14,
					AL6 = L13,
					AL7 = 1000
				end;	
			true ->
				AL0 = -1,
				AL1 = 0,
				AL2 = 0,
				AL3 = 0,
				AL4 = 0,
				AL5 = 0,
				AL6 = 0,
				AL7 = 1 				
			end,                       
			[AL0,AL1,AL2,AL3,AL4,AL5,AL6,AL7]
		end.	

disposalMemoryData_util(LR,TotalN,TotalMatchN,SwapUnit,Free,FreeMatch,Used,UsedMatch,UsedPercentage,F) ->
	disposalMemoryData_util_t(LR,TotalN,TotalMatchN,SwapUnit,Free,FreeMatch,Used,UsedMatch,UsedPercentage,-1,-1,-1,-1,F).
disposalMemoryData_util_t(_L,_TN,_TMN,_SwapUnit,_FreeN,_FreeMatchN,_UsedN,_UsedMatchN,_UsedPercentageN,L3,L5,L7,L9,false) -> {L3,L5,L7,L9};
disposalMemoryData_util_t(Li,TotN,TotalMN,SwapUnitN,FreeN,FreeMatchN,UsedN,UsedMatchN,UsedPercentageN,TL3,TL5,TL7,TL9,Flag) ->
    Bool = Li:skipLine(),
	CurrentLine = Li:getCurrentLine(),
	%io:format("Memory CurrentLine:~p~n",[CurrentLine]),
    Bool1 = textutils:match(CurrentLine,TotalMN),
	if TotalMN == not_subkey ->
	    %if Bool1 ->
		    Bool2 = true;
		%true ->
        %    Bool2 = true
        %end;
    true ->
        if Bool1 ->
		    Bool2 = true;
		true ->
            Bool2 = false
        end
    end,
    %Len2 = length(FreeMatchN),
	Bool3 = textutils:match(CurrentLine,FreeMatchN),
    if FreeMatchN == not_subkey ->
        %if 	Bool3 ->
		    Bool4 = true;
		%true ->	
    	%    Bool4 = true
		%end;
    true ->
        if Bool3 ->
		    Bool4 = true;
		true ->
            Bool4 = false
        end
    end,
    %Len3 = length(UsedMatchN),
    
	Bool5 = textutils:match(CurrentLine,UsedMatchN),
    if UsedMatchN == not_subkey ->
        %if 	Bool5 ->
		    Bool6 = true;
		%true ->	
    	%    Bool6 = true
		%end;
    true ->
        if Bool5 ->
		    Bool6 = true;
		true ->
            Bool6 = false
        end
    end,
    %io:format("Memory  Bool:~p~n Bool2:~p~nBool4:~p~nBool6:~p~n",[Bool,Bool2,Bool4,Bool6]),	
	if Bool == false ->
	    if TotN >= 0,Bool2 ->
		    S7 = Li:readColumn(TotN,total),
            %io:format("Total:~p~n",[S7]),
			if length(S7) > 0 ->
                [Total] = S7,                       
			    if TL3 == -1 ->
				    _L3 = 0;
                true ->
                    _L3 = TL3
                end,
		%%%%%%%%%%%%% oldhand add
		case string:to_integer(Total) of
		      {error,_} -> L3 = _L3;
		      {Total_integer,_} -> 
			L3 = _L3 + Total_integer * SwapUnitN
		end;
		%%%%%%%%%%%%% lin lei history code
		%%%%%%%%%%L3 = _L3 + list_to_integer(Total) * SwapUnitN;
            true ->
                L3 = -1
            end;
        true ->
            L3 = -1
        end,
        if 	FreeN >= 0, Bool4 ->
            S8 = Li:readColumn(FreeN,free),
            %io:format("Free:~p~n",[S8]),
			if length(S8) > 0 ->
                [Free] = S8,            
			    if TL5 == -1 ->
				    _L5 = 0;
                true ->
                    _L5 = TL5
                end,
		%%%%%%%%%%%%% oldhand add
		case string:to_integer(textutils:readLong(Free,1)) of
		      {error,_} -> L5 = _L5;
		      {Free_integer,_} -> L5 = _L5 + Free_integer * SwapUnitN
		end;
		%%%%%%%%%%%%% lin lei history code
               %%% L5 = _L5 + list_to_integer(textutils:readLong(Free,1)) * SwapUnitN;
            true ->
                L5 = -1
            end;
        true ->
            L5 = -1
        end,
        if UsedN >= 0 ,Bool6 ->
            S9 = Li:readColumn(UsedN,used),
			if length(S9) > 0 ->
                [Used] = S9,
                %%%io:format("Memory Used:~p~n",[Used]),  
			    if TL7 == -1 ->
				    _L7 = 0;
                true ->
                    _L7 = TL7
                end,	
		%%%%%%%%%%%%% oldhand add
		case string:to_integer(textutils:readLong(Used,1)) of
		      {error,_} -> L7 = _L7;
		      {Used_integer,_} -> L7 = _L7 + Used_integer * SwapUnitN
		end;
		%%%%%%%%%%%%% lin lei history code		
               %% L7 = _L7 + list_to_integer(textutils:readLong(Used,1)) * SwapUnitN;
            true ->
                L7 = -1
            end;
        true ->
            L7 = -1
        end,            			
    	if  UsedPercentageN >= 0 ->
            S10 = Li:readColumn(UsedPercentageN,usedPercentage),
	    %%%%%%%%%%%%% oldhand add
	    
	    S11 = case string:to_integer(lists:flatten(S10)) of
		      {error,_} -> 0;
		      {S10_integer,_} -> S10_integer			
	    end,
	    %%%%%%%%%%%%% oldhand end	
	    %%%io:format("_________11_______~p~n",[{S11,S10}]),  
            if length(S10) > 0 ->
                %%%L9 = list_to_integer(S10);
		L9 = S11;
            true ->
                L9 = TL9
            end;
        true ->
          	L9 = TL9
        end,
		%%%io:format("________________~p~n",[L9]),  
		Flag2 = Li:processLine(),
		disposalMemoryData_util_t(Li,TotN,TotalMN,SwapUnitN,FreeN,FreeMatchN,UsedN,UsedMatchN,UsedPercentageN,L3,L5,L7,L9,Flag2);
	true ->
	    Flag2 = Li:processLine(),
        disposalMemoryData_util_t(Li,TotN,TotalMN,SwapUnitN,FreeN,FreeMatchN,UsedN,UsedMatchN,UsedPercentageN,TL3,TL5,TL7,TL9,Flag2)
    end.		
        		
timeMillis() ->						
	 timer:seconds(calendar:datetime_to_gregorian_seconds(erlang:localtime())).
	 
memoryCommand(Host,Osnum) ->    	
    case Osnum of
	    1 ->
		     " =4 ";
		2 ->
            "/usr/sbin/swap -l";	
        5 ->
            "/usr/sbin/swapinfo -d";
        3 ->
		    "/sbin/swap -l";
		6 ->
            "/usr/bin/free -b";		
        _ ->
            undefined 		
    end. 			

pageFaults(Host) ->
    %get remote host type
    OsNum = machine:getOS(Host),
	case machine:getCommandString(pageFault,Host,[]) of
	{error,Reason} ->
	    Command = pageFaultCommand(OsNum);			
	{ok,Cmd} ->
	    Command = Cmd
	end,
    %C = makePageFaultsCommand(Host,Command),    
	%Res = os:cmd(C),
    %Date = string:tokens(Res,"\n"),
    {Status,Date} = siteview_commandline:exec(Host,Command),
    [M|_] = dbcs_machine:get_machine_match("my.host=" ++ Host),
	OsaI = osAdapter:new(list_to_atom(M#machine.os)),
    OsaI:initEts(),
    {_OutPFType,K} = OsaI:getCommandSettingAsInteger(pageFault, outPageFaults,0),
	if K == 0 ->
	    {_InPFType,J} = OsaI:getCommandSettingAsInteger(pageFault,pageFaults,1);
	true ->
	    {_InPFType,J} = OsaI:getCommandSettingAsInteger(pageFault,inPageFaults,1) 
    end,
    {_SatrtLineType,StartLine} = OsaI:getCommandSettingAsInteger(pageFault,startLine),
    if _SatrtLineType == ok ->
        Tdate = lists:sublist(Date,StartLine,length(Date)-StartLine+1);
    true ->
        Tdate = Date
    end,
    {ok,S2} = OsaI:getCommandSetting(pageFault,units,"pages"),
    EndsWith =  string:tokens(S2,"/"),
    Temp = length(EndsWith),
    if Temp == 2 ->
        L1 = 1000;
    true ->
	    L1 = -1    
	end,
	Tid = init_lineReader(),
	LineReader = lineReader:new(Tdate,machine:osToString(OsNum),Tid,pageFault),
	L = pageFault_do_while(LineReader,J,L1,K),
	ets:delete(Tid),
	if S2 == "k/sec" ->
	    {_PageSizeType,PageSize} = OsaI:getCommandSetting(pageFault,pageSize),
		(L * 1024) / PageSize;
	true ->
        L
    end.		


   
pageFault_do_while(LineRea,J,L1,K) ->
    pageFault_do_while_t(LineRea,J,L1,K,1,-1).
pageFault_do_while_t(LR,_J,_L1,_K,0,L) -> L;
pageFault_do_while_t(LineR,JT,TL1,TK,Num,RL) ->  	
    Bool1 = LineR:processLine(),
	Bool2 = LineR:skipLine(),
	if Bool1 /= true  ->
       pageFault_do_while_t(LineR,JT,TL1,TK,0,RL);
	true ->
       	if Bool2 == false ->
		    [S3] = LineR:readColumn(JT,pageFaults),
            %%%%io:format("~nS3:~p~n",[S3]),
			if RL == -1 ->
			    Tl =0;
			true ->
                Tl = RL
            end,				
			L = Tl +list_to_integer(S3) * TL1,
		    if 	TK /= 0 ->
		        [S4] = LineR:readColumn(TK,secondPageFaultsColumn),
				if RL == -1 ->
				    LL = 0;
				true ->
                    LL = L
                end,
                EL = LL + list_to_integer(S4) * TL1;
            true ->
                EL =L
            end,
			pageFault_do_while_t(LineR,JT,TL1,TK,Num,EL);
        true ->			
			pageFault_do_while_t(LineR,JT,TL1,TK,Num,RL)
		end
    end.
	
%just use ssh
makePageFaultsCommand(Host,Cmd) ->
        sslCommandLine(Host) ++ " " ++ Cmd.	
	      
pageFaultCommand(Osnum) ->
    case Osnum of
	5 ->
	     "/usr/bin/vmstat -s";
    6 ->
        "/usr/bin/vmstat -n 3 2";
    2 ->
        "/usr/bin/vmstat -s";
	3 ->
        "/usr/bin/sar -p 3";
    _ ->
        undefined
    end.		

	
getDirectoryPath(String1,String2) ->
    getDirectoryPathImplementation(String1,String2,null,false).
	
getUsedDirectoryPath(String1,String2) ->
    getDirectoryPathImplementation(String1,String2,null,true). 


getDirectoryPathImplementation(String1,String2,HTTPRequest,Flag) ->
    Bool = isStandardAccount(String2),
	OS = getOs(),
	case OS of
	1 ->
	    Separator = "\\";
	_ ->
        Separator = "/"
    end,		
	DIR = getRoot() ++  Separator ++ "accounts" ++ Separator ++ String2 ++ Separator ++ String1,
	EXISTS = filelib:is_file(DIR),
    if  Bool /= true ->
	    if Flag, EXISTS /= true ->
            Path = getRoot() ++ Separator ++ String1;
        true ->
            Path = DIR
	    end;
    true ->
        Path = 	getRoot() ++ Separator ++ String1
	end.	
	
getProcesses(Host) ->
    getProcesses(Host, false).
	
getProcesses(Host,Flag) ->
    readProcessList(Host,false,Flag).	

processList() ->
    readProcessList("",false,false).

readProcessList(Host) ->	
	readProcessList(Host,false).
	
readProcessList(Host,Flag) ->
    readProcessList(Host,Flag,false).

	
%%return {Flag,List},like {1,[a,b,c]},parameter	Host is like "\\\\192.168.0.1" or "192.168.0.1"  if Flag1 is true,Command id is serviceMonitor,if Flag is true  use psDetailCommand()
readProcessList(Host,Flag,Flag1) ->
	if Host == "" ->
		   Osnum = getOs(),
		   if Osnum =/= 1 ->            
				  Os_Name_List = "redhatenterpriselinux";
			  true ->
				  Os_Name_List = "nt" 
		   end,  
		   OsaI = osAdapter:new(list_to_atom(Os_Name_List)),
		   OsaI:initEts(),
		   if Flag1 ->            
				  S2 = serviceMonitor,
				  S = OsaI:getCommandString(S2),
				  case S of
					  {ok,V} ->
						  Cmdid = S2,
						  S1 = V;
					  _ ->
						  if Flag ->
								 Cmdid = processDetail,
								 T1 = OsaI:getCommandString(processDetail),
								 case T1 of
									 {ok,Val1} ->
										 S1 = Val1;
									 _ ->
										 S1 = psDetailCommand(Host,Osnum)
								 end;					
							 true ->
								 Cmdid = process,
								 T1 = OsaI:getCommandString(process),
								 case T1 of
									 {ok,Val} ->
										 S1 = Val;
									 _ ->
										 S1 = psCommand(Host,Osnum)
								 end					
						  end
				  end;
			  true ->
				  if Flag ->
						 Cmdid = processDetail,
						 T1 = OsaI:getCommandString(processDetail),
						 case T1 of
							 {ok,Val1} ->
								 S1 = Val1;
							 _ ->
								 S1 = psDetailCommand(Host,Osnum)
						 end;					
					 true ->
						 Cmdid = process,
						 T1 = OsaI:getCommandString(process),
						 case T1 of
							 {ok,Val} ->
								 S1 = Val;
							 _ ->
								 S1 = psCommand(Host,Osnum)
						 end					
				  end
		   end,
		   T = string:str(Host,"\\\\"),
		   if T > 0 ->
				  S3 = lists:sublist(Host,3,length(Host)-2);		
			  true ->
				  S3 = Host
		   end,
		   
		   F2 = machine:isNTSSH(Host),
		   F3 = Osnum =:= 1,
		   Machine = machine:getNTMachine(Host), %Machine is list
		   case Machine of
			   [_|_] ->
				F1 = true;
			   _ ->
				F1 = F3
		   end,
		   case {F1,F2} of
			{true, true} ->
				  Flag2 = true,
				  T3 = string:str(S1,"\\\\" ++ S3),
				  if T3 > 0 ->
						 TS1 = textutils:replaceString(S1,"\\\\"++S3,""),
						 TS2 = string:substr(TS1,string:str(TS1,"perfex"));
					 true ->
						 TS1 = S1,
						 TS2 = string:substr(TS1,string:str(TS1,"perfex"))
				  end,
				  [M] = Machine,		
				  %Res = sshcommandline:exec(TS2,M,false);
				  {Status,TArray} = siteview_commandline:exec(Host,TS2);
			 {true, _} ->
				Flag2 = false,
				case F3 of
					true->
						case Machine of
							[M]->
								ServerHost = Host,
								Login = M#machine.login,
								Password = M#machine.passwd;
							_ ->
								ServerHost = "127.0.0.1",
								Login = "administrator",
								Password = "1"
						end,
						TArray = case proxy:service(Osnum, ServerHost, Login, Password) of
									{ok, Services}->
										{length(Services), Services};
									_ ->
										{1, []}
							end;
								
					_ ->
						Cmd  =  S1,
						{Status,TArray} = siteview_commandline:exec(Host,Cmd)
				end;	
			  _ ->
				  Flag2 = false,
				  Cmd  =  S1,
				  {Status,TArray} = siteview_commandline:exec(Host,Cmd)
		   %Res = os:cmd(Cmd)	
		   end,
		   case Osnum  of
			   1 ->
				   if F1, F3 ->
					   TArray;
				    true ->
					   J = 0,
					   Array = disposeNtProcData(TArray,Flag2),
					   {length(Array),Array}
				   end;
			   _ ->
				   Len = length(TArray),
				   if Len > 0 ->
						  OsaI = osAdapter:new(list_to_atom(Os_Name_List)),
						  case  OsaI:initEts() of
							  {error,Reason} ->
								  J = 2,
								  Array = [];
							  _ ->
								  Tid = init_lineReader(),
								  LineR = lineReader:new(TArray,Os_Name_List,Tid,Cmdid),
								  Flag3= LineR:processLine(),
								  Array = readProcessList_util(Flag,LineR,Flag3),
								  ets:delete(Tid),
								  {length(Array),Array}
						  end;
					  true ->
						  Array = {1,[]}
				   end    		
		   end;
	   true ->        
		   Machi = dbcs_machine:get_machine_match("my.host=" ++ Host),
		   %Machi = machine:getNTMachine(Host),
		   case  Machi of
			   [] ->
				   {error,machine_undefined};
			   [_|_] ->	
				   OsaI = machine:getAdapter(Host),
				   Osnum = machine:getOS(Host),
				   if Flag1 ->
						  S2 = serviceMonitor,
						  S = OsaI:getCommandString(S2),
						  case S of
							  {ok,V} ->
								  Cmdid = S2,
								  S1 = V;
							  _ ->
								  if Flag ->
										 Cmdid = processDetail,
										 T1 = OsaI:getCommandString(processDetail),
										 case T1 of
											 {ok,Val1} ->
												 S1 = Val1;
											 _ ->
												 S1 = psDetailCommand(Host,Osnum)
										 end;					
									 true ->
										 Cmdid = process,
										 T1 = OsaI:getCommandString(process),
										 case T1 of
											 {ok,Val} ->
												 S1 = Val;
											 _ ->
												 S1 = psCommand(Host,Osnum)
										 end					
								  end
						  end;
					  true ->
						  if Flag ->
								 Cmdid = processDetail,
								 T1 = OsaI:getCommandString(processDetail),
								 case T1 of
									 {ok,Val1} ->
										 S1 = Val1;
									 _ ->
										 S1 = psDetailCommand(Host,Osnum)
								 end;					
							 true ->
								 Cmdid = process,
								 T1 = OsaI:getCommandString(process),
								 case T1 of
									 {ok,Val} ->
										 S1 = Val;
									 _ ->
										 S1 = psCommand(Host,Osnum)
								 end					
						  end
				   end,            
				   T = string:str(Host,"\\\\"),
				   if T > 0 ->
						  S3 = lists:sublist(Host,3,length(Host)-2);		
					  true ->
						  S3 = Host
				   end,
				   Machine = machine:getNTMachine(Host), %Machine is list
				   case Machine of
					   [_|_] ->
						   F1 = true;
					   _ ->
						   F1 = false
				   end,
				   F2 = machine:isNTSSH(Host),
				   F3 = machine:isNTWMI(Host),
				   case {F1,F2} of
					{true, true} ->
						  Flag2 = true,
						  T3 = string:str(S1,"\\\\" ++ S3),
						  if T3 > 0 ->
								 TS1 = textutils:replaceString(S1,"\\\\"++S3,""),
								 TS2 = string:substr(TS1,string:str(TS1,"perfex"));
							 true ->
								 TS1 = S1,
								 TS2 = string:substr(TS1,string:str(TS1,"perfex"))
						  end,
						  [M] = Machine,		
						  %Res = sshcommandline:exec(TS2,M,false);
						  {Status,TArray} = siteview_commandline:exec(Host,TS2);
					{true, _} ->
						Flag2 = false,
						case F3 of
							true->
								[M] = Machine,	
								TArray = case proxy:service(Osnum, Host, M#machine.login, M#machine.passwd) of
									{ok, Services}->
										{length(Services), Services};
									_ ->
										{1, []}
								end;
							_ ->
								 MachineUnix = dbcs_machine:get_machine_match("my.host=" ++ Host ),
								%%%%io:format("MachineUnix:~p~n",[MachineUnix]),
								  Flag2 = false,
								  [M|_] = MachineUnix,
								  if M#machine.method == "SSH" ->
										 io:format("S1:~p~n",[S1]),
										 %Res  = sshcommandline:exec(S1,M,false);
										 {Status,TArray} = siteview_commandline:exec(Host,S1);
									 true ->
										 Cmd  =  S1,
										 {Status,TArray} = siteview_commandline:exec(Host,Cmd)
								  %Res = os:cmd(Cmd)				
								  end
						end;	
					  _ ->
						  MachineUnix = dbcs_machine:get_machine_match("my.host=" ++ Host ),
						%%%%io:format("MachineUnix:~p~n",[MachineUnix]),
						  Flag2 = false,
						  [M|_] = MachineUnix,
						  if M#machine.method == "SSH" ->
								 io:format("S1:~p~n",[S1]),
								 %Res  = sshcommandline:exec(S1,M,false);
								 {Status,TArray} = siteview_commandline:exec(Host,S1);
							 true ->
								 Cmd  =  S1,
								 {Status,TArray} = siteview_commandline:exec(Host,Cmd)
						  %Res = os:cmd(Cmd)				
						  end
				   end,
				   %TArray = string:tokens(Res,"\r\n"),
				   case Osnum  of
					   1 ->
					           if F1, F3 ->
							   TArray;
						    true ->
							   J = 0,
							   Array = disposeNtProcData(TArray,Flag2),
							   {length(Array),Array}
						   end;         
					   _ ->
						   Len = length(TArray),
						   if Len > 0 ->                    
								  OsaI = machine:getAdapter(Host),
								  case  OsaI of
									  {error,Reason} ->
										  J = 2,
										  Array = [];
									  _ ->
										  Tid = init_lineReader(),
										  
										  LineR = lineReader:new(TArray,machine:osToString(Osnum),Tid,Cmdid),
										
										  %%io:format("===ets:tab2list======~p~n",[ets:tab2list(Tid)]),
										  Flag3= LineR:processLine(),									
										  Array = readProcessList_util(Flag,LineR,Flag3),
										  ets:delete(Tid),
										  {length(Array),Array}
								  end;
							  true ->
								  Array = {1,[]}
						   end    		
				   end;					
			   _ ->
				   {error,undefined}	
		   end	
	end.	
    	
disposeNtProcData(List,Flag) ->
    disposeNtProcData_t(List,Flag,length(List),[]).
disposeNtProcData_t(_L,_F,0,R) -> R;
disposeNtProcData_t(Li,Fa,Num,Re) ->
    [A|B] = Li,
    T1 = string:str(A,"end perfex"),
    T5 = string:str(A,"Connected"),
    T2 = string:str(A,"Copyright"),
    T3 = string:str(A,"Microsoft Windows"),
    T4 = string:str(A,"-s"),
    	
	if T1 == 0,T2 == 0,T3 == 0 ,T5 == 0->
	    if Fa /= true ->
		    disposeNtProcData_t(B,Fa,Num-1,lists:append(Re,[A]));
		true ->
            if T4 ->
                disposeNtProcData_t(B,Fa,Num-1,lists:append(Re,[A]));
            true ->
                disposeNtProcData_t(B,Fa,Num-1,Re)
            end
        end;
    true ->
        disposeNtProcData_t(B,Fa,Num-1,Re)
    end.		
	
	
readProcessList_util(Tf,LineReader,Flag) ->
    readProcessList_util_t(Tf,LineReader,Flag,[]).
readProcessList_util_t(F,_L,false,R) -> R; 
readProcessList_util_t(TF,Lr,F,Re) ->
    Bool1 = Lr:processLine(),    
    if Bool1 /= true  ->
        readProcessList_util_t(TF,Lr,false,Re);
    true ->       
	case Lr:checkhead() of
	   true ->
	        readProcessList_util_t(TF,Lr,F,Re);
	   _ ->
		TS5 = Lr:readColumnByName("name"),
		Index = string:str(TS5,"<defunct>"),		
		    if Index  == 0 ,TF ->
		     
			    S5 =  Lr:readColumnByName("size") ++ ","++ TS5;
			true ->
		    S5 = TS5
			end,
		
		case string:strip(S5) of
		     [] -> readProcessList_util_t(TF,Lr,F,Re);
		     Strip -> readProcessList_util_t(TF,Lr,Bool1,lists:append(Re,[Strip]))
		end
		
	end
    end.		
	 
psDetailCommand(_Host,Osnum) ->
    %%%%io:format("Host:~p~nOsnum:~p~n",[_Host,Osnum]),
    case Osnum of
    1 ->
        "  =230";
    6 ->
        "/bin/ps -el";
    2 ->
        "/usr/bin/ps -el";
    3 ->
        "/usr/bin/ps -el";
    5 ->
        "/usr/bin/ps -el";
    _ ->
	    "ps -el"
    end. 

psCommand(Host,Osnum) ->
    case Osnum of
    1 ->
        "  -s";
    2 ->
        "/usr/bin/ps -ef";
	6 ->
        "/bin/ps -ef";
    3 ->
        "/usr/bin/ps -eo args";
    5 ->
        "/usr/bin/ps -ef";
    _ ->
        "ps -ef"
    end.		

psCommand_flag(Host,Flag) ->
    Osnum = machine:getOS(Host),
	if Flag ->
	    S1 = psDetailCommand(Host,Osnum);
	true ->
        S1 =  psCommand(Host,Osnum)
    end,
    if Flag ->
        S2 = "processDetail";
    true ->
        S2 = "process"
    end,
    S3 = machine:getCommandString(S2, Host),
    case S3 of
	{error,Res} ->
	    S1;
	_ ->	
	    Len = length(S3),
        if Len > 0 ->
            S3;
        true ->
            S1
        end
    end.		

checkProcess(Server,Host,L) ->
    checkProcess(Server,Host,L,false).
    
    
checkProcess(Server,Host,L,Flag) ->
    checkProcess(Server,Host,L,Flag,null,null).


checkProcess(Server1,Host,L,Flag,AtomicMonitor,Array) ->
	Server = string:strip(Server1),
	T = machine:getOS(Host),
	case T of
	1 ->
	    Flag1 = true;
	_ ->
        Flag1 = false
    end,
    TS = string:to_lower(Server),
    Flag2 = L /= 0,

    Flag3 = case Flag1 of
	true->
		case Host of
			[]->
				true;
			_ ->
				machine:isNTWMI(Host)
		end;
	_ ->
		false
    end,

    case Flag3 of
	true->
		case Host of
			[]->
				proxy:service(T, "127.0.0.1", "administrator", "1", Server);
			_ ->
				[Mach|_] = machine:getNTMachine(Host),
				proxy:service(T, Host, Mach#machine.login, Mach#machine.passwd, Server)
		end;
	_ ->
		{Num,List} = readProcessList(Host,Flag2,Flag),
	    %%io:format("Flag2:~p~nFlag:~p~n",[Flag2,Flag]),
	    %%io:format("Num:~p~nList:~p~n",[Num,List]),
	    if Num == error ->
		[0,0,-1];
	    true ->
		    Flag4 = textutils:isRegularExpression(Server),
		%io:format("&&&&&&&&&&&&&Flag4:~p~n",[Flag4]),
		    {Long1,Long2} = checkProcess_util(L,0,List,Flag1,Flag2,Flag4,Server),
		    [Long1,Num,Long2]
	    end
    end.


%parameter is list of string,Flag is BOOL type	
checkProcess_util(L,L1,List,F1,F2,F3,S) ->
    checkProcess_util_t(L,L1,List,F1,F2,F3,S,length(List),{false,false}).
checkProcess_util_t(_L,_L1,_Li,_Fl,_F2,_F3,Str,0,R) ->	{_L1,R};
checkProcess_util_t(TL,TL1,Li,Flag1,Flag2,Flag3,St,N,Re) ->
    [A|B] = Li,
	S2 = string:to_lower(A),
	if Flag1 ->
	    Flag4 = S2 == St;
    true ->
        Flag4 = string:str(A,St) > 0
    end,
    if Flag4 ->
	    _TL1 =  TL1 + 1,
	    if Flag1 ->
		    checkProcess_util_t(TL,_TL1,Li,Flag1,Flag2,Flag3,St,0,Re); 
		true ->
		    if Flag2 ->
			    List = string:tokens(A,","),
				Len = length(List),
				if Len > 0 ->
				    [TS3] = lists:sublist(List,1,1),
					[S3] = string:tokens(TS3," "),
					if  TL == -2 ->
						Bool1 = textutils:endsWithIgnoreCase(S3,"k"),
						Bool2 = textutils:endsWithIgnoreCase(S3,"m"),
						Bool3 = textutils:endsWithIgnoreCase(S3,"g"),
                        if Bool1 ->
                            TTL1 = 1024;
                        true ->
                            if Bool2 ->
                                TTL1 = 16#100000;
                            true ->
                                if Bool3 ->
                                    TTL1 = 16#40000000;
                                true ->
                                    TTL1 = 1
                                end
                            end  
                		end;
						
                    true ->	
                        TTL1 = TL
                    end,
					F = list_to_integer(S3) * TTL1,
					L2 = F;
                true ->
                    L2 = -1
				end;
            true ->
                L2 = 1
            end,	
        checkProcess_util_t(TL,_TL1,B,Flag1,Flag2,Flag3,St,N-1,L2)		
        end;        	
	true ->
        checkProcess_util_t(TL,TL1,B,Flag1,Flag2,Flag3,St,N-1,Re)
    end.		
	    
processOK(String) ->
    processOK(String,"").

% String1 is PID,String2 is Host
processOK(String1,String2) ->	
    S2 = "/bin/ps -ef",
    %String = sshcommandline:exec(S2,String2,monitorLock),	
    %List = string:tokens(String,"\r\n"),
    {Statu,List} = siteview_commandline:exec(String2,String1),
    case Statu of
    ok ->
        ProcessList = List;
    _ ->
        ProcessList = [] 
    end, 
    {S,N} = processOK_util(String1,ProcessList),	
    if N >= 10 ->
	    "defunct";
	true ->
        S
    end.		
   
processOK_util(PID,List) ->
    processOK_util_t(PID,List,length(List),"not running",0).
processOK_util_t(_Id,_L,0,Str,Num) -> {Str,Num};
processOK_util_t(ID,L,N,S,Nu) ->
    [A|B] = L,
    S1 = string:tokens(A," "),
    S5 = lists:sublist(S1,3),%PPID
    Bool1 =  S5 == ID,
    Bool2 = string:str(A,"defunct") > 0,
    if Bool1,Bool2 ->
        Tnu = Nu + 1,
        S6 =  lists:sublist(S1,2),	%PID
        if S6 == ID ->
            S3 = "";
        true ->
            S3 = "not running"
        end,
		processOK_util_t(ID,B,N-1,S3,Tnu);
    true ->
        processOK_util_t(ID,B,N-1,S,Nu)
    end.		
	
	

cpuCommand(H,Os) ->
    case Os of
	1 ->
        " =238";
    2 ->
        "/usr/bin/mpstat 3 2";
    6 ->
        "top p 0 n 3 d 2 b";
    5 ->
        "/usr/sbin/sar 3";
    3 ->
        "/usr/bin/sar 3";
    9 ->
        "/usr/bin/top n 1 d 2 b -p 1";
    _ ->
        ""
    end.		
	
%return Bool    
isRemote(Host) ->
    isNTRemote(Host).
 

%just matching "remote:"
%isCommandLineRemote(Host) ->
    
%DirTy is number
dirCommand(Osnum,DirTy) ->
    case Osnum of
	1 ->
	    D = "dir  /B",
		case DirTy of
		1 ->
            Dirs = D ++ "  /AD";
        _ ->
            Dirs = D ++ "  /A-D"
        end;
    _ ->
        D = "ls -A -1 -p"
	end.	

%dirCommand,DirTy is number
dirCommandByHost(Host,DirTy) ->
    Cmd = machine:getCommandString(directory,Host),
    case Cmd of
	{ok,Cmd} ->
	    Cmd;
	{error,_Re} ->
        dirCommand(machine:getOS(Host),DirTy)
    end.		

currentDirectoryCommand(Host) ->
    Cmd = machine:getCommandString(currentDirectory,Host),
	case Cmd of
	{ok,Cmd} ->
	    Cmd;
	{error,_Re} ->
        currentDirectoryCommandByOs(machine:getOS(Host))
    end.

currentDirectoryCommandByOs(Osnum) ->
    case Osnum of
	1 ->
	    Cmd = "cd";
	_ ->
        Cmd = "pwd"
    end.


processCPU(Host,Key) ->
    processCPU(Host,Key,null,null).

%get NT machine process CPU
processCPU(Host,Key,Atomicmonitor, Array) ->
	Osnum = getOs(),
    Cmd =  perfexCommand(Host) ++ " -pc" ++ " =230",
    String = os:cmd(Cmd),        
    List = string:tokens(String,"\r\n"),          
    [A|B] = List,
	{L,L1,A1,A2,A3} = processCPU_util(-1,0,0,0,0,List,Key,false),
	if L1 =< 0 ->
		TL1 = 1;
	true ->
        TL1 = L1
    end,
    P = L * TL1,
    [P,A1,A2,A3]. 		
		
processCPU_util(L,L1,A1,A2,A3,List,Key,Flag) ->
    processCPU_util_t(L,L1,A1,A2,A3,List,Key,Flag,length(List)).
processCPU_util_t(_L,_L1,_A1,_A2,_A3,_Li,_K,_F,0) -> {_L,_L1,_A1,_A2,_A3};
processCPU_util_t(FL,FL1,AL1,AL2,AL3,_List,K,F,N) ->      
    [A|B] = _List,
    Bool1 = (string:str(A,"PerfTime100nSec:") > 0),
    Bool2 = (string:str(A,"processorCount:") > 0),
    Bool3 = (string:str(A,"name: ") > 0),
    if FL<0,Bool1 ->
        List = string:tokens(A," "),
        [String] = lists:sublist(List,length(List),1),	
        L = list_to_integer(String),
		L1 = 0;
	true ->
	    L = FL,
        if Bool2 ->
            List = string:tokens(A," "),
            [String] = lists:sublist(List,length(List),1),	
            L1 = list_to_integer(String);
		true ->
            L1 = FL1
        end
    end,		
    if Bool3 ->
        S5 = string:substr(A,7),
        TS5 = string:to_lower(S5),
		TK = string:to_lower(K),
		if TS5 ==  TK ->       
		    Flag = true;
        true ->
            Flag = false
        end,
	    processCPU_util_t(L,L1,AL1,AL2,AL3,B,K,Flag,N-1);	
    true ->
	    Len = length(A),
        if Len == 0 ->
            Flag = false,
		    TAL1 = AL1,
            TAL2 = AL2,
            TAL3 = AL3; 
        true ->
		    Flag = F,
            if Flag ->
			    Bool4 = string:str(A,"184:") > 0,
			    Bool5 = (string:str(A,"PERF_COUNTER_RAWCOUNT") > 0),
				if Bool4 ->
				    if Bool5 ->							
                        [TL2] = lists:sublist(string:tokens(A," "),length(string:tokens(A," "))-1,1),                      
					    L2 = list_to_integer(TL2);
					true ->			
                        [TL2] = lists:sublist(string:tokens(A," "),length(string:tokens(A," "))-1,1),                       
						L2 = list_to_integer(TL2)
                    end,
                    TAL3 = AL3 + L2;
                true ->  							
                    TAL3 = AL3
			    end,
				Bool6 = string:str(A,"6:") > 0 ,
				Bool7 = string:str(A, "% PERF_100NSEC_TIMER") > 0,
			    if Bool6 ->
				    if Bool7 ->
                         
                         [TL3] = lists:sublist(string:tokens(A," "),length(string:tokens(A," "))-2,1),					
					     L3 = list_to_integer(TL3),
					     TAL1 = AL1 + L3,
					     TAL2 = AL2 + 1;
					true ->
                        TAL1 = AL1,
                        TAL2 = AL2
                    end;						
				true ->
                    TAL1 = AL1,
                    TAL2 = AL2
                end;
				
            true ->
				TAL1 = AL1,
                TAL2 = AL2,
                TAL3 = AL3                				
            end
		
        end,	
	processCPU_util_t(L,L1,TAL1,TAL2,TAL3,B,K,Flag,N-1)	
    end.		
                            							
%return list,TL,TL1 is number
processUsed(Host,ProcessName,TL,TL1) ->
    OsType = machine:getOS(Host),  
    WMI = case OsType of
        1 ->
 	    machine:isNTWMI(Host);
	_ ->
	    false
     end,
	io:format("Mach:~p~n", [WMI]),
     case WMI of
	true ->
		[Mach|_] = machine:getNTMachine(Host),
		io:format("Mach:~p~n", [OsType]),
		io:format("Mach:~p~n", [Host]),
		io:format("Mach:~p~n", [Mach]),
		proxy:process(OsType, Host, Mach#machine.login, Mach#machine.passwd, ProcessName);
	_ ->
	    if TL == 0 ->
		AL =  processCPU(Host,ProcessName,null,null),
		[L] =  lists:sublist(AL,1,1),
		[L1] = lists:sublist(AL,2,1),
		timer:sleep(2000);
	    true ->
		    L = TL,
			L1 = TL1
		end,	
	    AL1 = processCPU(Host,ProcessName,null,null),
	    [L3] =  lists:sublist(AL1,1,1),
	    [L4] = lists:sublist(AL1,2,1),
	    [L5] = lists:sublist(AL1,3,1),
	    [L6] = lists:sublist(AL1,4,1),
		L7 = L3 - L,
	    if L7 > 0 ->
		L2 = (100 * (L4 - L1)) / L7,
		_L = L,
		_L1 = L1,
		_L3 = L3,
		_L4 = L4,
		_L5 = L5,
		_L6 = L6;		
	    true ->
		_L = L3,
		    _L1 = L4,
			timer:sleep(2000),
			AL2 = processCPU(Host,ProcessName,null,null),
			[_L3] = lists:sublist(AL2,1,1),
			[_L4] = lists:sublist(AL2,2,1),
		    [_L5] = lists:sublist(AL2,3,1), 
			[_L6] = lists:sublist(AL2,4,1),
			L8 = _L3 - _L,
			if L8 > 0 ->
			    L2 = 100 - (100 * (_L4 - _L1)) / L8;
			true ->
		    L2 = 0
		end
	    end,
	    if L2 < 0 ->
		_L2 = 0;
	    true ->
		_L2 = L2
	    end,
	    [_L2,_L5,_L3,_L4,_L6]
    end.

timeZoneName(TimeZone) ->
    List =   timeZones(),
	TZones = [element(2,X)||X<-List,element(1,X)==TimeZone],
    case TZones of
    [] ->
        "";
    [TZ] ->
        J = (25200 - TimeZone)	/ 3600,
		if J < 0 ->
            M = -J,
            Res = TZ ++ ", "++"GMT +" ++ integer_to_list(TimeZone) ++ ":00";
        true ->
            if J > 0 ->
                Res = TZ ++ ", "++"GMT -" ++ integer_to_list(TimeZone) ++ ":00";	
            true ->
             	Res = TZ ++ ", "++"GMT"
            end
        end;
	_ ->
        {error,undefined}
    end.		
		

timeZones() ->
   [
       {72000, ""},
	   {68400, "Auckland"},
	   {64800, ""},
	   {61200, "Melbourne, Sydney"},
	   {57600, "Tokyo, Seoul"},
	   {54000, "Hong Kong, Perth"},
	   {50400, "Bangkok, Jakarta"},
	   {46800, "Dhaka"},
	   {43200, ""},
	   {39600, ""},
	   {36000, "Moscow"},
	   {32400, "Helsinki, Eastern Europe, Israel"},
	   {28800, "Paris, Berlin, Stockholm, Amsterdam"},
	   {25200, "London"},
	   {21600, ""},
	   {18000, "Mid-Atlantic"},
	   {14400, "Brasilia, Buenos Aires"},
	   {10800, "Atlantic Time"},
	   {7200, "Eastern Time"},
	   {3600, "Central Time"},
	   {0, "Mountain Time"},
	   {-3600, "Pacific Time"},
	   {-7200, "Alaska"},
	   {-10800, "Hawaii"},
	   {-14400, ""},
	   {-18000, ""},
	   {-21600, ""}
   ].


%
processorCPU(Host,I,Osnum,Atomicmonitor,Array) ->   
    K = 3 + I,
    Cmd = cpuCommand(Host,Osnum),
    AList = lists:duplicate(K,-1),
    %Index1 =  string:str(Host,"\\\\"),
	%if Index1 > 0 ->
	%    S2 = string:substr(Host,3);
	%true ->
    %    S2 = Host
    %end,
    %if Host == "" ->
        %Machine = "";
    %true ->    
        %Machine = machine:getMachine(Host)
    %end,    
    if Osnum == 1 ->
        %String = os:cmd(Cmd);
        {Status,ResultL} = siteview_commandline:exec(Host,Cmd),
        case Status of
        ok ->
            Array1 =  ResultL;
        _ ->
            Array1 = []   
        end; 
    true ->
        %String = sshcommandline:exec(Cmd,Machine,true)
        {Status,ResultL} = siteview_commandline:exec(Host,Cmd),
        case Status of
        ok ->
            Array1 =  ResultL;
        _ ->
            Array1 = []   
        end         
    end,
	%Array1 =  string:tokens(String,"\r\n"),
		%if Atomicmonitor /= unll ->
		%    S3 = " monitor: " + Atomicmonitor:getFullID();
		%true ->
        %    S3 = ""
        %end,
    {L1,TL2,L3,AL} = processorCPU_util(Array1,AList), %[ ?,?,?,List]
    %%%%%io:format("L1:~p~nL2:~p~nL3~p~nAL~p~n",[L1,L2,L3,AL]),
    case is_integer(TL2) of
    true ->
        L2 = TL2;
    _ ->
        L2 = list_to_integer(TL2)
    end,
    AL0 =  L2 * L3, 
	AL1 =  L1,
	AL2 =  L3,
	Rec = lists:append([AL0,AL1,AL2],AL),
	Len2 = length(Rec),
	if Len2 < K ->
		lists:append(Rec,lists:duplicate(K-Len2,-1));
	true ->
        Rec
    end.		
	    
    	

processorCPU_util(List,AList) ->
    processorCPU_util(List,length(AList),length(List),-1,-1,0,false,0,[]).
processorCPU_util(_L,_Len1 ,0,_L1,_L2,_L3,_Flag,_J1,_AL) ->  
    {_L1,_L2,_L3,lists:append(_AL,lists:duplicate(_Len1 - length(_AL),-1))};
processorCPU_util(Li,Len1 ,Num,TL1,TL2,TL3,Flag,TJ1,TAL) ->
    [S4|B] = Li,
    if TL2 < 0	->
	    L2 =  textutils:findLong(S4,"PerfTime100nSec:",null);
    true ->
        L2 = TL2
    end,   
    Index1 = string:str(S4,"name: _Total"),
	Index2 = string:str(S4,"name: "),
	Len = length(S4),
	if Index1 == 1 ->
	    TFlag = false,
		processorCPU_util(B,Len1,Num-1,TL1,L2,TL3,TFlag,TJ1,TAL);
	true ->
        if Index2 == 1 ->
            TFlag = true,
			processorCPU_util(B,Len1,Num-1,TL1,L2,TL3,TFlag,TJ1,TAL);
        true ->
            if Len == 0 ->
                TFlag = false,
				processorCPU_util(B,Len1,Num-1,TL1,L2,TL3,TFlag,TJ1,TAL);
            true ->
                if Flag ->               
                    L4 =  getPerfData(S4,"6:", "% PERF_100NSEC_TIMER_INV"),
                    if L4 /= -1 ->
                        if  3 + TJ1 < Len1 ->                                                     
                            AL = lists:append(TAL,[L4]);
						true ->
                            AL = lists:append(TAL,[-1])
                        end,							
					    if TL1 == -1 ->
                            L1 = L4;
                        true ->
                            L1 = TL1 + L4
                        end,
                        L3 = TL3 + 1,
                        J1 = TJ1 + 1;
                    true ->
					    L1 = TL1,
                        L3 = TL3,
                        J1 = TJ1,
                        L1 = TL1, 						
                        AL = TAL
                    end,
					processorCPU_util(B,Len1,Num-1,L1,L2,L3,Flag,J1,AL);
                true ->	
                    processorCPU_util(B,Len1,Num-1,TL1,L2,TL3,Flag,TJ1,TAL)
				end
			end	
		end
    end.		
				
getPerfData(S1,String2,String3) ->
    Index1 = string:str(S1,String2),
    if Index1 == 0 ->
        -1;
    true ->    
        Len1 = length(String2),
        Str3 = string:substr(S1,Len1+1),	
        Index2 = string:str(Str3,String3),
        if Index2 == 0 ->
            -1;
        true ->            
            Len2 = length(Str3),
            Len3 = length(String3),
            Num = string:substr(Str3,1,Len2-Len3-1),   
	        [Number] = string:tokens(Num," "),
	        list_to_integer(Number)
        end
    end.        


cpuUsedWindows(Host,L,L1,List,AtomicMonitor,Array) ->
    I = length(List),
    J = 4 + I,
	if L == 0 ->
	    AL2 = processorCPU(Host,I,1,AtomicMonitor,Array),
	    if AL2 == [] ->
		    Bool1 = true,
			TL = L,
			TL1 = L1,
			AL = List,
	        lists:duplicate(J,-1);
		true ->
		    Bool1 = false,
		    [TL] = lists:sublist(AL2,1,1),
			[TL1] = lists:sublist(AL2,2,1),
			AL = string:substr(AL2,4),
			timer:sleep(2000)
        end;
    true ->
	    Bool1 = false,
    	TL = L,
		TL1 = L1,
        AL = List
	end,	
    AL3 = processorCPU(Host,I,1,AtomicMonitor,Array),
   if AL3 == [] ->
	    Bool2 = true,
        lists:duplicate(J,-1);
    true ->        
	    Bool2 = false,
	    [L3] = lists:sublist(AL3,1,1),
		[L4] = lists:sublist(AL3,2,1),
		[L5] = lists:sublist(AL3,3,1),
        L6 = L3 - TL,        
		if L6 > 0 ->
		    TL2 = 100 - (100 * (L4 - TL1)) / L6;
		true ->
            TL2 = -1
        end,			
	    if L6 =< 0 ->
            L2 = -1;
		true ->
            if L4 - TL1 < 0 ->
                L2 = -1;
            true ->
                if L3 == 0 ->
                    L2 = -1;
                true ->
                    if TL == 0 ->
                        L2 = -1;
                    true ->
                        if TL2 < 0 ->
                            L2 = -1;
                        true ->
                            if TL2 > 100 ->
                                L2 = -1;
                            true ->
                                L2 = TL2
                            end
                        end
                    end
                end
            end
        end,
       	if L5 > 0 ->
            L7 = L6 / L5;
        true ->
            L7 = L6
        end,
		{Bool3,TAL1} = cpuUsedWindows_util(AL,AL3,L5,L6,L7),
		if Bool1 ->
		    lists:duplicate(J,-1);
	    true ->
            if Bool2 ->
                lists:duplicate(J,-1);
            true ->
                if Bool3 ->
                    [];
                true ->
                     MaxCpusNum =  length(List),
                    if MaxCpusNum > L5 ->                     
                        lists:append([L2,L3,L4,L5],TAL1 ++ lists:duplicate(MaxCpusNum - L5,"n/a"));
                    true ->
                         lists:append([L2,L3,L4,L5],lists:sublist(TAL1,1,MaxCpusNum))
                     end
                end
            end
        end			
    end.	

cpuUsedWindows_util(AL,AL3,L5,L6,L7) ->
	cpuUsedWindows_util_t(AL,AL3,L5,L6,L7,1,true,false,[]).
cpuUsedWindows_util_t(_AL,_AL3,_L5,_L6,_L7,_N,false,Bool,AL1) -> {Bool,AL1};
cpuUsedWindows_util_t(Al,Al3,TL5,TL6,TL7,Num,Flag,Bl3,Al1) ->
    if 	Num > TL5 ->
	    cpuUsedWindows_util_t(Al,Al3,TL5,TL6,TL7,Num,false,Bl3,Al1);
	true ->
	    [LastMeasurementsAmong] = lists:sublist(Al,Num,1),
		[A3j1] = lists:sublist(Al3,3+Num,1),
	    if LastMeasurementsAmong /= "n/a" ->
		    AL14 = 100 - (100 * (A3j1 - LastMeasurementsAmong)) / TL7;
		true ->
            AL14 = -1
        end,
        if 	(AL14 < 0 ) and (LastMeasurementsAmong /= "n/a") ->
            TAL14 = 100 - (100 * (A3j1 - LastMeasurementsAmong)) / TL6;
        true ->
            TAL14 = AL14
        end,
        if  TAL14 < 0 ->
            Bool3 = true;
        true ->
            if 	TAL14 > 100 ->
                Bool3 = true;
            true ->
                Bool3 = false
            end
        end,
        TAL = lists:append(lists:append(lists:sublist(Al,1,Num-1),[A3j1]),string:substr(Al,Num+1)),
		TAl1 = lists:append(Al1,[TAL14]), 
        cpuUsedWindows_util_t(TAL,Al3,TL5,TL6,TL7,Num+1,Flag,Bool3,TAl1)		
    end.
	

cpuUsed(Host,LastMeasurementTime,LastMeasurement,LastMeasurements) ->
	cpuUsed(Host,LastMeasurementTime,LastMeasurement,LastMeasurements,null,null).

cpuUsed(Host,LastMeasurementTime,LastMeasurement,LastMeasurements,AtomicMonitor,Array) ->	
	case machine:getSnmpMachine(Host) of
        {ok, Ma=#machine{}} ->
            case snmp_machine:cpuUsed(Host) of
                {ok, Vbs} when erlang:is_list(Vbs) ->
                    Fun = 
                        fun(X) ->
                            MValue = X#varbind.value,
                            MValue
                        end,
                    MCoresV = [Fun(X)||X<-Vbs, erlang:is_record(X, varbind)],
                    CoreLen = string:len(MCoresV),
                    if
                        CoreLen == 1 ->
                            [MCoV] = MCoresV,
                            [MCoV,0,0,1];
                        CoreLen > 1 ->
                            Avgs = lists:sum(MCoresV)/CoreLen,
                            lists:append([Avgs,0,0,CoreLen], MCoresV);
                        true ->
                            [-1,0,0,1]
                    end;
                _ ->
                    [-1,0,0,1]
            end;
        _ ->
            LastMeasurementsLength = length(LastMeasurements),
            HostOsTypeNumber = machine:getOS(Host), 		
            case HostOsTypeNumber of
                1 ->
                if AtomicMonitor /= null ->
                    case machine:getNTMachine(Host) of
                            []->
                                CpuValueList1 = cpuUsedWindows(Host, LastMeasurementTime, LastMeasurement, LastMeasurements, AtomicMonitor, Array),
								if  CpuValueList1 == [] -> 
                                    cpuUsedWindows(Host, 0, LastMeasurement, LastMeasurements, AtomicMonitor, Array);
                                true ->
                                    CpuValueList1
                                end;
                            [Mach|_]->
                                case Mach#machine.method of
                                    "WMI"->
                                        proxy:cpu(HostOsTypeNumber, Host, Mach#machine.login, Mach#machine.passwd);
                                    _->
                                        CpuValueList1 = cpuUsedWindows(Host, LastMeasurementTime, LastMeasurement, LastMeasurements, AtomicMonitor, Array),
                                        if  CpuValueList1 == [] -> 
                                            cpuUsedWindows(Host, 0, LastMeasurement, LastMeasurements, AtomicMonitor, Array);
                                        true ->
                                            CpuValueList1
                                        end
                                end
                    end;
                true ->	 
                    [-1,0,0,1]
                end;			
            9 ->
                %K = 4 + LastMeasurementsLength,
                if Host == "" ->
                    Machine = "";
                true ->    
                    Machine = machine:getMachine(Host)
                end,
                    %Machine = dbcs_machine:get_machine_match("my.host=" ++ Host),
                case Machine of
                    [] -> {error,machine_undefined};
                    [M|_] ->
                        Cmd = machine:getCommandString(cpu,Host),
                        case Cmd of
                            {ok,Command} ->            
								%ResultString = sshcommandline:exec(Command,M,true);
								{Status,ResultL} = siteview_commandline:exec(Host,Command);
                            {error,_} ->
								%ResultString = sshcommandline:exec(cpuCommand(Host, HostOsTypeNumber),M,true)
								{Status,ResultL} = siteview_commandline:exec(Host,cpuCommand(Host, HostOsTypeNumber))                
                            end,
                        case Status of
                        ok ->
							ResultList = [re:replace(lists:nth(2,ResultL),",",", ",[global, {return, list}])];
							%%ResultList = ResultL;
                        _ ->
							ResultList = [] 
                        end,
                        %ResultList = string:tokens(ResultString,"\r\n"),            
                        OsaI = osAdapter:new(list_to_atom(M#machine.os)),
                        OsaI:initEts(),
                        {_Type1,CpuWaitInteger} =  OsaI:getCommandSettingAsInteger(cpu, wait, -1),
                        {_Type2,CpuIdleInteger} =  OsaI:getCommandSettingAsInteger(cpu, idle, 0),
                        {_Type3,CpuNum} =  OsaI:getCommandSettingAsInteger(cpu, cpu, -1),
                        LineReaderEtsTid = init_lineReader(),
                        OsNameString = machine:osToString(HostOsTypeNumber),
                        LineR = lineReader:new(ResultList,OsNameString,LineReaderEtsTid,cpu),
                        ReadListEndFlag = LineR:processLine(),
                        SkipLineFlag = LineR:skipLine(),
                        {IdleInteger,J2,List} = cpuUsed_util_for_redhat(LineR,OsaI,ReadListEndFlag,SkipLineFlag,CpuWaitInteger,CpuIdleInteger,-1,CpuNum,0),             
						ets:delete(LineReaderEtsTid),
                        if IdleInteger /= -1 ->
                                Len1 = length(List),
                                if Len1 == 0 ->
                                    TempCpuValueList1 = lists:duplicate(LastMeasurementsLength,-1),
                                    CpuUsage = 100 - IdleInteger,
                                    CpuNumber = 1;
                                true ->
                                    if Len1 == 1 ->
                                        TempCpuValueList1 = lists:duplicate(LastMeasurementsLength,-1),
                                        Temp = integer_to_list(J2),
                                        Res = [X||X<-List,element(1,X) =:= Temp],
                                        if Res /= [] ->
                                                [{_TT,I3}] = Res,
                                                CpuUsage = 100 - I3,
                                                CpuNumber = 1;
                                            true ->	
                                                CpuUsage = -1,
                                                CpuNumber = 1
                                        end;
                                    true ->
                                        TL5 = length(List),
                                        if TL5 > LastMeasurementsLength -> CpuNumber = LastMeasurementsLength;
                                            true -> CpuNumber = TL5
                                        end,
                                        {J3,TempCpuValueList1,TAL} = cpuUsed_util_2(LastMeasurementsLength,J2,0,0,List),
                                        TL2 = (100 * CpuNumber - J3) /CpuNumber,
										io:format("------aqc2-------TL2:~p~n",[TL2]),
                                        if TL2 < 0 -> CpuUsage = 0;
                                            true -> 
                                                if TL2 > 100 -> CpuUsage = 100;
                                                    true -> CpuUsage = TL2
                                                end
                                        end							
                                                                end					
                                                        end,
                                        [CpuUsage,0,0,CpuNumber];
                                true ->[-1,0,0,1]
                                    end;
                    _ ->
                        [-1,0,0,1]
                        end;
            _ ->
                %K = 4 + LastMeasurementsLength,
                Cmd = machine:getCommandString(cpu,Host),
                case Cmd of
                {ok,Command} ->
                    %FullCommand= sslCommandLine(Host) ++ Command;
                    %%io:format("Command:~p~n",[Command]),
                    {Status,ResultList} = siteview_commandline:exec(Host,Command);
                {error,_} ->
                    %%io:format("Command:~p~n",[cpuCommand(Host, HostOsTypeNumber)]),
                    {Status,ResultList} = siteview_commandline:exec(Host,cpuCommand(Host, HostOsTypeNumber))
                    %FullCommand = sslCommandLine(Host) ++ cpuCommand(Host, HostOsTypeNumber)
                end,
                %%io:format("Status:~p~nResultList:~p~n",[Status,ResultList]), 
            case Status of
                ok ->
                    %ResultString = os:cmd(FullCommand),
                    %ResultList = string:tokens(ResultString,"\r\n"),     
                    Machine = dbcs_machine:get_machine_match("my.host=" ++ Host),
                    case Machine of
                    [] ->
                        {error,machine_undefined};
                    [M|_] ->  
                        OsNameString = machine:osToString(HostOsTypeNumber),             
                        OsaI = osAdapter:new(list_to_atom(OsNameString)),
                        OsaI:initEts(),
                        {_Type1,CpuWaitInteger} =  OsaI:getCommandSettingAsInteger(cpu, wait, -1),
                        {_Type2,CpuIdleInteger} =  OsaI:getCommandSettingAsInteger(cpu, idle, 0),
                        {_Type3,CpuNum} =  OsaI:getCommandSettingAsInteger(cpu, cpu, -1),
                        LineReaderEtsTid = init_lineReader(),			
                        LineR = lineReader:new(ResultList,OsNameString,LineReaderEtsTid,cpu),
                        ReadListEndFlag = LineR:processLine(),
                        SkipLineFlag = LineR:skipLine(),        
                    {IdleInteger,J2,List} = cpuUsed_util(LineR,OsaI,ReadListEndFlag,SkipLineFlag,CpuWaitInteger,CpuIdleInteger,-1,CpuNum,0),
                        ets:delete(LineReaderEtsTid),
                        if IdleInteger /= -1 ->
                            Len1 = length(List),
                            if Len1 == 0 ->                
                                TempCpuValueList1 = lists:duplicate(LastMeasurementsLength,-1),
                                CpuUsage = 100 - IdleInteger,
                            CpuNumber = 1;
                            true ->
                                if Len1 == 1 ->
                                    TempCpuValueList1 = lists:duplicate(LastMeasurementsLength,-1),
                                    Temp = integer_to_list(J2),
                                    Res = [X||X<-List,element(1,X) =:= Temp],
                                    if Res /= [] ->
                                        [{_TT,I3}] = Res,
                                        CpuUsage = 100 - I3,
                                        CpuNumber = 1;
                                    true ->	
                                        CpuUsage = -1,
                                        CpuNumber = 1
                                    end;
                                true ->
                                    TL5 = length(List),
                                    if TL5 > LastMeasurementsLength ->
                                        CpuNumber = LastMeasurementsLength;
                                    true ->
                                        CpuNumber = TL5
                                    end,
                                    {J3,TempCpuValueList1,TAL} = cpuUsed_util_2(LastMeasurementsLength,J2,0,0,List),                       
                                TL2 = (100 * CpuNumber - J3) /CpuNumber,
                                    if TL2 < 0 ->
                                        CpuUsage = 0;
                                    true -> 
                                        if TL2 > 100 ->
                                            CpuUsage = 100;
                                        true ->
                                            CpuUsage = TL2
                                        end
                                    end							
                                end					
                            end,
                            lists:append([CpuUsage,0,0,CpuNumber],TempCpuValueList1);
                        true ->
                            [-1,0,0,1]
                        end;               
                    _ ->
                        [-1,0,0,1]
                    end;
                _ ->
                    [-1,0,0,1]  
                end 
            end
    end.

cpuUsed_util_for_redhat(LR,OsaI,FlagP,FlagS,J1,K1,K2,I2,J2) ->
    cpuUsed_util_t_for_redhat(LR,OsaI,FlagP,FlagS,J1,K1,K2,I2,J2,[]).
cpuUsed_util_t_for_redhat(_LR,_OsaI,'false',FS,_J1,_K1,_K2,_I2,_J2,R) -> {_K2,_J2,R};
cpuUsed_util_t_for_redhat(LineR,TOsaI,TFlagP,TFlagS,TJ1,TK1,TK2,TI2,TJ2,Re) ->
	if TFlagP /= true -> cpuUsed_util_t_for_redhat(LineR,TOsaI,false,TFlagS,TJ1,TK1,TK2,TI2,TJ2,Re);
		     true ->
			if TFlagS == false ->
			     if TJ1 >= 0 -> [S2] = LineR:readColumn(TJ1, "wait");
				    true ->S2 = ""
			     end,
			     [S3] = LineR:readColumn(TK1, "idle"),
			     Len = length(S3),
			     if Len > 0 ->
					    if S2 == "" -> K4 = 0;
						true ->	
						    %[WaitTemp]  = lists:sublist(string:tokens(S2,"%"),1,1),
						    WaitTemp = textutils:readLong(S2,1),   
						    if  WaitTemp == "" -> K4 = 0;
								   true -> K4 = list_to_integer(WaitTemp)
						    end    
					     end,
					    %[IdleTemp] = lists:sublist(string:tokens(S3,"%"),1,1),	
					    IdleTemp = textutils:readLong(S3,1),
					    if  IdleTemp /= "" -> 
						       I5 = list_to_integer(IdleTemp),
						       TJ5 = K4 + I5,
						       if TJ5 < 0 -> J5 = 0;
						             true ->
								if TJ5> 100 -> J5 = 100;
								true -> J5 = TJ5
								end
						       end,
						       K2 = J5,
						       Flag1 = LineR:processLine(),
						       Flag2 = LineR:skipLine(),
						       cpuUsed_util_t_for_redhat(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,K2,TI2,TJ2,Re);
						true ->
						    cpuUsed_util_t_for_redhat(LineR,TOsaI,false,TFlagS,TJ1,TK1,-1,TI2,TJ2,Re)
						end;
				      true ->
					  Flag1 = LineR:processLine(),
					  Flag2 = LineR:skipLine(),
					  cpuUsed_util_t_for_redhat(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,TK2,TI2,TJ2,Re)			
			      end;    			
			true ->
			    Flag1 = LineR:processLine(),
			    Flag2 = LineR:skipLine(),	
			    cpuUsed_util_t_for_redhat(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,TK2,TI2,TJ2,Re)	                
			end
        end.	


	
cpuUsed_util(LR,OsaI,FlagP,FlagS,J1,K1,K2,I2,J2) ->
    cpuUsed_util_t(LR,OsaI,FlagP,FlagS,J1,K1,K2,I2,J2,[]).
cpuUsed_util_t(_LR,_OsaI,false,FS,_J1,_K1,_K2,_I2,_J2,R) -> {_K2,_J2,R};
cpuUsed_util_t(LineR,TOsaI,TFlagP,TFlagS,TJ1,TK1,TK2,TI2,TJ2,Re) ->
    %io:format("TFlagP:~p~nTFlagS:~p~n",[TFlagP,TFlagS]),
	if TFlagP /= true ->
        cpuUsed_util_t(LineR,TOsaI,false,TFlagS,TJ1,TK1,TK2,TI2,TJ2,Re);
    true ->
        if TFlagS == false ->
            if TJ1 >= 0 ->
                [TS2] = LineR:readColumn(TJ1, "wait"),                 
                S2 = textutils:readLong(TS2,1);
			true ->
			    S2 = ""
			end,	 
		    [TS3] = LineR:readColumn(TK1, "idle"),
            S3 = textutils:readLong(TS3,1),
            %io:format("S3:~p~nS2:~p~n",[TS3,S2]),
		    if TI2 >= 0 ->
				[TS4] = LineR:readColumn(TI2, "cpu"),
			    {_Type4,S5} = TOsaI:getCommandSetting(cpu,cpuNamePrefix,"CPU"),
			    Index = string:str(string:to_upper(TS4),string:to_upper(S5)),
				if Index == 1 ->
					S4 = string:substr(TS4,length(S5)+1);
				true ->
                    S4 = TS4
                end,
                J4 = list_to_integer(S4),
                if  J4 > TJ2 ->
                    J2 = J4;
                true ->
                    J2 = TJ2
                end;
		    true ->
			    J4 = -1,
                J2 = TJ2
            end,
			Len = length(S3),
            if Len > 0 ->
			    if S2 == "" ->
				    K4 = 0;
				true ->			
			        K4 = list_to_integer(S2)
				end,	                
				I5 = list_to_integer(S3),
				TJ5 = K4 + I5,
				if TJ5 < 0 ->
				    J5 = 0;
				true ->
                    if TJ5> 100 ->
                        J5 = 100;
                    true ->
                        J5 = TJ5
                    end
                end,
                K2 = J5,
				Temp =  integer_to_list(J4),  
				if J4 >= 0 ->
				    L = [X||X<-Re,element(1,X) =:= Temp],
					if L == [] ->
					    TRE = lists:append(Re,[{Temp,J5}]),
				        Flag1 = LineR:processLine(),
                        Flag2 = LineR:skipLine(),
                        cpuUsed_util_t(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,K2,TI2,J2,TRE);
                    true ->
                        Flag1 = LineR:processLine(),
                        Flag2 = LineR:skipLine(),
                        cpuUsed_util_t(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,K2,TI2,TJ2,Re)
                    end;
                true ->
                    Flag1 = LineR:processLine(),
                    Flag2 = LineR:skipLine(),
                    cpuUsed_util_t(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,K2,TI2,TJ2,Re)
                end;
            true ->
                Flag1 = LineR:processLine(),
                Flag2 = LineR:skipLine(),
                cpuUsed_util_t(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,TK2,TI2,TJ2,Re)			
            end;    			
        true ->
            Flag1 = LineR:processLine(),
            Flag2 = LineR:skipLine(),
            cpuUsed_util_t(LineR,TOsaI,Flag1,Flag2,TJ1,TK1,TK2,TI2,TJ2,Re)	                
        end
    end.	
                     					

cpuUsed_util_2(I,J2,J3,K3,List) ->
    cpuUsed_util_2_t(I,J2,J3,K3,0,List,true,[],[]).
cpuUsed_util_2_t(_I,_J2,_J3,_K3,_Num,_List,false,AL1,AL) -> {_J3,AL1,AL};
cpuUsed_util_2_t(TI,TJ2,TJ3,TK3,Num,TList,Flag,RAL1,RAL) ->
    if Num > TJ2 ->
	    cpuUsed_util_2_t(TI,TJ2,TJ3,TK3,Num,TList,Num,TList,false);
	true ->	
        Temp = integer_to_list(Num),
        L = [X||X<-TList,element(1,X) =:= Temp],
	    if L == [] ->
	        cpuUsed_util_2_t(TI,TJ2,TJ3,TK3,Num,TList,Num+1,TList,Flag);
		true ->
		    [{_T,V}] = L,
            AL14k3 = [100 - V],
            J3 =  TJ3 + V,
            ALK3 = [1],	
            if TK3 + 1 >= TI ->
                cpuUsed_util_2_t(TI,TJ2,J3,TK3,Num,TList,false,lists:append(RAL1,AL14k3),lists:append(RAL,ALK3));
            true ->
			     cpuUsed_util_2_t(TI,TJ2,J3,TK3,Num+1,TList,Flag,lists:append(RAL1,AL14k3),lists:append(RAL,ALK3))
            end
        end			
    end.			
			
										
getSystemTime() ->
    erlang:localtime().
 
 

    
contactUsingNetBIOS(Host,Flag) ->
    Len = length(Host),
    if Flag ->
        String = " -connect";
    true ->
        String = " -disconnect"
    end,
    if Host /= "this server" , Len /= 0 ->
        Command =  perfexCommand(Host) ++ String,
        List = string:tokens(os:cmd(Command)),        
        Flag1 = contactUsingNetBIOS_util(List),
        if Flag1 ->
            200;
        true ->
            -998
        end;      
    true ->
        200
    end.         

contactUsingNetBIOS_util(List) ->
    contactUsingNetBIOS_util_t(List,length(List),false).
contactUsingNetBIOS_util_t(_L,0,Flag) ->  Flag;
contactUsingNetBIOS_util_t(Li,Num,F) ->
    [A|B] = Li,
    Index = string:str(A,"Connected to"),
    if Index == 0 ->
        contactUsingNetBIOS_util_t(B,Num-1,F);
    true ->
        contactUsingNetBIOS_util_t(B,0,true)
    end.        
 
getSysInfo() ->
    Osnum = getOs(),
	case Osnum of
	6 ->
        os:cmd("/bin/uname -a");
	_ ->	
		""
	end.

%return a list	
getWebServers(Host) ->
	Index1 = string:str(Host,"http://"),

	if Index1 == 1 ->
	    Cmd = Host ++ "perfex.exe?-cgi -w";
	true ->	
       Cmd = perfexCommand(Host) ++ " -w"
    end,
    List = string:tokens(os:cmd(Cmd),"\r\n"),
	%List = string:tokens(os:cmd("tools\\perfex.exe -w"),"\r\n"),
	%%%%io:format("test~p~n",[List]),
    {F,F1,F2,F3,F4,F5,L} = getWebServers_util(List,false,false,false,false,false,false),
    if F ->
        TL1 =  lists:append(L,["Microsoft|"]),
		TL2 = lists:append(TL1,["Microsoft IIS"]);
	true ->
        TL2 = L
    end,
    if F1 ->
        TL3 = lists:append(TL2,["Microsoft4|"]),
        TL4 = lists:append(TL3,["Microsoft IIS"]);
    true ->
        TL4 = TL2
    end,
    if F2 ->
        TL5 = lists:append(TL4,["WebSite|"]),
        TL6 = lists:append(TL5,["O'Reilly WebSite"]);
    true ->
        TL6 = TL4
    end,
    TL6.	
	                                              
	     
getWebServers_util(List,Flag,Flag1,Flag2,Flag3,Flag4,Flag5) ->
    getWebServers_util_t(List,length(List),Flag,Flag1,Flag2,Flag3,Flag4,Flag5,[]).
getWebServers_util_t(_L,0,_F,_F1,_F2,_F3,_F4,_F5,R) -> {_F,_F1,_F2,_F3,_F4,_F5,R};
getWebServers_util_t(L,Num,_Flag,_Flag1,_Flag2,_Flag3,_Flag4,_Flag5,Re) ->	
    [A|B] = L,
	%%%%io:format("test~p~n",[A]),
	Index1 = string:str(A,"name: "),
	%%%%io:format("name:~p~n",[Index1]),
    if  _Flag3,Index1 == 1 ->        
        S4 = string:substr(A,7,length(A)),
        Tr31 = lists:append(Re,["Netscape|"++S4]),
        Tr32 = lists:append(Tr31,["Netscape server (" ++ S4 ++ ")"]);
    true ->
        Tr32 = Re
    end,
    if  _Flag4,Index1 == 1 ->  
	    S5 = string:substr(A,7,length(A)),
        Tr41 = lists:append(Tr32,["Netscape3|"++S5]),
        Tr42 = lists:append(Tr41,["Netscape server (" ++ S5 ++ ")"]);	
	true ->
        Tr42 = Tr32
    end,
    if 	_Flag5,Index1 == 1 ->
	    S6 = string:substr(A,7,length(A)),
        Tr51 = lists:append(Tr42,["Netscape35|"++S6]),
        Tr52 = lists:append(Tr51,["Netscape server (" ++ S6 ++ ")"]);
    true ->
        Tr52 = Tr42
    end,
    Index2 = string:str(A,"object:"),
	%%%%io:format("Index2:~p~n",[Index2]),
    if Index2 == 1 ->
        Tf3 = false,
        Tf4 = false,
        Tf5 = false;
    true ->
        Tf3 = _Flag3,
        Tf4 = _Flag4,
        Tf5 = _Flag5
    end,
	Index3 = string:str(A,"object: Netscape Server"),
	Index4 = string:str(A,"object: Netscape Enterprise 3.0"),
	Index5 = string:str(A,"object: Netscape Enterprise 3.5"),
	Index6 = string:str(A,"object: Web Service"),
	Index7 = string:str(A,"object: HTTP Service"),
	Index8 = string:str(A,"object: WebServer"),
	%%%%io:format("Index6:~p~n",[Index6]),
    if Index3 == 1 ->
        TFlag3 = true,
		TFlag = _Flag,
		TFlag1 = _Flag1,
		TFlag2 = _Flag2,
		TFlag4 = _Flag4,
		TFlag5 = _Flag5;
    true ->
	    TFlag3 = Tf3, 
        if Index4 == 1 ->
            TFlag4 = true,
            TFlag3 = _Flag3,
		    TFlag = _Flag,
		    TFlag1 = _Flag1,
		    TFlag2 = _Flag2,
		    TFlag5 = _Flag5;			
        true ->
		    TFlag4 = Tf4,
            if Index5 == 1 ->
                TFlag5 = true,
		        TFlag = _Flag,
		        TFlag1 = _Flag1,
		        TFlag2 = _Flag2;				
            true ->
                TFlag5 = Tf5,
				if Index6 == 1 ->
				    %%%%io:format("test Index6:~p~n",[Index6]),
                    TFlag1 = true,
		            TFlag = _Flag,
		            TFlag2 = _Flag2;					
                true ->
				    TFlag1 = _Flag1,
                    if Index7 == 1 ->
                        TFlag = true,
		                TFlag2 = _Flag2;						
                    true ->
					    TFlag = _Flag,
                        if Index8 == 1 ->
                            TFlag2 = true;
                        true ->
                            TFlag2 = _Flag2
                        end
					end
                end
            end
        end
    end,
    getWebServers_util_t(B,Num-1,TFlag,TFlag1,TFlag2,TFlag3,TFlag4,TFlag5,Tr52).	
                         						

hasLibrary() ->	
ok.

checkPermissions(Host) ->    
	if Host == 	"this server" ->
        if length(Host) == 0 ->
            Val = 0;
        true ->
            Val = 0
        end;
    true ->
        if length(Host) == 0 ->
            Val = 0;
        true ->
            Cmd = perfexCommand(Host) ++ "  =238",
			Array = os:cmd(Cmd)			
        end
    end.		

%% tuxedoTest1()->
%% 	Cmd = platform:perfexCommand("192.168.6.166") ++ " -map",
%% 	CmdLine = command_line:new(),
%% 	io:format("Cmd11:~p~n",[Cmd]),
%% 	Rets = CmdLine:exec(Cmd),
%% 	Rets. 

tuxedoGetParam(Server, Port, UserName, UserPwd, TaskType, TaskParam)->
	Cmd = tuxedoCommand(Server, Port, UserName, UserPwd, TaskType, TaskParam),
	io:format("Cmd:~p~n",[Cmd]),
	CmdLine = command_line:new(),
	Rets = CmdLine:exec(Cmd),
	io:format("Rets:~p~n",[Rets]),
	I = string:rstr(Rets, "FileSplitFlag"),	
	L = string:len(Rets),
	Rets1 = string:substr(Rets, I+13, L-13),
	J = string:rstr(Rets1, "EndSplitFlag"),
	Rets2 = string:substr(Rets1, 1, J-1),	
	io:format("Rets2:~p~n",[Rets2]),
	case TaskParam of
		"null" ->
			[lists:nth(1, string:tokens(X, "="))||X <- string:tokens(Rets2, "*@"), string:equal(X, "=")==false];
		_->
			Rets2
	end.

tuxedoCommand(Server, Port, UserName, UserPwd, TaskType, TaskParam) ->
	case getOs() of
		1->
			"bin\\TuxedoGet.exe " ++ " " ++ Server ++ " " ++ Port ++ " " ++ UserName ++ " " ++ UserPwd ++ " " ++ TaskType ++ " " ++ TaskParam;
		_->
			"bin\\TuxedoGet.exe " ++ " " ++ Server ++ " " ++ Port ++ " " ++ UserName ++ " " ++ UserPwd ++ " " ++ TaskType ++ " " ++ TaskParam			
	end.

killCommand() ->
    Osnum = getOs(),
	case Osnum of
	1 ->
        os:cmd("cd") ++ "\\tools\\kill.exe";
    2 ->
        "/usr/sbin/killall";
    6 ->
 	    "/usr/bin/killall"
	end.

netstatCommand(Osnum) ->
    case Osnum of
	1 ->
        "netstat";
    2 ->
        "/usr/bin/netstat";
    3 ->
        "/usr/etc/netstat";
    6 ->
        "/bin/netstat";
    5 ->
        "/usr/bin/netstat" 	
    end.

registryArray() -> ok.
    

netEthernetStatsCommand() ->
    OSnum = getOs(),
	case OSnum of
	1 ->
	    "netstat -e";
	_ ->
       ""
    end.

netConnectionStatsCommand() ->
    Osnum = getOs(),
    case Osnum of
    1 ->
        "netstat -n";
    _ ->
        ""
    end.

getNetStats() ->
    Cmd1 = netEthernetStatsCommand(),
    Cmd2 = netConnectionStatsCommand(),
	String1 = os:cmd(Cmd1),
	String2 = os:cmd(Cmd2),
	case isWindows() of
	true ->
	    Li1 =  string:tokens(String1,"\r\n"),
		Li2 =  string:tokens(String2,"\r\n"),
		List1= getNetStats_util1(Li1),
        List2 = getNetStats_util2(Li1),
		Int = getNetStats_util3(Li2),
        if Int >= 2 ->
            AL3 = Int - 2;
        true ->
            AL3 = -1
        end,
        lists:append(lists:append(List1,List2),[AL3]);
    _ ->
        []
    end.		
 
getNetStats_util1(List) ->
    getNetStats_util1_t(List,length(List),[-1,-1]).
getNetStats_util1_t(_L,0,R) -> R;
getNetStats_util1_t(Li,Num,Re) ->
    [A|B] = Li,    
    L = string:tokens(A," "),
    [SubList] = lists:sublist(L,1,1),
    if (SubList /= "Bytes") and (SubList /= "×Ö½Ú") ->
        getNetStats_util1_t(B,Num-1,Re);
    true ->
        S4 = lists:sublist(L,2,1),
		S7 = lists:sublist(L,3,1),
		Len1 = length(S4),
		Len2 = length(S7),
		if Len1 == 0 ->
            AL0 = -1;
        true ->
            [TS4] =S4, 
            AL0 = list_to_integer(TS4)
        end,
        if Len2 == 0 ->
            AL1 = -1;
        true ->
            [TS7] = S7,
            AL1 = list_to_integer(TS7)
        end,
        getNetStats_util1_t(B,0,[AL0,AL1])
    end.	  
 
getNetStats_util2(List) ->
    getNetStats_util2_t(List,length(List),[]).
getNetStats_util2_t(_L,0,R) -> R;
getNetStats_util2_t(Li,Num,Re) ->
    [A|B] = Li,
    L = string:tokens(A," "),
    [SubList] = lists:sublist(L,1,1),
    if (SubList /= "Errors") and (SubList /= "´íÎó") ->
        getNetStats_util2_t(B,Num-1,Re);
    true ->
        S5 = lists:sublist(L,2,1),
		S8 = lists:sublist(L,3,1),
		Len1 = length(S5),
		Len2 = length(S8),
        if Len1 > 0 ,Len2 > 0 ->
            [TS5] = S5,
            [TS8] = S8,
		    AL2 = list_to_integer(TS5) + list_to_integer(TS8),
            getNetStats_util2_t(B,0,[AL2]);
		true ->
            getNetStats_util2_t(B,0,[-1])		
        end		
    end.	     	
 
getNetStats_util3(List) ->
    getNetStats_util3_t(List,length(List),0).
getNetStats_util3_t(_L,0,I) -> I;
getNetStats_util3_t(Li,Num,Re) ->
    [A|B] = Li,
    L = string:tokens(A," "),
    SubList = lists:sublist(L,1,1),
    if  SubList /= "" ->
        getNetStats_util3_t(B,Num-1,Re+1);
    true ->
	    getNetStats_util3_t(B,Num-1,Re)
    end.	

getLocalIPAddress() ->
    S = "ipconfig",
    String = os:cmd(S),
	getLocalIPAddress_util(string:tokens(String,"\r\n")).
    	
getLocalIPAddress_util(List) ->
	getLocalIPAddress_util_t(List,length(List),[]).
getLocalIPAddress_util_t(_L,0,R) -> R;
getLocalIPAddress_util_t(L,N,Re) ->
    [A|B] = L,
    List1 = string:tokens(A," "),
    List2 = getLocalIPAddress_util_1(List1),    
    getLocalIPAddress_util_t(B,N-1,lists:append(Re,List2)).

getLocalIPAddress_util_1(Array) ->
   	getLocalIPAddress_util_1_t(Array,length(Array),[]).
getLocalIPAddress_util_1_t(_L,0,R) -> R;
getLocalIPAddress_util_1_t(L,N,Re) ->
    [A|B] = L,
	Str = string:tokens(A," "),
	Bool1 = isDottedIP(Str),
	Bool2 = Str /= "0.0.0.0",
    if Bool1,Bool2 ->
        getLocalIPAddress_util_1_t(B,N-1,lists:append(Re,[Str]));
    true ->
        getLocalIPAddress_util_1_t(B,N-1,Re)
    end.		


isDottedIP(String) ->
    if length(String) < 7 ->
        false;
    true ->
        List = string:tokens(String,"."),
        if length(List) == 4 ->
            true;
        true ->
            false
        end
    end.

tryGetRoot() ->
    Osnum = getOs(),
    Root  = case Osnum of
	1 ->
	    [Dir] =  string:tokens(os:cmd("cd"),"\r\n");
    _ ->
        [Dir] =  string:tokens(os:cmd("pwd"),"\n")
    end,
    Dir.	

getRoot() ->
    tryGetRoot().	
	

%parameters are string	
getDiskFull(Host,Disk)->
	getDiskFull(Host,Disk,null).
	
getDiskFull(Host,Disk,Monitor)->
	%~ io:format("Host:~p,Disk:~p~n",[Host,Disk]),
	
	case machine:getSnmpMachine(Host) of
		{ok, Ma=#machine{}} ->
			getSnmpDisk(Host,Disk);
	        _ ->
			I = machine:getOS(Host),
			Cmd = case machine:getCommandString(disk,Host,[{"disk",Disk}]) of
			    {ok,Val}->
						Val;
					_->
						dfCommand(Host,Disk,I)
				  end,
		    %%%%io:format("~nCmd~p~n",[Cmd]),    
			%%Name = case Host of
			%%		"\\\\" ++ V2->
			%%			V2;
			%%		_->
			%%			Host
			%%		end,
			if
				I==1-> % windows platform
					case machine:getNTMachine(Host) of
						[]->
							{Status,TLine1} = siteview_commandline:exec(Host,Cmd),
							case Status of
								ok ->                        
									getDiskFullFromLine(TLine1,Disk);
								_ ->
									getDiskFullFromLine([],Disk)  
							end;
						[Mach|_]->
							case Mach#machine.method of
								"WMI"->
									proxy:disk(I, Host, Mach#machine.login, Mach#machine.passwd, Disk);	
								_->
									{Status,TLine2} = siteview_commandline:exec(Host,Cmd),                       
									case Status of
										ok ->     
											getDiskFullFromLine(TLine2,Disk);
										_ ->
											getDiskFullFromLine([],Disk)  
									end  
							end           
					end;
			true->
					Machine = machine:getMachine(Host),
			    case Machine of
			    [] ->
				[-1,0,-1];
			    [UnixMachine] ->
				
				case UnixMachine#machine.method of
				"SSH" ->
				    %String = sshcommandline:exec(Cmd,UnixMachine,true),
				    %DiskList = string:tokens(String,"\n"), 
				    {Status,DiskList} = siteview_commandline:exec(Host,Cmd),
				    case Status of
				    ok ->                    
					OsAdap = machine:getAdapter(Host),
					{_T,Total} = OsAdap:getCommandSettingAsInteger(disk,total),
					{_F,Free}  = OsAdap:getCommandSettingAsInteger(disk,free),
					{_P,PecentUsed} = OsAdap:getCommandSettingAsInteger(disk,percentUsed),
					% Tid = init_lineReader(),
					% LineR = lineReader:new(DiskList,machine:osToString(I),Tid,disk),
					%[Str_PerfTime] = [string:strip(element(2,X),both,32)||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "PerfTime"],
					TList = [X||X<-DiskList,string:str(X,Disk) == 1],                    
					case TList of
					[] ->
					    [-1,0,-1];
					[L] ->
					%if length(TList) == 1 ->
					    List =  TList,
								    
					    TDiskL = string:tokens(L," "),
					    Length = length(TDiskL),
					    if Length < PecentUsed ->
						Rows = get_Rows(DiskList,L),
						[NumberStr] = lists:sublist(DiskList,Rows+1,1),
						DL = L ++ " " ++ NumberStr, 
						DiskL = string:tokens(DL," "),                            
						if length(DiskL) < PecentUsed ->
						    [-1,0,-1];
					       true ->
						    [Tot] = lists:sublist(DiskL,Total,1),
						    [Fre] = lists:sublist(DiskL,Free,1),
						    [Pec] = lists:sublist(DiskL,PecentUsed,1),
						    [list_to_integer(textutils:readLong(Pec,1)),list_to_integer(Tot)*1024 - list_to_integer(Fre)*1024,list_to_integer(Tot)*1024]
						end;
					    true ->    
						[Tot] = lists:sublist(TDiskL,Total,1),
						[Fre] = lists:sublist(TDiskL,Free,1),
						[Pec] = lists:sublist(TDiskL,PecentUsed,1),
						[list_to_integer(textutils:readLong(Pec,1)),list_to_integer(Tot)*1024 - list_to_integer(Fre)*1024,list_to_integer(Tot)*1024]
					    end
					end;
				    _ ->
					[-1,0,-1] 
				    end;
				    
				    
				    
				    
				    
				    
				    "Telnet" ->
				    io:format("Host:~p,Disk:~p~n",[Host,Disk]),
				    %String = sshcommandline:exec(Cmd,UnixMachine,true),
				    %DiskList = string:tokens(String,"\n"), 
				    {Status,DiskList} = siteview_commandline:exec(Host,Cmd),
				    case Status of
				    ok ->                    
					OsAdap = machine:getAdapter(Host),
					{_T,Total} = OsAdap:getCommandSettingAsInteger(disk,total),
					{_F,Free}  = OsAdap:getCommandSettingAsInteger(disk,free),
					{_P,PecentUsed} = OsAdap:getCommandSettingAsInteger(disk,percentUsed),
					% Tid = init_lineReader(),
					% LineR = lineReader:new(DiskList,machine:osToString(I),Tid,disk),
					%[Str_PerfTime] = [string:strip(element(2,X),both,32)||X<-[list_to_tuple(string:tokens(X,":"))||X<-Da],element(1,X) =:= "PerfTime"],
					TList = [X||X<-DiskList,string:str(X,Disk) == 1],                    
					case TList of
					[] ->
					    [-1,0,-1];
					[L] ->
					%if length(TList) == 1 ->
					    List =  TList,
								    
					    TDiskL = string:tokens(L," "),
					    Length = length(TDiskL),
					    if Length < PecentUsed ->
						Rows = get_Rows(DiskList,L),
						[NumberStr] = lists:sublist(DiskList,Rows+1,1),
						DL = L ++ " " ++ NumberStr, 
						DiskL = string:tokens(DL," "),                            
						if length(DiskL) < PecentUsed ->
						    [-1,0,-1];
					       true ->
						    [Tot] = lists:sublist(DiskL,Total,1),
						    [Fre] = lists:sublist(DiskL,Free,1),
						    [Pec] = lists:sublist(DiskL,PecentUsed,1),
						    [list_to_integer(textutils:readLong(Pec,1)),list_to_integer(Tot)*1024 - list_to_integer(Fre)*1024,list_to_integer(Tot)*1024]
						end;
					    true ->    
						[Tot] = lists:sublist(TDiskL,Total,1),
						[Fre] = lists:sublist(TDiskL,Free,1),
						[Pec] = lists:sublist(TDiskL,PecentUsed,1),
						[list_to_integer(textutils:readLong(Pec,1)),list_to_integer(Tot)*1024 - list_to_integer(Fre)*1024,list_to_integer(Tot)*1024]
					    end
					end;
				    _ ->
					[-1,0,-1] 
				    end;
				    
				    
				    
				    
				    
				_ -> 
				    [-1,0,-1]
				end;                    
			    _ ->                
				[-1,0,-1]
			    end                   
			end
		end.


get_Rows(List,String) ->
    get_Rows_t(List,String,length(List),0).
get_Rows_t(_L,_S,0,E) -> E;
get_Rows_t(Li,Str,Num,En) ->
    [A|B] = Li,
    if A == Str ->
        get_Rows_t(B,Str,0,En+1);
    true ->
        get_Rows_t(B,Str,Num-1,En+1) 
    end.   

getDiskFullFromLine([],_)->[0,0,0];
getDiskFullFromLine([L|T],Disk)->
	case regexp:match(L,"name:[\s]+" ++Disk ++"[:]?") of
		{match,_,_} ->
			io:format("getDiskFullFromLine:~p~n",[L]),
			[L1,L2|_] = T,                 
				R1 = case regexp:match(L1,"^408:[\s]+[0-9]+[\s]+% PERF_RAW_FRACTION[\s]?") of
						{match,_,_}->
							[_,T1|_] = string:tokens(L1,": "),
							case string:to_integer(T1) of
								{error,_}->
									0;
								{V1,_}->
									V1
							end;
						_->
							0
					end,
				R2 = case regexp:match(L2,"^408:[\s]+[0-9]+[\s]+PERF_RAW_BASE[\s]?") of
						{match,_,_}->
							[_,T2|_] = string:tokens(L2,": "),
							case string:to_integer(T2) of
								{error,_}->
									1;
								{V2,_}->
									V2
							end;
						_->
							1
					end,
				io:format("getDiskFullFromLine:~p,~p,~n",[R1,R2]),
				R3 = (100*R1)/R2,
				[round(100-R3),(R2-R1)*1024*1024,R2*1024*1024];
		_->
			getDiskFullFromLine(T,Disk)
	end.

%% get disks,return lists of disk
getDisks(S)->

	case machine:getSnmpMachine(S) of
		{ok, Ma=#machine{}} ->
			io:format("snmp_machine:getDisks  snmp_machine ~n",[]),
			snmp_machine:getDisks(S);
	_ ->
		I = machine:getOS(S),
		%%%%io:format("~p:getDisks,os:~p~n",[?MODULE,I]),
		Command = case machine:getCommandString(disks,S) of
		{error,_}->
		    dfCommand(S,"",I);
			{ok,V1}->
				V1;
			_->
				dfCommand(S,"",I)            
		end,
	       %~ io:format("~nCommand~p~n",[Command]),
		if
			I==1-> %windows platform
				Machs = machine:getNTMachine(S),
				case Machs of
					[]->
						{Status,ResultL} = siteview_commandline:exec(S,Command),
						 Ret = case Status of
									    ok ->
										ResultL;
									    _ ->
										[] 
								   end,     
						Lines = [list_to_tuple(string:tokens(X,":"))||X<-Ret],
						Disks = getDiskFromLine(Lines,""),
						%~ io:format("111Disks:~p~n", [Disks]),
						Disks;
					[Mach|_]->
						case Mach#machine.method of
							"WMI"->
								 %~ io:format("S:~p~n", [S]),
								proxy:disk(I, S, Mach#machine.login, Mach#machine.passwd);								
							_->
								{Status,ResultL} = siteview_commandline:exec(S,Command),
								Ret = case Status of
									    ok ->
										ResultL;
									    _ ->
										[] 
								   end,     
								Lines = [list_to_tuple(string:tokens(X,":"))||X<-Ret],
								Disks = getDiskFromLine(Lines,""),
								%~ io:format("2222Disks:~p~n", [Disks]),
								Disks
						end
				end;
		    
			true->
		    Machine = machine:getMachine(S),
		    if Machine == [] ->
			[];
		    true ->  
			[UnixMachine] = Machine, 
			if UnixMachine#machine.method == "SSH" orelse UnixMachine#machine.method == "Telnet" ->                
			    %String = sshcommandline:exec(Command,UnixMachine,true),
			    %DateList = string:tokens(String,"\n"),
			     %%io:format("DateList:~p~n",[{S,Command}]),
			    {Status,DateList} = siteview_commandline:exec(S,Command),
			   %% io:format("DateList:~p~n",[DateList]),
			    case Status of
			    ok ->                    
					    OsAdap = machine:getAdapter(S),
					    case OsAdap of
						    {error,_}->
							    [];
						_->
							    {ok,DisksName} = OsAdap:getCommandSettingAsInteger('disks', 'name'),
							    {ok,DisksMount} = OsAdap:getCommandSettingAsInteger('disks', 'mount'),
							    %%%%io:format("DisksName~p~nDisksMount~p~n",[DisksName,DisksMount]),
							    {NoNameFilterType,NoNameFilter} = OsAdap:getCommandSetting('disks', noNameFilter),      
					Flag1 = (NoNameFilterType == ok),
					Tid = init_lineReader(),                    
					LineReader = lineReader:new(DateList,machine:osToString(I),Tid,disks),                    
					Duu=getDisks_for_unix_util(S,DateList,LineReader,LineReader:processLine(),DisksName,DisksMount,Flag1,length(DateList)),
									ets:delete(Tid),
									Duu
									
					    end;
			    _ ->
				[] 
			    end; 
			true ->
			    []
			end                
		    end   
		end
	end.

getDisks_for_unix_util(Host,DateList,LineReader,PLflag,DisksNa,DisksMou,Flag1,ListNum) ->
    getDisks_for_unix_util_t(Host,DateList,LineReader,PLflag,DisksNa,DisksMou,Flag1,ListNum,[]).
getDisks_for_unix_util_t(_Hst,_DateLi,_LineR,false,_DisksN,_DisksM,_Flag1,_LN,R) -> R; 
getDisks_for_unix_util_t(Hst,DateLi,LineR,PLf,DisksN,DisksM,F1,LN,Re) ->
    Bool = LineR:skipLine(),
    if PLf ->
        Int = DisksM,
        if Bool ->
            getDisks_for_unix_util_t(Hst,DateLi,LineR,LineR:processLine(),DisksN,DisksM,F1,LN-1,Re);  
        true ->
            CurrentLine = LineR:getCurrentLine(),
            %%io:format("CurrentLine:~p~n",[CurrentLine]),
            CLen = length(CurrentLine),
            Index = string:str(CurrentLine,"/dev"),
            if F1 ->
                if Index == 1 ->
                    Bool1 = true;
                true ->
                    Bool1 = true
                end;
            true ->
                if Index == 1 ->
                    Bool1 = true;
                true ->
                    Bool1 = false
                end
            end,  
            if CLen > 1 , Bool1 ->
                NameString = LineR:readColumn(DisksN,"name"),
                CurrentListBool = (length(string:tokens(CurrentLine," ")) == 1),
                if CurrentListBool ->
                    LineR:processLine(),
                    DisksMT = Int - 1;
                true ->
                    DisksMT = Int
                end,
                MountStringT = LineR:readColumn(DisksMT,"mount"),
                MountStringLen = length(MountStringT),
                if  MountStringLen == 0 , LN >= 1 ->
                    NextLine = lists:sublist(DateLi,length(DateLi)-LN,1),
                    MountString = LineR:readColumn(DisksMT-1,"mount");
                true ->
                    MountString = MountStringT
                end,
                NameStringLen = length(NameString),
                MountStringLen = length(MountString),                
                if  NameStringLen == 0, NameStringLen == 0 ->
                    getDisks_for_unix_util_t(Hst,DateLi,LineR,false,DisksN,DisksM,F1,LN-1,[{?ERR_PARAM_API_UNABLE_TO_RETRIEVE_DISKS,Hst}]);
                true ->
                    [NS] = NameString,
                    [MS] = MountString,                    
                    getDisks_for_unix_util_t(Hst,DateLi,LineR,LineR:processLine(),DisksN,DisksM,F1,LN-1,lists:append(Re,[NS++" "++"("++MS++")"]))
                end;
            true ->
                getDisks_for_unix_util_t(Hst,DateLi,LineR,LineR:processLine(),DisksN,DisksM,F1,LN-1,Re)
            end                
        end;
    true ->
        getDisks_for_unix_util_t(Hst,DateLi,LineR,false,DisksN,DisksM,F1,LN,Re)
    end.    
        

getDiskFromLine([],_)->[];
getDiskFromLine([L|T],Obj)->
	if
		element(1,L)=="object"->
			getDiskFromLine(T,string:strip(element(2,L),both,32));
		element(1,L)=="name"->
			V = string:strip(element(2,L),both,32),
			if
				((Obj=="236 236") or (Obj == "LogicalDisk 236")) andalso (V=/="_Total") ->
					[V] ++ getDiskFromLine(T,Obj);
				true->
					getDiskFromLine(T,Obj)
			end;
		true ->
			getDiskFromLine(T,Obj)
	end.

getServers()->
	CM = [{X#machine.name,X#machine.host}|| X<-dbcs_machine:get_all()],
	%%R = os:cmd("net view"),
	%%T = string:tokens(R,"\r\n"),
	[{"this server",""}] ++ CM.%% ++ [{X,X}||X<-strip_line(T)].


strip_line([])->[];
strip_line([L|T])->
	NL = string:strip(L,both,32),
	[NNL|_] = string:tokens(NL," "),
	case string:substr(NNL,1,2) of
		"\\\\"->
			[NNL] ++ strip_line(T);
		_->
			strip_line(T)
	end.
	

dfCommand(Host,Cmd,HostType)->
	%%%%io:format("dfCommand:~p,~p,~p~n",[Host,Cmd,HostType]),
	case HostType of
		1->
			" =236";
		2->
			"/usr/bin/df -k " ++ Cmd;
		3->
			"/usr/sbin/df -k " ++ Cmd;
		4->
			"/usr/sbin/df -k " ++ Cmd;
		5->
			"/usr/bin/df -kP " ++ Cmd;
		6->
			"/bin/df -k " ++ Cmd;
		_->
			"/bin/df -k" ++ Cmd
	end.

%-record(machine,{id,name="",host="",login="",passwd="",os="nt",status="unknown",method="",sshcommand="",sshport=22,version="",keyfile="",sshauthmethod=""}).
%If the name is no initial value, may be undefined, list_to_binary (name) when abnormal
sslCommandLine(Host) ->
    case  isWindows() of
	    true ->
		    case machine:getMachine(Host) of
	        [] ->
		        "";
	        [M|_] ->
                case M#machine.keyfile of
                "" ->
                    "tools\\plink -ssh " ++ M#machine.login++"@"++M#machine.host++" -P "++integer_to_list(M#machine.sshport) ++ " -pw " ++ M#machine.passwd  ++ "  ";				
                _ ->
				    "tools\\plink -ssh " ++ M#machine.login++"@"++M#machine.host++" -P "++integer_to_list(M#machine.sshport) ++ " -pw " ++ M#machine.passwd ++" -i "++M#machine.keyfile ++ "  "
                end;           
		   _ ->
                ""
            end;
        _ ->
            "ssh  " ++ Host 
    end.			

perfexCommand(Host)->
	case isWindows() of
		true->
			case Host of
				""->
					"tools\\perfex.exe";
				_->
					%%Name = case Host of 
					%%		"\\\\"++V->
					%%			V;
					%%		_->
					%%			Host
					%%		end,
					case machine:getNTMachine(Host) of
						[]->
							"tools\\perfex.exe -connect " ++ Host;
						[M|_]->
							"tools\\perfex.exe -connect " ++ M#machine.host ++ " -u " ++ M#machine.login ++ " -p " ++ M#machine.passwd
					end
			end;
		_->
			"perfex -connect " ++ Host
	end.
perfexCommand1(Host)->
	case isWindows() of
		true->
			case Host of
				""->
					"tools\\sqlserver.exe";
				_->
					%%Name = case Host of 
					%%		"\\\\"++V->
					%%			V;
					%%		_->
					%%			Host
					%%		end,
					case machine:getNTMachine(Host) of
						[]->
							"tools\\sqlserver.exe -connect " ++ Host;
						[M|_]->
							"tools\\sqlserver.exe -connect " ++ M#machine.host ++ " -u " ++ M#machine.login ++ " -p " ++ M#machine.passwd
					end
			end;
		_->
			"perfex -connect " ++ Host
	end.

    
closeAndConnectNetBIOSIfRemoteDefined(Path) ->
    HostName = getHostname(Path),
    Len = length(HostName),
    if Len > 0 ->
        Machine = machine:getNTMAchine(HostName),
        if Machine /= [] ->
            contactUsingNetBIOS(HostName,false), 
            Int =  contactUsingNetBIOS(HostName,true),
            if Int == -998 ->
                ResString =  "SiteView was not able to connect to NT remote: <b>" ++ HostName ++ ".</b>  The remote file needs to be mounted locally or the NT remote needs to test OK.";                
            true ->
                ResString = ""
            end;
        true -> 
            ResString = ""
        end;
    true ->
        ResString = ""
    end.        

getHostname(Path) ->
    Ind1 = string:str(Path,"\\\\"),
    if Ind1 == 1 ->
        Ind2 = string:str(string:substr(Path,3),"\\"),
        if Ind2 > 1 ->
            string:substr(string:substr(Path,3),1,Ind2-1);
        true ->
            ""
        end;
    true ->
        ""
    end.        

pathSeparator(OsNum) ->
    case OsNum of
	1 ->
	    Str = "\\";
    _ ->
        Str = "/"
    end.

%return bool
isNTRemote(Host) ->
    Ind = string:substr(Host,1,2),
    if Ind == "\\" ->
	    true;
    true ->
        false
    end. 		

%
getLocalCode() ->
    case getOs() of
    1 ->
        "GBK";
    _ ->
        %OsAda = machine:getAdapter(""),
        %Cmd = OsAda:getCommandString(echoLang),
        [Str] = lists:sublist(string:tokens(os:cmd("echo $LANG"),"\n"),1,1),
        [Type,Code] = string:tokens(Str,"."),
        Code         
    end. 

sleep(Time)->
	receive
	after Time ->
	ok
	end.	