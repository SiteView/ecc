%%
%% machine
%%

%% @doc machine
%% @version 1.0
%% @copyright 2009 dragonflow.com
%% @author lei.lin@dragonflow.com
-module(machine).
-compile(export_all).
-include("monitor.hrl").
-define(OS_NAME,[{"nt",1},{"sunsolaris",2},{"sun_solaris",2},{"sgi",3},{"macosx",4},{"hp",5},{"linux",6},{"openserver",7},{"open_server",7},{"hp64",8},{"redhatenterpriselinux",9},{"red_hat_enterprise_linux",9},{"redhat4",9},{"suselinux10",9},{"suselinux11",9},{"sco",10},{"tru64",11},{"freebsd",12},{"digital",13},{"aix",14}]).

-export([getMachineTable/0,addNTMachineLogin/3,getNTMachineTable/0,getNTMachine/1,getCurrentUser/0,createMachines/1,createMachine/1,getMachine/1,getMachinePathSeparator/1,getMachineByHost/1,getMachineName/1,getCommandSetting/3,getCommandString/3,getOS/1,stringToOS/1,getAdapter/1]).

%% @spec getMachineTable() -> Result
%% Result = [tuple()]
%% @doc get all machine,value is list of records.
%get all machine,value is list of records
getMachineTable()->
	dbcs_machine:get_all().

%% @spec addNTMachineLogin(Host,Username,Password) -> ok
%%  Host = string()
%%  Username = string()
%%  Password = string() 
%% @doc  create windows machine recode.
addNTMachineLogin(Host,Login,Pass)->
	case dbcs_machine:get_machine(Host) of
		[]->
			dbcs_machine:create_machine(#machine{id=dbcs_base:get_id(),host=Host,login=Login,passwd=Pass,os="nt",method="NetBIOS"});
		_->
			dbcs_machine:remove_machine_by_host(Host),
			dbcs_machine:create_machine(#machine{id=dbcs_base:get_id(),host=Host,login=Login,passwd=Pass,os="nt",method="NetBIOS"})
	end.

%% @spec getSnmpMachine(Host) -> {ok, [Machine=#machine{}]} | {error, _}
%%  Host = string() 
%% @doc  create windows machine recode.
getSnmpMachine(Host) ->
    snmp_machine:getSnmpMachine(Host).

%% @spec getNTMachineTable() -> Result
%% Result = [tuple()]
%% @doc get all windows machine,value is list of records.
%get all NT machine,value is list of records
getNTMachineTable()->
	dbcs_machine:get_NTmachine().

%% @spec getNTMachine(Host) -> Result
%%  Host = string()
%% Result = [tuple()]
%% @doc get windows machine by Ip
getNTMachine(Host)->
	dbcs_machine:get_machine_match("my.host=" ++ Host ++ "&my.os=nt").

%% @spec getCurrentUser() -> Result
%% Result = atom()
%% @doc get current user
getCurrentUser()->
	 platform:currentUser().


registerMachines(_Machines)->
	ok.

registerNTMachines(_S)->
	ok.

registerMachines(_Props,_S)->
	ok.

%% @spec createMachines(MachsList) -> ok
%% MachsList = [term()]
%% @doc create Machines,parameter is list of records.
%create Machines,parameter is list of records
createMachines(MachsList)->
createMachines_t(MachsList,length(MachsList)).
createMachines_t(_L,0) -> ok;
createMachines_t(L,N) ->
    [A|B] = L,
	dbcs_machine:create_machine(A),
	createMachines_t(B,N-1).

%% @spec createMachine(Machine) -> ok
%% Machine = term()
%% @doc create Machine,parameter is  record.
%create machine 
createMachine(Machi) ->
    dbcs_machine:create_machine(Machi).
	
	
%% @spec getMachine(Host) -> Result
%%  Host = string()
%%  Result =  term()
%% @doc get machine by Ip
getMachine(Host)->
	dbcs_machine:get_machine_by_host(Host).

getMachine(Localhost,Host)->
	dbcs_machine:get_machine_by_host(Localhost,Host).

getNTAllowedMethods()->
	[{"NetBIOS","NetBIOS"},{"ssh","ssh"}].


%% @spec getMachinePathSeparator(Host) -> Result
%%  Host = string()
%%  Result =  string()
%% @doc get Machine Path Separator,windows is \"\\\",unix is \"/\"  
%return string
getMachinePathSeparator(Host) ->
    platform:pathSeparator(getOS(Host)).


%return machine list of record
getMachineByHost(Host) ->
	Machine = dbcs_machine:get_machine_match("my.host=" ++ Host),
	case Machine of
	[] ->
	    [];
	[M|_] ->
        [M];
	_ ->
       	[]
    end.
    % MachiList = getMachineTable(),
    % getMachineByHost_t(MachiList,length(MachiList),Host).	
% getMachineByHost_t(L,0,_Hnane) -> L;
% getMachineByHost_t(List,Num,Hname) ->
    % [A|B] =List,
	% HN = A#machine.host,
	% if HN == Hname ->
	    % getMachineByHost_t([A],0,Hname);
	% true ->
	    % getMachineByHost_t(B,Num-1,Hname)
	% end.	



getNTMachineHost(Name) ->
    Machine = dbcs_machine:get_machine_match("my.name=" ++ Name),
	case Machine of
	[] ->
	    {error,host_undefined};
	[M] ->
        M#machine.host;
	_ ->
       	{error,undefined}
    end.

%
getMachineByName(Name) ->
	Machine = dbcs_machine:get_machine_match("my.name=" ++ Name),
	case Machine of
	[] ->
	    [];
	[M] ->
        M;
	_ ->
       	[]
    end.
	
    % MachiList = getMachineTable(),
    % getMachineByName_t(MachiList,length(MachiList),Name).	
% getMachineByName_t(L,0,_Hnane) -> L;
% getMachineByName_t(List,Num,Hname) ->
    % [A|B] =List,
	% HN = A#machine.name,
	% if HN == Hname ->
	    % getMachineByName_t(A,0,Hname);
	% true ->
	    % getMachineByName_t(B,Num-1,Hname)
	% end.	
	
%% @spec getMachineName(Host) -> Result | {error,Reason}
%%  Host = string()
%%  Result =  string()
%% @doc get machine name by host.
%get machine name by host
getMachineName(Host) ->    
    Machine = dbcs_machine:get_machine_match("my.host=" ++ Host),
    case Machine of
	[] ->
	    {error,host_undefined};
	[M] ->
        M#machine.name;
    _ ->
        {error,undefined}
	end.

%%?
getMachieHost(Name) ->
    Machine = getMachine(Name),
    case Machine of
	[] ->
	    {error,host_undefined};
	[M] ->
        M#machine.host;
    _ ->
        {error,undefined}
	end.
   
%   
%getProperty() ->


%getAllowedSshConnectionMethods() ->
 
%getAllowedSshAuthMethods() -> 
 
%return OSs list [{ID,Name}] 
%getAllowedOSs() -> 
%    [{}]    


%return boolean 
isPortalMachineID(Host) ->
    Index = string:str(Host,"@"),
	if Index > 0 ->
	    true;
	true ->
	    false
    end. 	

%if Host include "@",return "test",like "test@dragonflow.com"
getMachineFromMachineID(Host) ->
    Index = string:str(Host,"@"),
    if Index > 0 ->
        string:substr(Host,1,Index-1);
    true ->
        Host
    end.		
   
%% @spec getCommandSetting(Host,TemplateItemName,SubTemplateItemName) -> Result | {error,Reason}
%%  Host = string()
%%  TemplateItemName = atom()
%%  SubTemplateItemName = atom()
%%  Result =  term()
%% @doc get os template subitem value.
%just call osAdapter's getCommandSetting()   
getCommandSetting(Host,TemplateTermName,SubTemplateTermName) ->
	M = dbcs_machine:get_machine_match("my.host=" ++ Host),
    case M of
    [] ->
		{error,machine_undefined};
    [_Machi|_] ->    
        OsaI = getAdapter(Host),
        OsaI:getCommandSetting(TemplateTermName,SubTemplateTermName);
    _ ->
        {error,undefined}
    end.      

%getServerIDFromMachineID(Host) ->

%getMachineFromMachineID() ->

%getFullMachineID() ->



%getRemoteCommandLine() ->
  


%getRemoteCommandLineClasses() ->


%isLocalHostname(Host) ->
%    if Host ==  ""
     
     
 
isNetBIOSFormattedHostname(Host) ->
    Index = string:str(Host,"\\\\"),
    Index == 1.



getCommandString(_Operate,_Host) ->
    getCommandString(_Operate,_Host,[]).


%% @spec getCommandString(TemplateItemName,Host,List) -> Result | {error,Reason}
%%  Host = string()
%%  TemplateItemName = atom()
%%  List = [term()]
%%  Result =  string()
%% @doc get command.
%TemplateItemName is atom,OsName is string,Params is list,if not find just return {error,command_undefined}
getCommandString(TemplateItemName,Host,List)->
        Bool = isPortalMachineID(Host),
        if  Bool ->
            Hostname = getMachineFromMachineID(Host),
            Len = length(Hostname),
	        Index = string:str(Host,"\\\\"),
            if Len > 0,Index == 0 ->
                "page=remoteOp&operation=run&command=" ++  atom_to_list(TemplateItemName) ++ "&machineID=" ++  Hostname;
            true ->
               {error,host_dirty_value}
            end;
        true ->
	        M = dbcs_machine:get_machine_match("my.host=" ++ Host),
		    case M of
		    [] ->
		        {error,machine_undefined};
		    [_Machi|_] ->               
		        OsaI = getAdapter(Host),
                Len = length(List),
                if Len > 0 ->		        
                    OsaI:getCommandString(TemplateItemName,List);
                true ->
                    OsaI:getCommandString(TemplateItemName)
                end;                    
            _ ->
          	    {error,undefined}	
		    end	
        end.	 
 
make_os_name(List) ->
    make_os_name_t(List,length(List),"").
make_os_name_t(_L,0,R) -> R;
make_os_name_t(L,N,Re) ->
    [A|B] = L,
	make_os_name_t(B,N-1,Re ++ A).

%return bool
isNTSSH(Host) ->
    Startstr = string:substr(Host,1,2),
	if Startstr == "\\\\" ->
	    %Hn =  string:substr(Host,3),
		[Machine] = getNTMachine(Host),
		if Machine#machine.os=="nt",Machine#machine.method=="SSH" ->
		    true;
        true ->
            false
        end; 			
	true ->	
	    false
    end.
    
isNTWMI(Host) ->
    Startstr = string:substr(Host,1,2),
	if Startstr == "\\\\" ->
	    %Hn =  string:substr(Host,3),
		[Machine] = getNTMachine(Host),
		if Machine#machine.os=="nt",Machine#machine.method=="WMI" ->
		    true;
        true ->
            false
        end; 			
	true ->	
	    false
    end.


%if command have parameter, like {command,"/bin/df -k <disk>"} 
replace_parameter(Cmd,Para) ->
    replace_parameter_t(Cmd,Para,length(Para)).
replace_parameter_t(C,_P,0) ->	C;
replace_parameter_t(Command,Pa,Num) ->
    [A|B] = Pa,
	{Name,Value} = A,
	case regexp:sub(Command,"<" ++ atom_to_list(Name)++ ">",Value) of
	    {ok,Res} ->
            replace_parameter_t(Res,B,Num-1);
        _ ->			
            replace_parameter_t(Command,Pa,0)   
    end.

%% @spec getOS(Host) -> Result
%%  Host = string()
%%  Result =  string()
%% @doc get os name by number.
getOS("")->platform:getOs();
getOS(S)->
	case S of
		"\\\\" ++ _V1 ->
			1;
		_->
			case getMachine(S) of
				[]->
					platform:getOs();
				[M|_]->
				        %io:format("OS:~p~n",[{M#machine.os,stringToOS(M#machine.os),getOSlist()}]),
					stringToOS(M#machine.os);
				_->
					platform:getOs()
			end
	end.
	

%%  oldhand add
%% @doc get os list from conf/os.conf.
getOSlist()->
        case file:consult("conf/os.conf") of
		{ok,Data} -> Data;	   
		_ -> ?OS_NAME
        end.
	
%% @spec stringToOS(Osname) -> Result
%%  Osname = string()
%%  Result =  integer()
%% @doc get os  number by os name.		
stringToOS(Name)->
        LowerName = string:to_lower(Name),
	case lists:keysearch(LowerName,1,getOSlist()) of
		{value,{LowerName,Os}}->
			Os;
		_->
			1
	end.


osToString(OsNum) ->
   	case lists:keysearch(OsNum,2,?OS_NAME) of
		{value,{Name,_Os}}->
			textutils:delete_bar(Name);
		_->
			1
	end. 

isLocalHostname(_HostName)->ok.

getOSName("")->
	I = platform:getOs(),
	case lists:keysearch(I,2,?OS_NAME) of
		{value,{Name,_}}->
			Name;
		_->
			"nt"
	end;
getOSName(S)->
	case re:run(S,"^\\\\") of
		{match,_}->
			"nt";
		_->
			case getMachine(S) of
				[]->
					getOSName("");
				[M|_]->
					M#machine.os
			end
	end.

%% @spec getAdapter(Host) -> Result
%%  Host = string()
%%  Result = term()
%% @doc get Adapter object.	
getAdapter(Host)->
    OsName = getOSName(Host),   
    List = string:tokens(OsName,"_"),      
	Adap=osAdapter:new(list_to_atom(make_os_name(List))),
	Adap:initEts(),
	Adap.

