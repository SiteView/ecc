%%
%% osAdapter
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2008-2009 Dragonflow
%% @version 1.0
%% @doc osAdapter
-module(osAdapter,[ETStable]).
-compile(export_all).


getOSName() ->
    Value  = ets:lookup(ETStable,name),
	case Value of
	[] ->
	    {error,undefined};
	[{_,Val}] ->
        Val;
    _ ->
     	{error,undefined}
	end.

getOSID() ->
    Value = ets:lookup(ETStable,id),
	case Value of
	[] ->
	    {error,undefined};
	[{_,Val}] ->
        Val;
    _ ->
     	{error,undefined}
	end.

%return {ok,String}
getCommandString(CmdIdValue) ->
    List = getCommand(CmdIdValue),    
    case List of
	{ok,Val} ->
	    V = textutils:getValue(Val,command),
		case V of
		"" ->
		    {error,undefined};
		_ ->
            {ok,V}
        end;
    {error,Reason} ->
	    {error,Reason}
	end.	
    
%List is list of tuple    
getCommandString(CmdIdValue,List) ->
    case length(List) of
    0 ->
        getCommandString(CmdIdValue);
    _ ->
        Res =getCommand(CmdIdValue),
        case Res of
        {ok,Val} ->
            V = textutils:getValue(Val,command),
            case V of
            "" ->
                {error,undefined};
            _ ->
                {ok,replaceVariables(V,List)}
            end;
        {error,Reason} ->
            {error,Reason}
        end            
    end. 
    
%List is list of tuple    
replaceVariables(String,List) ->
    replaceVariables_t(String,List,length(List)).
replaceVariables_t(Str,Li,0) ->Str;
replaceVariables_t(S,L,N) ->
    Index1 = string:str(S,"<"),
    Index2 = string:str(S,">"),
    Key = string:substr(S,Index1+1,Index2-Index1-1),
    case lists:keysearch(Key,1,L) of
    {value,{_K,Value}} ->
        SubStr1 = string:substr(S,1,Index1-1), 
        SubStr2 = string:substr(S,Index2+1),
        String = SubStr1 ++ Value++ " " ++SubStr2,
        replaceVariables_t(String,L,N-1);
    _ ->
        replaceVariables_t(S,L,N-1)
    end.       

%get Command Subkey Value
getCommandSetting(CmdIdValue,Subkey) ->
    List = getCommand(CmdIdValue),
	case List of
	    {ok,Value} ->
		    V = textutils:getValue(Value,Subkey),
            case V of
            "" ->
                {error,not_subkey};
            _ ->
                {ok,V}
            end;
        {error,_Reason} ->
            {error,not_subkey}
    end.
	
%if not subkey value,return Default
getCommandSetting(CmdIdValue,Subkey,Default) ->
    Value = getCommandSetting(CmdIdValue,Subkey),
	case Value of
    {error,_Rea} ->
	    {ok,Default};
	{ok,Val} ->
       	{ok,Val};
    _ ->
        {ok,-1}	
    end.

%get Command Subkey Value as integer
getCommandSettingAsInteger(CmdIdValue,Subkey) ->
    Res = getCommandSetting(CmdIdValue,Subkey),
    case Res of
	{ok,Val} ->
	    {ok,textutils:toInt(Val)};
	Other ->
    	{error,Other}
	end.
	
%get Command Subkey Value as integer,if not subkey value,return Default
getCommandSettingAsInteger(CmdIdValue,Subkey,Default) ->
    Res = getCommandSetting(CmdIdValue,Subkey),
    case Res of
	{ok,Val} ->
	    {ok,textutils:toInt(Val)};
	_Other ->
    	{ok,Default}
	end.
             
%get CmdId value from ets,value is list,CmdIdValue like 'pageFault'
getCommand(CmdIdValue) ->
    Value = ets:lookup(ETStable,CmdIdValue),
	case Value of
	[] ->
	    {error,undefined_command};
	[{_,Val}] ->
	    {ok,Val}; 
    _ ->
        {error,undefined}
    end. 		

%get match value from list
getMatchedCommandSettings(CmdIdVal,Match) ->
    Value = getCommand(CmdIdVal),
	case Value of
	{error,Res} ->
	     {error,Res};
	{ok,Va} ->
        List = getKeys(Va),
        V = match(List,Match),
		case V of
		[] ->
		    {error,not_match};
		Val ->
            {ok,Val} %Val is list
        end;			
    _ ->
        {error,undefined}
    end.		

% _List is atom list
match(_List,Key) ->
    match_t(_List,Key,length(_List),[]).
match_t(_L,_K,0,R) -> R;
match_t(L,K,N,Re) ->
    [A|B] = L,
    Num = string:str(atom_to_list(A),K),
    if Num > 0 ->
        match_t(B,K,N-1,lists:append(Re,[A]));
    true ->
        match_t(B,K,N-1,Re)
    end.		

%get keys from [{K1,V1},{K1,V2}],return list.
getKeys(List) ->
    getKeys_t(List,length(List),[]).
getKeys_t(_L,0,R) -> R;
getKeys_t(L,N,Re) ->
    [A|B] = L,
    Value = erlang:element(1,A),
    getKeys_t(B,N-1,lists:append(Re,[Value])).	
			




%init ets table,like java's hashmap
initEts() ->
    Osnum = platform:getOs(),
    case is_atom(ETStable) of
	true ->
        case ets:info(ETStable) of
        undefined ->
		    ets:new(ETStable,[public,named_table]), 
			if Osnum == 1 ->
                Cos = file:consult("template.os\\" ++ atom_to_list(ETStable) ++ ".config");
            true ->
			    Cos = file:consult("template.os/" ++ atom_to_list(ETStable) ++ ".config")
            end,		
            case Cos of
			{error,Reason} ->
                {error,Reason};
            {ok,Res} ->
			    insertEts_from_List(ETStable,Res);                
            _ ->
                {error,undefined}
            end;
        _ ->
            ok
		end;	
    _ ->
        {error,bad_argument}
    end.
	

insertEts_from_List(Tab,List) ->
insertEts_from_List_t(Tab,List,length(List)).
insertEts_from_List_t(_T,_L,0) ->ok;
insertEts_from_List_t(T,L,N) ->
    [A|B] = L,
	ets:insert(T,A),
	insertEts_from_List_t(T,B,N-1).


getOSAdapter(S)->
	{ok,[M|_]}=file:consult(io_lib:format("/template.os/~p.conf",[S])),
	M.

testCPU(Machine,SiteviewGroup,Flag)->
	ok.

testDiskFull(Machine,SiteviewGroup,Flag)->
	ok.

testMemory(Machine,SiteviewGroup,Flag)->
	ok.

testService(Machine,SiteviewGroup,Flag)->
	ok.

runMonitor(Monitor,Flag)->
	Monitor:testUpdate().

getOSName(S)->
	{ok,[M|_]}=file:consult(io_lib:format("/template.os/~p.conf",[S])),
	case lists:keysearch(name,1,M) of
		{value,{name,Name}}->
			Name;
		_->
			""
	end.
	
getOSID(S)->
	{ok,[M|_]}=file:consult(io_lib:format("/template.os/~p.conf",[S])),
	case lists:keysearch(id,1,M) of
		{value,{id,Id}}->
			Id;
		_->
			""
	end.


