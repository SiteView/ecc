%% ---
%% mailto
%%
%%---
-module(script,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").

-define(PRODUCT_NAME,"elecc").

-define(LOG_TYPE,"Script alert run").
-define(TIMEOUT,30).
-define(TAG_STYLE_TAG,"[Tag-Style:").

-define(runScriptsDLL,"runWinScripts").  %%-- yi.duan execute Scrpts on remote Windows

new(Monitor,Rule)->
	Obj = action:new(Monitor,Rule),
	{?MODULE,Obj,Monitor,Rule}.
	
get_monitor()->{ok,{monitor,Monitor}}.

get_rule()->{ok,{rule,Rule}}.
 
execute() ->
    executeSync().
    
executeSync()->
	{ok,{_,Params}} = Rule:get_property(action_param),        
    {ok,{_,Enabled}} = Rule:get_property(enabled),
	case Rule:get_property(disabled) of
		{ok,{_,true}}->
			{error,"disabled"};
		_->
			case THIS:check_enabled(Enabled) of
				false ->
					{error,"time_disabled"};
				_->
					runScriptAlert(Params)
			end
	end.    

is_lock(Fidid) ->
	global:set_lock({Fidid, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock(Fidid) ->	
	global:del_lock({Fidid, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).

replace(String,OldChar,NewChar) ->
    replace_t(String,OldChar,NewChar,length(String),"").
replace_t(_S,_O,_N,0,Res) -> Res;
replace_t(Str,Old,New,Len,R) ->
    Char = string:substr(Str,Len,1),
    if Char == Old ->
        replace_t(string:substr(Str,1,Len-1),Old,New,Len-1,New++R);
    true ->
         replace_t(string:substr(Str,1,Len-1),Old,New,Len-1,Char++R)  
    end.

verify(Params)->
	{ok,""}.
    
getScalarValues(Prop,Params)->
	case Prop of
		script->
			case lists:keysearch(machine,1,Params) of
		    {value,{machine,Machine}} ->
				make_tuple(cleanlist(getScriptList(Machine,"scripts")));
		     _->
				make_tuple(cleanlist(getScriptList("","scripts")))
			end;
		template->
			[{filename:basename(X),filename:basename(X)}||X<-filelib:wildcard("templates.script/*")];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

make_tuple(ScriptNameList) ->
   make_tuple_t(ScriptNameList,length(ScriptNameList),[]).
make_tuple_t(_L,0,R) ->  R;
make_tuple_t(L,N,Re) ->
    [A|B] = L,
    make_tuple_t(B,N-1,lists:append(Re,[{A,A}])).

cleanlist(ScriptList) ->
    cleanlist_t(ScriptList,length(ScriptList),[]).
cleanlist_t(_S,0,E) -> E;
cleanlist_t([A|B],Num,En) ->
    Index = string:str(A,"."),
    Index2 = string:str(A,".txt"),     
    if Index == 1 ->
        cleanlist_t(B,Num-1,En);
    true ->
        if Index2 > 1 ->
            cleanlist_t(B,Num-1,En);
        true ->    
            cleanlist_t(B,Num-1,[A|En])
        end            
    end.

getScriptList(Host,LocalScriptLocation) ->
    if Host /= [] -> 
        Osnum = machine:getOS(Host),
        Bool1 = machine:isNTSSH(Host),
        io:format("Osnum: ~p~n", [Osnum]),
        io:format("Bool1: ~p~n", [Bool1]),
        if Bool1 ->
            FilesList = remotefile:listFiles(Host,"scripts",Osnum);
        true ->
            %Bool2 = platform:isCommandLineRemote(Host),
            FilesList = remotefile:listFiles(Host,"scripts",Osnum),
            io:format("FilesList: ~p~n", [FilesList]),
            case FilesList of
            [] ->
                Path = platform:getDirectoryPath("scripts",""),
                case  file:list_dir(Path)  of
                {ok,FilesL} ->
                    FilesL;
                _ ->
                    []
                end;
            FlieL ->
                filt_file(FlieL,Osnum)         
            end    
        end;
    true ->
        Path = platform:getDirectoryPath(LocalScriptLocation,""),    
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

get_template_property()->
	%BASE:get_template_property() ++ 
	[
        #property{name=machine,title="Host",type=server,editable=true,order=1},   
	    #property{name=script,title="Script",type=scalar,multiple = true,listSize=5,description="the name of the script to be run. Scripts must reside in a directory called \"scripts\" on the server selected above."},
	    #property{name=parameters,title="Parameters",type=text},  
        #property{name=template,title="Template",type=scalar,description="the template used to create the file referenced by the script."},
        #property{name=parameters,title="Parameters",type=text,description="additional parameters to pass to the script."}  
	].
    
remoteCommandLineAllowed()->
	true.


appendParameters(List,NewParams) ->
    if length(NewParams) /= 0 ->       
        TokenResList = textutils:tokenize(NewParams), %将参数按要求token
        NewParaList = appendParameters_util(TokenResList),%去处非法字符 
        List ++ NewParaList;
    true ->
        List   
    end. 
        
appendParameters_util(TokenResList) ->     
    appendParameters_util_rec(TokenResList,length(TokenResList),[]).
appendParameters_util_rec(TokenR,0,Res) -> Res;
appendParameters_util_rec([A|B],Len,R) ->
    Bool = textutils:hasChars(A,"`;&|<>"),
    if Bool ->
        io:format("Removed illegal characters from script parameter \"" ++ A ++"\""),
        String = textutils:removeChars(A,"`;&|<>"),
        appendParameters_util_rec(B,Len-1,R ++ [String]);           
    true -> 
        appendParameters_util_rec(B,Len-1,R ++ [A])
    end.
    
runScriptAlert(Params) ->   
    %localLogFilePath = localLogFilePath(), 
    [Host] = Params#script_alert.server, 
    io:format("Host: ~p~n", [Host]),
    [Script] = Params#script_alert.script, 
	io:format("Script: ~p~n", [Script]),
    [InitParams] = Params#script_alert.params,
    AnnalFileName = "alert" ++ ".txt",
	
    Name = case Monitor:get_property(?NAME) of
		      {ok,{_,N}}->
			    N;
		      _->
			    ""
		   end,
		   
    STATE_STRING = case Monitor:get_attribute(?STATE_STRING) of
		              {ok,{_,State}}->
					     State;
					   _->
						 ""
				   end,  
				   
    case platform:getOs() of
		1 ->
			AnnalFilePath = "scripts" ++ "\\"  ++ AnnalFileName;
		_ ->
			AnnalFilePath = "scripts" ++ "/"  ++ AnnalFileName 
	end,
	
    if length(Script) == 0 ->
        S1 = "missing script name",
        S7 = "missing";
    true ->
        S7 = Script,
        S1 = ""
    end,  
	
    ScriptParams = replace(replace(InitParams,"_"," "),"#","_"),
	io:format("replace(InitParams)->ScriptParams = ~p~n",[ScriptParams]),
    if length(S1) == 0 ->
        Msg = THIS:createMessage("templates.script",Params#script_alert.template),
        %% *** 
        %% is_lock(AnnalFilePath),        
        %% case file:write_file(AnnalFilePath,list_to_binary(Msg)) of
        %% ok ->
        %%    ErrorMessage = "";
        %% _ ->
        %%    ErrorMessage = "Error writing alert.txt" 
        %% end,
        %% release_lock(AnnalFilePath);
        ErrorMessage = "",
        ok;
      true ->  
        ErrorMessage = S1      
    end,
	
	if length(ErrorMessage) =:= 0 ->
        {FileExists,ScriptFilePath,ScriptDirPath} = scriptPath(Host,Script),  
        if FileExists; Script =:= "playRemoteSound" ->
            IsWindows = platform:isWindows(),
            if IsWindows -> 
                NameHasSpaces = textutils:hasSpaces(Name),
				  if NameHasSpaces ->
                      Rname = "\"" ++ Name ++ "\"";
						true ->
                      Rname = Name   
				  end;
				true ->
					Rname = Name              
            end,
            OsNum = machine:getOS(Host),
            RemoteIsWindows = platform:isWindows(OsNum),
            if RemoteIsWindows -> 
                 Array1 = ["cmd","/c"];
               true ->
                 Array1 = [] 
            end, 
            RnamehasChars = textutils:hasChars(Rname,"`;&|<>"),
          
        	if RnamehasChars -> 
                io:format("Removed illegal characters from monitor name \"" ++ Rname ++ "\" before running script"), 
                RRname = textutils:removeChars(Rname,"`;&|<>");
            true ->    
                RRname = Rname 
            end,
            NextArray1 = Array1++ [ScriptFilePath] ++ [ScriptDirPath] ++ [RRname] ++ [STATE_STRING] ++ [AnnalFilePath],
            % TimeOutMillisecond = ?TIMEOUT * 1000,  
            % RParamsList = appendParameters(NextArray1,ScriptParams),

            is_lock({Script,Host}),
            if (Host /= []) and (Host  /= "this server") ->  
				case Script of
                   	 "playRemoteSound" ->
					    %% {ok,Result} 		
						[Machine]= machine:getMachine(Host),
						
						io:format("playRemoteSound getMachine = ~p....~n",[Machine]),
						MachineList = tuple_to_list(Machine),
						User = lists:nth(5,MachineList),
						PassWord = lists:nth(6,MachineList),
						
					    %% delete "////"
						StatuTem = runWinScripts({lists:nthtail(2,Host), User, PassWord, InitParams}),
						io:format("Statu = ~p~n", [StatuTem]),
						Result = StatuTem,
						Statu = list_to_atom(StatuTem);
					 Other ->
						io:format("Other ~p~n", [Other]),
						{Statu,Result} = siteview_commandline:exec(Host,"./"++ScriptFilePath ++" "++ ScriptParams)
				end;
              true ->
                Result = string:tokens(os:cmd(ScriptFilePath ++" "++ ScriptParams),"\r\n"),
                Statu = ok
            end,  
            release_lock({Script,Host}), 
			
            case Statu of
            ok ->
                ExitValue = 0;
            _ ->
                ExitValue = -1 
            end, 
			
            RunStatuString = Script ++ "(" ++ integer_to_list(ExitValue) ++ ")",
            if ExitValue == 0 ->
                RunFlag = true;
            true ->
                RunFlag = false 
            end,
            io:format("Result: ~p~n", [Result]), 
            NewlineString = makeNewlineString(Result); 
        true ->
            ExitValue = -1, 
            RunFlag = false, 
            NewlineString = "", 
            RunStatuString =  "file missing: " ++ ScriptFilePath 
        end;        
    true -> 
        RunStatuString = ErrorMessage, 
        ExitValue = -1, 
        RunFlag = false,
        NewlineString = ""        
    end,
	
    if Host == [] ->
        ScriptServer = "localhost";
    true ->
        ScriptServer = Host 
    end,
	
    if RunFlag /= true -> 
        %send email           
        RunStr = "SCRIPT ALERT ERROR RESULT",        
        % THIS:logAlert(?LOG_TYPE, ScriptServer, Script++" "++"on"++" "++ ScriptServer,ExitValue,"fail");
		THIS:logAlert(?LOG_TYPE, ScriptServer, Script++" "++"on"++" "++ ScriptServer,NewlineString,"fail");
    true ->     
        RunStr = "Script alert performed",
        % THIS:logAlert(?LOG_TYPE, ScriptServer, Script++" "++"on"++" "++ ScriptServer,ExitValue,"ok")  
		THIS:logAlert(?LOG_TYPE, ScriptServer, Script++" "++"on"++" "++ ScriptServer,NewlineString,"ok")
    end,
	
    {ok,{script,Name,RunStr,ExitValue,RunStatuString,STATE_STRING,NewlineString}}.


scriptPath(Host,Script) ->
        if Host =:= [] ->
            case platform:getOs() of
            1 ->
                ScriptFilePath =  "scripts" ++ "\\" ++ Script,
                ScriptDirPath =  "scripts";
            _ ->
                ScriptFilePath =  "scripts" ++ "/" ++ Script,
                ScriptDirPath =  "scripts"                  
            end,
            FileExists = filelib:is_file(ScriptFilePath);            
        true ->
            FileExists = true,
            case machine:getOS(Host) of
            1 -> 
                ScriptFilePath =  "scripts" ++ "\\" ++ Script,
                ScriptDirPath =  "scripts";
            _ ->
                ScriptFilePath =  "scripts" ++ "/" ++ Script,
                ScriptDirPath =  "scripts"                  
            end                     
        end,
        {FileExists,ScriptFilePath,ScriptDirPath}.

makeNewlineString(List) ->
    makeNewlineString(List,length(List),"").
makeNewlineString(_L,0,Str) ->Str;
makeNewlineString([A|B],Len,S) ->
    makeNewlineString(B,Len-1,S++A++"<br>").  

localLogFilePath() ->
    case platform:getOs() of
    1 ->    
        "scripts" ++ "\\" ++ "alert.txt";
    _ ->
        "scripts" ++ "/" ++ "alert.txt"  
    end. 
 
%%------------------------------------------------------------------
%% yi.duan execute Scrpts on remote Windows
init() ->
    {_, Path} = file:get_cwd(),  
    case erl_ddll:load_driver(Path++"\\tools", ?runScriptsDLL) of
	ok -> ok;
	{error, already_loaded}  -> ok;
	{error,ErrorDesc} ->
		io:format("===>~p~n", [erl_ddll:format_error(ErrorDesc)]),
		exit({error, could_not_load_driver});
	_ ->  exit({error, could_not_load_driver})
    end. 
 
runWinScripts(Msg) ->
	case init() of
	  ok ->
		 Port = open_port({spawn, ?runScriptsDLL}, [binary]),
		 Bin = term_to_binary(Msg),
		 port_command(Port,Bin),
		 Result = receive_port_data(Port),
		 case Result of
			{error,timeout} -> 
			  {error,timeout};
			_ ->	    
			  port_close(Port),
			  Result,
			  binary_to_term(Result)
		 end;
	  Error -> Error
	end.

receive_port_data(Port) ->
    receive
        {Port, {data, Data}} ->  Data
	after 8000 ->  {error,timeout}
    end.
	
	