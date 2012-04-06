%
%masterConfig.erl
%author:lei.lin@dragonflow.com
%

-module(masterconfig).
-compile(export_all).
 
start() ->
    init().
 
 
init() ->
    Os = platform:getOs(),
	case Os of
    1 ->			
        Root = platform:getRoot() ++ "\\groups\\master.config";
    _ ->
        Root = platform:getRoot() ++ "/groups/master.config"
    end,
	MASTER_CONFIG = Root, 	
    case ets:info('masterconfig_status') of
    undefined ->
		ets:new('masterconfig_status',[public,named_table]),
        ets:insert('masterconfig_status',[{shutdownDueToError,false},{masterConfig,null},{masterConfigShouldExist,false},{retryMasterConfigReads,null},{'MASTER_CONFIG',MASTER_CONFIG}]);
    _ ->
        nothing
    end,
	case ets:info('masterconfig_content') of
	undefined ->
	    ets:new('masterconfig_content',[public,named_table]),
		Os = platform:getOs(),
	    case Os of
	    1 ->			
            Root = platform:getRoot() ++ "\\groups\\master.config";
        _ ->
            Root = platform:getRoot() ++ "/groups/master.config"
        end,
        File = file:read_file(Root),			
        case File of
        {error,_Reason} ->
            null;
        {ok,Binary} ->
            String = binary_to_list(Binary),
	        List = string:tokens(String,"\r\n"),
		    Objects = loadMasterConfig_util(List),
		    ets:insert('masterconfig_content',Objects),
		    MasterConfigShouldExist = ets:lookup('masterconfig_status',masterConfigShouldExist),
		    case MasterConfigShouldExist of
		    [] ->
				null;
            [{masterConfigShouldExist,F}] ->
                if F == false ->
                    ets:insert('masterconfig_status',[{masterConfigShouldExist,true}]);
                true ->
                    true						 
                end 					
            end
        end;			
    _ ->
	    nothing
    end.		
		

%read master.config insert ets table,if succeed,return true, if not ,return null 
loadMasterConfig() ->
    case makesure_ets() of
    undefined ->
	    init(),
	    V = ets:lookup('masterconfig_status',shutdownDueToError),	
	    case V of
	    [] ->
	        null;
	    [{shutdownDueToError,Value}] ->
            if Value /= false ->
		        Os = platform:getOs(),
			    case Os of
			    1 ->			
                    Root = platform:getRoot() ++ "\\groups\\master.config";
                _ ->
                    Root = platform:getRoot() ++ "/groups/master.config"
                end,
                File = file:read_file(Root),			
                case File of
                {error,_Reason} ->
                        null;
                {ok,Binary} ->
                    String = binary_to_list(Binary),
				    List = string:tokens(String,"\r\n"),
				    Objects = loadMasterConfig_util(List),
				    ets:insert('masterconfig_content',Objects),
				    MasterConfigShouldExist = ets:lookup('masterconfig_status',masterConfigShouldExist),
				    case MasterConfigShouldExist of
				    [] ->
				        null;
                    [{masterConfigShouldExist,F}] ->
                        if F == false ->
                            ets:insert('masterconfig_status',[{masterConfigShouldExist,true}]);
                        true ->
                            true						 
                        end 					
                    end				
                end;			
            true ->
                null
            end
        end;
	_ ->
	    V = ets:lookup('masterconfig_status',shutdownDueToError),	
	    case V of
	    [] ->
	        null;
	    [{shutdownDueToError,Value}] ->
            if Value /= false ->
		        Os = platform:getOs(),
			    case Os of
			    1 ->			
                    Root = platform:getRoot() ++ "\\groups\\master.config";
                _ ->
                    Root = platform:getRoot() ++ "/groups/master.config"
                end,
                File = file:read_file(Root),			
                case File of
                {error,_Reason} ->
                        null;
                {ok,Binary} ->
                    String = binary_to_list(Binary),
				    List = string:tokens(String,"\r\n"),
				    Objects = loadMasterConfig_util(List),
				    ets:insert('masterconfig_content',Objects),
				    MasterConfigShouldExist = ets:lookup('masterconfig_status',masterConfigShouldExist),
				    case MasterConfigShouldExist of
				    [] ->
				        null;
                    [{masterConfigShouldExist,F}] ->
                        if F == false ->
                            ets:insert('masterconfig_status',[{masterConfigShouldExist,true}]);
                        true ->
                            true						 
                        end 					
                    end				
                end;			
            true ->
                null
            end
        end
    end.		

loadMasterConfig_util(List) ->
    loadMasterConfig_util_t(List,length(List),[]).
loadMasterConfig_util_t(_L,0,R) -> R;
loadMasterConfig_util_t(Li,Num,Re) ->
    [A|B] = Li,
	Index = string:str(A,"="),
	if Index  == 0 ->
	    loadMasterConfig_util_t(B,Num-1,Re);
    true ->		
	    Key = string:substr(A,1,Index-1),
	    Value = string:substr(A,Index+1),
        loadMasterConfig_util_t(B,Num-1,lists:append(Re,[{Key,Value}]))
    end.		

%if master.config in this ets tables,return 'true', if not ,return 'null'
getMasterConfig() ->
    case makesure_ets() of
    undefined ->
	    init(),
        V = ets:lookup('masterconfig_status',shutdownDueToError),
        MasterConfig = ets:lookup('masterconfig',masterConfig), 	
	    case V of
	    [] ->
	        null;
	    [{shutdownDueToError,Value}] ->
            if Value /= false ->
		        case MasterConfig of
			    [] ->
			        null;
			    [{masterConfig,M}] ->
			        if M == null ->
                        Value = loadMasterConfig(),				
            		    ets:insert('masterconfig_status',[{masterConfig,Value}]);
  				    true ->
                        true
                    end					
			    end;
		    true ->
		        null
		    end   
        end;
    _ ->
        V = ets:lookup('masterconfig_status',shutdownDueToError),
        MasterConfig = ets:lookup('masterconfig_status',masterConfig), 	
	    case V of
	    [] ->
	        null;
	    [{shutdownDueToError,Value}] ->
            if Value /= false ->
		        case MasterConfig of
			    [] ->
			        null;
			    [{masterConfig,M}] ->
			        if M == null ->
                        Value = loadMasterConfig(),				
            		    ets:insert('masterconfig_status',[{masterConfig,Value}]);
  				    true ->
                        true
                    end					
			    end;
		    true ->
		        null
		    end   
        end
    end.		

clearConfigCache() ->
    case ets:info('masterconfig_status') of
    undefined ->
	    init();
	_ ->
        ets:insert('masterconfig_status',[{masterConfig,null}])
    end.


%List like [{"_activeWatchServerURL","http://tms-siteview.dragonflowinteractive.com/topaz/"}]	
saveMasterConfig(List) ->
    case makesure_ets() of
    undefined ->
	    init(),
		loadMasterConfig(),
		Os = platform:getOs(),
	    case Os of
	    1 ->			
            Root = platform:getRoot() ++ "\\groups\\master.config";
        _ ->
            Root = platform:getRoot() ++ "/groups/master.config"
        end,
		ets:insert('masterconfig_content',List),
		_L = ets:tab2list('masterconfig_content'),
		Object = write_file_object(_L),
		file:write_file(Root,Object),
		platform:chmod(Root,"rw"),
	    [{masterConfigShouldExist,F}] = ets:lookup('masterconfig_status',masterConfigShouldExist),
		if F ->
			nothing;
	    true ->
			ets:insert('masterconfig_status',[{masterConfigShouldExist,true}]) 
        end;        			
    _ ->
	    V = ets:lookup('masterconfig_status',shutdownDueToError),	
	    case V of
	    [] ->
	        {error,not_shutdownDueToError};
	    [{shutdownDueToError,Value}] -> 
            if Value /= false ->
		        Os = platform:getOs(),
	            case Os of
	            1 ->			
                    Root = platform:getRoot() ++ "\\groups\\master.config";
                _ ->
                    Root = platform:getRoot() ++ "/groups/master.config"
                end,
		        ets:insert('masterconfig_content',List),
		        _L = ets:tab2list('masterconfig_content'),
		        Object = write_file_object(_L),
		        file:write_file(Root,Object),
		        platform:chmod(Root,"rw"),
	            [{masterConfigShouldExist,F}] = ets:lookup('masterconfig_status',masterConfigShouldExist),
		        if F ->
			        nothing;
	            true ->
			        ets:insert('masterconfig_status',[{masterConfigShouldExist,true}]) 
                end;
            true ->
			    nothing
            end			
	    end
    end.	 
	
	

write_file_object(List) ->
    write_file_object_t(List,length(List),"").
write_file_object_t(_L,0,R) -> R;
write_file_object_t(Li,Num,String) ->	
    [A|B] = Li,
	{Prefix,Value} = A,
	write_file_object_t(B,Num-1,lists:append(String,Prefix ++ "=" ++ Value ++ "\r\n")).

	
makesure_ets() ->
    B1 =  ets:info('masterconfig_content'),
    B2 =  ets:info('masterconfig_status'),
    if B1 == undefined ->
	    if B2 == undefined ->
		    undefined;
		true ->
            undeifned
        end;
    true ->
        if B2 == undefined ->
		    undefined;
		true ->
            ok
        end
    end.		



	

	