-module(sec_config).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").
-include("xmerl.hrl").
-compile(export_all).

init() ->
    ets:new(siteview_syslog_mq, [set,named_table,public]),	
    ets:new(siteview_syslog_analysis, [set,named_table,public]),	
    ets:new(siteview_syslog_config, [set,named_table,protected]),	
    Decoderxml = sec_decoderxml:decoderxml(verifydecoderpath()),
    ets:insert(siteview_syslog_config, {decoder,Decoderxml}),
    %?Log({"_____~p~n",[Decoderxml]}),    
    Rules = sec_decoderxml:rules(verifyrulespath()),
    %?Log({"__________~p~n",[Rules]}),
    ets:insert(siteview_syslog_config, {rules,Rules}).

verifydecoderpath() ->
    case filelib:is_file("decoder.xml") of
         true -> "decoder.xml";
         _ ->
         case filelib:is_file("sec/decoder.xml") of
             true -> "sec/decoder.xml";
             _ -> "decoder.xml"           
         end
    end.
    
verifyrulespath() ->
    case filelib:is_dir("rules") of
         true -> "rules";
         _ ->
         case filelib:is_file("sec/rules") of
             true -> "sec/rules";
             _ -> "rules"        
        end
    end.    

getconfig(Key) ->
    case ets:lookup(siteview_syslog_config,Key) of
	     [{_,Data}] -> Data;
		 _ -> []
	end.   
    
parentdecodes() -> 
    case ets:lookup(siteview_syslog_config,decoder) of
	     [{_,Data}] -> 
            F = fun(X) ->
                    case X of
                         {decoder,_,DecoderData} ->
                              Fun = fun({program_name,_}) -> true;
                                        (_) -> false
                                    end,    
                              lists:any(Fun,DecoderData);
                         _ -> false
                    end
                end,
            lists:filter(F,Data);
		 _ -> []
	end. 

matchdecodes() -> 
    case ets:lookup(siteview_syslog_config,decoder) of
	     [{_,Data}] -> 
            F = fun(X) ->
                    case X of
                         {decoder,_,DecoderData} ->
                              Fun = fun({parent,_}) -> true;
                                        ({program_name,_}) -> true;
                                        (_) -> false
                                    end,    
                              not lists:any(Fun,DecoderData);
                         _ -> false
                    end
                end,
            lists:filter(F,Data);
		 _ -> []
	end.

childdecodes(Parent) -> 
    case ets:lookup(siteview_syslog_config,decoder) of
	     [{_,Data}] -> 
            F = fun(X) ->
                    case X of
                         {decoder,_,DecoderData} ->
                              Fun = fun({parent,Name}) when Name =:= Parent -> true;
                                        (_) -> false
                                    end,    
                              lists:any(Fun,DecoderData);
                         _ -> false
                    end
                end,
            lists:filter(F,Data);
		 _ -> []
	end. 
    
  
rules_vars() ->  
      Rules = sec_config:getconfig(rules),
      xmlfile(Rules,[]).
      
      
xmlfile([{_XmlFile,Groups}|L],Acc) ->  xmlfile(L,group(Groups,Acc));  
xmlfile([_|L],Acc) -> xmlfile(L,Acc);         
xmlfile([],Acc) ->Acc.    

group([{Key,Value}|L],Acc) -> group(L,[{Key,Value}|Acc]);    
group([{group,_,_}|L],Acc) -> group(L,Acc);  
group([_|L],Acc) -> group(L,Acc);                  
group([],Acc) -> Acc. 

vars("$"++Value) ->
   vars(rules_vars(),list_to_atom(Value));
vars(Value) -> Value.

vars([{Key,Value}|_],Key) -> {ok,Value};
vars([_|L],Key) -> vars(L,Key);
vars([],_) -> {error,nomatch}.