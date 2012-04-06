-module(sec_rule).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").
-include("../store/src/kvs_define.hrl").
-include("xmerl.hrl").
-compile(export_all).


%%%[[li, N] || N <- [1,2,3]] = [[li,1],[li,2],[li,3]]


datetime({{Y,M,D},{HH,MM,SS}}) ->
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[Y,M,D,HH,MM,SS]));
datetime(_) ->
    {{Y,M,D},{HH,MM,SS}} = calendar:now_to_local_time(now()),
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[Y,M,D,HH,MM,SS])).
    
keyvalue([[]],Acc) -> [];     
keyvalue([{Key,Value1,Value2}|L],Acc) -> keyvalue(L,[{atom_to_binary(Key,utf8),list_to_binary(Value1),list_to_binary(Value2)}|Acc]); 
keyvalue([{Key,Value}|L],Acc) -> keyvalue(L,[{atom_to_binary(Key,utf8),list_to_binary(Value)}|Acc]);
keyvalue([],Acc) -> lists:reverse(Acc).    
   

executelist(Rules,Severity,Body,Host,Program,[Master_decoder|L],Sub_decoder,Match) -> 
    case executexml(Rules,Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match) of        
         {ok,Result} -> {ok,Result,Master_decoder}; 
         _ -> executelist(Rules,Severity,Body,Host,Program,L,Sub_decoder,Match)
    end;
executelist(_,_,_,_,_,[],_,_) ->   {error,nomatch}. 
    
execute(Facility, Severity, Timestamp, Host, Program,Pid, Body,{decoder,Decodername},Sub_decoder,Match) -> 
  % ?Log({"____~p~n",[Decodername]}),   
   Rules = sec_config:getconfig(rules),
   case executelist(Rules,Severity,Body,Host,Program,Decodername,Sub_decoder,Match) of
         {ok,{Rule_id,Rule_match,Rule_description},Master_decoder} ->            
             Record = #log{
				time = list_to_integer(datetime(Timestamp)), 
				facility= atom_to_binary(Facility,utf8), 
				level= Severity, 
				machine= list_to_binary(Host), 
				program= list_to_binary(Program),
				pid= list_to_integer(Pid),
				message= list_to_binary(Body),
				master_decoder = list_to_binary(Master_decoder),
				sub_decoder = list_to_binary(Sub_decoder),
                rule_id = Rule_id, 
				rule_match = keyvalue([Rule_match],[]),
				rule_description = list_to_binary(Rule_description),
				my= keyvalue(Match,[])
				},	           
             kvs_content:put(Record),
             %?Log({"Result: ~p~n", [Record]}),
             %?Log({"Result: ~p~n", [{Rule_id,Rule_match,Rule_description}]}),
             sec_mq:insert(Record),
            ?writelog("match.log",{Facility, Severity, Timestamp, Host, Program,Pid,Body,Master_decoder,Sub_decoder,Match,Rule_id,Rule_match,Rule_description});
         _ -> 
            Record = #log{
				time = list_to_integer(datetime(Timestamp)), 
				facility= atom_to_binary(Facility,utf8), 
				level= Severity, 
				machine= list_to_binary(Host), 
				program= list_to_binary(Program),
				pid= list_to_integer(Pid),
				message= list_to_binary(Body),
				master_decoder = <<>>,
				sub_decoder = <<>>,
                rule_id = 0, 
				rule_match = [],
				rule_description = <<>>,
				my= []
				},	          
             kvs_content:put(Record),
	     %?Log({"Result: ~p~n", [Record]}),
             %?Log({"Result: ~p~n", [Result]}),
             sec_mq:insert(Record)                   
   end,
   ?Log({"_____~p~n~p~n", [Body,{Facility,Severity,Host,Program,Decodername,Sub_decoder,Match}]});  
   
   
execute(Facility, Severity, Timestamp, Host, Program,Pid,Body,Master_decoder,Sub_decoder,Match) ->
   Rules = sec_config:getconfig(rules),
   case executexml(Rules,Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match) of
        % {ok,ExecutexmlMatch} -> 
         {ok,{Rule_id,Rule_match,Rule_description}} ->        
            ?Log({"____________~p~n", [{ok,{Rule_id,Rule_match,Rule_description}}]}),         
             Record = #log{
				time = list_to_integer(datetime(Timestamp)), 
				facility= atom_to_binary(Facility,utf8), 
				level= Severity, 
				machine= list_to_binary(Host), 
				program= list_to_binary(Program),
				pid= list_to_integer(Pid),
				message= list_to_binary(Body),
				master_decoder = list_to_binary(Master_decoder),
				sub_decoder = list_to_binary(Sub_decoder),
                rule_id = Rule_id, 
				rule_match = keyvalue([Rule_match],[]),
				rule_description = list_to_binary(Rule_description),
				my= keyvalue(Match,[])
				},	           
             kvs_content:put(Record),
             %?Log({"Result: ~p~n", [Result]}),
             %?Log({"Result: ~p~n", [{Rule_id,Rule_match,Rule_description}]}),
             sec_mq:insert(Record),
            ?writelog("match.log",{Facility, Severity, Timestamp, Host, Program,Pid,Body,Master_decoder,Sub_decoder,Match,Rule_id,Rule_match,Rule_description});
         _ -> 
            Record = #log{
				time = list_to_integer(datetime(Timestamp)), 
				facility= atom_to_binary(Facility,utf8), 
				level= Severity, 
				machine= list_to_binary(Host), 
				program= list_to_binary(Program),
				pid= list_to_integer(Pid),
				message= list_to_binary(Body),
				master_decoder = <<>>,
				sub_decoder = <<>>,
                rule_id = 0, 
				rule_match = [],
				rule_description = <<>>,
				my= []
				},	          
             kvs_content:put(Record),
             %?Log({"Result: ~p~n", [Result]}),
             sec_mq:insert(Record)                   
   end,
   ?Log({"____~p~n~p~n", [Body,{Facility,Severity,Host,Program,Master_decoder,Sub_decoder,Match}]}).
   
executexml([{_XmlFile,Groups}|L],Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match) ->           
         case group(Groups,Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match,Groups) of
            {ok,GroupMatch} -> {ok,GroupMatch};
            _ -> executexml(L,Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match)
         end;
executexml([_|L],Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match) -> executexml(L,Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match);         
executexml([],_,_,_,_,_,_,_) -> {error,nomatch}.      



group([_|L],Severity,Body,Host,Program,[],[],[],Groups) -> group(L,Severity,Body,Host,Program,[],[],[],Groups); 



group([{group,Groupname,Rules}|L],Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match,Groups) -> 
   %?Log({"____~p~n",[{}]}),                             
   case match_decoded_as(Rules,{Master_decoder,Sub_decoder}) of
        {ok,Decoded_as_id,Ruledata} ->
            %?Log({"____~p~n",[{Decoded_as_id,Program,Master_decoder,Sub_decoder}]}),  
            Childrules = childrules(Rules,Decoded_as_id,[]),
           % ?Log({"Childrules: ~p~n",[Childrules]}),
	     %?Log({"Childrules: ~p~n",[{Rules,Master_decoder,Sub_decoder,Decoded_as_id,Ruledata}]}),
            case rule(Childrules,Severity,Body,Match) of
                {ok,RuleMatch} -> 
                     case sec_analysis:analysis(Rules,RuleMatch,Match,Host) of
                           {ok,AnalysisRuleMatch} -> {ok,AnalysisRuleMatch};
                           _ ->  {ok,RuleMatch}
                     end;                    
                _ -> 		
		    Description = getdescription(Ruledata,[]), 
		   % ?Log({"Childrules: ~p~n",[{Decoded_as_id,Ruledata,Description}]}),		    
		    {ok,{Decoded_as_id,[],Description}}
            end;
        _ -> group(L,Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match,Groups)
   end;
              
group([_|L],Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match,Groups) -> group(L,Severity,Body,Host,Program,Master_decoder,Sub_decoder,Match,Groups);                  
group([],_,_,_,_,_,_,_,_) -> {error,nomatch}.   

rule([{rule,{{id,Id},{level,Level},{ignore,Ignore},{frequency,Frequency},{timeframe,Timeframe}},Ruledata}|L],Severity,Body,Match) -> 
   %?Log({"Result: ~p~n",[{Id,Level,Ignore,Frequency,Timeframe,Ruledata}]}),
   %?Log({"Result: ~p~n",[{Severity,Body,Match}]}),
   case getmatch(Ruledata, []) of
        {ok,Value} ->
                %?Log({"__: ~p~n",[{Value,regex:match(Value,Body)}]}), 
                case regex:match(Value,Body) of
                     {ok,match} ->                                                
                          Description = getdescription(Ruledata,[]),                  
                          {ok,{Id,[],Description}};
                     _ ->
                       case match(Match,Ruledata) of
                          {ok,Matchrule} ->                  
                              Description = getdescription(Ruledata,[]),                  
                              {ok,{Id,Matchrule,Description}};
                          _ -> rule(L,Severity,Body,Match)
                       end
                end;
        _ ->
           case match(Match,Ruledata) of
              {ok,Matchrule} ->                  
                  Description = getdescription(Ruledata,[]),                  
                  {ok,{Id,Matchrule,Description}};
              _ -> rule(L,Severity,Body,Match)
           end
   end;   
    
rule([_|L],Severity,Body,Match) -> rule(L,Severity,Body,Match);           
rule([],_,_,_) -> {error,nomatch}.   

getmatch([{match,Description}|L],Acc)  -> getmatch(L,[Description|Acc]);
getmatch([_|L],Acc)  -> getmatch(L,Acc);
getmatch([],Acc)  -> {ok,lists:flatten(lists:reverse(Acc))}.

childrules([Rule={rule,_,Ruledata}|L],Decoded_as_id,Acc) ->  
     case keyof(Ruledata, if_sid) of
           {ok,Value} ->
                F = fun(X) -> X =:= Decoded_as_id end,                
                case lists:any(F,Value) of
                     true -> childrules(L,Decoded_as_id,[Rule|Acc]);
                     _ -> childrules(L,Decoded_as_id,Acc)
                end;
           _ -> childrules(L,Decoded_as_id,Acc)
     end;     
childrules([],_,Acc) -> lists:reverse(Acc).


match_decoded_as(Rules,{Master_decoder,Sub_decoder}) ->   
     case decoded_as(Rules,Sub_decoder) of
          {ok,Id,Ruledata} ->  {ok,Id,Ruledata};
	  _ -> 
             case decoded_as(Rules,Master_decoder) of
		  {ok,Id,Ruledata} ->  {ok,Id,Ruledata};
		  _ ->   {error,nomatch} 
	     end	  
     end;
match_decoded_as(_,_) ->  {error,nomatch}.  
     
decoded_as([{rule,{{id,Id},_,_,_,_},Ruledata}|L],Decoded_as) ->  
     case decodedas(Ruledata,Decoded_as)  of
          ok -> {ok,Id,Ruledata};
          _ -> decoded_as(L,Decoded_as) 	  	  
     end;    
decoded_as([_|L],Decoded_as) -> decoded_as(L,Decoded_as);             
decoded_as([],_) -> {error,nomatch}.   

decodedas([{decoded_as,Key}|_],Key) -> ok; 
decodedas([_|L],Key) -> decodedas(L,Key);
decodedas([],_) -> {error,nomatch}.   


%~ checklevel(_,undefined) -> ok;
%~ checklevel(Level,Level) -> ok;
%~ checklevel(_,_) -> false.
checklevel(_,_) -> ok.

keyof([{Key,Value}|_],Key) -> {ok,Value};
keyof([_|L],Key) -> keyof(L,Key);
keyof([],_) -> false.


match([{Key,Value}|L],Ruledata) ->         
      case matchrule(Ruledata,Key,Value) of
          {ok,Match} ->  {ok,Match};
          _ -> match(L,Ruledata)
      end;
match([_|L],Ruledata) -> match(L,Ruledata);       
match([],_) ->  {error,nomatch}.   

matchrule([{if_sid,_}|L],MatchKey,Matchdata) -> matchrule(L,MatchKey,Matchdata);
matchrule([{description,_}|L],MatchKey,Matchdata) -> matchrule(L,MatchKey,Matchdata);
matchrule([{MatchKey,Value}|L],MatchKey,Matchdata) ->
        %?Log({"Result: ~p~n",[{MatchKey,Value,Matchdata,regex:match(Value,Matchdata)}]}),
        case regex:match(Value,Matchdata) of
               {ok,match} -> {ok,{MatchKey,Value,Matchdata}};
               _ -> matchrule(L,MatchKey,Matchdata)
        end;
matchrule([_|L],MatchKey,Matchdata) -> matchrule(L,MatchKey,Matchdata);      
matchrule([],_,_) -> {error,nomatch}.  

getdescription([{description,Description}|L],Acc)  -> getdescription(L,[Description|Acc]);
getdescription([_|L],Acc)  -> getdescription(L,Acc);
getdescription([],Acc)  -> lists:flatten(lists:reverse(Acc)).