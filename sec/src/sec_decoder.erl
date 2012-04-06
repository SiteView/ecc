-module(sec_decoder).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").

-include("xmerl.hrl").
-compile(export_all).


decode([{decoder,Decodername,Decoderdata}|L],Body) ->   
    %?Log({"match: ~p~n", [{Decodername,Decoderdata}]}),
    case prematch(Decoderdata,Body) of         
        {ok,Prebody} ->  
            put_decodername(Decodername),       
            case getkey(Decoderdata,order) of
                  {order,Order} ->
                        %?Log({"match: ~p~n", [{Decodername,Decoderdata,Prebody,Order}]}),                       
                        case  after_prematch(Decoderdata,Order,Prebody) of
                            {ok,Match} -> 
                                %?Log({"match: ~p~n", [{ok,Match}]}),     
                                {ok,Decodername,"",Match};   
                            _  -> 
                               case  after_regex(Decoderdata,Order,Body) of
                                {ok,Match} -> 
                                    %?Log({"match: ~p~n", [{ok,Match}]}),     
                                    {ok,Decodername,"",Match};   
                                _  -> decode(L,Body)
                           end    
                       end;                 
                  _ ->                                         
                 %   decode(L,Body)
		      % ?Log({"________: ~p~n", [{Body,sec_config:childdecodes(Decodername)}]}),    
		      % ?Log({"________: ~p~n", [{Decodername,Body,childdecode(sec_config:childdecodes(Decodername),Body)}]}),    
		       case childdecode(sec_config:childdecodes(Decodername),Body) of
			    {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};  
			    _ ->  decode(L,Body)
		       end 		 
            end; 
         _ -> decode(L,Body) 
    end;
decode([],_) -> {error,nomatch}.

childdecode([{decoder,Decodername,Decoderdata}|L],Body) ->
    % ?Log({"match: ~p~n", [{Decodername,Decoderdata,Body,prematch(Decoderdata,Body)}]}),     
     case childprematch(Decoderdata,Body) of        
        {ok,Prebody} ->       
             %?Log({"match: ~p~n", [{Decodername,Decoderdata,Prebody}]}),         
             case getkey(Decoderdata,order) of
                  {order,Order} ->  
                       %?Log({"Order: ~p~n", [{Order,after_prematch(Decoderdata,Order,Prebody)}]}),                      
                       case  after_prematch(Decoderdata,Order,Prebody) of
                            {ok,Match} -> {ok,Decodername,Match};    
                            _  -> childdecode(L,Body)
                       end;        
                  _ -> childdecode(L,Body)                       
             end;      
         _ ->
             case getkey(Decoderdata,order) of
                  {order,Order} ->  
                       %?Log({"Order: ~p~n", [{Order,regex(Decoderdata,Order,Body)}]}),                      
                       case  regex(Decoderdata,Order,Body) of
                            {ok,Match} -> {ok,Decodername,Match};  
                            _  -> childdecode(L,Body)
                       end;        
                  _ -> childdecode(L,Body)                       
             end 
    end;
childdecode([],_) -> {error,nomatch}.

after_regex([{regex,Regex}|L],Order,Body) ->   
      %?Log({"regex:regex_str: ~p~n", [{Regex,Order,Body,regex:regex_str(Regex,Body)}]}),
     Orderlist = string:tokens(Order,","),
     case regex:regex_str(Regex,Body) of
          {ok,[NextBody|Match]} ->               
             %?Log({"match: ~p~n", [{Match,Order,merge(Match,Orderlist)}]}),
             case merge(Match,Orderlist) of
                  error -> 
                     %?Log({"match: ~p~n", [{Match,Order,regex(L,NextBody)}]}),
                    case regex(L,NextBody) of
                         {ok,[_|Matchregex]} ->  
                              case merge(Match++Matchregex,Orderlist) of
                                  error ->  {error,nomatch};
                                  Merge -> {ok,Merge}                                
                              end;         
                         _ -> {error,nomatch}
                    end;
                  Merge -> {ok,Merge}
             end;
          _ -> after_regex(L,Order,Body)
     end;
after_regex([_|L],Order,Body) -> after_regex(L,Order,Body);      
after_regex([],_,_)  -> {error,nomatch}.

prematch([{prematch,Prematch}|L],Body) ->   
     case regex:prematch(Prematch,Body) of
          {ok,Prebody} -> {ok,string:strip(Prebody)};
          _ -> prematch(L,Body)
     end;
prematch([_|L],Body) -> prematch(L,Body);      
prematch([],_)  -> {error,nomatch}. 


decode(Facility, Severity, Timestamp, Host, "", Body) ->
  %?Log({"Logging: ~p~n", [{Facility, Severity, Timestamp, Host, Body}]}),
  put(decodername,undefined),
  case decode(sec_config:matchdecodes(),Body) of
       {ok,Master_decoder,Sub_decoder,Match} -> 
         %?Log({"Result: ~p~n", [{Body,Master_decoder,Sub_decoder,Match}]}),	
         sec_rule:execute(Facility, Severity, Timestamp, Host, "","0",Body,Master_decoder,Sub_decoder,Match);
       _ ->         
         case get(decodername) of
             undefined -> sec_rule:execute(Facility, Severity, Timestamp, Host, "","0",Body,[],[],[]);
             Decodername -> sec_rule:execute(Facility, Severity, Timestamp, Host, "","0",Body,{decoder,Decodername},[],[])
         end         
  end;  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


decode(Facility, Severity, Timestamp, Host, Tag, Body) ->
  %?Log({"Logging: ~p ~p ~p ~p ~p ~p~n", [Facility, Severity, Timestamp, Host, Tag, Body]}),
  {Program,Pid} = analysepid(Tag),
  put(decodername,undefined),
  case decode(sec_config:parentdecodes(),Program,Body) of
       {ok,Master_decoder,Sub_decoder,Match} -> 
         %?Log({"Result: ~p~n", [{Facility, Severity, Timestamp, Host, Program,Pid,Body,Master_decoder,Sub_decoder,Match}]}),	
         sec_rule:execute(Facility, Severity, Timestamp, Host, Program,Pid,Body,Master_decoder,Sub_decoder,Match);
       _ -> 
         %?Log({"Result: ~p~n", [{Facility, Severity, Timestamp, Host, Program,Pid, Body}]}),
         case get(decodername) of
             undefined -> sec_rule:execute(Facility, Severity, Timestamp, Host, Program,"0",Body,[],[],[]);
             Decodername -> sec_rule:execute(Facility, Severity, Timestamp, Host, Program,"0",Body,{decoder,Decodername},[],[])
         end
  end.



analysepid(Tag) ->
   case  regex:regex_str("(\\S+)[(\\d+)]",Tag) of
        {ok,[[],Program,Pid]} -> {Program,Pid};
        _ -> {Tag,"0"}
   end.
  
strip(PreBody,Key) ->
   %?Log({"~p~n", [{regex:prematch("\\("++Key++"\\S+\\):",PreBody),PreBody,Key}]}),  
   case regex:prematch("\\("++Key++"\\S+\\):",PreBody) of
        {ok,NewPreBody} -> NewPreBody;
        _ -> PreBody
   end.
  
put_decodername(Decodername) ->
   case get(decodername) of
          undefined ->  put(decodername,[Decodername]);
          Old_Decodername -> put(decodername,[Decodername|Old_Decodername])
   end.
  
decode([{decoder,Decodername,Decoderdata}|L],Tag,Body) ->    
    case prematch(Decoderdata,Tag,Body) of
         {ok,match} ->    
             put_decodername(Decodername),
             case getkey(Decoderdata,prematch) of  
                  {prematch,Prematch} ->
                      %?Log({"________: ~p~n", [{Decodername,Decoderdata,Prematch}]}), 
                     case regex:prematch(Prematch,Body) of
                          {ok,Prebody} -> 
                               %?Log({"________: ~p~n", [{Decodername,Decoderdata,Prematch,Prebody}]}), 
                               case getkey(Decoderdata,order) of
                                  {order,Order} ->
                                       %?Log({"________: ~p~n", [{Decodername,Decoderdata,Prematch,Prebody,Order}]}),                    
                                        case  after_prematch(Decoderdata,Order,Prebody) of
                                            {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};  
                                            _  -> decode(L,Tag,Body)
                                       end;                 
                                  _ ->  
                                       %?Log({"________: ~p~n", [{Decodername,childdecode(sec_config:childdecodes(Decodername),Tag,Prebody)}]}),    
                                       case childdecode(sec_config:childdecodes(Decodername),Tag,Prebody) of
                                            {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};  
                                            _ ->  decode(L,Tag,Body)
                                       end                          
                              end;   
                          _ -> 
                               case getkey(Decoderdata,order) of
                                  {order,Order} ->
                                        %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Body,Order}]}),                       
                                        case  after_prematch(Decoderdata,Order,Body) of
                                            {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};  
                                            _  -> decode(L,Tag,Body)
                                       end;                 
                                  _ ->  
                                       %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Body}]}), 
                                       case childdecode(sec_config:childdecodes(Decodername),Tag,Body) of
                                            {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};  
                                            _ ->  decode(L,Tag,Body)
                                       end                          
                              end                          
                     end;
                   _ -> 
                     case getkey(Decoderdata,order) of
                          {order,Order} ->
                                %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Body,Order}]}),                       
                                case  after_prematch(Decoderdata,Order,Body) of
                                    {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};  
                                    _  -> decode(L,Tag,Body)
                               end;                 
                          _ ->  
                               %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Body,sec_config:childdecodes(Decodername)}]}), 
                               case childdecode(sec_config:childdecodes(Decodername),Tag,Body) of
                                    {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};  
                                    _ ->  decode(L,Tag,Body)
                               end                          
                     end 
             end;
        {ok,Prebody} -> 
            put_decodername(Decodername),      
            case getkey(Decoderdata,order) of
                  {order,Order} ->
                       %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Prebody,Order}]}),                       
                        case  after_prematch(Decoderdata,Order,strip(Prebody,Tag)) of
                            {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};   
                            _  -> decode(L,Tag,Body)
                       end;                 
                  _ ->  
                       %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Prebody}]}),  
                       case childdecode(sec_config:childdecodes(Decodername),Tag,strip(Prebody,Tag)) of
                            {ok,ChildDecodername,Match} -> {ok,Decodername,ChildDecodername,Match};                         
                            _ ->  decode(L,Tag,Body)
                       end                          
            end; 
         _ -> decode(L,Tag,Body) 
    end;
decode([],_,_) -> {error,nomatch}. 

prematch([{program_name,Program_name}|L],Tag,Body) ->   
     case regex:match(Program_name,Tag) of
          {ok,match} -> {ok,match};
          _ -> prematch(L,Tag,Body)
     end;     
prematch([{prematch,Prematch}|L],Tag,Body) ->   
     case regex:prematch(Prematch,Body) of
          {ok,Prebody} -> {ok,string:strip(Prebody)};
          _ -> prematch(L,Tag,Body)
     end;
prematch([_|L],Tag,Body) -> prematch(L,Tag,Body);      
prematch([],_,_)  -> {error,nomatch}. 

getkey([{Key,KeyData}|_],Key) -> {Key,KeyData};
getkey([_|L],Key) -> getkey(L,Key);
getkey([],_) -> {error,nofound}. 
 

childprematch([{prematch,Prematch}|L],Body) ->   
     case regex:prematch(Prematch,Body) of
          {ok,Prebody} -> {ok,Prebody};
          _ -> childprematch(L,Body)
     end;
childprematch([_|L],Body) -> childprematch(L,Body);      
childprematch([],_)  -> {error,nomatch}. 


childdecode([{decoder,Decodername,Decoderdata}|L],Tag,Body) ->
     %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Body,prematch(Decoderdata,Tag,Body)}]}),     
     case childprematch(Decoderdata,Body) of        
        {ok,Prebody} ->       
             %?Log({"match: ~p~n", [{Decodername,Decoderdata,Tag,Prebody}]}),         
             case getkey(Decoderdata,order) of
                  {order,Order} ->  
                      % ?Log({"Order: ~p~n", [{Order,after_prematch(Decoderdata,Order,Prebody)}]}),                      
                       case  after_prematch(Decoderdata,Order,Prebody) of
                            {ok,Match} -> {ok,Decodername,Match};    
                            _  -> childdecode(L,Tag,Body)
                       end;        
                  _ -> childdecode(L,Tag,Body)                       
             end;      
         _ ->
             case getkey(Decoderdata,order) of
                  {order,Order} ->  
                       %?Log({"Order: ~p~n", [{Order,regex(Decoderdata,Order,Body)}]}),                      
                       case  regex(Decoderdata,Order,Body) of
                            {ok,Match} -> {ok,Decodername,Match};  
                            _  -> childdecode(L,Tag,Body)
                       end;        
                  _ -> childdecode(L,Tag,Body)                       
             end 
    end;
childdecode([],_,_) -> {error,nomatch}.


regex([{regex,Regex}|L],Order,Body) ->   
      %?Log({"regex:regex_str: ~p~n", [{Regex,Order,Body}]}),
     Orderlist = string:tokens(Order,","),
     case regex:regex_str(Regex,Body) of
          {ok,[NextBody|Match]} ->               
             %?Log({"match: ~p~n", [{Match,Order,merge(Match,Orderlist)}]}),
             case merge(Match,Orderlist) of
                  error -> 
                    case regex(L,NextBody) of
                         {ok,[_|Matchregex]} ->  
                              case merge(Match++Matchregex,Orderlist) of
                                  error ->  {error,nomatch};
                                  Merge -> {ok,Merge}                                
                              end;         
                         _ -> {error,nomatch}
                    end;
                  Merge -> {ok,Merge}
             end;
          _ -> regex(L,Order,Body)
     end;
regex([_|L],Order,Body) -> regex(L,Order,Body);      
regex([],_,_)  -> {error,nomatch}. 


after_prematch([{after_prematch,Regex}|L],Order,Body) ->   
      %?Log({"regex:regex_str: ~p~n", [{Regex,Order,Body}]}),
     Orderlist = string:tokens(Order,","),
     case regex:regex_str(Regex,Body) of
          {ok,[NextBody|Match]} ->               
             %?Log({"match: ~p~n", [{Match,Order,merge(Match,Orderlist)}]}),
             case merge(Match,Orderlist) of
                  error -> 
                    case regex(L,NextBody) of
                         {ok,[_|Matchregex]} ->  
                              case merge(Match++Matchregex,Orderlist) of
                                  error ->  {error,nomatch};
                                  Merge -> {ok,Merge}                                
                              end;         
                         _ -> {error,nomatch}
                    end;
                  Merge -> {ok,Merge}
             end;
          _ -> after_prematch(L,Order,Body)
     end;
after_prematch([_|L],Order,Body) -> after_prematch(L,Order,Body);      
after_prematch([],_,_)  -> {error,nomatch}. 

regex([{regex,Regex}|L],Body) ->       
     case regex:regex_str(Regex,Body) of
          {ok,Match} -> {ok,Match};   
          _ -> regex(L,Body)
     end;
regex([_|L],Body) -> regex(L,Body);      
regex([],_)  -> {error,nomatch}. 


merge(Regex,Order) when length(Regex) =:= length(Order) -> merge(Regex,Order,[]);
merge(_,_) -> error.

merge([Regex|L],[Order|L1],Acc) -> merge(L,L1,[{list_to_atom(string:strip(Order)),Regex}|Acc]);
merge([],[],Acc) -> lists:reverse(Acc).
