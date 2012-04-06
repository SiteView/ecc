-module(sec_analysis).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").

-include("xmerl.hrl").
-compile(export_all).



analysis(Rules,{Rule_id,Rule_match,Rule_description},Match,Host) ->
    Matched_childrules = childrules(Rules,Rule_id,[]),
    ?Log({"_____________ ~p~n", [{Matched_childrules,Rule_id,Match,Host}]}),
    excuteanalysis(Matched_childrules,{Rule_id,Rule_match,Rule_description},Match,Host);
analysis(_,_,_,_) -> {error,nomatch}.


excuteanalysis([{rule,{{id,Id},{level,Level},{ignore,Ignore},{frequency,Frequency},{timeframe,Timeframe}},Ruledata}|L],{Rule_id,Rule_match,Rule_description},Match,Host) -> 
    Hashkey = list_to_atom(hash(lists:flatten([Host,integer_to_list(Rule_id),hashdata(Match,[])]))),    
    Numer_Frequency = frequency(Frequency),
    ?Log({"_____________ ~p~n", [{Numer_Frequency,Hashkey}]}),
    case analysismem(Hashkey) of
          {{ignore,MemIgnore},{frequency,MemFrequency},{timeframe,MemTimeframe}} when Frequency =/= null andalso MemFrequency >= Numer_Frequency andalso Numer_Frequency =/= -1 ->
              ?Log({"_____________ ~p~n", [{Numer_Frequency,Id,Rule_match,getdescription(Ruledata,[])}]}),
              {ok,{Id,Rule_match,getdescription(Ruledata,[])}};
          _ -> excuteanalysis(L,{Rule_id,Rule_match,Rule_description},Match,Host)
    end;  
excuteanalysis([],_,_,_) ->  {error,nomatch}.


frequency(Value) ->
   case sec_config:vars(Value) of
        {ok,NewValue} -> lists_to_integer(NewValue);
	_ -> lists_to_integer(Value)
   end.

lists_to_integer(Value) ->
  try list_to_integer(Value)
  catch 
    _:_ -> -1
  end.

analysismem(Hashkey) ->
       Data = case ets:lookup(siteview_syslog_analysis,Hashkey) of
	        [{_,{{ignore,Ignore},{frequency,Frequency},{timeframe,Timeframe}}}] -> 
			{{ignore,Ignore},{frequency,Frequency+1},{timeframe,Timeframe}};
		 _ ->
		   NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
		   {{ignore,NowSeconds},{frequency,1},{timeframe,NowSeconds}}
	      end,
       ?Log({"_____________ ~p~n", [Data]}),
       ets:insert(siteview_syslog_analysis, {Hashkey,Data}),
       Data.
       

keyof([{Key,Value}|_],Key) -> {ok,Value};
keyof([_|L],Key) -> keyof(L,Key);
keyof([],_) -> false.

childrules([Rule={rule,_,Ruledata}|L],Rule_id,Acc) ->  
     case keyof(Ruledata, if_matched_sid) of
           {ok,Value} when Value=:=Rule_id ->
                childrules(L,Rule_id,[Rule|Acc]);                    
           _ -> childrules(L,Rule_id,Acc)
     end;     
childrules([],_,Acc) -> lists:reverse(Acc).

hashdata([{_,Value}|L],Acc) -> hashdata(L,[Value|Acc]);
hashdata([],Acc) -> lists:flatten(lists:reverse(Acc)).

hash(Data) ->
    <<I:160/integer>> = crypto:sha(Data),
    string:to_lower(lists:flatten(io_lib:fwrite("~31..0s", [erlang:integer_to_list(I, 36)]))).
    
getdescription([{description,Description}|L],Acc)  -> getdescription(L,[Description|Acc]);
getdescription([_|L],Acc)  -> getdescription(L,Acc);
getdescription([],Acc)  -> lists:flatten(lists:reverse(Acc)). 

getgroup([Group = {group,_}|L],Acc)  -> getgroup(L,[Group|Acc]);
getgroup([_|L],Acc)  -> getgroup(L,Acc);
getgroup([],Acc)  -> lists:reverse(Acc). 
