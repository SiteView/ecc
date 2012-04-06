-module(match).

-include("log.hrl").
-export([match/5]).

  
match(_,"head",_,_,_) -> common:respond(other);	

match(Req,Method,Hostname,Path,Raw_path) ->	
	case ets:lookup(siteview_3ren_config,list_to_atom(Method)) of
	     [{_,Data}] -> 
			 case lists:any(fun (X) -> match_fun(X,Req,Hostname,Path,Raw_path) end, Data) of
			        true -> ok;
					_ -> ?Log("______************_______~n"),Req:respond(common:respond(other))
             end;			 
		 _ -> Req:respond(common:respond(other))
	end.

match_fun({Indicate,[],Func,Modname},Req,Hostname,Path,Raw_path) ->
    case prefixmacth(Path,Indicate) of
	     {ok,Matchok} ->
		        Return = match_apply(Modname,Func,[Hostname,Req,Matchok,Raw_path]),				
				Req:respond(Return),
				true;
		 _ -> false	  
	end;
match_fun({Indicate,Match_list,Func,Modname},Req,Hostname,Path,Raw_path) ->
    case prefixmacth(Path,Indicate) of
	     {ok,Matchok} ->
		         %?Log({"______~p~n",[{Matchok,Match_list}]}),
		         case lists:any(fun (X) -> match_fun_two(X,Req,Hostname,Matchok,Raw_path) end, Match_list) of
			        true -> true;
					_  when Func =/= [] ->  
					    % ?Log({"______~p~n",[{Path}]}),
						Return = match_apply(Modname,Func,[Hostname,Req,Matchok,Raw_path]),				
						Req:respond(Return),
						true;
					_   -> false 						
                 end;		       
		 _ -> false	  
	end;	
match_fun(_,_,_,_,_) -> false.

match_fun_two({Indicate,{[],Func,Modname}},Req,Hostname,Path,Raw_path) ->    
	case tokens(Path,"/"++Indicate) of
		{ok,[Q1,Q2]} ->      
		    %?Log({"______~p~n",[{Indicate,Q1,Q2}]}),
			Return = match_apply(Modname,Func,[Hostname,Req,Q1,Q2,Raw_path]),
			Req:respond(Return),
			true;
		_ -> false
	end;
match_fun_two({Indicate,{Match_list,Func,Modname}},Req,Hostname,Path,Raw_path) ->    
	case tokens(Path,"/"++Indicate) of
		{ok,[Q1,Q2]} ->      
		        % ?Log({"______~p~n",[{Indicate,Q1,Q2}]}),
			     case lists:any(fun (X) -> match_fun_three(X,Req,Hostname,Q1,Q2,Raw_path) end, Match_list) of
			        true -> true;
					_  when Func =/= [] ->  
					    % ?Log({"______~p~n",[{Func,Modname}]}),
						Return = match_apply(Modname,Func,[Hostname,Req,Q1,Q2,Raw_path]),				
						Req:respond(Return),
						true;
					_  -> false 						
                 end;	
		_ -> false
	end;
match_fun_two(_,_,_,_,_) -> false.

match_fun_three({Indicate,Func,Modname},Req,Hostname,Q1,Q2,Raw_path) ->    
	case tokens(Q2,"/"++Indicate) of
	  {ok,[Q3,Q4]} ->
	    %?Log({"______~p~n",[{Indicate,Q1,Q3,Q4}]}),
		Return = match_apply(Modname,Func,[Hostname,Req,Q1,Q3,Q4,Raw_path]),				
		Req:respond(Return),
		true;
		_ -> false
	end;
match_fun_three(_,_,_,_,_,_) -> false.

prefixmacth(String,Key) -> prefixmacth(lists:prefix(Key,String),String,Key).
prefixmacth(false,_,_) ->  nomatch;  
prefixmacth(true,String,Key) ->  {ok,lists:sublist(String,length(Key)+1,length(String)-length(Key))}. 
	

tokens(S, Seps) ->
    case string:str(S,Seps) of
	    0 -> nomatch;
		Number -> 
		  {ok, [lists:sublist(S,Number-1),lists:nthtail(Number+length(Seps)-1,S)]}
	end.



match_apply(Modname,Func,Params) ->
    Result = case Params of
         [Hostname,Req,Matchok,Raw_path] -> {Hostname,Req,parse(strip(Matchok),Raw_path)};
         [Hostname,Req,Q1,Q2,Raw_path] -> {Hostname,Req,parse(lists:flatten([strip(Q1),"&",strip(Q2)]),Raw_path)};
         [Hostname,Req,Q1,Q3,Q4,Raw_path] -> {Hostname,Req,parse(lists:flatten([strip(Q1),"&",strip(Q3),"&",strip(Q4)]),Raw_path)}
    end,
    %?Log({"______~p~n",[Result]}),
    {_HostName,Request,{{params,CallParams},{filter,Filter}}} = Result,
    %?Log({"______~p~n",[Request:get(method)]}),
    case Request:get(method) of
         'GET' ->
           case  {checkmatch(CallParams),checkmatch(Filter)} of
                  {match,match} -> 
                       case checkexports(Modname,Func,CallParams)  of
                            true -> 
                             % F = fun({_,Param}) -> string:strip(Param,both,$') end,
                              F = fun({_,Param}) -> autoparam(Param) end,
                              ?Log({"~p~n",[{CallParams,lists:map(F,CallParams)}]}),
                              ResData = apply(Modname,Func,lists:map(F,CallParams)),
                              %?Log({"~p~n",[ResData]}),
                              FilterData = filter(ResData,Filter),
                              %?Log({"~p~n",[FilterData]}),
                              common:respond(tuplesxml:tuple_to_xml(FilterData)); 
                            _ -> 
                             XmlData = codeerror("errorcode:1","Invalid Params"), 
                             common:respond(XmlData) 
                       end;
                  _ ->             
                      XmlData = codeerror("errorcode:2","Invalid Params"), 
                      common:respond(XmlData) 
            end;
         'POST' ->  
              RequestData = binary_to_list(Request:recv_body()),
              TuplesData = tuplesxml:xml_to_tuple(RequestData),
              ?Log({"~p~n",[TuplesData]}),
              case parseparam(TuplesData,[]) of
                   {ok,PostCallParams} ->
                       case checkexports(Modname,Func,PostCallParams)  of
                            true ->                              
                              %?Log({"~p~n",[{Modname,Func,PostCallParams}]}),
                              ResData = apply(Modname,Func,PostCallParams),    
                              %?Log({"~p~n",[ResData]}),                              
                              common:respond(tuplesxml:tuple_to_xml(ResData)); 
                            _ -> 
                             XmlData = codeerror("errorcode:7","Invalid Put Params"), 
                             common:respond(XmlData) 
                       end;
                   _ ->
                      XmlData = codeerror("errorcode:6","Invalid Put Params"), 
                      common:respond(XmlData)
              end;
           'PUT' ->  
              RequestData = binary_to_list(Request:recv_body()),
              TuplesData = tuplesxml:xml_to_tuple(RequestData),
              ?Log({"~p~n",[TuplesData]}),
              case parseparam(TuplesData,[]) of
                   {ok,PostCallParams} ->
                       case checkexports(Modname,Func,PostCallParams)  of
                            true ->                              
                              %?Log({"~p~n",[{Modname,Func,PostCallParams}]}),
                              ResData = apply(Modname,Func,PostCallParams),    
                              ?Log({"~p~n",[ResData]}),                              
                              common:respond(tuplesxml:tuple_to_xml(ResData)); 
                            _ -> 
                             XmlData = codeerror("errorcode:5","Invalid Post Params"),                             
                             common:respond(XmlData) 
                       end;
                   _ ->
                      XmlData = codeerror("errorcode:4","Invalid Post Params"),                     
                      common:respond(XmlData)
              end;  
           'DELETE' ->
             case  {checkmatch(CallParams),checkmatch(Filter)} of
                  {match,match} -> 
                       case checkexports(Modname,Func,CallParams)  of
                            true -> 
                              F = fun({_,Param}) -> string:strip(Param,both,$') end,
                              %?Log({"~p~n",[{Modname,Func,lists:map(F,CallParams)}]}),
                              ResData = apply(Modname,Func,lists:map(F,CallParams)),
                              %?Log({"~p~n",[ResData]}),                            
                              common:respond(tuplesxml:tuple_to_xml(ResData)); 
                            _ -> 
                             XmlData = codeerror("errorcode:1","Invalid Params"), 
                             common:respond(XmlData) 
                       end;
                  _ ->             
                      XmlData = codeerror("errorcode:2","Invalid Params"),
                      common:respond(XmlData) 
            end;   
         _ -> 
          XmlData = codeerror("errorcode:0","Invalid Request"),          
          common:respond(XmlData) 
    end.


codeerror(Code,Msg) ->
     lists:flatten(["<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                      "<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:xn=\"http://www.siteview.com/atom/1.0\">",
                      "<tuple>"
                      "<value type=\"string\">",Code,"</value>" 
                      "<value type=\"string\">",Msg,"</value>"
                      "</tuple>",
                      "</feed>"]).

    
parse(Params,Raw_path) -> 
    Paramslist = string:tokens(Params,"&"),
    Raw_pathlist = string:tokens(Raw_path,"&"),
    F = fun(X) -> analyse(X,[]) end,
    {{params,lists:map(F,Paramslist)},{filter,lists:map(F,Raw_pathlist)}}. 
 
filter([],_) -> [];
filter(Data,Filter) when is_list(Data) ->
    case {keyvalue(Filter,from),keyvalue(Filter,to)} of
         {{match,From},{match,To}} when To > length(Data) andalso From < 1 -> lists:sublist(Data,1,length(Data));
         {{match,From},{match,To}} when From < 1  -> lists:sublist(Data,1,To); 
         {{match,From},{match,To}} when To > length(Data)  -> lists:sublist(Data,From,length(Data)-From+1);
         {{match,From},{match,To}} -> lists:sublist(Data,From,To-From+1);          
         _ -> Data
    end;
filter(Data,_) -> Data.

parseparam([{Name,Value}|R],Acc) when is_atom(Name) -> parseparam(R,[Value|Acc]);
parseparam([_|_],_) -> error;
parseparam([],Acc) -> {ok,lists:reverse(Acc)}.


keyvalue([{to,Value}|_],to) -> {match,list_to_integer(Value)};
keyvalue([{from,Value}|_],from) -> {match,list_to_integer(Value)};
keyvalue([{Key,Value}|_],Key) -> {match,Value};
keyvalue([_|R],Key) -> keyvalue(R,Key);
keyvalue([],_) -> nomatch.

analyse([$=|R], Acc) ->  {list_to_atom(lists:reverse(Acc)),R};
analyse([H|R], Acc) ->  analyse(R, [H|Acc]);
analyse([], _) -> nomatch.   

checkmatch([nomatch|_]) -> nomatch;
checkmatch([_|R]) ->  checkmatch(R);
checkmatch([]) -> match.

checkexports(Modname,Func,Params) ->
    Exports = apply(Modname,module_info,[exports]),
    ?Log({"~p~n",[{Exports,Modname,Func,Params}]}),
    F = fun(X) -> 
        case X of
             {ExportName,Number} when  ExportName =:= Func andalso Number =:= length(Params) -> true;
             _ -> false
        end
    end,
    lists:any(F,Exports).
    
autoparam(Value) ->
    case matchstrip(Value,"\"","\"") of
         {match,NewValue} -> NewValue;
         _ ->
            case matchstrip(Value,"'","'") of
                 {match,NewValue} -> list_to_atom(NewValue);
                 _ ->
                    case matchstrip(Value,"(",")") of
                         {match,NewValue} -> list_to_integer(NewValue);
                         _ ->
                            case matchstrip(Value,"<",">") of
                                 {match,NewValue} -> list_to_binary(NewValue);
                                 _ ->  Value                                   
                            end
                    end
            end
    end.
    
strip(String) -> strip(String,"(",")").

strip(String,Prefix,Suffix) ->
    case {lists:prefix(Prefix,String),lists:suffix(Suffix,String)} of
          {true,true} -> lists:sublist(String,length(Prefix)+1,length(String)-length(Prefix)-length(Suffix));
          _ -> String
    end.

matchstrip(String,Prefix,Suffix) ->
    case {lists:prefix(Prefix,String),lists:suffix(Suffix,String)} of
          {true,true} -> {match,lists:sublist(String,length(Prefix)+1,length(String)-length(Prefix)-length(Suffix))};
          _ -> nomatch
    end.


    