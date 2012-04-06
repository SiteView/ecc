-module(match).

-include("log.hrl").
-export([match/5]).

match(_,"head",_,_,_) -> common:respond(other);	

match(Req,Method,Hostname,Path,Raw_path) ->	
	
	case ets:lookup(siteviwe_elecc_config,list_to_atom(Method)) of
	     [{_,Data}] -> 
			 case lists:any(fun (X) -> match_fun(X,Req,Hostname,Path,Raw_path) end, Data) of
			        true -> ok;
					_ -> ?Log({"______************_______~n"}),Req:respond(web_common:respond(other))
             end;			 
		 _ -> Req:respond(web_common:respond(other))
	end.

match_fun({Indicate,[],Func,Modname},Req,Hostname,Path,Raw_path) ->
    case prefixmacth(Path,Indicate) of
	     {ok,Matchok} ->
				?Log({"______~p~n",[{Matchok}]}),
		        Return = my_apply(Modname,Func,[Hostname,Req,Matchok,Raw_path]),				
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
						Return = my_apply(Modname,Func,[Hostname,Req,Matchok,Raw_path]),				
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
			Return = my_apply(Modname,Func,[Hostname,Req,Q1,Q2,Raw_path]),
			Req:respond(Return),
			true;
		_ -> false
	end;
match_fun_two({Indicate,{Match_list,Func,Modname}},Req,Hostname,Path,Raw_path) ->    
	case tokens(Path,"/"++Indicate) of
		{ok,[Q1,Q2]} ->      
		        ?Log({"______~p~n",[{Indicate,Q1,Q2}]}),
			     case lists:any(fun (X) -> match_fun_three(X,Req,Hostname,Q1,Q2,Raw_path) end, Match_list) of
			        true -> true;
					_  when Func =/= [] ->  
					    % ?Log({"______~p~n",[{Func,Modname}]}),
						Return = my_apply(Modname,Func,[Hostname,Req,Q1,Q2,Raw_path]),				
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
	    ?Log({"______~p~n",[{Indicate,Q1,Q3,Q4}]}),
		Return = my_apply(Modname,Func,[Hostname,Req,Q1,Q3,Q4,Raw_path]),				
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

my_apply(Mod,Fun,Arg)->
%%	case erlang:function_exported(Mod,Fun,length(Arg)) of
%%		true->
%%			apply(Mod,Fun,Arg);
%%		Else->
%%			web_common:respond(func_not_found)
%%	end.
	%%apply(Mod,Fun,Arg).
	try apply(Mod,Fun,Arg)
	catch
	_:_->io:format("~p~n",[erlang:get_stacktrace()]),web_common:respond(func_not_error)
	end.