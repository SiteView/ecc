-module(config).
-author('oldhand <oldhand@sina.com>').
-include("log.hrl").
-include("config.hrl").
-include("xmerl.hrl").

-export([init/0,getconfig/1]).


-export([test/0]).


test() ->   
   init().
   
init() ->
    case ets:info(siteviwe_elecc_config) of
	     undefined ->		 
		    case file:consult("siteview.conf") of
				{ok,Data} ->
				   %io:format("~p~n",[Data]),		   
				   ets:new(siteviwe_elecc_config, [set,named_table,protected]),				   
				   F = fun({Key,Value}) ->
					            case Key of
								     system ->									  
									     lists:foreach(fun({SytemKey,SytemValue}) ->	ets:insert(siteviwe_elecc_config, {SytemKey,SytemValue}) end,Value);
									 _ -> ets:insert(siteviwe_elecc_config, {Key,Value})
								end
								
							end,
				   lists:foreach(F,Data);
				_ -> ?Log({"siteview.conf not found!~n"}),error
		    end;
		 _ -> ok
	end.
	
	

getconfig('domain') ->
    case ets:lookup(siteviwe_elecc_config,'domain') of
	     [{_,Data}] -> Data;
		 _ -> "3ren.com"
	end;		
getconfig(Key) ->
    case ets:lookup(siteviwe_elecc_config,Key) of
	     [{_,Data}] -> Data;
		 _ -> []
	end.	
	

