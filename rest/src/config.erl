-module(config).
-author('oldhand <oldhand@sina.com>').
-include("log.hrl").

-export([init/0,getconfig/1]).


-export([test/0]).


test() ->   
   init().
   
init() ->
    case ets:info(siteviwe_3ren_config) of
	     undefined ->		 
		    case file:consult("rest.conf") of
				{ok,Data} ->
				   %io:format("~p~n",[Data]),
				   %erlmagick:start(),
				   ets:new(siteview_3ren_config, [set,named_table,protected]),				   
				   F = fun({Key,Value}) ->
					            case Key of
								     system ->									  
									     lists:foreach(fun({SytemKey,SytemValue}) ->	ets:insert(siteview_3ren_config, {SytemKey,SytemValue}) end,Value);
									 _ -> ets:insert(siteview_3ren_config, {Key,Value})
								end
								
							end,
				   lists:foreach(F,Data);
				_ -> ?Log({"rest.conf not found!~n"}),error
		    end;
		 _ -> ok
	end.
	

getconfig(Key) ->
    case ets:lookup(siteview_3ren_config,Key) of
	     [{_,Data}] -> Data;
		 _ -> []
	end.	
	
