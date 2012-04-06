-module(cache).

-include("log.hrl").
-export([set/2]).


set(Hashkey,{Binary,File_info,Gzip}) ->
   Size = byte_size(Binary),
   %?Log({"~p~n",[{Hashkey,Size,config:getconfig(cache)}]}),
   case config:getconfig(cache) of
	    {Cache_size,Key_size} when Size <  Key_size  ->	
           %?Log({"~p~n",[{get_ets_size(siteview_3ren_cache),Cache_size,Key_size}]}),		
		   lruset(Hashkey,{Binary,File_info,Gzip}),
		   lrudelete(get_ets_size(siteview_3ren_cache),Cache_size);
	    _  -> ok
   end.


get_ets_size(Tab) ->
     case ets:info(Tab) of
	     [{memory,Size}|_] -> Size;
		 _ -> 0
	 end.
	 
lruset(Hashkey,Binary)	 ->
    NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	New_Key = list_to_atom(Hashkey),
	case ets:lookup(siteview_3ren_keylists,New_Key) of 
         [{_,_}] -> 
		    %?Log({"~p~n",[New_Key]}),		
		    ets:insert(siteview_3ren_keylists, {New_Key,NowSeconds});		      
        _ -> 
		    %?Log({"~p~n",[New_Key]}),		
		    ets:insert(siteview_3ren_keylists, {New_Key,NowSeconds}),
		    ets:insert(siteview_3ren_cache, {New_Key,Binary})
     end.
	 	 
lrudelete(Size,Cache_size)	when Size > Cache_size  ->    
	Data = ets:tab2list(siteview_3ren_keylists),
    Fun = fun(X, Y)-> element(2, X) < element(2, Y) end,
    Sortdata = lists:sort(Fun, Data),
	{Key,_} = hd(Sortdata),
	ets:delete(siteview_3ren_cache, Key),		
	ets:delete(siteview_3ren_keylists, Key);					

lrudelete(_,_) -> ok.


	 