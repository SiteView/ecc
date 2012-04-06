%%
%%http_receive
%%Author lei.lin@dragonflow.com
%%


-module(http_receive).
-compile(export_all).


%return {Status,Data}
receive_asynchronous_data(Url,ProxyHostname,ProxyPort,ProxyUser,ProxyPassword,Time) ->
    if  length(ProxyHostname) == 0 ->
        case http:request(get,{Url,[]},[{timeout, Time}],[{sync,false},{stream, self}]) of
        {ok,RequestId} ->
            loop(RequestId,"");              
        {error, Reason} ->
            {error, Reason}
        end;
    true ->
        case http:request(get,{Url,[]},[{timeout, Time}],[{sync,false},{stream, self},{proxy,{{ProxyHostname, ProxyPort},[]}},{proxy_auth,{ProxyUser,ProxyPassword}}]) of
        {ok,RequestId} ->
            loop(RequestId,"");
        {error, Reason} ->
            {error, Reason}
        end        
    end.
                                        
            
loop(RequestId,Data) ->
    receive
        {http, {RequestId, Result}} ->
            case Result of
            {error,String} ->
                {error,999};                
            {A,B,C} ->
                {Type,Code,Str} = A,
                http:cancel_request(RequestId),
                {error,Code};
            _ ->
                {error,-1}             
            end;    
        {http, {RequestId, stream_start, Headers}} ->
            loop(RequestId,Data);
        {http, {RequestId, stream, BinBodyPart}} ->
            loop(RequestId,Data ++ binary_to_list(BinBodyPart));
        {http, {RequestId, stream_end, Headers}} ->
            {ok,Data};
        {http, {RequestId, {error, Reason}}} ->
            http:cancel_request(RequestId),
            {error,-1};
        _ ->
            http:cancel_request(RequestId),
            {error,-1}       
    end.        