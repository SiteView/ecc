-module(commandLine).

-compile(export_all).

-include("monitor.hrl").

-behaviour(gen_server).

-define(SERVER,'commandLine').
   
start() ->
    start_link(). 

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?SERVER], []).
    
init([TableName]) ->
   ets:new(TableName,[named_table]),
   {ok,""}.

stop() ->
    gen_server:cast(?SERVER, stop).
 
%ParaList is list of tuple ,example:[{user,"root"},{password,"root"}],return {ok,Object} | {error,reason} 
get(Host,Cmd)-> 
    Localhost = dbcs_base:get_app(),  
    gen_server:call(?SERVER, {get,Host,Cmd,Localhost}).
    
get(Host,Cmd,Localhost)->
    gen_server:call(?SERVER, {get,Host,Cmd,Localhost}).
   
%remove connect count from ets table
remove(Host) ->
    gen_server:call(?SERVER, {remove,Host}).    

getConnectCount(Host) ->
    case ets:lookup(commandLine,Host) of
    [{_,Num}] ->
        Num;
    _ ->
        0
    end.        



handle_call({get,Host,Cmd,Localhost}, _, State)->
    case Host of
    ""-> 
        Object = siteview_command:new(Host,Cmd,""),
        {reply,{ok,Object},State};          
    _ ->        
        case  machine:getMachine(Localhost,Host) of
        [] ->
            {reply,{error,"no machine"},State};
        [Machine|_] ->        
            case Machine#machine.method of
            "SSH" ->
                Connlimit = Machine#machine.connlimit,
                case ets:lookup(commandLine,Host) of
                [{_,Num}] ->
                    if Num >= Connlimit ->             
                        {reply,{error,"connlimit"},State};
                    true ->
                        ets:insert_new(commandLine,{Host,Num+1}),
                        Object = siteview_command:new(Host,Cmd,Machine),
                        {reply,{ok,Object},State}          
                    end;
                _->
                    ets:insert_new(commandLine,{Host,1}),
                    Object = siteview_command:new(Host,Cmd,Machine),
                    {reply,{ok,Object},State}
                end;    
            _ ->
                Object = siteview_command:new(Host,Cmd,Machine),
                {reply,{ok,Object},State}        
           end
        end
    end;    
            
    
handle_call({remove,Host}, _, State)-> 
    case Host of
    ""-> 
        {reply,ok,State};
    _ ->    
        case  machine:getMachine(Host) of
        [] -> 
            {reply,ok,State};
        [Machine|_] ->        
            case Machine#machine.method of
            "SSH" ->    
                case ets:lookup(commandLine,Host) of
                [{_,Num}] ->
                    ets:insert_new(commandLine,{Host,Num-1}),
                    {reply,ok,State};
                _ ->
                    {reply,ok,State} 
                end;
            _ ->
                {reply,ok,State}    
            end
        end
    end;    
    
handle_call(Req, _, State) ->
    {reply, {error,unknown_request}, State}.

handle_cast(stop, S) ->
    dets:close(commandLine),
    {stop, normal, S};
    
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.    
    
