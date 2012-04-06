-module(user_behaviour_record).
-behaviour(gen_server).
-define(DIR,"conf/perferences/").
%% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,code_change/3, terminate/2]).
%% module   api         
-export([save_info/1,lookup/1,all_data/0,all_keys/0,all_data_sort_by_times/0,all_data_sort_by_datetime/0]).

-compile(export_all).

start() ->  gen_server:start_link({local,?MODULE},?MODULE,[],[]).
stop()  ->  
    dets:close(?MODULE),
    cast(stop).

init([]) ->  
    dets:open_file(?MODULE,[{file,?DIR ++atom_to_list(?MODULE)},{auto_save,1000}]),
    ets:new(?MODULE,[named_table]),  
    {ok,ets:from_dets(?MODULE,?MODULE)}.

%% @spec save_info(Monitor)-> {ok}|{error,Reason1,Reason2}
%% @doc save a user's behaviour
save_info(MonitorId)  when is_list(MonitorId) ->
    save_info(list_to_atom(MonitorId));
save_info(MonitorId)  ->
    IsMonitor = case api_monitor:info(MonitorId) of
                {error, _} -> false;
                MonitorInfo ->
                    {value,{class,Class}} = lists:keysearch(class,1,MonitorInfo),
                    case Class=:=nnm_node_poller orelse Class=:= group of
                        true -> false;
                        _ -> true
                    end
                end,
    case IsMonitor of
        false   ->
            {error,errorMonitor,errorMonitor};
        _ ->
        Dateime = sv_datetime:now(),
        call({update,{MonitorId,Dateime}})
    end.

%% @spec lookup(MonitorId)-> [Object]
%% @spec Object = {times,MicroSeconds}
%% @doc lookup a monitor's record
lookup(MonitorId)  ->
    case whereis(?MODULE) of
        undefined   ->  start();
        _   ->  ok
    end,
    {_MonitorId,{Times,MicroSeconds}} = ets:lookup(?MODULE,MonitorId),
    {Times,MicroSeconds}.

%% @spec allkeys()-> [keyorkeys]
%% @doc return all keys
all_keys() ->
    case whereis(?MODULE) of
        undefined   ->  start();
        _   ->  ok
    end,
    case ets:first(?MODULE) of
       '$end_of_table'-> [];
       Key  -> [Key]++all_keys(Key)
    end.
all_keys(Key1) ->
    case ets:next(?MODULE, Key1) of
        '$end_of_table' -> [];
        Key2    -> [Key2] ++ all_keys(Key2)
    end.

%% @spec alldata()-> [ObjectorObjects]
%% @spec Object = {monitorId,{times,MicroSeconds}}
%% @doc get all monitor's record        
all_data() ->
    case whereis(?MODULE) of
        undefined   ->  start();
        _   ->  ok
    end,
    case ets:first(?MODULE) of
       '$end_of_table'-> [];
       Key  -> ets:lookup(?MODULE,Key)++all_data(Key)
    end.
all_data(Key1)    ->
    case ets:next(?MODULE, Key1) of
        '$end_of_table' -> [];
        Key2    ->ets:lookup(?MODULE,Key2) ++ all_data(Key2)
    end.

%% @spec alldata()-> [ObjectorObjects]
%% @spec Object = {monitorId,{times,MicroSeconds}}
%% @doc get all monitor's record sort by MicroSeconds
all_data_sort_by_datetime()  ->
    UserRecords = all_data(),
    Fun = fun({_MonitorId1,{_Times1,MicroSeconds1}}, {_MonitorId2,{_Times2,MicroSeconds2}}) ->
                case MicroSeconds1 > MicroSeconds2 of
                    true -> true;
                    _   ->  false
                end
          end,
    lists:sort(Fun,UserRecords).

%% @spec alldata()-> [ObjectorObjects]
%% @spec Object = {monitorId,{times,MicroSeconds}}
%% @doc get all monitor's record sort by times
all_data_sort_by_times()  ->
    UserRecords = all_data(),
    Fun = fun({_MonitorId1,{Times1,_MicroSeconds1}}, {_MonitorId2,{Times2,_MicroSeconds2}}) ->
                case Times1 > Times2 of
                    true -> true;
                    _   ->  false
                end
          end,
    lists:sort(Fun,UserRecords).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server api %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    dets:close(?MODULE),
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({update,{MonitorId,MicroSeconds}},_From,_Tab)    ->
    Result_Ets = case ets:lookup(?MODULE,MonitorId)   of
                []  ->  ets:insert(?MODULE,{MonitorId,{1,MicroSeconds}}),
                        ok;
                [{_MonitorId1,{Times1,_MicroSeconds1}}]  ->
                        ets:insert(?MODULE,{MonitorId,{Times1+1,MicroSeconds}}),
                        ok;
                Error1   ->
                        Error1
    end,
    Result_Dets = case dets:lookup(?MODULE,MonitorId)   of
                []  ->  dets:insert(?MODULE,{MonitorId,{1,MicroSeconds}}),
                        ok;
                [{_MonitorId2,{Times2,_MicroSeconds2}}]  ->
                        dets:insert(?MODULE,{MonitorId,{Times2+1,MicroSeconds}}),
                        ok;
                Error2   ->
                        Error2
    end,                    
    case Result_Ets =:= ok andalso Result_Dets =:= ok of
        true    ->
                {reply,{ok},MonitorId};
                
        _   ->  {reply,{error,Result_Ets,Result_Dets},MonitorId}
    end.
    
call(Info) ->
    case whereis(?MODULE) of
        undefined   ->  start();
        _   ->  ok
    end,
    gen_server:call(?MODULE, Info).

cast(Msg) ->
    gen_server:cast(?MODULE, Msg).



