-module(api_nmap_scan).
-compile(export_all).
-include("monitor.hrl").
-behaviour(gen_server).
-export([action/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,code_change/3, terminate/2]).

-define(SLEEPTIME,5000).

-record(state, {pid,events}).

%%%%%%%%%%%%%%%user_api%%%%%%%%%%%%%%%%%%

%%Gen_server scan start
action(IPList)when is_list(IPList)->call_action(IPList).

%%%%%%%%%%%%%%gen_server api%%%%%%%%%%%%%%%%

start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
%%Start the scanning process directly
init([])->
    Pid = proc_lib:spawn_link(fun()->loop_S() end),
    {ok,#state{pid=Pid,events=[]}}.

handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
stop() ->
    gen_server:cast(?MODULE, stop).
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call({state},_From,State)->    {reply,{State},State};
handle_call({update_state,NewState},_From,_State)->
    {reply,{ok},NewState};
handle_call({action,IPList},_From,State)->
    Events = State#state.events,
     NewEvents = case length(Events)>0 of
    true->
        {Key,_} = lists:last(Events),
         Events ++ [{Key+1,IPList}];
    _->
        [{1,IPList}]
    end,    
    {reply,{ok},#state{pid=State#state.pid,events=NewEvents}}.
%%%%%%%%%%%%%%%%%%%% scan function %%%%%%%%%%%%%%%%%%%%%%
call(Info) ->
    case whereis(?MODULE) of
        undefined   ->  start_link();
        _   ->  ok
    end,
    gen_server:call(?MODULE, Info,infinity).
call_action(Info)->
    call({action,Info}).
call_state()->
    call({state}).
call_state_update(NewState)->
    call({update_state,NewState}).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop_S()->
    case call_state() of
    []->
        timer:sleep(10),
        ok; 
    {[]}->
        timer:sleep(10),
        ok;
    {State}->
        Events = State#state.events,
        case length(Events)>0 of
            true->
                NewEvent =do_process(Events),
                call_state_update(#state{pid=State#state.pid,events=NewEvent});
            _->
                timer:sleep(10)
        end
    end,
    loop_S().

do_process(Events)->
    case length(Events)>0 of
    true->
        {Key,Event} = lists:nth(1,Events),
        scan(Event),
        %%Tell gen_server end of the scan.
        ?MODULE ! {stop,"scan over"},
        lists:keydelete(Key,1,Events);
    _->
        Events
   end.

scan(IPList)->
    %%First, empty the cache of data, Dets: process_pool_store
    process_pool:open_dets(),
    process_pool:clear_all_data(),
    %%Then start the scan
    pool_manage:do_scan(IPList),
    %%Repeated inquiry scan state is ended.
    Result = loop_result(),
    %%Will return the results read out and then cycle to update the database.
    FUN= fun(IPVALUE)->
        IP = proplists:get_value(ip,IPVALUE),
        ServiceAttr = proplists:get_value(service,IPVALUE),
        OSAttr = proplists:get_value(os,IPVALUE),
        Vendor = case OSAttr of
                    undefined->[];
                    _ -> proplists:get_value(vendor,OSAttr)
                end,
        OSVendor = remoteMachineTag:translate_OS(Vendor),
        case remoteMachineTag:get_machine_ByIP(IP) of
        []->[];
        {error,_}->[];
        MachineList ->
            Fun_do=
              %%Replace the machine with the vendor of the value of the os value to ServiceAttr and OSAttr into machine other.
                fun(Machine)->
                Other = Machine#machine.other,
                Other1 = case proplists:lookup(nmap_service,Other) of
                                  none->     Other++[{nmap_service,ServiceAttr}];
                                  _->   proplists:delete(nmap_service,Other)++[{nmap_service,ServiceAttr}]
                                  end,
                Other2 = case proplists:lookup(nmap_os,Other1) of
                                  none->     Other1++[{nmap_os,ServiceAttr}];
                                  _->   proplists:delete(nmap_os,Other1)++[{nmap_os,ServiceAttr}]
                                  end,             
                NEW_MACHINE = Machine#machine{other=Other2},
                api_machine:update_machine(NEW_MACHINE)
            end,
            lists:foreach(Fun_do,MachineList)
        end
    end,
    lists:foreach(FUN,Result),
    process_pool:close_dets(),
    io:format("~n nmap scan over!").
    
%%Query status circle
loop_result()->
    timer:sleep(?SLEEPTIME),
    case pool_manage:get_state() of
    "pool free"-> 
    %%Have scanned over, return to full scan
    process_pool:get_all();
    _->
    loop_result()
end.


    
    