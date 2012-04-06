-module(process_events).
-compile(export_all).
-define(TIMEOUT,5000).
%%存储消息
loop_store(A)->
receive
    {From,stop}->
        From ! {self(),{ok,loop_store_stopped}};
    {From,get_all}->
        From ! {self(),{ok,A}},
            loop_store(A);
    {From,{set_one,{Eventid,Events}}}->
            From ! {self(),{ok,"set ok"}},
            loop_store(A++[{Eventid,Events}]);
    {From,{remove_all}}->
            From ! {self(),{ok,"remove all ok"}},
            loop_store([]);
    {From,{remove_one,Key}}->
            loop_store(lists:keydelete(Key,1,A));
    {From,{get_one}}-> %%取出一个并且删除取出的那一个
    case A of
    []-> 
        From ! {self(),{error,"no message"}},
        loop_store([]);
    _->
        From ! {self(),{ok,lists:nth(1,A)}},
        loop_store(A)
    end;
    {From,_Other}->
        From ! {self(),{error,"bad argument"}},
        loop_store(A)
end.
    
%%存储进程，及进程状态
loop_pool(A)->
receive
    {From,stop}->
        [stop_pid(Pid) || {Pid,_}<-A],
        From ! {self(),{ok,loop_pool_stopped}};
    {From,get_all}->
        From ! {self(),{ok,A}},
            loop_pool(A);
    {From,{set,Pid}}->  %%存储进来的应该是Pid
        From ! {self(),{ok,"set ok"}},
        loop_pool(A++[{Pid,"free"}]);
     {From,{has_a_freeOne}}->
        case lists:keyfind("free",2,A) of
        false->
            From ! {self(),{error,"all busy"}},
            loop_pool(A);
        {Pid,_State}->
            From ! {self(),{ok,Pid}},
            loop_pool(A)
        end;
    {From,{get_a_freeOne}}->
    case lists:keyfind("free",2,A) of
        false->
            From ! {self(),{error,"all busy"}},
            loop_pool(A);
        {Pid,_State}->
            From ! {self(),{ok,Pid}},
            loop_pool(lists:keyreplace(Pid,1,A,{Pid,"busy"}))
    end;
    {From,{setFree,Pid}}->
        case lists:keyfind(Pid,1,A) of
            false->
                From ! {self(),{error,"not found pid"}},
                loop_pool(A);
            _->
                 From ! {self(),{ok,"Pid is free"}},
                 A2 = lists:keyreplace(Pid,1,A,{Pid,"free"}),
                 loop_pool(A2)
        end;
    {From,{replace,OPid,{NPid,State}}}->
        From ! {self(),{ok,NPid}},
        A2 = lists:keyreplace(OPid,1,A,{NPid,State}),
        loop_pool(A2); 
    {From,_Other}->
        From ! {self(),{error,"bad argument"}},
        loop_pool(A)
end.

stop_pid(Pid)->
    Pid ! {self(),stop},
    receive
        {_From,{ok,stopped}}-> ok;
         _->stop_pid(Pid)
    end. 