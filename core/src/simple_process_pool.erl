%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc simple process pool.
%% simple process pool module
-module(simple_process_pool, [Pid]).
-compile(export_all).
-define(TIMEOUT, 5000).
-define(DEFAULT_OPTION, [{pool_timeout, 3600000}, {worker_timeout, 60000}, {process, 5}, {loop, fun(X)->X end}, {save_result, true}]).

new(Pid)->
	Id = spawn(fun()->simple_pool_action:start_pool() end),
	{?MODULE,Id}.
    
get_pid() ->
    Pid.
    
is_alive() ->
    is_process_alive(Pid).
    
%Define the number of processes, whether to save the results, how to deal with the results
set_option(Opt) ->
    Options = parse_option(Opt, ?DEFAULT_OPTION),
    Pid ! {self(), {set_option, Options}},
    receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
    
stop()->
	Pid ! {self(),stop},
	receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
    
%clear pool state, stop workers, make process pool do something else
clear() ->
    Pid ! {self(),clear},
	receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

get_result() ->
    Pid ! {self(), result},
    receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
    
get_work_state() ->
    Pid ! {self(), work_state},
    receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
    
get_pool_state() ->
    Pid ! {self(), pool_state},
    receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
    
get_idle_process() ->
    Pid ! {self(), get_idle_process},
    receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%args is that each execution Mod: Fun (Arg) in the total list of arg, arg must be a list of each parcel
request(Mod, Fun, Args) ->
    Total = length(Args),
    RequestPid = spawn(fun()->send_request(Mod, Fun, Args) end),
    Pid ! {self(), {request, RequestPid, Total}},
    receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.

%request process
send_request(_, _, []) -> Pid ! {self(), request_finish}; %auto dead
send_request(Mod, Fun, [F|R]=Args) ->
    receive
        {From, {wait_for_request, IdlePid}} ->
            %io:format("~p done, please assign other work~n", [IdlePid]),
            From ! {self(), {client, IdlePid, Mod, Fun, F}},
            send_request(Mod, Fun, R);
        {From, stop} ->
            From ! {self(), {ok, stopped}};
        {_From, {error, _}} ->
            send_request(Mod, Fun, Args);
        {From, IdlePid} ->
            From ! {self(), {client, IdlePid, Mod, Fun, F}},
            send_request(Mod, Fun, R);
        Other ->
            io:format("request process receive unkown message:~p~n",[Other]),
            send_request(Mod, Fun, Args)
    after 100 ->
        Pid ! {self(), get_idle_process},
        send_request(Mod, Fun, Args)
    end.
    
cancel_request() ->
    Pid ! {self(), cancel_request},
    receive
		{Pid,Ret}->
			Ret
	after ?TIMEOUT->
		{error,timeout}
	end.
    
parse_option([], Result) ->Result;
parse_option([{pool_timeout, PO}=F|R], Default) when is_integer(PO) ->
    parse_option(R, lists:keyreplace(pool_timeout, 1, Default, F));
parse_option([{worker_timeout, WO}=F|R], Default) when is_integer(WO) ->
    parse_option(R, lists:keyreplace(worker_timeout, 1, Default, F));
parse_option([{process, PN}=F|R], Default) when is_integer(PN) ->
    parse_option(R, lists:keyreplace(process, 1, Default, F));
parse_option([{loop, Fun}=F|R], Default) when is_function(Fun) ->
    parse_option(R, lists:keyreplace(loop, 1, Default, F));
parse_option([_F|R], Default) -> parse_option(R, Default).

%test instance
test_request_urls() ->
    clear(),
    set_option([{process, 10}, {loop, fun(X)->
        case X of
            {ok, {{_, State, _}, _, _}} ->
                {ok, State};
            Other ->
                Other
        end end}]),
    request(httpc, request, lists:duplicate(100, ["http://www.baidu.com"])),
    simple_pool_action:sleep(5000),
    io:format("work_state:~p, pool_state:~p~n Result:~p~n",[get_work_state(), get_pool_state(), get_result()]).
    
%ex: IpAddress -> 192.168.0.1-255
%Do not play too many processes...
test_nmap_scan(IpAddress) ->
    clear(),
    IPs = analyse_ip_string:analy_IPString(IpAddress),
    NmapStr = "  -T5 -sS -O",
    Args = [[[X] ++ [NmapStr]]||X<- IPs],
    set_option([{process, 10}]),
    request(nmap, scan, Args).
    
    
    
    