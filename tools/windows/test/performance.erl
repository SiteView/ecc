-module(performance).

-compile(export_all).


-define(TIMEOUT, 120000).


start(Count, Time)->
	List = lists:seq(1, Count),
	lists:foreach(fun(_)-> spawn(?MODULE, loop, [Time]) end, List).
	
sleep(Time)->
	receive
		after Time ->
			ok
	end.

loop(Time)->
    {_, Y, Z} = now(),
    {Rnd, _} = random:uniform_s(1000000, {Time, Y, Z}),
    Value = Rnd rem 900 + 1,
    Interval = if 
	Value < 15 ->
		15;
	true ->
		Value
    end,
    sleep(Interval * 1000),
    worker(Interval, Rnd),
    loop(Rnd).

worker(Sleep, Seed)->
	Tasks = {cpu, memory, disk},
	Parameters = {["192.168.6.197", "administrator", "kennyy"], ["localhost", "administrator", "dragonflow8425" ], ["192.168.6.114", "administrator", "chen11"]},
	{_, Y, Z} = now(),
	{Rnd, _} = random:uniform_s(1000000, {Seed, Y, Z}),
	Index = Rnd rem tuple_size(Tasks) + 1,
	Task = element(Index, Tasks),
	Parameter = element(Index, Parameters),
	T1  = now(),
	{Flag, _} = rpc:call(wmi@developer, wmi, Task, [1|Parameter], ?TIMEOUT),
	T2 = now(),
	Tdiff = timer:now_diff(T2, T1) div 1000,
	io:format("PID:~p, Sleep:~p, Host:~p, Task:~p, Time:~p, Result:~p~n", [self(), Sleep, hd(Parameter), Task, Tdiff, Flag]).


