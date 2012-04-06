
-module(extension_sup).
-behaviour(supervisor).

-export([start_link/1, start/0, init/1]).


start() ->
	spawn(fun() ->
		{ok, Pid}= extension_sup:start_link([]),
		unlink(Pid)
	end).

start_link(Args) ->	
    {ok, Pid}= supervisor:start_link({local, ?MODULE}, ?MODULE, 
									 [{callback, extension}, {args, Args}]),
 	{ok, _}=  supervisor:start_child(?MODULE, []),	
    {ok, Pid}.

init(_) ->	
    {ok,{{simple_one_for_one, 10, 1}, 
		 [{extension, 
		   {extension, start_link, []},
           permanent, 20000, worker, [extension]}]}}.

