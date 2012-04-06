-module(remoteMachineManager).
-behaviour(supervisor).
-include("monitor.hrl").
-include("remoteMachine.hrl").

-export([start_link/1, start_link/0]).
-export([init/1]).

start_link() ->
    Super = 
        try start_link([]) of
            V ->
                V
        catch
            E ->
                E
        end,
    Super.
    
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).  

%% gen_server callbacks

init(Opts) ->
    process_flag(trap_exit, true),
    %% ��������
    %%{ok, Pid} = remoteMachineConfig:start_link(),       %% Զ�̷��������ñ����
    %%{ok, PPid} = remoteMachineTag:start_link(),         %% Զ�̷�������ǩ�����
    %%
	{ok, {{one_for_one, 3, 10},
	  [{remoteMachineConfig, {remoteMachineConfig, start_link, []},transient,brutal_kill,worker,[remoteMachineConfig]},
	   {remoteMachineTag, {remoteMachineTag, start_link, [Opts]},transient,brutal_kill,worker,[remoteMachineTag]}  
	  ]}}.
    


%% API

    

    
    
    
