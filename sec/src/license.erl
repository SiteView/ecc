
-module(license).
-author('zhenming.wang@dragonflow.com').

-behaviour(gen_server).

-compile(export_all).

%% Internal exports, call-back functions.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).


start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_, Path} = file:get_cwd(),
    io:format("--->~p~n", [Path]),
    case erl_ddll:load_driver(Path++"/priv/lib", license_drv) of
	ok -> ok;
	{error, already_loaded} -> ok;
	{error,ErrorDesc} -> io:format("===>~p~n", [erl_ddll:format_error(ErrorDesc)]),exit({error, could_not_load_driver});
	_ ->  exit({error, could_not_load_driver})
    end,
    Port = open_port({spawn, license_drv}, []),
    {ok, Port}.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

port()->
    gen_server:call(?MODULE, port, 1000).

handle_call(port, _, Port)->
    {reply, Port, Port};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};
handle_info({'EXIT', _Pid, _Reason}, Port) ->
    {noreply, Port};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, Port) ->
    Port ! {self, close},
    ok.
 
local_time() ->
    {{Y,M,D},_}	= calendar:local_time(),
    lists:flatten([integer_to_list(Y),lists:flatten(io_lib:format("~2.10.0B", [M])),lists:flatten(io_lib:format("~2.10.0B", [D]))]).
    
createlicense(Pdn,Machinecode,Customer,Modules,Points,Devices,Startdate,Delaydays,User,Support,Isfinal,Allowmachine,Isdefault) ->
    Port = port(),
    Bin = term_to_binary({Pdn,Machinecode,Customer,Modules,Points,Devices,Startdate,Delaydays,User,Support,Isfinal,Allowmachine,Isdefault,local_time()}),
    Result = port_control(Port, 0, Bin),
    binary_to_term(Result).
    
isvalidmachinecode(Machinecode) ->
    Port = port(),
    Bin = term_to_binary({Machinecode}),
    Result = port_control(Port, 1, Bin),
    binary_to_term(Result).    

createkey(Machinecode) ->
    Port = port(),
    Bin = term_to_binary({Machinecode}),
    Result = port_control(Port, 2, Bin),
    binary_to_term(Result).  


getlicstring(FileName) ->
    io:format("FileName: ~p~n", [FileName]),
    Port = port(),
    Bin = term_to_binary({FileName}),
    Result = port_control(Port, 3, Bin),
    binary_to_term(Result).  

decrypt2data(SzLic) ->
    Port = port(),
    Bin = term_to_binary({SzLic}),
    Result = port_control(Port,4, Bin),
    binary_to_term(Result). 


isvalidlicense(Module, Licstr, DaltTime, BVerify) ->
    Port = port(),
    Bin = term_to_binary({Module, Licstr, DaltTime, BVerify}),
    Result = port_control(Port,5, Bin),
    binary_to_term(Result). 
    
mac() ->
    {ok,""}.
