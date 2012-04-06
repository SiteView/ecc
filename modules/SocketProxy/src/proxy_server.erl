%%%Author:huanghao
%%%CreateDate:2011-03-08
%%%LastModify:2011-03-15
-module(proxy_server).
-include("config.hrl").
-behavior(gen_server).
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). 
-compile(export_all).
-record(proxyinfo,{pid,host,port,user,password,type}).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
stop() -> gen_server:call(?MODULE, stop). 

init([]) ->
	crypto:start(),
	ssh:start(),
	ets:new(?MODULE, [set,named_table,public]),
	{ok,[]}.  
	  
open(Socket,Host, Port,User, Password,ssh) ->
	Token = [Socket,Host],
	%~ Hashkey = list_to_atom(hash(Host)),
	%~ Hashkey = list_to_atom(hash(Token)),
	Hashkey = Token,
	case getkey(Hashkey) of
		[] ->  ssh_open(Hashkey,Host, Port,User, Password);
		'opening'  -> {error,"opening"}; 
		ProxyInfo ->
			case {erlang:is_process_alive(ProxyInfo#proxyinfo.pid),process_info(ProxyInfo#proxyinfo.pid)} of
				{true,Process_info} when Process_info =/= undefined -> ok;  
				_ -> ssh_open(Hashkey,Host, Port,User, Password)
			end,
		{ok,ProxyInfo#proxyinfo.pid}     
	end;

open(Socket,Host, Port,User, Password,telnet) ->
	Token = [Socket,Host],
	Hashkey = Token,
	case getkey(Hashkey) of
		[] ->  telnet_open(Hashkey,Host, Port,User, Password);
		'opening'  -> {error,"opening"}; 
		ProxyInfo ->
			case {erlang:is_process_alive(ProxyInfo#proxyinfo.pid),process_info(ProxyInfo#proxyinfo.pid)} of
				{true,Process_info} when Process_info =/= undefined -> ok;  
				_ -> telnet_open(Hashkey,Host, Port,User, Password)
			end,
		{ok,ProxyInfo#proxyinfo.pid}     
	end;

open(_,_,_,_,_,_) -> {error,"protol error!"}.

ssh_open(Hashkey,Host, Port,User, Password) ->
	ets:insert(?MODULE,{Hashkey,'opening'}),
	{ok,Pid} = mm_ssh:start(),
	try mm_ssh:connect(Pid,Host,Port,User,Password) of
		{ok,connected} ->
			ProxyInfo = #proxyinfo{pid = Pid,host = Host,port=Port,user=User,password=Password,type='ssh'},
			ets:insert(?MODULE,{Hashkey,ProxyInfo}),
			io:format("Connect to ~p by proxy[ssh] server success~n",[Host]);
		Reason -> Reason
	catch 
		error:_Error -> mm_ssh:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash};
		_:_ ->  mm_ssh:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash}
	end.
	
telnet_open(Hashkey,Host, Port,User, Password) ->
	ets:insert(?MODULE,{Hashkey,'opening'}),
	{ok,Pid} = telnet_client:start(),
	try telnet_client:connect(Pid,Host,Port,User,Password) of
		{ok,connected} ->
			ProxyInfo = #proxyinfo{pid = Pid,host = Host,port=Port,user=User,password=Password,type='telnet'},
			ets:insert(?MODULE,{Hashkey,ProxyInfo}),
			io:format("Connect to ~p by proxy[telnet] server success~n",[Host]);
		Reason -> Reason
	catch 
		error:_Error -> telnet_client:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash};
		_:_ ->  telnet_client:close(Pid),ets:insert(?MODULE, {Hashkey,[]}),{error,crash}
	end.
	
getdatabykey(Socket,Host,Key) ->
	Token = [Socket,Host],
	%~ Hashkey = list_to_atom(hash(Token)),
	Hashkey = Token,
	case getkey(Hashkey) of
		[] -> {error,"Please open host."};
		'opening'  -> {error,"opening"}; 
		ProxyInfo ->
			case {erlang:is_process_alive(ProxyInfo#proxyinfo.pid),process_info(ProxyInfo#proxyinfo.pid)} of
				{true,Process_info} when Process_info =/= undefined -> getdatabyprotocol(ProxyInfo,Key);  
				_ -> {error,"Process no alive!"}
			end
	end.
			
getdatabyprotocol(ProxyInfo,Key) when ProxyInfo#proxyinfo.type =:= 'ssh' ->
	case mm_ssh:send(ProxyInfo#proxyinfo.pid,Key) of
		{ok,Result} ->	 {ok,Result};
		_ -> {error,"exec " ++ Key ++ " error."}
	end;

getdatabyprotocol(ProxyInfo,Key) when ProxyInfo#proxyinfo.type =:= 'telnet' ->
	case telnet_client:cmd(ProxyInfo#proxyinfo.pid,Key) of
		{ok,Result} ->	 {ok,Result};
		_ -> {error,"exec " ++ Key ++ " error."}
	end;

getdatabyprotocol(_,_) -> {error,"Error Protocol!"}.

hash(Data) ->
	io:format("hash data is ~p~n",[Data]),
    %~ <<I:160/integer>> = crypto:sha(list_to_binary(Data)),
    %~ string:to_lower(lists:flatten(io_lib:fwrite("~31..0s", [erlang:integer_to_list(I, 36)]))).
	Data.
	
getkey(Key) ->
    case ets:lookup(?MODULE,Key) of
	     [{_,Data}] -> Data;
	     _ -> []
	end.

handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab};
handle_call(init, _From, Tab) -> {reply, ok, Tab}.
handle_cast(_Msg, State) -> {noreply, State}.  
handle_info(_Info, State) -> {noreply, State}.  
terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}.

	