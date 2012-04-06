-module(server_conf).

-behaviour(gen_server).


-compile(export_all).

-record(state, {conf_tab,hostname}).

-define(EVENT_THREAD_COUNT,100).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([start_link/0,start_link/1,stop/0]).

start_link() ->
    start_link([]).
	
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).
	

init([Opts])->
	Tab = ets:new(siteview_elecc_conf, [set,public]),
	case file:consult("conf/server.conf") of
		{ok,Data}->
			ets:insert(Tab,Data),
			{ok,#state{conf_tab=Tab,hostname=gethostname()}};
		_->
			{stop, "open file error"}
	end.
	
stop() ->
    gen_server:cast(?MODULE, stop).
	
getServerID()->
	% start_link(),
	gen_server:call(?MODULE, {<<"get_server_id">>}).
	
get_db_node()->
	% start_link(),
	gen_server:call(?MODULE, {<<"get_db_node">>}).
	
getWmiNode()->	
	% start_link(),
	gen_server:call(?MODULE, {<<"get_wmi_node">>}).	
getLayoutNode()->	
	% start_link(),
	gen_server:call(?MODULE, {<<"get_layout_node">>}).		
	
getServerConf(Key)->
	% start_link(),
	gen_server:call(?MODULE, {<<"server_conf">>,Key}).	

get_java_node()->
	% start_link(),
	gen_server:call(?MODULE, {<<"get_java_node">>}).
	
get_ofbiz_node()->
	% start_link(),
	gen_server:call(?MODULE, {<<"get_ofbiz_node">>}).

get_release_date()->
	% start_link(),
	gen_server:call(?MODULE, {<<"release_date">>}).
	
setServerConf(Key,Val)->
	% start_link(),
	gen_server:call(?MODULE, {<<"server_conf">>,Key,Val}).	
	
get_sh_process()->
	% start_link(),
	gen_server:call(?MODULE, {<<"sh_process">>}).
	
save()->
	gen_server:call(?MODULE, {<<"save">>}).
	
getServerConf(Key,Default)->
	case getServerConf(Key) of
		undefined->
			Default;
		V->
			V
	end.
	
handle_call({<<"get_server_id">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,serverID) of
		[]->
			{reply,'99',State};
		[{_,ServerId}|_]->
			{reply,ServerId,State}
	end;
handle_call({<<"get_layout_node">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,layout) of
		[]->
			{reply,list_to_atom("layout@" ++ State#state.hostname),State};
		[{_,Val}|_]->
			{reply,Val,State}
	end;	
handle_call({<<"get_db_node">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,dbNode) of
		[]->
			{reply,list_to_atom("db@" ++ State#state.hostname),State};
		[{_,Val}|_] -> 
			{reply,Val,State}
%% 		  when (string:chr(Val, $@) > 0) ->
%% 			if 
%% 				(string:chr(atom_to_list(Val), $@) > 0) -> {reply,Val,State};
%% 				(string:chr(atom_to_list(Val), $@) == 0) -> {reply,list_to_atom(atom_to_list(Val)++"@"++State#state.hostname),State}
%% 			end
	end;
handle_call({<<"get_java_node">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,javaNode) of
		[]->
			{reply,list_to_atom("java_monitor@" ++ State#state.hostname),State};
		[{_,Val}|_] -> {reply,Val,State}
	end;
handle_call({<<"get_ofbiz_node">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,ofbizNode) of
		[]->
			{reply,list_to_atom("ofbiz@" ++ State#state.hostname),State};
		[{_,Val}|_]->
			{reply,Val,State}
	end;
handle_call({<<"get_wmi_node">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,wmiNode) of
		[]->
			{reply,list_to_atom("wmi@" ++ State#state.hostname),State};
		[{_,Val}|_]->
			{reply,Val,State}
	end;	
handle_call({<<"release_date">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,release_date) of
		[]->
			{reply,"",State};
		[{_,Val}|_]->
			{reply,Val,State}
	end;
handle_call({<<"sh_process">>}, _, State) ->
	case ets:lookup(State#state.conf_tab,sh_process) of
		[]->
			{reply,?EVENT_THREAD_COUNT,State};
		[{_,Val}|_]->
			{reply,Val,State}
	end;
handle_call({<<"server_conf">>,Key}, _, State) ->
	case ets:lookup(State#state.conf_tab,Key) of
		[]->
			{reply,undefined,State};
		[{_,Val}|_]->
			{reply,Val,State}
	end;
handle_call({<<"server_conf">>,Key,Val}, _, State) ->
	case ets:insert(State#state.conf_tab,{Key,Val}) of
		true->
			{reply,ok,State};
		Else->
			{reply,{error,Else},State}
	end;
handle_call({<<"save">>}, _, State) ->
	Data = ets:tab2list(State#state.conf_tab),
	Str = lists:foldl(fun(X,R)->
					R ++ io_lib:format("~p.~n",[X])
					end,[],Data),
	Ret = file:write_file("conf/server.conf",list_to_binary(Str)),
	{reply,Ret,State};
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
	

	
handle_cast(stop, S) ->
	ets:delete(S#state.conf_tab),
    {stop, normal, S};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
	ets:delete(_State#state.conf_tab),
    ok.	
		
gethostname()->
	case inet:gethostname() of
		{ok,Name}->
			Name;
		_->
			"localhost"
	end.

gethostip()->
	case inet:gethostbyname(gethostname()) of
		{ok,{hostent,_,_,_,_,[Ip|_]}}->
			case Ip of
				{N1,N2,N3,N4}->
					lists:flatten(io_lib:format("~p.~p.~p.~p",[N1,N2,N3,N4]));
				_->
					"127.0.0.1"
			end;
		_->
			"127.0.0.1"
	end.
			
