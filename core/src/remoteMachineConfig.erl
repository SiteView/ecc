-module(remoteMachineConfig).
-behaviour(gen_server).
-define(SERVER,'ecc_remoteMachineConfig').
-define(PREFS_DIR,"logs/").
-include("monitor.hrl").
-include("remoteMachine.hrl").

-record(state, {parent,files=[]}).

-export([start_link/0, set/3, get/2, remove/2, removeAll/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         

%% gen_server callbacks

init(Opts) ->
    case filelib:is_dir(?PREFS_DIR) of
        true ->
            ok;
        _ ->
            file:make_dir(?PREFS_DIR)
    end,
    [dets:open_file(X,[{file,?PREFS_DIR ++ atom_to_list(X)}])||X<-Opts],
	{ok,#state{files=Opts}}.
    
handle_call({set, File, Key, Val}, _From, State) ->
    case lists:member(File,State#state.files) of
		false ->
			case dets:open_file(File,[{file,?PREFS_DIR ++ atom_to_list(File)}]) of
				{ok,_}->
					dets:insert(File,{Key,Val}),
					dets:sync(File),
					{reply,{ok,File},State#state{files=State#state.files++[File]}};
				_->
				{reply,{error,write_file_error},State}
			end;
		true ->
			dets:insert(File,{Key,Val}),
			dets:sync(File),
			{reply,{ok,File},State}
	end;
handle_call({get, File, Key}, _From, State) ->
%% 	io:format("****************handle_call***********~p~n~p~n",[Key,File]),
    case lists:member(File,State#state.files) of
		false ->
%% 			io:format("****************handle_callaaa***********~p~n~p~n",[Key,File]),
			FilePath = ?PREFS_DIR ++ atom_to_list(File),
			case dets:is_dets_file(FilePath) of
				true->
%% 				io:format("****************handle_callbbb***********~p~n~p~n",[Key,File]),
					case dets:open_file(File,[{file,FilePath}]) of
						{ok,_}->
%% 							io:format("****************handle_callcccc***********~p~n~p~n",[Key,File]),
							{reply,{ok,dets:lookup(File,Key)},State#state{files=State#state.files++[File]}};
						_->
							{reply,{error,read_file_error},State}
					end;
				_->
					{reply,{error,read_file_error},State}
			end;
		true ->
			{reply,{ok,dets:lookup(File,Key)},State}
	end;
handle_call({remove, File, Key}, _From, State) ->
    case lists:member(File,State#state.files) of
		false ->
			{reply,{error,read_file_error},State};
		true ->
			dets:delete(File,Key),
			dets:sync(File),
			{reply,{ok,deleted},State}
	end;
handle_call({removeAll, File}, _From, State) ->
    case lists:member(File,State#state.files) of
		false ->
			{reply,{error,read_file_error},State};
		true ->
			dets:delete_all_objects(File),
			dets:sync(File),
			{reply,{ok,deleted},State}
	end;
handle_call(Other, _From, State) ->
    {reply, {error, notsupport},State}.
    
handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

start_link() ->
    start_link([?REMOTECONF_SET, ?REMOTECONF_RELTAGANDMA]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
    
cast(Req) ->
    gen_server:cast(?SERVER, Req).

 
%% api 
set(File, Key, Val) ->
    call({set, File, Key, Val}).
    
get(File, Key) ->
%% 	io:format("****************************"),
    call({get, File, Key}).
    
remove(File, Key) ->
    call({remove, File, Key}).
    
removeAll(File) ->
    call({removeAll, File}).


stop() ->
    cast(stop).
    

    
    
    
