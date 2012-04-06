%% ---
%%your comment
%%
%%---
-module(monitor_status_store).
-behaviour(gen_server).

-include("monitor.hrl").

-export([start_link/0,stop/0,
		save/2,read/1,read/2,save/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {parent,file,mdate,app}).




-define(LOGDIR,"logs").
-define(LOGNAME,monitor_status).
-define(FILENAME,"monitor_status.db").


-record(monitor_status,{id,data}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).


init([]) ->
	FileName = ?LOGDIR ++ "/" ++ ?FILENAME,
	filelib:ensure_dir(FileName),
	dets:open_file(?LOGNAME,[{file,FileName},{type,set},{keypos,2},{auto_save,10000}]),
	{ok, #state{}}.


%% @spec log(Terms)->{ok,Log} | {error,Reason}
%% where
%%	Data = list)
%%	Log = string()
%% Reason = string()
%% @doc write alert log file
save(Id,Data)->
    App = atom_to_list(dbcs_base:get_app()),
	call({save,Id,Data,App}).
	
save(App,Id,Data)->
 	call({save,Id,Data,App}).

read(App,Id)->
	call({read,Id, App}).
	
read(Id)->
	 App = atom_to_list(dbcs_base:get_app()),
	 call({read,Id, App}).


handle_call({save,Id,Data,App}, _, State) ->
	FileName = ?LOGDIR ++ "/" ++ ?FILENAME,
	filelib:ensure_dir(FileName),
	NewId = lists:flatten(io_lib:format("~s-~s",[App,Id])),
	% case dets:open_file(?LOGNAME,[{file,FileName},{type,set},{keypos,2}]) of
	%	{ok,_}->
			case dets:insert(?LOGNAME,#monitor_status{id=NewId,data=Data}) of
				ok->
					% dets:close(?LOGNAME),
					{reply, ok, State};
				Else->
					dets:close(?LOGNAME),
					{reply, Else, State}
			end;
	%	Err->
	%		{reply, Err, State}
	% end;
  
handle_call({read, Id, App}, _, State) ->
	FileName = ?LOGDIR ++ "/" ++ ?FILENAME,
	filelib:ensure_dir(FileName),
	NewId = lists:flatten(io_lib:format("~s-~s",[App,Id])),
	Sel = make_select([{id,'=',NewId}],[]),
	% io:format("match spec:~p~n",[Sel]),
	case Sel of
		{error,ERR}->
			{reply,{error,ERR},State};
		_->
			% case dets:open_file(?LOGNAME,[{file,FileName},{type,set},{keypos,2}]) of
			%	{ok,_}->
					Ret = dets:select(?LOGNAME,Sel),
					% dets:close(?LOGNAME),
					case Ret  of
						[]->
							{reply,{ok,[]},State};
						[R|_]->
							{reply,{ok,R#monitor_status.data},State}
					end
			%	Err->
			%		{reply,Err,State}
			% end
	end;
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
	

handle_cast(stop, State) ->
	{stop,normal,State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

make_select([],R)->[{'$1',R,['$1']}];
make_select([{K,'=',V}|T],R)->
	Pos = find_pos(K),
	if
		Pos>0->
			make_select(T,R ++ [{'==',{element,Pos,'$1'},V}]);
		true->
			{error,error_key}
	end;
make_select([{K,'>',V}|T],R)->
	Pos = find_pos(K),
	if
		Pos>0->
			make_select(T,R ++ [{'>',{element,Pos,'$1'},V}]);
		true->
			{error,error_key}
	end;
make_select([{K,'<',V}|T],R)->
	Pos = find_pos(K),
	if
		Pos>0->
			make_select(T,R ++ [{'<',{element,Pos,'$1'},V}]);
		true->
			{error,error_key}
	end;
make_select([{K,'>=',V}|T],R)->
	Pos = find_pos(K),
	if
		Pos>0->
			make_select(T,R ++ [{'>=',{element,Pos,'$1'},V}]);
		true->
			{error,error_key}
	end;
make_select([{K,'=<',V}|T],R)->
	Pos = find_pos(K),
	if
		Pos>0->
			make_select(T,R ++ [{'=<',{element,Pos,'$1'},V}]);
		true->
			{error,error_key}
	end;
make_select([_|T],_)->{error,error_condition}.

find_pos(K)->
	find_pos(K,record_info(fields,operation_log),1).

find_pos(_,[],_)->0;	
find_pos(K,[K|T],I)->I+1;
find_pos(K,[_|T],I)->
	find_pos(K,T,I+1).
%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	dets:close(?LOGNAME),
    ok.

code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


call(Req) ->
    gen_server:call(?MODULE, Req, infinity).

cast(Msg) ->
    gen_server:cast(?MODULE, Msg).