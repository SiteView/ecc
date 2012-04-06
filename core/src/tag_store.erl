%% ---
%%your comment
%%
%%---
-module(tag_store).
-behaviour(gen_server).

-include("monitor.hrl").

-export([start_link/0,stop/0,
		tag/3,q/1,untag/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {parent,file,mdate,app}).

-record(tag,{oid,otype,tagid}).

-define(LOGDIR,"logs").
-define(LOGNAME,tag_store).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).


init([]) ->
	{ok, #state{}}.

tag(OId,OType,Tags)->
	start_link(),
	App = atom_to_list(dbcs_base:get_app()),
	call({tag,OId,OType,Tags,App}).

untag(OId)->
	start_link(),
	App = atom_to_list(dbcs_base:get_app()),
	call({untag,OId,App}).
	
q(Params)->
	start_link(),
	 App = atom_to_list(dbcs_base:get_app()),
	 call({q,Params,App}).

handle_call({tag,Oid,OType,Tags,App}, _, State) ->
	FileName = ?LOGDIR ++ "/" ++ App ++ "/tag.db",
	filelib:ensure_dir(FileName),
	case dets:open_file(?LOGNAME,[{file,FileName},{type,bag},{keypos,2}]) of
		{ok,_}->
			dets:delete(?LOGNAME,Oid),
			TagData = lists:map(fun(X)->#tag{oid=Oid,otype=OType,tagid=X} end,Tags),
			case dets:insert(?LOGNAME,TagData) of
				ok->
					dets:close(?LOGNAME),
					{reply, ok, State};
				Else->
					dets:close(?LOGNAME),
					{reply, Else, State}
			end;
		Err->
			{reply, Err, State}
	end;
handle_call({untag,Oid,App}, _, State) ->
	FileName = ?LOGDIR ++ "/" ++ App ++ "/tag.db",
	filelib:ensure_dir(FileName),
	case dets:open_file(?LOGNAME,[{file,FileName},{type,bag},{keypos,2}]) of
		{ok,_}->
			{reply, dets:delete(?LOGNAME,Oid), State};
		Err->
			{reply, Err, State}
	end;
handle_call({q, Params, App}, _, State) ->
	FileName = ?LOGDIR ++ "/" ++ App ++ "/tag.db",
	filelib:ensure_dir(FileName),
	Sel = make_select(Params,[]),
	case Sel of
		{error,ERR}->
			{reply,{error,ERR},State};
		_->
			case dets:open_file(?LOGNAME,[{file,FileName},{type,bag},{keypos,2}]) of
				{ok,_}->
					Ret = dets:select(?LOGNAME,Sel),
					dets:close(?LOGNAME),
					{reply,{ok,Ret},State};
				Err->
					{reply,Err,State}
			end
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
	find_pos(K,record_info(fields,tag),1).

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