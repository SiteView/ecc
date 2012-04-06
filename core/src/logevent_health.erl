%% @doc log event health
%% 
%%
-module(logevent_health).
-behaviour(gen_server).

-export([start_link/0,stop/0,
		log/1,add_searchs/1,get_matchs/1,reset/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {searchs,matchs}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).

init([]) ->
	%process_flag(trap_exit, true),
	{ok, #state{searchs=[],matchs=dict:new()}}.


add_searchs(Str)->
	call({add_searchs,Str}).

get_matchs(Str)->
	call({get_matchs,Str}).

log(Str)->
	call({log,Str}).

reset()->
	call({reset}).

handle_call({log,Str}, _, State) ->
	NS = match(State#state.searchs,Str,State),
	{reply,ok,NS};

handle_call({add_searchs,Str}, _, State) ->
	{reply,ok,#state{searchs=State#state.searchs ++ [Str],matchs = State#state.matchs}};	

handle_call({get_matchs,Str}, _, State) ->
	{reply,dict:find(Str,State#state.matchs),State};

handle_call({reset}, _, State) ->
	{reply,ok,#state{searchs=State#state.searchs,matchs=dict:new()}};

handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.

call(Req) ->
    gen_server:call(?MODULE, Req, infinity).

cast(Msg) ->
    gen_server:cast(?MODULE, Msg).


match([],_,Dict)->Dict;
match([X|T],Str,Dict)->
	case re:run(Str,X) of
		{match,_}->
			NS = #state{searchs=Dict#state.searchs,matchs=dict:append(X,Str,Dict#state.matchs)},
			match(T,Str,NS);
		_->
			match(T,Str,Dict)
	end.

handle_cast(stop, State) ->
	{stop,normal,State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

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