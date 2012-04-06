%% 
%% @doc preferences module,save system config parameter.
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(preferences).
-compile(export_all).

-behaviour(gen_server).

%% gen_server callbacks
 -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% -export([verify/2,test/1]).

-export([get/2,remove/2,set/3,set/2,all/1]).

-record(state, {parent,files=[]}).

-define(PREFS_DIR,"conf/perferences/").

-define(SERVER,'elecc_perferences').

-define(Table,"elecc_perferences").

-include("dbcs_common.hrl").
-include("monitor.hrl").


data_to_db(Data,Id)->
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Data],
	case [is_atom(Id)] of
		[true]->
			{content,list_to_atom(?Table), Id, <<"elecc_perferences">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_data(GroupData)->
	case GroupData of
		{_,_,_,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv,V=/=null];
		_->
			[]
	end.
	

%% @spec start_link() ->(ok | {error,Reason})
%% where
%%	Reason = atom()
%% @doc start preferences services
%%
start_link() ->
%% 	erlide_log:log("******* preferences starting ***************"),
    start_link(['email_settings','additional_email_settings','master_config','general','log','snmp','additional_snmp_settings','multi_config','snmpnode','tuopuname','additional_sms_settings']).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

init([Opts])->
	% [dets:open_file(X,[{file,?PREFS_DIR ++ atom_to_list(X)}])||X<-Opts],
	{ok,#state{files=Opts}}.

stop() ->
    gen_server:cast(?SERVER, stop).

%% @spec get(File,Key)->({ok,[{Key,Value}]} | {error,Reason})
%% where
%%	File = atom()
%%	Key = atom()
%%	Value = term()
%%	Reason = atom()
%% @doc get config value from settings,File is the category.
%%
get(File,Key)->
	Id = makeid(File),
	App = dbcs_base:get_app(),
	erlcache:start_link(),
	HKey = erlang:phash2({App,preferences,File}),
	Ret = 
	case erlcache:get(HKey) of
		{ok,R}->
			R;
		_->
			db_ecc:get_data(?DBName,?Table,"id="++Id)
	end,
	erlcache:set(HKey,Ret,30),
	case Ret of
		{error,Resean}->
			{error,read_db_error};
		[]->
			{ok,[]};
		[Group|_]->
			%[Group|_] = Ret,
			D1 = db_to_data(Group),
			case proplists:get_value(Key,D1) of
				undefined->
					{ok,[]};
				Val->
					{ok,[{Key,Val}]}
			end;
		Else->
			Else
	end.

sync_get(File,Key)->
	App = dbcs_base:get_app(),
	gen_server:call(?SERVER,{get,File,Key,App}).

%% @spec remove(File,Key)-> ({ok,deleted} |{error,Reason})
%% where
%% 	File = atom()
%%	Reason = atom()
%% @doc remove a key from a setting category.
%%
remove(File,Key)->
	App = dbcs_base:get_app(),
	erlcache:start_link(),
	HKey = erlang:phash2({App,preferences,File}),
	erlcache:remove(HKey),
	gen_server:call(?SERVER, {remove,File,Key,App}).

%% @spec set(File,Key,Val)-> ({ok,Return}|{error,Reason})
%% where
%%	File = atom()
%%	Key = atom()
%%	Val = term()
%%	Return = term()
%%	Reason = atom()
%% @doc set a Key's value,File is category.
set(File,Key,Val)->
	App = dbcs_base:get_app(),
	erlcache:start_link(),
	HKey = erlang:phash2({App,preferences,File}),
	erlcache:remove(HKey),
	gen_server:call(?SERVER,{set,File,Key,Val,App}).
	
%% @spec set(File,Data)-> ({ok,Return}|{error,Reason})
%% where
%%	File = atom()
%%	Data = [{key,value}]
%%	Return = term()
%%	Reason = atom()
%% @doc set a Key's value,File is category.
set(File,Data)->
	App = dbcs_base:get_app(),
	erlcache:start_link(),
	HKey = erlang:phash2({App,preferences,File}),
	erlcache:remove(HKey),
	gen_server:call(?SERVER,{set,File,Data,App}).

%% @spec all(File)-> ({error,Reason} | {ok,[{Key,Value}]})
%% where
%%	File = atom()
%%	Reason = atom()
%%	Key = atom()
%%	Value = term()
%% @doc get all key's value from a category file.
all(File)->
	start_link(),
	App = dbcs_base:get_app(),
	gen_server:call(?SERVER,{all,File,App}).

%% @spec new(File,Key,Val)-> ({ok,Return} | {error,Reason})
%% where
%%	File = atom()
%%	Reason = atom()
%%	Key = atom()
%%	Value = term()
%%	Return = atom()
%% @doc create a new key in a category file.
new(File,Key,Val)->
	App = dbcs_base:get_app(),
	erlcache:start_link(),
	HKey = erlang:phash2({App,preferences,File}),
	erlcache:remove(HKey),
	gen_server:call(?SERVER,{new,File,Key,Val,App}).

reload()->
	gen_server:call(?SERVER,reload).
	
files()->
	gen_server:call(?SERVER,files).
	
hash(File)->
	gen_server:call(?SERVER,{hash,File}).

sync(Node)->
	gen_server:cast(?SERVER,{sync,Node}).
	
ff(F,P)->
	lists:flatten(io_lib:format(F,P)).
	
makeid(File)->
	ff("elecc_perferences#~s",[File]).
	
handle_call({get,File,Key,App}, _, State)->
	dbcs_base:set_app(App,true),
	Id = makeid(File),
	Ret = db_ecc:get_data(?DBName,?Table,"id="++Id),
	case Ret of
		{error,Resean}->
			{reply,{error,read_db_error},State};
		[]->
			{reply,{ok,[]},State};
		[Group|_]->
			%[Group|_] = Ret,
			D1 = db_to_data(Group),
			case proplists:get_value(Key,D1) of
				undefined->
					{reply,{ok,[]},State};
				Val->
					{reply,{ok,[{Key,Val}]},State}
			end;
		Else->
			{reply,Else,State}
	end;
	
handle_call({set,File,Key,Val,App}, _, State)->
	% io:format("set preferences app:~p~n",[App]),
	dbcs_base:set_app(App,true),
	Id = makeid(File),
	F = fun()->
			case db_ecc:insert_data(?DBName,?Table,data_to_db([{Key,Val}],list_to_atom(Id))) of
				{ok,_}->
					{ok,File};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end
		end,
	Ret = db_ecc:get_data(?DBName,?Table,"id="++Id),
	case Ret of
		{error,_}->
			R1 = F(),
			{reply,R1,State};
		[]->
			R1 = F(),
			{reply,R1,State};
		[Group|_]->
			% [Group|_] = Ret,
			D1 = db_to_data(Group),
			D2 = proplists:delete(Key,D1) ++ [{Key,Val}],
			R2 = db_ecc:update_data(?DBName,?Table,"id=" ++ Id,data_to_db(D2,list_to_atom(Id))),
			R3 = 
			case R2 of
				{ok,_}->
					{ok,File};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end,
			{reply,R3,State};
		Else->
			{reply,Else,State}
	end;
	
handle_call({set,File,Data,App}, _, State)->
	dbcs_base:set_app(App,true),
	Id = makeid(File),
	F = fun()->
			case db_ecc:insert_data(?DBName,?Table,data_to_db(Data,list_to_atom(Id))) of
				{ok,_}->
					{ok,File};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end
		end,

	Ret = db_ecc:get_data(?DBName,?Table,"id="++Id),
	case Ret of
		{error,_}->
			R1 = F(),
			{reply,R1,State};
		[]->
			R1 = F(),
			{reply,R1,State};
		[Group|_]->
			% [Group|_] = Ret,
			D1 = db_to_data(Group),
			D2 = lists:ukeymerge(1,lists:ukeysort(1,Data),lists:ukeysort(1,D1)),
			R2 = db_ecc:update_data(?DBName,?Table,"id=" ++ Id,data_to_db(D2,list_to_atom(Id))),
			R3 = 
			case R2 of
				{ok,_}->
					{ok,File};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end,
			{reply,R3,State};
		Else->
			{reply,Else,State}
	end;
handle_call({remove,File,Key,App}, _, State)->
	dbcs_base:set_app(App,true),
	Id = makeid(File),
	Ret = db_ecc:get_data(?DBName,?Table,"id="++Id),
	case Ret of
		{error,Resean}->
			{reply,{error,read_db_error},State};
		[]->
			{reply,{ok,notexistd},State};
		[Group|_]->
			% [Group|_] = Ret,
			D1 = db_to_data(Group),
			D2 = proplists:delete(Key,D1),
			R2 = 
			case D2 of
				[]->
					db_ecc:delete_data(?DBName,?Table,"id=" ++ Id);
				_->
					db_ecc:update_data(?DBName,?Table,"id=" ++ Id,data_to_db(D2,list_to_atom(Id)))
			end,			
			% R2 = db_ecc:update_data(?DBName,?Table,"id=" ++ Id,data_to_db(D2,list_to_atom(Id))),
			R3 = 
			case R2 of
				{ok,_}->
					{ok,deleted};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end,
			{reply,R3,State};
		Else->
			{reply,Else,State}
	end;
	
handle_call({new,File,Key,Val,App}, _, State)->
	dbcs_base:set_app(App,true),
	Id = makeid(File),
	F = fun()->
			case db_ecc:insert_data(?DBName,?Table,data_to_db([{Key,Val}],list_to_atom(Id))) of
				{ok,_}->
					{ok,File};
				{error,Err}->
					{error,Err};
				Err2->
					{error,Err2}
			end
		end,
	Ret = db_ecc:get_data(?DBName,?Table,"id="++Id),
	case Ret of
		{error,_}->
			R1 = F(),
			{reply,R1,State};
		[]->
			R1 = F(),
			{reply,R1,State};
		[_|_]->
			{reply,{error,key_already_exists},State};
		Else->
			{reply,Else,State}
	end;
	
handle_call({all,File,App}, _, State)->
	dbcs_base:set_app(App,true),
	% io:format("all preferences app:~p~n",[dbcs_base:get_app()]),
	Id = makeid(File),
	Ret = db_ecc:get_data(?DBName,?Table,"id="++Id),
	case Ret of
		{error,Resean}->
			{reply,{error,read_db_error},State};
		[]->
			{reply,{ok,[]},State};
		[Group|_]->
			[Group|_] = Ret,
			D1 = db_to_data(Group),
			{reply,{ok,D1},State};
		Else->
			{reply,Else,State}
	end;
	
handle_call(reload, _, State)->
	lists:map(fun(X)->
			dets:close(X),
			dets:open_file(X,[{file,?PREFS_DIR ++ atom_to_list(X)}])
		end,State#state.files),
	{reply,ok,State};
	
handle_call(files, _, State)->
	{reply,{ok,State#state.files},State};
	
handle_call({hash,File}, _, State)->
	case lists:member(File,State#state.files) of
		false ->
			{reply,{error,read_file_error},State};
		true ->
			{reply,{ok,file_hash(File)},State}
	end;
	


handle_call(Req, _, State) ->
    {reply, {error,unknown_request}, State}.


handle_cast({sync,Node},State)->
	case node() of
		Node ->
			{noreply, State};
		_->
			try
				file_sync(Node,master_config,State#state.files),
				{noreply, State}
				
			% case rpc:call(Node,preferences,files,[]) of
				% {ok,Files}->
					% Rf =
					% lists:foldl(fun(X,R)->
								% timer:sleep(1),
								% case rpc:call(Node,preferences,hash,[X]) of
									% {ok,H1}->
										% case file_hash(X) of
											% H1->
												% R;
											% _->
												% case file_sync(Node,X) of
													% ok->
														% R;
													% _->
														% R ++ [X]
												% end
										% end;
									% _->
										% R ++ [X]
								% end
							% end,[], Files),
					% case Rf of
						% []->
							% {noreply, State};
						% _->
							% io:format("preferences sync error:~p~n",[Rf]),
							% {noreply, State}
					% end;
				% Error->
					% {noreply, State}
			% end
			catch
				_:E1->{noreply, State}
			end
	end;
handle_cast(stop, S) ->
    [dets:close(X)||X<-S#state.files],
    {stop, normal, S};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

verify(_,_)->
	{ok,""}.
	
file_hash(File)->
	Ret = dets:match(File,{'$1','$2'}),
	erlang:phash2([list_to_tuple(X)||X<-Ret]).
	
file_sync(Node,File,Files)->
	case rpc:call(Node,preferences,all,[File]) of
		{ok,R}->
			try
				case lists:member(File,Files) of
						false ->
							case dets:open_file(File,[{file,?PREFS_DIR ++ atom_to_list(File)}]) of
								{ok,_}->
									dets:insert(File,R),
									dets:sync(File),
									dets:close(File),
									ok;
								_->
									error
							end;
						true ->
							dets:insert(File,R),
							dets:sync(File),
							ok
					end			
			catch
			E1:E2->
				io:format("file_sync error(~p:~p)~n",[E1,E2]),
				error
			end;
		_->
			error
	end.
	
test(_)->
	ok.

test()->
	start_link(),
	set('email_settings',mailServer,"mail.dragonflow.com"),
	set('email_settings',autoEmail,"xianfang.shi@dragonflow.com"),
	io:format("~p~n",[get('email_settings',mailServer)]),
	io:format("~p~n",[get('email_settings',autoEmail)]),
	stop(),
	ok.