%% ---
%% Log Manager Module
%%
%%---
-module(log_manager).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([start_link/0,start_link/1,stop/0,test/0]).

-export([log/5,log/6,registerLogger/2,unregisterLogger/1]).

-record(state, {logger=[]}).

-define(SERVER,'elecc_log_manager').

-define(HEALTH_MONITORS,[logevent_health_monitor]).

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
	logevent_health:start_link(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

init([Opts])->
	BufDuration = case proplists:get_value(buffer_duration,Opts) of
					undefined->
						0;
					Pv->
						Pv
				end,
	BufSize = case proplists:get_value(buffer_size,Opts) of
				undefined->
					0;
				Pv2->
					Pv2
				end,
	SiteviewLog = case proplists:get_value(siteview_log,Opts) of
				undefined->
					0;
				Pv3->
					Pv3
				end,
	MaxSize = case preferences:get(log,logTotalLimit) of
		{ok,[{_,V1}|_]}->
			case string:to_integer(V1) of
				{N1,[]}->
					N1;
				_->
					0
			end;
		_->
			0
	end,
	MaxDays = case preferences:get(log,logKeepDays) of
		{ok,[{_,V2}|_]}->
			case string:to_integer(V2) of
				{N2,[]}->
					N2;
				_->
					0
			end;
		_->
			0
	end,
	Logs = 
	case proplists:get_value(debug,Opts) of
		true ->
			ConLog = console_logger:new(),
			[
			{'RunMonitor',ConLog},
			{'Error',ConLog},
			{'Alert',ConLog},
			% {'URL',ConLog},
			% {'SiteViewLog',ConLog},
			{'AccountSiteViewLog',ConLog}
			];
		_->
			[]
	end ++
	case preferences:get(log,createDailyErrorLog) of
		{ok,[{_,true}|_]}->
			DlyLog = dialy_file_logger:new("error.log",MaxSize,MaxDays,BufDuration,BufSize),

			[{'Error',DlyLog}];
		_->
			FileLog = file_logger:new("error.log",MaxSize,MaxDays,BufDuration,BufSize),
			[{'Error',FileLog}]
	end ++
	case preferences:get(log,createDailyRunMonitorLog) of
		{ok,[{_,true}|_]}->
			MonitorLog = dialy_file_logger:new("error.log",MaxSize,MaxDays,BufDuration,BufSize),
			[{'RunMonitor',MonitorLog}];
		_->
			MonitorLog2 = file_logger:new("error.log",MaxSize,MaxDays,BufDuration,BufSize),
			[{'RunMonitor',MonitorLog2}]
	end ++
	if
		SiteviewLog ->
			SvLog = dialy_file_logger:new("svecc.log",MaxSize,MaxDays,BufDuration,BufSize),
			[{'SiteViewLog',SvLog}];
		true ->
			SvLog2 = file_logger:new("svecc.log",MaxSize,MaxDays,BufDuration,BufSize),
			[{'SiteViewLog',SvLog2}]
	end,
	{ok,#state{logger=Logs}}.

log(Type,Mod,Line,Format,Params)->
	log(Type,Mod,Line,Format,Params,sv_datetime:now2str(sv_datetime:now())).

log(Type,Mod,Line,Format,Params,Date)->
	gen_server:cast(?SERVER, {log,Type,Mod,Line,Format,Params,Date}).


registerLogger(Type,Obj)->
	gen_server:call(?SERVER, {registerLogger,Type,Obj}).

unregisterLogger(Type)->
	gen_server:call(?SERVER, {unregisterLogger,Type}).

stop() ->
    gen_server:cast(?SERVER, stop).


handle_call({registerLogger,Type,Obj}, _, State) ->
	case lists:keysearch(Type,1,State#state.logger) of
		{value,_}->
			{reply,{error,logger_exist},State};
		_->
			{reply,ok,State#state{logger=State#state.logger++[{Type,Obj}]}}
	end;

handle_call({unregisterLogger,Type}, _, State) ->
	case lists:keysearch(Type,1,State#state.logger) of
		{value,_}->
			NewLoggers = [{X,Y}||{X,Y}<-State#state.logger,X=/=Type],
			{reply,ok,State#state{logger=NewLoggers}};
		_->
			{reply,{error,logger_not_exist},State}
	end;

handle_call({log,Type,Mod,Line,Format,Params,Date}, _, State) ->
	
	LogOn = false,
	% case preferences:get(master_config,disable_health_log) of
		% {ok,[{_,true}|_]}->
			% if 
				% Mod == logevent_health_monitor ->
					% false;
				% true ->
					% true
			% end;
		% _->
			% true
	% end,

		if 
			Type=='Error' andalso LogOn->
				logevent_health:log(lists:flatten(io_lib:format(Format,Params)));
			true ->
				pass
		end,
		F = fun({X,Y})->
				case X of
					Type->
						Y:log(Mod,Line,Date,Format,Params);
					_->
						pass
				end
			end,
		lists:map(F,State#state.logger),
		{reply,ok,State};

		
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.


handle_cast({log,Type,Mod,Line,Format,Params,Date},State) ->
	
	LogOn = false,
	% case preferences:get(master_config,disable_health_log) of
		% {ok,[{_,true}|_]}->
			% if 
				% Mod == logevent_health_monitor ->
					% false;
				% true ->
					% true
			% end;
		% _->
			% fals
	% end,

		if 
			Type=='Error' andalso LogOn->
				logevent_health:log(lists:flatten(io_lib:format(Format,Params)));
			true ->
				pass
		end,
		F = fun({X,Y})->
				case X of
					Type->
						Y:log(Mod,Line,Date,Format,Params);
					_->
						pass
				end
			end,
		lists:map(F,State#state.logger),
		{noreply,State};
	
handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


test()->
	ok.