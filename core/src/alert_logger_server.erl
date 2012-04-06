%% ---
%%your comment
%%
%%---
-module(alert_logger_server).
-behaviour(gen_server).

-include("monitor.hrl").
-include("alert.hrl").
-include("pgsql.hrl").

-export([start_link/0,stop/0,
		log/1,q/1,q/2,q/3,q/5,call/1,query_data/6,query_data/8
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {conn}).

-define(LOGDIR,"logs").
-define(LOGNAME,alert_log).
-define(ONCE_MAX,1000).
-define(MAX_READ,20000).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).


init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{conn=ok}}.
%% 	case pgsql:connect(localhost, "root", "root", [{database,"postgres"}]) of
%% 		{ok, C}->
%% 			{ok, #state{conn = C}};
%% 		_->
%% 			{ok, #state{}}
%% 	end.

%% @spec log(Terms)->{ok,Log} | {error,Reason}
%% where
%%	Terms = record()
%%	Log = string()
%% Reason = string()
%% @doc write alert log file
log(Terms)->
    App = atom_to_list(dbcs_base:get_app()),
	call({log,Terms,App}).

%% @spec q(Date)->{ok,Log} | {error,Reason}
%% where
%%	Date = string()
%%	Log = {eof,[MonitorLogs]}
%% MonitorLogs = record()
%% Reason = string()
%% Reason = string()
%% @doc get alert log by Date
%%date format is:Y-M-D,example:2010-3-12,Note that the length of day and month are not necessarily two
q(Date)->
    App = atom_to_list(dbcs_base:get_app()),
	call({q,Date,App}).

%% @spec q(Date,Id)->{ok,Log} | {error,Reason}
%% where
%%	Date = string()
%% Id = atom()
%%	Log = {eof,[MonitorLogs]}
%% MonitorLogs = record()
%% Reason = string()
%% @doc get alert log by Date and monitor Id
q(Date,Id)->
    App = atom_to_list(dbcs_base:get_app()),
	call({q,Date,Id,App}).
    
%% @spec q(StartDate,EndDate,Type)->{ok,Log} | {error,Reason}
%% where
%%	StartDate = EndDate = {Y,M,D}
%% Y = M = D = integer()
%% Type = list()
%%	Log = [MonitorLogs]
%% MonitorLogs = record()
%% Reason = string()
%% @doc get alert log by period and type
q(StartDate,EndDate,Type)->
    App = atom_to_list(dbcs_base:get_app()),
	Params = 
	case Type of
		"all"->
			[];
		_->
			[{type,'=',Type}]
	end,
	
	query_data(App,StartDate,{0,0,0},EndDate,{23,59,59},Params).
	% call({q,StartDate,EndDate,Type,App}).
	
	
q(StartDate,StartTime, EndDate,EndTime, Params)->
	 App = atom_to_list(dbcs_base:get_app()),
	 query_data(App,StartDate,StartTime,EndDate,EndTime,Params).
	 % call({q,StartDate,StartTime, EndDate,EndTime, Params, App}).

%% get_connect2()->
%% 	call({get_connect2}).
%% %%groupid,responsetime,responder,responsecontent,cleartime,times	 
%% create_table(Cnn,Tbl)->
%% 	Sql = lists:flatten(io_lib:format("CREATE TABLE \"~s\"
%% 		   (id character varying(128) NOT NULL,
%% 			app character varying(128) NOT NULL,
%% 			type character varying(128),
%% 			name character varying(1024),
%% 			monitor character varying(128),
%% 			receiver character varying(64),
%% 			title character varying(1024),
%% 			\"time\" timestamp without time zone,
%% 			result character varying(32),			
%% 			content text,
%% 			alert_level character varying(32),
%% 			groupid character varying(128),
%% 			responsetime timestamp without time zone,
%% 			responder character varying(128),
%% 			responsecontent  text,
%% 			cleartime timestamp without time zone,
%% 			times integer
%% 			);CREATE INDEX \"~s_index\" 
%% 				ON \"~s\" USING hash (id);",[Tbl,Tbl,Tbl])),
%% 	case pgsql:squery(Cnn,Sql) of
%% 		{error,Err}->
%% 			{error,Err#error.message};
%% 		[{ok,_,_},{ok,_,_}]->
%% 			{ok,create_table};
%% 		Else->
%% 			io:format("create_table:~p~n",[Else]),
%% 			{error,Else}
%% 	end.
	
convert_row([])->[];
convert_row([R|T])->
	Id = list_to_atom(binary_to_list(element(1,R))),
	Type = binary_to_list(element(3,R)),
	Name = binary_to_list(element(4,R)),
	Monitor = list_to_atom(binary_to_list(element(5,R))),
	Receiver = binary_to_list(element(6,R)),
	Title = binary_to_list(element(7,R)),
	
	Temp = element(2, element(8,R)),
	Seconds = round(element(3,Temp)),
	Time = {element(1, element(8, R)), setelement(3, Temp, Seconds)},
	
	Result = binary_to_list(element(9,R)),
	Content = binary_to_list(element(10,R)),
	Alert_level = case element(11,R) of
			    null -> "notice";
			    [] -> "notice";
	                    Value -> binary_to_list(Value)			    
	              end, 
	Groupid = binary_to_list(element(12,R)),
	Responsetime = element(13,R),
	Responder = binary_to_list(element(14,R)),
	Responsecontent = binary_to_list(element(15,R)),
	Cleartime = element(16,R),
	Times = integer_to_list(element(17,R)),
	
	
	[#alertlog{id=Id,type=Type,name=Name,monitor=Monitor,receiver=Receiver,title=Title,time=Time,result=Result,alert_level=Alert_level,content=Content,groupid=Groupid,responsetime=Responsetime,responder=Responder,responsecontent=Responsecontent,cleartime=Cleartime,times=Times}] 
	++ convert_row(T).
convert_ofbizrow([],Acc)->lists:reverse(Acc);
convert_ofbizrow([R|T],Acc)->
	[Id,Type,Name,Monitor,Receiver,Title,Time,Result,Alert_level,Content,Groupid,Responsetime,Responder,Responsecontent,Cleartime,Times]=R,
	Log = #alertlog{id=Id,type=Type,name=Name,monitor=Monitor,receiver=Receiver,title=Title,time=Time,result=Result,alert_level=Alert_level,content=Content,groupid=Groupid,responsetime=Responsetime,responder=Responder,responsecontent=Responsecontent,cleartime=Cleartime,times=Times}, 
	convert_ofbizrow(T,[Log|Acc]).		
encode_time({{Y,M,D},{HH,MM,SS}})->
	lists:flatten(io_lib:format("~w-~w-~w ~w:~w:~w",[Y,M,D,HH,MM,SS])).


decode_time(TimeStr)->
	[Date,Time] = string:tokens(TimeStr," "),
	[Y,M,D] = string:tokens(Date,"-"),
	[HH,MM,SS] = string:tokens(Time,":"),
	{{list_to_integer(Y),list_to_integer(M),list_to_integer(D)},
	{list_to_integer(HH),list_to_integer(MM),list_to_integer(SS)}}.
	
get_connect(State)->
	case erlang:is_pid(State#state.conn) andalso erlang:is_process_alive(State#state.conn) of
		false->
			case pgsql:connect(localhost, "root", "root", [{database,"postgres"}]) of
				{ok, C}->
					C;
				_->
					undefined
			end;
		_->
			State#state.conn
	end.

to_str(Term) when is_integer(Term)->
	integer_to_list(Term);
to_str(Term) when is_float(Term)->
	float_to_list(Term);
to_str(Term) when is_atom(Term)->
	atom_to_list(Term);
to_str(Term)->
	Term.

handle_call({log,Terms,App}, _, State) ->
	io:format("alert_logger_server:~p~n", ["alertlog"]),
	Params = [Terms#alertlog.id,App,Terms#alertlog.type,Terms#alertlog.name,Terms#alertlog.monitor,Terms#alertlog.receiver,
					Terms#alertlog.title,Terms#alertlog.time,Terms#alertlog.result,to_str(Terms#alertlog.content),Terms#alertlog.alert_level,
					Terms#alertlog.groupid,Terms#alertlog.responsetime,Terms#alertlog.responder,Terms#alertlog.responsecontent,
					Terms#alertlog.cleartime,Terms#alertlog.times],
%% 	java_node:ofbizcall("alertLogger",[{listValue,Params}]);
	ofbiz:call("alertLogger", [{listValue,Params}]);
%% 	ofbiz:callservice("alertLogger",[{listValue,Params}]);
    
handle_call({q,Date,App}, _, State) ->
	io:format("alert_logger_server:~p~n", ["q1"]),
	Params=["",Date,Date,""],
	case ofbiz:callservice("alertloggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, q}, error};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, null};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},ok}
			end
	end;
	

handle_call({q,Date,Id,App}, _, State) ->
	io:format("alert_logger_server:~p~n", ["q2"]),
	Params=[Id,Date,Date,""],
	case ofbiz:callservice("alertloggerquery",[{listValue,Params}]) of
		{error,Err} ->
			 {reply, {error, q}, error};
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					{reply, {ok, []}, null};
				{_,R} ->
					Ret=convert_ofbizrow(R,[]),
					{reply,{ok,Ret},ok}
			end
	end;
	

%%get alert log by period
% handle_call({q, StartTime, EndTime, Type, App}, _, State) ->
	% Params = 
	% case Type of
		% "all"->
			% [];
		% _->
			% [{type,'=',Type}]
	% end,
	
	% query_data(App,StartTime,{0,0,0},EndTime,{23,59,59},Params,State);
	
% handle_call({q,StartDate,StartTime, EndDate,EndTime, Params, App}, _, State) ->
	% query_data(App,StartDate,StartTime,EndDate,EndTime,Params,State);
	
%% handle_call({get_connect2}, _, State) ->
%% 	case erlang:is_pid(State#state.conn) andalso erlang:is_process_alive(State#state.conn) of
%% 		false->
%% 			case pgsql:connect(localhost, "root", "root", [{database,"postgres"}]) of
%% 				{ok, C}->
%% 					{reply,C,State#state{conn=C}};
%% 				_->
%% 					{reply,error,State}
%% 			end;
%% 		_->
%% 			{reply,State#state.conn,State}
%% 	end;
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
	

query_data(App,StartDate,StartTime,EndDate,EndTime,Params)->
	io:format("alert_logger_server:~p~n", ["query_data1"]),
	Condition=["",{StartDate,StartTime},{EndDate,EndTime},Params],
	Ret=case java_node:ofbizcall("alertloggerquery",[{listValue,Condition}]) of
		{error,Err} ->
			 [];
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					[];
				{_,R} ->
					convert_ofbizrow(R,[])
			end
	    end,
	{eof,Ret}.
	
query_data(App,StartDate,StartTime,EndDate,EndTime,Params,From,Count)->
	Condition=[{StartDate,StartTime},{EndDate,EndTime},Params,From,Count,App],
	Ret=case java_node:ofbizcall("alertloggerquerylarge",[{listValue,Condition}]) of
		{error,Err} ->
			 [];
		{_,RRR} ->
			case lists:keyfind(loggervalues,1, RRR) of
				false->
					[];
				{_,R} ->
					convert_ofbizrow(R,[])
			end
	    end,
	{eof,Ret}.
%% 	io:format("alert_logger_server:~p~n", [Params]),
%% 	C = get_connect2(),
%% 	Dates = get_dates(StartDate,EndDate),
%% 	F = fun(X,R)->
%% 			{Y,M,D} = X,
%% 			TblName = lists:flatten(io_lib:format("alert_~w-~w-~w",[Y,M,D])),
%% 			Sql = lists:flatten(io_lib:format("select count(*) from \"~s\" where app='~s' and time >= '~s' and time < '~s'",
%% 								[TblName,App,encode_time({StartDate,StartTime}),encode_time({EndDate,EndTime})]))
%% 			++
%% 			case make_where(Params) of
%% 				[]->
%% 					";";
%% 				Where->
%% 					" and " ++ Where ++ ";"
%% 					
%% 			end,
%% 			% io:format("sql:~p~n",[Sql]),
%% 			case pgsql:equery(C,Sql) of
%% 				{ok,_,[{CCount}|_]}->
%% 					R ++ [CCount];
%% 				_->
%% 					R ++ [0]
%% 			end 
%% 		end,
%% 	TabCount = lists:foldl(F,[],Dates),
%% 	Total = lists:sum(TabCount),
%% 	% io:format("Table Count:~p~n",[TabCount]),
%% 	
%% 	{Offset,SelDates,_,_,_} = lists:foldl(fun(X,{O,Sd,Start,Left,Index})->
%% 									if
%% 										O == -1 andalso Start > X ->
%% 											{-1,Sd,Start-X,Left,Index+1};
%% 										O == -1 andalso Start =< X andalso Left > 0 andalso X>0->
%% 											{Start,Sd ++ [lists:nth(Index,Dates)],0,Left-(X-Start+1),Index+1};
%% 										O =/= -1 andalso Left > 0 andalso X>0->
%% 											{O, Sd ++ [lists:nth(Index,Dates)], 0, Left-X,Index+1};
%% 										true->
%% 											{O,Sd,Start,Left,Index+1}
%% 									end end,{-1,[],From,Count,1},TabCount),
%% 									
%% 	case Offset of
%% 		-1->
%% 			{eof,[]};
%% 		_->
%% 			F2 = fun(X)->
%% 					{Y,M,D} = X,
%% 					TblName = lists:flatten(io_lib:format("alert_~w-~w-~w",[Y,M,D])),
%% 					lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and time >= '~s' and time < '~s'",
%% 										[TblName,App,encode_time({StartDate,StartTime}),encode_time({EndDate,EndTime})]))
%% 					++
%% 					case make_where(Params) of
%% 						[]->
%% 							"";
%% 						Where->
%% 							" and " ++ Where
%% 							
%% 					end
%% 					
%% 				end,
%% 			UnionSql = lists:flatten(io_lib:format("select * from (" ++  string:join(lists:map(F2,SelDates)," union ") 
%% 									++ ") as Temp order by time limit ~w offset ~w;",[Count,Offset-1])),
%% 			% io:format("unionsql:~p~n",[UnionSql]),
%% 			case pgsql:equery(C,UnionSql,[]) of
%% 				{ok,_,Rows}->
%% 					LeftCount = Total-From-Count+1,
%% 					Ret = if
%% 							LeftCount > 0 -> LeftCount;
%% 							true-> eof
%% 						end,
%% 					{Ret,convert_row(Rows)};
%% 				{error,Err}->
%% 					{error,Err};
%% 				Else->
%% 					{error,Else}
%% 			end
%% 	end.
	
	
	
get_dates(StartDate,EndDate) when StartDate > EndDate ->
	[];
get_dates(StartDate,EndDate) when StartDate == EndDate ->
	[StartDate];
get_dates(StartDate,EndDate)->
	[StartDate]
	++ get_dates(sv_datetime:next_date(StartDate),EndDate).
	
make_where(Params)->
	F = fun(C,R)->
		case C of
			{id,'=',V1}->
				R ++ [lists:flatten(io_lib:format("id = '~s'",[V1]))];
			{id,'<',V2}->
				R ++ [lists:flatten(io_lib:format("id < '~s'",[V2]))];
			{id,'>',V3}->
				R ++ [lists:flatten(io_lib:format("id > '~s'",[V3]))];
			{title,'=',V4}->
				R ++ [lists:flatten(io_lib:format("title = '~s'",[V4]))];
			{time,'>',V5}->
				R ++ [lists:flatten(io_lib:format("time > '~s'",[encode_time(V5)]))];
			{time,'>=',V6}->
				R ++ [lists:flatten(io_lib:format("time >= '~s'",[encode_time(V6)]))];
			{time,'=',V7}->
				R ++ [lists:flatten(io_lib:format("time = '~s'",[encode_time(V7)]))];
			{time,'<',V8}->
				R ++ [lists:flatten(io_lib:format("time < '~s'",[encode_time(V8)]))];
			{time,'<=',V9}->
				R ++ [lists:flatten(io_lib:format("time <= '~s'",[encode_time(V9)]))];
			{type,'=',V10}->
				R ++ [lists:flatten(io_lib:format("type = '~s'",[V10]))];
			{name,'=',V11}->
				R ++ [lists:flatten(io_lib:format("name = '~s'",[V11]))];
			{receiver,'=',V12}->
				R ++ [lists:flatten(io_lib:format("receiver = '~s'",[V12]))];
			{'alert_level','=',V13}->
				R ++ [lists:flatten(io_lib:format("alert_level = '~s'",[V13]))];
			{'alert_level','in',V14}->
				R ++ [lists:flatten(io_lib:format("alert_level in ~s",[V14]))];	
			{'groupid','=',V15}->
				R ++ [lists:flatten(io_lib:format("groupid = '~s'",[V15]))];	
			{responsetime,'>',V5}->
				R ++ [lists:flatten(io_lib:format("responsetime > '~s'",[encode_time(V5)]))];
			{responsetime,'>=',V6}->
				R ++ [lists:flatten(io_lib:format("responsetime >= '~s'",[encode_time(V6)]))];
			{responsetime,'=',V7}->
				R ++ [lists:flatten(io_lib:format("responsetime = '~s'",[encode_time(V7)]))];
			{responsetime,'<',V8}->
				R ++ [lists:flatten(io_lib:format("responsetime < '~s'",[encode_time(V8)]))];
			{responsetime,'<=',V9}->
				R ++ [lists:flatten(io_lib:format("responsetime <= '~s'",[encode_time(V9)]))];	
			{responder,'=',V12}->
				R ++ [lists:flatten(io_lib:format("responder = '~s'",[V12]))];	
			{responsecontent,'=',V12}->
				R ++ [lists:flatten(io_lib:format("responsecontent = '~s'",[V12]))];	
			{cleartime,'>',V5}->
				R ++ [lists:flatten(io_lib:format("cleartime > '~s'",[encode_time(V5)]))];
			{cleartime,'>=',V6}->
				R ++ [lists:flatten(io_lib:format("cleartime >= '~s'",[encode_time(V6)]))];
			{cleartime,'=',V7}->
				R ++ [lists:flatten(io_lib:format("cleartime = '~s'",[encode_time(V7)]))];
			{cleartime,'<',V8}->
				R ++ [lists:flatten(io_lib:format("cleartime < '~s'",[encode_time(V8)]))];
			{cleartime,'<=',V9}->
				R ++ [lists:flatten(io_lib:format("cleartime <= '~s'",[encode_time(V9)]))];
			{times,'=',V12}->
				R ++ [lists:flatten(io_lib:format("times = '~s'",[V12]))];	
				
			_->
				R
		end
	end,
	Wa = lists:foldl(F,[],Params),
	string:join(Wa," and ").



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


call(Req) ->
    gen_server:call(?MODULE, Req, infinity).

cast(Msg) ->
    gen_server:cast(?MODULE, Msg).