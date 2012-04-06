%% ---
%% monitor_logger_server
%%
%%---
-module(monitor_logger_serversvpg).
-behaviour(gen_server).

-compile(export_all).

-include("monitor.hrl").
-include("pgsql.hrl").

-export([start_link/0,stop/0,
		log/1,logdb/1,q/1,q/2,q/3,qc/3,qdstr/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {conn}).

-define(LOGDIR,"logs").
-define(LOGNAME,monitor_log).
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
	erlide_log:log("******************* monitor_logger_server OK*************************"),
	process_flag(trap_exit, true),
	case pgsql:connect(localhost, "root", "root", [{database,"postgres"}]) of
		{ok, C}->
			{ok, #state{conn = C}};
		_->
			{ok, #state{}}
	end.



%% @spec log(Terms)->{ok,Log} | {error,Reason}
%% where
%%	Terms = record()
%%	Log = string()
%% Reason = string()
%% @doc write monitor log file
log(Terms)->
    App = atom_to_list(dbcs_base:get_app()),
	call({log,Terms,App}).
    
%%Not yet distinguish between multiple applications
logdb({Id, Time, Terms})->
	call({logdb,{Id, Time, Terms}}).

%% @spec q(Date)->{ok,Log} | {error,Reason}
%% where
%%	Date = string()
%%	Log = {eof,[MonitorLogs]}
%% MonitorLogs = record()
%% Reason = string()
%% Reason = string()
%% @doc get monitor log by Date
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
%% @doc get monitor log by Date and monitor Id
q(Date,Id)->
    App = atom_to_list(dbcs_base:get_app()),
	call({q,Date,Id,App}).


%% @spec q(StartDate,EndDate,Ids)->{ok,Log} | {error,Reason}
%% where
%%	StartDate = EndDate = {Date, Time}
%% Date = {Y,M,D}
%% Time = {H,M,S}
%% Y = M = D = H = M = S = integer()
%% Ids = [Id]
%% Id = atom()
%%	Log = {eof,[MonitorLogs]}
%% MonitorLogs = record()
%% Reason = string()
%% @doc get monitor log by Date between start to end,and monitor ids
q(StartDate,EndDate,Ids)->
    App = atom_to_list(dbcs_base:get_app()),
	%%io:format("monitor_logger:q: ~p ~n", [App]),
	call({q,StartDate,EndDate,Ids,App}).
	

%% Take a time log
q(Id,StartDate,StartTime,EndDate,EndTime)->
	App = atom_to_list(dbcs_base:get_app()),
	call({q,Id,StartDate,StartTime,EndDate,EndTime,App}).
	
ql(StartDate,StartTime, EndDate,EndTime, Params)->
	 App = atom_to_list(dbcs_base:get_app()),
	 call({ql,StartDate,StartTime, EndDate,EndTime, Params, App}).
	
%%
%%
%%
qc(Ids, Count, DaysLimit)->
	App = atom_to_list(dbcs_base:get_app()),
	call({qc, Ids, Count, DaysLimit, App}).
%% 
%%=============================dstr =========================
%% 
qdstr(StartDate,EndDate,Ids) ->
 	 Temids=string:tokens(Ids, ","),
	 App = atom_to_list(dbcs_base:get_app()),
	%%io:format("monitor_logger:q: ~p ~n", [App]),
	{ok,Tmp}=call({q,StartDate,EndDate,Temids,App}),
	 Datalog=builddata(Tmp,[]),
	 getDstrList(Datalog, Temids, []).
builddata([],Data)->
	Data;
builddata([H|T],Data)->
	builddata(T,Data++element(2,H)).
%% @spec getDstrListChild(X) -> ok 
%% Obj = term()
%% @doc
getMonitorDstrList([], DstrList)->
	DstrList;
getMonitorDstrList([H|T], DstrList)->
	getMonitorDstrList(T, DstrList ++ [{H#monitorlog.time,{H#monitorlog.category, H#monitorlog.desc, measurementtoString(H#monitorlog.measurement, [])}}]).

%% @spec measurementtoString(X) -> ok 
%% Obj = term()
%% @doc
measurementtoString([], Str)->
	Str;
measurementtoString([H|T], Str)->
	{Key, Value} = H,
%% 	measurementtoString(T, Str ++ to_list(Key) ++ "=" ++ to_list(Value) ++ ",").
	measurementtoString(T, Str ++ [{Key, to_value(Value)}]).
  		
%% @spec getDstrList(X) -> ok 
%% Obj = term()
%% @doc
getDstrList(DstrList, [], MonitorDstrList)->
	MonitorDstrList;
getDstrList(DstrList, [H|T], MonitorDstrList)->
%% 	F=fun(X)->
%% 		case X#monitorlog.id of
%% 			H ->
%% 				true;
%% 			_  ->
%% 				false
%% 		end
%% 	end,
%% 	ChildSrcDstrList = lists:filter(F, DstrList),
    ChildSrcDstrList=[X||X<-DstrList,X#monitorlog.id=:=list_to_atom(H)],
	ChildDesDstrList = getMonitorDstrList(ChildSrcDstrList, []),
	getDstrList(DstrList, T, MonitorDstrList ++ [{"(dstr)" ++ H, ChildDesDstrList}]).
to_value(X) when is_float(X)->
  io_lib:format("~.2f", [X]);
to_value(X) when is_list(X)->
	try list_to_float(X) of
		V->V
	catch
		_:E ->
			try list_to_integer(X) of
	            V1->V1
            catch
	           _:Ex ->
				X
            end
	end;
to_value(X) ->
  X.

	
create_table(Cnn,Tbl)->
	Sql = lists:flatten(io_lib:format("CREATE TABLE \"~s\"
		   (id character varying(128) NOT NULL,
			app character varying(128) NOT NULL,
			name character varying(1024),
			\"time\" timestamp without time zone,
			category character varying(16),
			description text,
			measurement bytea
			);CREATE INDEX \"~s_index\" 
				ON \"~s\" USING hash (id);",[Tbl,Tbl,Tbl])),
	case pgsql:squery(Cnn,Sql) of
		{error,Err}->
			{error,Err#error.message};
		[{ok,_,_},{ok,_,_}]->
			{ok,create_table};
		Else->
			io:format("create_table:~p~n",[Else]),
			{error,Else}
	end.
	
encode_measurement(C)->
	term_to_binary(C).
	
decode_measurement(E)->
	binary_to_term(E).
	
convert_rowold([],Acc)->lists:reverse(Acc);
convert_rowold([R|T],Acc)->
	Id = list_to_atom(binary_to_list(element(1,R))),
	Name = binary_to_list(element(3,R)),
	Time = element(4,R),
	Category = list_to_atom(binary_to_list(element(5,R))),
	Desc = binary_to_list(element(6,R)),
	Measure = decode_measurement(element(7,R)),
	Log = #monitorlog{id=Id,name=Name,time=Time,category=Category,desc=Desc,measurement=Measure}, 
	convert_rowold(T,[Log|Acc]).

convert_row([],Acc)->lists:reverse(Acc);
convert_row([R|T],Acc)->
	Id = list_to_atom(binary_to_list(element(1,R))),
	Name = binary_to_list(element(3,R)),
%% 	Time = decode_time(binary_to_list(element(4,R))),
	
	Temp = element(2, element(4,R)),
	Seconds = round(element(3,Temp)),
	Time = {element(1, element(4, R)), setelement(3, Temp, Seconds)},
	
	Category = list_to_atom(binary_to_list(element(5,R))),
	Desc = binary_to_list(element(6,R)),
%% 	Measure = decode_measurement(element(7,R)),
%%	io:format("create_table:~p~n",[element(7,R)]),
%% 	Measure = binary_to_term(element(7,R)),
 	Measure = element(7,R),
	Log = #monitorlog{id=Id,name=Name,time=Time,category=Category,desc=Desc,measurement=Measure}, 
	convert_row(T,[Log|Acc]).
	
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
				{ok, C} -> 
					C;
				_->
					undefined
			end;
		_->
			State#state.conn
	end.
		
handle_call({log,Terms,App}, _, State) ->
%    jdbc_Logger:log(Terms),
	C = get_connect(State),
	{{Y,M,D},_} = Terms#monitorlog.time,
	TblName = lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D])),
	% Sql = lists:flatten(io_lib:format("insert into \"~s\" values('~s','~s','~s','~s','~s','~s','~s');",
					% [TblName,Terms#monitorlog.id,App,Terms#monitorlog.name,encode_time(Terms#monitorlog.time),Terms#monitorlog.category
					% ,Terms#monitorlog.desc,encode_measurement(Terms#monitorlog.measurement)])),
	Esql = lists:flatten(io_lib:format("insert into \"~s\" values($1,$2,$3,$4,$5,$6,$7);",
					[TblName])),
	Params = [Terms#monitorlog.id,App,Terms#monitorlog.name,Terms#monitorlog.time,Terms#monitorlog.category
					,Terms#monitorlog.desc,encode_measurement(Terms#monitorlog.measurement)],
	case pgsql:equery(State#state.conn,Esql,Params) of
		{error,Err} when Err#error.code == <<"42P01">>->
			case create_table(State#state.conn,TblName) of
				{ok,_}->
					Ret = pgsql:equery(State#state.conn,Esql,Params),
					{reply,Ret,State#state{conn=C}};
				Else->
					{reply,Else,State#state{conn=C}}
			end;
		Else->
			{reply,Else,State#state{conn=C}}
	end;


handle_call({q,Date,App}, _, State) ->
	C = get_connect(State),
	TblName = case Date of
				""->
					{Y,M,D} = date(),
					lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D]));
				_->
					lists:flatten(io_lib:format("siteview_~s",[Date]))
				end,
	Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s';",[TblName,App])),
	case pgsql:equery(State#state.conn,Sql,[]) of
		{ok,Columns,Rows}->
			{reply,{ok,convert_row(Rows,[])},State#state{conn=C}};
		Else->
			{reply,Else,State#state{conn=C}}
	end;

handle_call({q,Date,Id,App}, _, State) ->
	C = get_connect(State),
	TblName = case Date of
				""->
					{Y,M,D} = date(),
					lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D]));
				_->
					lists:flatten(io_lib:format("siteview_~s",[Date]))
				end,
	Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and id='~s';",[TblName,App,Id])),
	case pgsql:equery(C,Sql,[]) of
		{ok,Columns,Rows}->
			{reply,{ok,convert_row(Rows,[])},State#state{conn=C}};
		Else->
			{reply,Else,State#state{conn=C}}
	end;
handle_call({q, StartTime, EndTime, Ids, App}, _, State) ->
	io:format("--->q:~p,~p,~p,~p~n",[StartTime,EndTime,Ids,App]),
	C = get_connect(State),
	{StartDate, _} = StartTime,
	{EndDate, _} = EndTime,	
	Dates = nextPath([], StartDate, EndDate),
	IdsStr =  string:join(lists:map(fun(X)-> lists:flatten(io_lib:format("'~s'",[X])) end,Ids),","),
	F = fun(X,R)->
			{Y,M,D} = X,
			TblName = lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D])),
			Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and id in (~s) and time >= '~s' and time < '~s';",
								[TblName,App,IdsStr,encode_time(StartTime),encode_time(EndTime)])),
			case pgsql:equery(C,Sql,[]) of
				{ok,_,Rows}->
%					R ++ [{eof,convert_row(Rows)}];
					R ++ convert_row(Rows,[]);
				_->
					R
			end 
		end,
	
	Ret = lists:foldl(F,[],Dates),
%	io:format("--->q Result:~p,~p,~p,~p~n",[StartTime,EndTime,Ids,Ret]),
	{reply,{ok,Ret},State#state{conn=C}};
	
handle_call({ql,StartDate,StartTime, EndDate,EndTime, Params, App}, _, State) ->
	C = get_connect(State),
	Dates = get_dates(StartDate,EndDate),
	F = fun(X,R)->
			{Y,M,D} = X,
			TblName = lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D])),
			Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and time >= '~s' and time < '~s'",
								[TblName,App,encode_time({StartDate,StartTime}),encode_time({EndDate,EndTime})]))
			++
			case make_where(Params) of
				[]->
					";";
				Where->
					" and " ++ Where ++ ";"
					
			end,
			case pgsql:equery(C,Sql,[]) of
				{ok,_,Rows}->
					R ++ convert_row(Rows,[]);
				_->
					R
			end 
		end,
	
	Ret = lists:foldl(F,[],Dates),
	{reply,{ok,Ret},State#state{conn=C}};
	

handle_call({qc, Ids, Count, DaysLimit, App}, _, State) ->
	io:format("--->qc:~p,~p,~p,~p~n",[Ids,Count,DaysLimit,App]),
	C = get_connect(State),
	EndDate =  date(),
	StartDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(EndDate)-DaysLimit+1),
 	Dates = lists:reverse(nextPath([], StartDate, EndDate)),
	IdsStr =  string:join(lists:map(fun(X)-> "'" ++ X ++ "'" end,Ids),","),
	F = fun(X,R)->
			{Y,M,D} = X,
			Left = Count - length(R),
			if
				Left > 0 ->
					TblName = lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D])),
					Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and id in (~s) order by time desc limit ~w;",
										[TblName,App,IdsStr,Left])),
					case pgsql:equery(C,Sql,[]) of
						{ok,_,Rows}->
							lists:reverse(convert_row(Rows,[])) ++ R;
						_->
							R
					end;
				true->
					R
			end
		end,
	
	Ret = lists:foldl(F,[],Dates),
	{reply,{ok,Ret},State#state{conn=C}};


handle_call({q,Id,StartDate,StartTime,EndDate,EndTime,App}, _, State) ->
	C = get_connect(State),
	Dates = nextPath([], StartDate, EndDate),
	F = fun(X,R)->
			{Y,M,D} = X,
			TblName = lists:flatten(io_lib:format("siteview_~w-~w-~w",[Y,M,D])),
			Sql = lists:flatten(io_lib:format("select * from \"~s\" where app='~s' and id='~s' and time >= '~s' and time < '~s';",
								[TblName,App,Id,encode_time({StartDate,StartTime}),encode_time({EndDate,EndTime})])),
			case pgsql:equery(C,Sql,[]) of
				{ok,_,Rows}->
					R ++ convert_row(Rows,[]);
				_->
					R
			end 
		end,
	
	Ret = lists:foldl(F,[],Dates),
	{reply,{ok,Ret},State#state{conn=C}};
	
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.

handle_cast(stop, State) ->
	pgsql:close(State#state.conn),
	{stop,normal,State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.
	

make_where(Params)->
	F = fun(C,R)->
		case C of
			{id,'in',V11}->
				IdsStr =  string:join(lists:map(fun(X)-> "'" ++ X ++ "'" end,V11),","),
				R ++ ["id in (" ++ IdsStr ++ ")"];
			{id,'=',V1}->
				R ++ [lists:flatten(io_lib:format("id = '~s'",[V1]))];
			{id,'<',V2}->
				R ++ [lists:flatten(io_lib:format("id < '~s'",[V2]))];
			{id,'>',V3}->
				R ++ [lists:flatten(io_lib:format("id > '~s'",[V3]))];
			{name,'=',V4}->
				R ++ [lists:flatten(io_lib:format("name = '~s'",[V4]))];
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
			{category,'=',V10}->
				R ++ [lists:flatten(io_lib:format("category = '~s'",[V10]))];
			_->
				R
		end
	end,
	Wa = lists:foldl(F,[],Params),
	string:join(Wa," and ").
	
get_dates(StartDate,EndDate) when StartDate > EndDate ->
	[];
get_dates(StartDate,EndDate) when StartDate == EndDate ->
	[StartDate];
get_dates(StartDate,EndDate)->
	[StartDate]
	++ get_dates(sv_datetime:next_date(StartDate),EndDate).


compare(StartTime, EndTime) ->
	L = calendar:date_to_gregorian_days(StartTime),
	L1 = calendar:date_to_gregorian_days(EndTime),
	if(L > L1) ->
		true;
	true ->
		false
	end.

nextPath(Times, StartTime, EndTime) ->
	case compare(StartTime, EndTime) of 
		true -> 
			Times;
		false ->
			T2 = Times ++ [StartTime],
			nextPath(T2, sv_datetime:next_date(StartTime), EndTime)
	end.

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

