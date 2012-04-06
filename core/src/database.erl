%% Author: Administrator
%% Created: 2010-1-25
%% Description: TODO: Add description to database_alert
-module(database,[BASE,Monitor,Rule]).
-extends(action).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("alert.hrl").

-define(PRODUCT_NAME,"elecc").

-define(LOG_TYPE,"database alert run").
-define(TIMEOUT,5).
-define(SQL, "INSERT INTO SiteViewAlert(time,N.groupname,N.name,N.state) VALUES('<time>', '<group>', '<name>', '<state>')").

new(Monitor,Rule)->
	Obj = action:new(),
	Obj:set_monitor(Monitor),
	Obj:set_rule(Rule),
	Obj:set_attribute(runType,3),
	{?MODULE,Obj,Monitor,Rule}.


execute()->
	{ok,{_,Params}} = Rule:get_property(action_param), 
	Database= Params#database_alert.url,
	Statement= Params#database_alert.sql,
	Username=Params#database_alert.dbuser,
	Password =Params#database_alert.dbpasswd,
	Driver =Params#database_alert.dbdriver,
	DatabaseBackup =Params#database_alert.dbBackup,
	{ok,{_,Enabled}} = Rule:get_property(enabled),
	case Rule:get_property(disabled) of
		{ok,{_,true}}->
			{error,"disabled"};
		_->
			case THIS:check_enabled(Enabled) of
				false ->
					{error,"time_disabled"};
				_->
					R=alertSend(Database,Statement,Username,Password,Driver)
			end
	end.

recover(This)->
    {ok,{_,Params}} = Rule:get_property(action_param), 
	Database= Params#database_alert.url,
	Statement= ?SQL,
	Username=Params#database_alert.dbuser,
	Password =Params#database_alert.dbpasswd,
	Driver =Params#database_alert.dbdriver,
    alertSend(Database,Statement,Username,Password,Driver).

check_schedule(Shedule)->
	Day = calendar:day_of_the_week(date()),
	case lists:keysearch(Day,1,Shedule) of
		{value,{_,Flag,St,Et}}-> % "12:12","24:24"
			[HHS,MMS|_] = case string:tokens(St,":") of
								[]->
									["0","0"];
								[T1]->
									[T1,"0"];
								[T2,T3|_]->
									[T2,T3]
							end,
			[HHE,MME|_] = case string:tokens(Et,":") of
								[]->
									["24","60"];
								[T4]->
									[T4,"60"];
								[T5,T6|_]->
									[T5,T6]
							end,
			Sth = case string:to_integer(HHS) of
					{error,_}->
						0;
					{V1,_}->
						V1
				end,
			Stm = case string:to_integer(MMS) of
					{error,_}->
						0;
					{V2,_}->
						V2
				end,
			Seh = case string:to_integer(HHE) of
					{error,_}->
						24;
					{V3,_}->
						V3
				end,
			Sem = case string:to_integer(MME) of
					{error,_}->
						60;
					{V4,_}->
						V4
				end,
			Sts = sv_datetime:time({Sth,Stm,0}),
			Ste = sv_datetime:time({Seh,Sem,0}),
			Now = sv_datetime:time(time()),
			%io:format("from:~p to ~p~n",[Sth,Seh]),
			case Flag of
				"enable" ->
					if
						Now >= Sts andalso Now =< Ste ->
							true;
						true ->
							false
					end;
				_->
					if
						Now < Sts orelse Now > Ste ->
							true;
						true ->
							false
					end
			end;
		_->
			true
	end.

verify(Params)->
	{ok,""}.
to_list(X) when is_atom(X)->
	atom_to_list(X);
to_list(X) when is_float(X)->
	Ret=lists:flatten(io_lib:format("~.2f", [X])),
	hd(Ret);
to_list(X) when is_integer(X)->	
    integer_to_list(X);
to_list(X) ->
  X.

replacefieldtovalue([],Ret)->
	Ret;
replacefieldtovalue([H|E],Ret)->
	S1=H,
	Index= string:rstr(S1, "<"),
	case Index of
		0->
			TRet=Ret++S1;
		_ ->
			case S1 of
				"<time>" ->
					Value=case Monitor:get_attribute(?LAST_UPDATE) of
							{ok,{_,LAST_UPDATE}} ->
								Time=sv_datetime:getDateTime(LAST_UPDATE),
								"'"++sv_datetime:tostr(Time)++"'";
							  _ -> "''"
						  end;
				"<group>" ->
					Pname=Monitor:get_parent_full_name(),
					AA=iconv:convert("utf-8","gbk", Pname),
					Value="'"++AA++"'";
				"<state>" ->
					Value=case Monitor:get_attribute(?CATEGORY) of
							{ok,{_,Category}} ->
								"'"++atom_to_list(Category)++"'";
							  _ -> "''"
						  end;
				"<name>" ->
					Value=case Monitor:get_property(?NAME) of
							{ok,{_,Name}} ->
								AA=iconv:convert("utf-8","gbk", Name),
								"'"++AA++"'";
							  _ -> "''"
						  end;
				 _ ->
					 Eindex=string:rstr(S1, ">"),
					 Field=string:sub_string(S1,2,Eindex-1),
				   	 Value= case Monitor:get_property(Field) of
						 {ok,{_,F}}->
							 "'"++to_list(F)++"'";
						 _ ->"''"
					 end
%% 					{ok,{_,Value}}=Monitor:get_property(Field)
			end,
			TRet=Ret++Value
	end,
	replacefieldtovalue(E,TRet).
			
getfieldvalue(Statement)->
	L=string:tokens(Statement, "'"),
	replacefieldtovalue(L,"").
alertSend(Database,Statement,Username,Password,Driver)->
	application:start(odbc),
	Msg = ?PRODUCT_NAME ++ " database_alert, " ++ 
	case Monitor:get_attribute(?CATEGORY) of 
		{ok,{_,Category}} -> 
			atom_to_list(Category);
		_->
			""
	end ++ "," ++
	case Monitor:get_property(?NAME) of
		{ok,{_,Name}}->
			Name;
		_->
			""
	end ++ "," ++
	case Monitor:get_attribute(?STATE_STRING) of
		{ok,{_,State}}->
			State;
		_->
			""
	end,
	io:format("DataBase alertSend;~p~n",[Msg]),
    case odbc:connect("DSN="++Database++";UID="++Username++";PWD="++Password, []) of
		{error,Error} ->
			THIS:logAlert(?LOG_TYPE,"Database connection ",Error,"fail"),
			Ret={error,Error,"Database connection"};
		{ok,Ref} ->
			 SQL=getfieldvalue(Statement),
			 case odbc:sql_query(Ref,SQL) of
				 {error,Err} ->
					 io:format("SQL:~p ~n", [SQL]),
					 THIS:logAlert(?LOG_TYPE,"update table",Err,"fail"),
					 Ret={error,Err,"update table"};
				 {_,_} ->
					 io:format("SQL:~p ~n", [SQL]),
					 THIS:logAlert(?LOG_TYPE,"update table",Msg,"ok"),
					 Ret= {ok,Msg,"update table"}
			 end,
			 case odbc:disconnect(Ref) of
				 {error,E1}->
					 THIS:logAlert(?LOG_TYPE,"Database disconnect",E1,"fail");
				 _->
					 ok
			 end
	end,
	application:stop(odbc),
	Ret.

