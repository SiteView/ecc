%% ---
%%scheduler
%%
%%---
-module(scheduler,[Name]).
-compile(export_all).
-include("monitor.hrl").

% -define(EVENT_THREAD_COUNT,100).
-define(EVENT_THREAD_COUNT,server_conf:get_sh_process()).
-define(MAX_PROGRESS,20).
-define(CUR_PRE_RUN,cur_pre_run).
-define(CUR_RUN_COUNT,cur_run_count).
-define(CUR_COUNT_TIME,cur_count_time).
-define(MAX_PRE_RUN,max_pre_run).
-define(MAX_PRE_TIME,max_pre_time).
-define(MAX_RUNNING_COUNT,max_running_count).
-define(MAX_RUNNING_TIME,max_running_time).

-define(TIMEOUT,5000).

-define(EVENT_TIMEOUT, 180000).


-record(running,{start,done,result,event}).

startScheduler()->
	case whereis(Name) of
		undefined->
			SId = spawn(fun()-> THIS:loop(Name,[]) end),
			register(Name,SId),
			spawn(fun()->process_flag(trap_exit,true),
						link(SId),THIS:stat_loop() end),
			spawn(fun()->process_flag(trap_exit,true),
						link(SId),THIS:event_loop(Name,0,?EVENT_THREAD_COUNT,[]) end);
		_->
			{error,{Name,scheduler_already_started}}
	end.


stopScheduler()->
	Name ! {self(),stop},
	receive
		{Name,Ret}->
			{ok,Ret};
		Else->
			{error,Else}
	after ?TIMEOUT->
		{error,timeout}
	end.

suspendScheduler()->
	Name ! {self(),suspend},
	receive
		{Name,Ret}->
			{ok,Ret};
		Else->
			{error,Else}
	after ?TIMEOUT->
		{error,timeout}
	end.


resumeScheduler()->
	Name ! {self(),resume},
	receive
		{Name,Ret}->
			Ret;
		Else->
			{error,Else}
	after ?TIMEOUT->
		{error,timeout}
	end.
	
	
get_stat()->
	Name ! {self(),get_stat},
	receive
		{Name,get_stat,Ret}->
			{ok,Ret};
		Else->
			{error,Else}
	after ?TIMEOUT->
		{error,timeout}
	end.
	
get_running()->
	Name ! {self(),get_running},
	receive
		{Name,get_running,Ret}->
			{ok,Ret};
		Else->
			{error,Else}
	after ?TIMEOUT->
		{error,timeout}
	end.
	
get_recent()->
	Name ! {self(),get_recent},
	receive
		{Name,get_recent,Ret}->
			{ok,Ret};
		Else->
			{error,Else}
	after ?TIMEOUT->
		{error,timeout}
	end.
	
event_count()->
	case get(?CUR_RUN_COUNT) of
		undefined->
			put(?CUR_RUN_COUNT,1);
		CC->
			put(?CUR_RUN_COUNT,CC+1)
	end.

running_stat()->
	Now = sv_datetime:now(),
	put(?CUR_COUNT_TIME,Now),
	CRC = case get(?CUR_RUN_COUNT) of
				undefined->
					put(?CUR_RUN_COUNT,0),
					0;
				_->
					get(?CUR_RUN_COUNT)
			end,
	MPR = case get(?MAX_PRE_RUN) of 
					undefined->
						put(?MAX_PRE_RUN,0),
						put(?MAX_PRE_TIME,0),
						0;
					_-> 
						get(?MAX_PRE_RUN) 
				end,
	put(?CUR_PRE_RUN,CRC),
	put(?CUR_COUNT_TIME,Now),
	put(?CUR_RUN_COUNT,0),
	if
		CRC>MPR->
			put(?MAX_PRE_RUN,CRC),
			put(?MAX_PRE_TIME,Now);
		true->
			pass
	end.

running_count()->
	MaxRun = get(?MAX_RUNNING_COUNT),
	CurRun = case  get(running) of
				undefined->
					0;
				_->
					length(get(running))
			end,
	case MaxRun of
		undefined->
			put(?MAX_RUNNING_COUNT,CurRun),
			put(?MAX_RUNNING_TIME,sv_datetime:now());
		_->
			if
				CurRun > MaxRun->
					put(?MAX_RUNNING_COUNT,CurRun),
					put(?MAX_RUNNING_TIME,sv_datetime:now());
				true ->
					pass
			end
	end.
	
	
on_main_exit(Pid)->
	spawn(fun()->process_flag(trap_exit,true),
				link(Pid),
				receive
					{'EXIT',Pid,normal}->
						ok;
					{'EXIT',Pid,Why}->
						io:format("Main Pid:~p,Exit:~p~n",[Pid,Why])
				end
		end).
		
on_event_exit(Pid,E,M)->
	spawn(fun()->process_flag(trap_exit,true),
				link(Pid),
				receive
					{'EXIT',Pid,Why}->
						M ! {Pid,event_done,E,Why},
						% io:format("Event Pid:~p,Exit:~p~n",[Pid,Why]),
						ok
				end
		end).
		
		
stat_loop()->
	Name ! {self(),running_stat},
	receive
		{'EXIT',_,_}->
			ok
	after 60000->
		stat_loop()
	end.
		

event_loop(Name,C,Max,Plist) when C == Max->
	check_event_timeout(Plist),
	receive
		{'EXIT',_,_}->
			ok;
		{Pid,event_done,E,Why}->
			Name ! {self(),event_done,E,Why},
			event_loop(Name,C-1,Max,proplists:delete(Pid,Plist))
	after 10000->
		event_loop(Name,C-1,Max,Plist)
	end;
event_loop(Name,C,Max,Plist)->
	check_event_timeout(Plist),
	receive
		{'EXIT',_,_}->
			ok;
		{Pid,event_done,E,Why}->
			Name ! {self(),event_done,E,Why},
			event_loop(Name,C-1,Max,proplists:delete(Pid,Plist))
	after 1->
		Rq = erlang:statistics(run_queue),
		% Lp = erlang:system_info(logical_processors),
		if
			Rq>0->
				event_loop(Name,C,Max,Plist);
			true->
				Name ! {self(),get_event},
				receive
					{Name,get_event,[]}->
						event_loop(Name,C,Max,Plist);
					{Name,get_event,E}->
						T1 = E:get_time(),
						T2 = sv_datetime:now(),
						if
							T1 == 0 ->
								Name ! {self(),idle_event,E},
								event_loop(Name,C, Max ,Plist);
							T1 > T2 ->
								Name ! {self(),idle_event,E},
								event_loop(Name,C, Max,Plist);
							true ->
								Name ! {self(),run_event,E},
								EPid = spawn(fun()->THIS:do_action(E) end),
								on_event_exit(EPid,E,self()),
								event_loop(Name,C+1, Max,Plist ++ [{EPid,sv_datetime:now()}])
						end
				end
		end
	end.
	
	
check_event_timeout([])->ok;
check_event_timeout([{Pid,Tm}|T])->
	Now = sv_datetime:now(),
	if
		Now - Tm > ?EVENT_TIMEOUT ->
			erlang:exit(Pid,timeout);
		true->
			pass
	end,
	check_event_timeout(T).



%% is_suspend()->(true | false | {error,timeout})
%%
is_suspend()->
	Name ! {self(),is_suspend},
	receive
		{Name,Ret}->
			Ret;
		_->
			false
	after ?TIMEOUT->
		{error,timeout}
	end.

handle_msg(Name,Data,Msg)->
	case Msg of
		{From,stop}->
			From ! {Name,stopped};
		{From,{schedule,Event}}->
			From ! {Name,schedule_ok},
			THIS:do_loop(Name,Data ++ [Event]);
		{From,{schedulereport,Event}}->
			From ! {Name,schedulereport_ok},
			THIS:do_loop(Name,Data ++ [Event]);
		{From,{unschedule,Action}}->
			F1 = fun(X,R)->
				E = X#running.event,
				A = E:get_action(),
				if
					A =:= Action ->
						R ++ [E];
					true->
						R
				end
			end,
			Running = case get(running) of undefined->[];V2->V2 end,
			NewData = 
			case lists:foldl(F1,[],Running) of
				[]->
					From ! {Name,{ok,unschedule_ok}},
					[X||X<-Data,X:get_action() =/= Action];
				_->
					From ! {Name,{error,is_running}},
					Data
			end,
			THIS:do_loop(Name,NewData);
		{From,suspend}->
			put(suspend,true),
			From ! {Name,{ok,suspend}},
			THIS:do_loop(Name,Data);
		{From,resume}->
			put(suspend,false),
			From ! {Name,{ok,resume}},
			THIS:do_loop(Name,Data);
		{From,is_suspend}->
			case get(suspend) of
				true->		
					From ! {Name,true};
				_->
					From ! {Name,false}
			end,
			THIS:do_loop(Name,Data);
		{From,get_event}->
			case get(suspend) of
				true->
					From ! {Name,get_event,[]},
					THIS:do_loop(Name,Data);
				_->
					case Data of
						[E|T]->
							From ! {Name,get_event,E},
							THIS:do_loop(Name,T);
						_->
							From ! {Name,get_event,[]},
							THIS:do_loop(Name,Data)
					end
			end;
		{_,run_event,E}->
			case get(running) of
				undefined->
					put(running,[#running{start=sv_datetime:now(),event=E}]);
				RE->
					put(running,RE ++ [#running{start=sv_datetime:now(),event=E}])
			end,
			THIS:running_count(),
			THIS:do_loop(Name,Data);
		{_,idle_event,E}->
			THIS:do_loop(Name,Data ++ [E]);
		{_,event_done,E,Why}->
			RDL = 
			case get(running) of
				undefined->
					io:format("********schedule error~n"),
					undefined;
				RE->
					{NRE,DD} = lists:partition(fun(X)->E=/=X#running.event end,RE),
					put(running,NRE),
					DD
			end,
			case RDL of
				undefined->
					pass;
				[]->
					pass;
				[RD|_]->
					case get(progress) of
						undefined->
							put(progress,[RD#running{done=sv_datetime:now(),result=Why}]);
						PE->
							if
								length(PE) < ?MAX_PROGRESS ->
									put(progress,[RD#running{done=sv_datetime:now(),result=Why}]++PE);
								true ->
									put(progress,[RD#running{done=sv_datetime:now(),result=Why}]++lists:sublist(PE,?MAX_PROGRESS-1) )
							end
					end
			end,
			THIS:event_count(),
			THIS:running_count(),
			case Why of
				timeout->
					Act = E:get_action(),
					Act:on_timeout();
				_->
					pass
			end,
			case E:is_repeated() of 
				true->
					N = E:calculateNextTime(sv_datetime:now()),
					THIS:do_loop(Name,Data ++ [N]);
				_->
					THIS:do_loop(Name,Data)
			end;
		{From,get_stat}->
			Running = case get(running) of undefined->[];V->V end,
			Crc = case get(?CUR_RUN_COUNT) of undefined->0;V2->V2 end,
			Mrc = case get(?MAX_RUNNING_COUNT) of undefined->0; V1->V1 end,
			Mrt = case get(?MAX_RUNNING_TIME) of undefined->0;V3->V3 end,
			Mpr = case get(?MAX_PRE_RUN) of undefined->0;V4->V4 end,
			Mpt = case get(?MAX_PRE_TIME) of undefined->0;V5->V5 end,
			
			From ! {Name,get_stat,{length(Running),Crc,Mrc,Mrt,Mpr,Mpt}},
			
			THIS:do_loop(Name,Data);
		{From,get_running}->
			% io:format("get running:~p~n",[get(running)]),
			Recent = 
			case get(running) of
				undefined->
					[];
				RE->
					process_running(RE)
			end,
			From ! {Name,get_running,Recent},
			THIS:do_loop(Name,Data);
		{From,get_recent}->
			% Recent = 
			% case get(progress) of
			%	undefined->
			%		[];
			%	RE->
			%		process_recent(RE)
			% end,
			% From ! {Name,get_recent,Recent},
			 From ! {Name,get_recent,[]},
			THIS:do_loop(Name,Data);
		{_,running_stat}->
			THIS:running_stat(),
			THIS:do_loop(Name,Data);
		{From,Msg}->
			From ! {Name,unkown_message},
			try
			?ERROR_LOG2("SCHEDULER:~p from ~p~n",[Msg,From])
			catch
			_:Err-> io:format("SCHEDULER LOG ERROR:~p~n",[Err])
			end,
			THIS:do_loop(Name,Data);
		Else->
			try
			?ERROR_LOG2("SCHEDULER:~p~n",[Else])
			catch
			_:Err-> io:format("SCHEDULER LOG ERROR:~p~n",[Err])
			end,
			THIS:do_loop(Name,Data)
	end.


do_loop(Name,Data)->
	case get(suspend) of
		true->
			THIS:suspend_loop(Name,Data);
		_->
			THIS:loop(Name,Data)
	end.


suspend_loop(Name,Data)->
	receive
		All->
			THIS:handle_msg(Name,Data,All)
	end.
loop(Name,Data)->
	receive
		All->
			THIS:handle_msg(Name,Data,All)
	end.

process_recent([])->
	[];
process_recent([R|T])->
	E = R#running.event,
	Act = E:get_action(),
	case Act:get_monitor() of
		{error,_}->
			process_recent(T);
		M->
			case M:get_property(id) of 
				{ok,{_,Mid}}->
					[{sv_datetime:now2str(R#running.done),{M:get_app(),Mid}}] ++process_recent(T);
				_->
					process_recent(T)
			end
	end.
	
	
process_running([])->
	[];
process_running([R|T])->
	E = R#running.event,
	Act = E:get_action(),
	case Act:get_monitor() of
		{error,_}->
			process_running(T);
		M->
			case M:get_property(id) of 
				{ok,{_,Mid}}->
					[{sv_datetime:now2str(R#running.start),{M:get_app(),Mid}}] ++process_running(T);
				_->
					process_running(T)
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%report scheduler  start%%%%%%%%%%%%%%%%%
%% do_reportloop(Name,Data)->
%% 	case get(suspend) of
%% 		true->
%% 			THIS:suspend_loop(Name,Data);
%% 		_->
%% 			THIS:reportloop(Name,Data)
%% 	end.
%% reportloop(Name,Data)->
%% 	receive
%% 		All->
%% 			THIS:handle_msg(Name,Data,All)
%% 	after 5->
%% 		Que=1,%%erlang:statistics(run_queue),
%% 		if
%% 			Que > 5 ->
%% 				erlang:yield(),
%% 				THIS:reportloop(Name,Data);
%% 			true ->
%% 				case length(Data) of
%% 					0->
%% 						THIS:reportloop(Name,Data);
%% 					_->
%% 						[E|_] = Data,
%% 						T1 = E:get_time(),
%% 						T2 = sv_datetime:now(),
%% 						THIS:if_reportloop(Name,Data,T1,T2)
%% 				end
%% 		end
%% 	end.
%% if_reportloop(Name,[E|T],0,_) ->
%% 	THIS:reportloop(Name,T++[E]);
%% if_reportloop(Name,[E|T],T1,T2) when T1>T2;T1=:=T2 ->
%% 	THIS:reportloop(Name,T++[E]);
%% if_reportloop(Name,[E|T],_,_)->
%% 	%%io:format("if_loop ~p,@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@~n",[self()]),
%% 	Action= E:get_action(),
%% 	THIS:doreportaction(Action),
%% 	case E:is_repeated() of 
%% 		true->
%% 			N = E:calculateNextTime(sv_datetime:now()),
%% 			THIS:reportloop(Name,T++[N]);
%% 		_->
%% 			THIS:reportloop(Name,T)
%% 	end.
%% doreportaction(Action)->
%% 	Action.
%% 
%% %%
%% %%
%% %%
%% schedulereport(Event) ->
%% 	 Name!{self(),{schedulereport,Event}},
%% 	 receive
%% 		{Name,Ret}->
%% 			{ok,Ret};
%% 		Else->
%% 			{error,Else}
%% 	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%end%%%%%%%%%%%%%%%%%%%%%%%%%%

do_action(E)->
	%io:format("do_action:~p~n",[E]),
	%%E:do_action().
	try E:do_action()
	catch
	EE1:Err->
		try
		?ERROR_LOG2("ERROR:~p:~p~n~p",[EE1,Err,erlang:get_stacktrace()])
		catch
		EE2:Err2->io:format("SCHEDULER LOG ERROR:~p:~p~n~p~n",[EE2,Err2,erlang:get_stacktrace()])
		end,
	{error,Err}
	end.

%%schedule(Event)->{ok,schedule_ok}|{error,Else}
%%Event=ScheduleEvent:new().
%%
schedule(Event)->
	Name! {self(),{schedule,Event}},
	receive
		{Name,Ret}->
			{ok,Ret};
		Else->
			{error,Else}
	end.
schedulereport(Event) ->
	 Name!{self(),{schedulereport,Event}},
	 receive
		{Name,Ret}->
			{ok,Ret};
		Else->
			{error,Else}
	end.

%%unschedule(Action)->{ok,unschedule_ok}|{error,Else}
%%
%%Action=action:new().
unschedule(Action)->
	Name! {self(),{unschedule,Action}},
	receive
		{Name,Ret}->
			Ret;
		Else->
			{error,Else}
	end.
%%
%% 
%%S -> schedule
%%Repeat  ->true or false
%%L ->Period
%%
scheduleAction(Action,ScheduleSpec,Repeat,LastTime)->
%% 	Action:run().
try
 As=string:tokens(ScheduleSpec, "  "),
 As1=lists:nth(1,As),
 As2=lists:nth(2,As),
	
 if length(As)=:=3 ->
		As3=lists:nth(3,As),
		case As1 of 
			"weekday" ->
				THIS:scheduleDailyAction(Action, As2, As3, Repeat, LastTime);
			"monthday" ->
				THIS:scheduleDatedAction(Action, As2, As3, Repeat, LastTime);
			_ ->error
		end;
	length(As)=:=2,As1=:="periodic" ->
        THIS:scheduleReportPeriodicAction(Action, As2, Repeat, LastTime);
    true ->
       error
  end
catch
_:_->
	io:format("sheduler:scheduleAction error!")
end.

%%
%%
%%
scheduleRepeatedDailyAction(Action , Days,Times) ->
 scheduleDailyAction(Action, Days, Times, true, 0).

%%
%%
%%
scheduleDailyAction(Action, Days,Times, Flag, Lasttime)->	
 	Compoundevent = compound_event:new(Action,[]),
	Sec=string:tokens(Times, ","),
	Seconds= erlang:list_to_integer(hd(Sec)),
	LDays=string:tokens(Days, ","),	
	Daily_events=[daily_event:new(null,getDay(Day),Seconds,Lasttime) ||Day<-LDays],
	Compoundevent1=Compoundevent:addEvent(Daily_events), 	
	schedulereport(Compoundevent1).
%%
%%
%%
getDay(H) ->
	DaysInteger=getDaysInteger(),
	case lists:keysearch(H, 1, DaysInteger) of
		false->
			Ret=false;
		{_,{_,Day}} ->
			Ret=Day
	end,
	Ret.
%%
%%U,M,T,W,R,F,S
%%
getDaysInteger() ->
	Ret=[{"U",7},{"M",1},{"T",2},{"W",3},{"R",4},{"F",5},{"S",6}],
	Ret.

%%
%%
%%
scheduleRepeatedDatedAction(Action , Days,Times) ->
 scheduleDailyAction(Action, Days, Times, true, 0).
%%
%%
%%
scheduleDatedAction(Action, Days,Times, Repeated, Lasttime)->
	Compoundevent = compound_event:new(Action,[]),
	Sec=string:tokens(Times, ","),
	Seconds= erlang:list_to_integer(hd(Sec)),
	LDays=string:tokens(Days, ","),
	Dated_events=[dated_event:new(null,list_to_integer(Day),Seconds,Lasttime) ||Day<-LDays],
	Compoundevent1=Compoundevent:addEvent(Dated_events),
 	schedulereport(Compoundevent1).

%%
%%
%%
scheduleReportPeriodicAction(Action, L, Flag, L1) ->
	scheduleReportPeriodicAction.



%%
%%old
%%
%%scheduleAction(Action,S,Repeat,L)->
%% 	As = string:tokens(S, ","),
%%     F1 = string:equal(lists:nth(0, (As)), "weekday"),
%% 	F2 = string:equal(lists:nth(0, (As)), "monthday"),
%% 	F3 = string:equal(lists:nth(0, (As)), "periodic"),
%% 	if
%% 	   (length(As) == 3) ->
%% 		   if			   
%% 			  (F1) ->				
%% 					scheduleDailyAction(Action, lists:nth(1, (As)), lists:nth(2, (As)), Repeat, L);				
%% 			  (F2) ->					
%% 					scheduleDatedAction(Action, lists:nth(1, (As)), lists:nth(2, (As)), Repeat, L);
%% 			  true->
%% 					ok
%% 		   end;
%% 		(length(As) == 2 and F3)->
%% 			schedulePeriodicAction(Action, lists:nth(1, (As)), Repeat, L);
%%         true ->
%% 			ok
%% 	end.
%%	ok.
%% 	PericEvent = periodic_event:new(2,0,Action,Repeat),
%% 	schedule(PericEvent).
%% 	Action:run(),
%% 	ok.
	
%%         String as[] = TextUtils.split(s);
%%         try {
%%             if (as.length == 3) {
%%                 if (as[0].equalsIgnoreCase("weekday")) {
%%                     scheduleDailyAction(action, as[1], as[2], flag, l);
%%                 } else if (as[0].equalsIgnoreCase("monthday")) {
%%                     scheduleDatedAction(action, as[1], as[2], flag, l);
%%                 }
%%             } else if (as.length == 2 && as[0].equalsIgnoreCase("periodic")) {
%%                 schedulePeriodicAction(action, Long.parseLong(as[1]), flag, l);
%%             }
%%         } catch (NumberFormatException numberformatexception) {
%%         }

testreportPeriodicAction(Action,Time)->
 Reportrealtimeevent= reportrealtime_event:new(Time,sv_datetime:now(),Action,true),
 schedule(Reportrealtimeevent).

%%
%%
%%
schedulePeriodicAction(Action,Cf,Repeat,LastUpdate)->
	PericEvent = periodic_event:new(Cf,LastUpdate,Action,Repeat),
	%%PericEvent:set_repeated(Repeat),
	schedule(PericEvent).

scheduleRepeatedPeriodicAction(Action,LastUpdate)->
	THIS:scheduleRepeatedPeriodicAction(Action,LastUpdate,true,0).

scheduleRepeatedPeriodicAction(Action,Cf,LastUpdate)->
	THIS:schedulePeriodicAction(Action,Cf,true,LastUpdate).

scheduleAbsolutePeriodicAction(Action,S,L)->
	AbsoluteEvent = absolute_event:new(Action,S),
	schedule(AbsoluteEvent).

scheduleRepeatedAction(Action,S)->
	scheduleRepeatedAction(Action,S,0).

scheduleRepeatedAction(Action,ScheduleSpec,LastTime)->
	scheduleAction(Action,ScheduleSpec,true,LastTime).