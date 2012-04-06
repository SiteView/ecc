%% ---
%%your comment
%%
%%---
-module(siteview).
-behaviour(gen_server).

-compile(export_all).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,terminate/2,stop/0]).

-include("common.hrl").
-include_lib("kernel/include/file.hrl").
-include("user_spl.hrl").
-include("dbcs_common.hrl").
-include("config.hrl").



-define(TIMEOUT,10000).

-record(sv,{app,siteview,totalPointsUsed,start_time}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


	
stop()->
	logs_archive:stop(),
	catch(gsmOperate:stop()),
	SV = get_current_siteview(),
	SV:stop_siteview(),
	SV:delete(),
	log_manager:stop(),
	ssh:stop(),
	% ets:delete(elecc_object_table),
	% ets:delete(siteview_elecc),
    gen_server:cast(?MODULE, stop).

%% @spec wait_ofbiz(N,Node)->void()
%% @doc wait to connect to ofbiz nodes, at least one ofbiz erlangnode needed to continue
%%
wait_ofbiz(N,Node) when N<100 ->
	case net_adm:ping(Node) of
		pong->
			io:format("*******************get ofbiz node*****~p~n",[Node]),
			case dbcs_group:get_all() of
				[G|_]->
					true;
				_->
					io:format("database is empty....~n"),
					api_siteview:reset(),
					wait_ofbiz(1,Node)
			end;
		_->
			io:format("connect to ofbiz node fail, wait for ~p seconds to retry....~n",[trunc(5*math:log(N))]),
			platform:sleep(trunc(5000*math:log(N))),
			wait_ofbiz(N+1,Node)
			end;
wait_ofbiz(100,Node) -> wait_ofbiz(99,Node).
	
wait_db()->
	case net_adm:ping(server_conf:get_db_node()) of
		pong->
			case dbcs_group:get_all() of
				[G|_]->
					true;
				_->
					io:format("[Siteview:wait_db]database is empty....~n"),
					io:format("[Siteview:wait_db]init database ....~n"),
					api_siteview:reset(),
					wait_db()
			end;
		_->
			io:format("[Siteview:wait_db]connect to database node [~p] fail.  Retry in 20 seconds...~n",[server_conf:get_db_node()]),
			platform:sleep(20000),
			wait_db()
	end.
	
	
init(_)->
	put(lastrun,now()),
	siteview_global:start_link(),
	siteview_object_table:start_link(),
	SV = siteview_group:new(),
	siteview_global:set_siteview(SV),
	% ets:new(siteview_elecc, [set,named_table,public,{write_concurrency,true}]),
	% ets:insert(siteview_elecc,{siteview,SV}),
	% ets:insert(siteview_elecc,{totalPointsUsed,0}),
	% ets:insert(siteview_elecc,{start_time,sv_datetime:now()}),
	% init_object_table(),
	% 
	wait_db(),
	wait_ofbiz(1,server_conf:get_ofbiz_node()),
	check_install_date(),
	
	try
		app_init()
	catch
		error:Err->
			io:format("siteview init(app) error:~p~n",[Err])
	end,
	SV:init(SV,[{id,siteview:getServerID()},{name,"root"}]),
	SV:start_siteview(),
	{ok,#sv{app=SV}}.
	
	
app_init()->
%% 	iconv:start(),
	network_bandwidth_config:static(),
	init_os_template:start(),
    remoteMachineManager:start_link(),
	inets:start(),
	ssh:start(permanent),        
	log_manager:start_link([{debug,true}]),
	gsmOperate:start(),
	create_index_process(),
%% not used, disable for now  (JOHN, 09/25/2011)
%% 	incident_init:init_project(),
%% 	dbcs_workflow:init_status(),
%%  tr069_Manager:start_link([{dbname,?DBName}]),
%% 	eplot_main:init(),
	logs_archive:start().
	
	
	
handle_call(_, _From, Sv) ->
	{reply,ok,Sv}.
	
handle_cast(stop, S) ->
    {stop, normal, S};	
handle_cast(_, Sv) ->
	{noreply,  Sv}.

	
%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
   stop().


%% @spec get_start_time()->integer()
%% @doc return system started time, unit is microsecond
%%
get_started_time()->
	siteview_global:get_started_time().
	% case ets:lookup(siteview_elecc,start_time) of
		% []->
			% ets:insert(siteview_elecc,{start_time,sv_datetime:now()}),
			% 0;
		% [{_,Time}|_]->
			% Now = sv_datetime:now(),
			% Now -Time
	% end.

%% @doc initial elecc object table
%%
%%
% init_object_table()->
	% ets:new(elecc_object_table, [set,named_table,public,{write_concurrency,true}]).

%% @doc insert a object to table
%%
%%
set_object(Id,Class,Object)->
	siteview_object_table:set_object(Id,Class,Object).
	% App = dbcs_base:get_app(),
	% ets:insert(elecc_object_table,{{App,Id},Class,Class,Object}).

%% @doc insert a object to table
%%
%%
set_object(Id,Type,Class,Object)->
	siteview_object_table:set_object(Id,Type,Class,Object).
	% App = dbcs_base:get_app(),
	% ets:insert(elecc_object_table,{{App,Id},Type,Class,Object}),
	% C = Object:getCostInLicensePoints(),
	% ets:update_counter(siteview_elecc,totalPointsUsed,C).
	% addPoints(C).
	
	
%% @doc get a object from table
%%
get_object(Id)->
	siteview_object_table:get_object(Id).
%%
	% App = dbcs_base:get_app(),
	% case ets:lookup(elecc_object_table,{App,Id}) of
		% []->
			% [];
		% R->
			% [Y||{_,_,_,Y}<-R]
	% end.

%% @doc get objects from table
%%
%%
get_object_by_class(Class)->
	siteview_object_table:get_object_by_class(Class).
	% App = dbcs_base:get_app(),
	% case ets:match_object(elecc_object_table,{{App,'_'},'_',Class,'_'}) of
		% []->
			% [];
		% R->
			% [Y||{_,_,_,Y}<-R]
	% end.
	
%% @doc get objects from table
%%
%%
get_object_by_class(Type,Class)->
	siteview_object_table:get_object_by_class(Type, Class).
	% App = dbcs_base:get_app(),
	% case ets:match_object(elecc_object_table,{{App,'_'},Type,Class,'_'}) of
		% []->
			% [];
		% R->
			% [Y||{_,_,_,Y}<-R]
	% end.

get_object_by_type(Type)->
	siteview_object_table:get_object_by_type(Type).
	% App = dbcs_base:get_app(),
	% case ets:match_object(elecc_object_table,{{App,'_'},Type,'_','_'}) of
		% []->
			% [];
		% R->
			% [Y||{_,_,_,Y}<-R]
	% end.
	
%% @doc delete a object from table
%%
%%
remove_object(Id)->
	siteview_object_table:remove_object(Id).
	% App = dbcs_base:get_app(),
	% remove_object(App,Id).
	%% case get_object(Id) of
		%% []->
			%% {error,object_not_found};
		%% [Object|_]->
			%% C = Object:getCostInLicensePoints(),
			%% io:format("~n***********************ɾ��֮ǰ = ~p~n***********************~n",[C]),
			%% ets:update_counter(siteview_elecc,totalPointsUsed,-C),
			%% ets:delete(elecc_object_table,{App,Id}),
			%% io:format("~n***********************ɾ��֮�� = ~p~n***********************~n",[Object:getCostInLicensePoints()])
	%% end.
	

remove_object(App,Id)->
	siteview_object_table:remove_object(App,Id).
	% case get_object(Id) of
		% []->
			% {error,object_not_found};
		% [Object|_]->
			% C = Object:getCostInLicensePoints(),
			% reducePoints(C),
			% ets:delete(elecc_object_table,{App,Id})
	% end.

get_current_siteview()->
	siteview_global:get_current_siteview().
	% [{siteview,SV}|_] = ets:lookup(siteview_elecc,siteview),
	% SV.

load_conf()->
	server_conf:start_link().
%	ets:new(siteview_elecc_conf, [set,named_table,public]),
%	case file:consult("conf/server.conf") of
%		{ok,Data}->
%			%proplists:get_value(serverID,Data);
%			ets:insert(siteview_elecc_conf,Data);
%		_->
%			error
%	end.

getServerID()->
	server_conf:getServerID().
%	case ets:lookup(siteview_elecc_conf,serverID) of
%		[]->
%			'99';
%		[{serverID,ServerId}|_]->
%			ServerId
%	end.


totalPointsUsed()->
	siteview_global:totalPointsUsed().
	% App = dbcs_base:get_app(),
	% case ets:lookup(siteview_elecc,{App,totalPointsUsed}) of
		% []->
			% undefined;
		% [{_,V}|_]->
			% V
	% end.
	
addPoints(Count)->
	siteview_global:addPoints(Count).
	% App = dbcs_base:get_app(),
	% case ets:lookup(siteview_elecc,{App,totalPointsUsed}) of
		% []->
			% ets:insert(siteview_elecc,{{App,totalPointsUsed},Count});
		% _->
			% ets:update_counter(siteview_elecc,{App,totalPointsUsed},Count)
	% end.
	
reducePoints(Count)->
	siteview_global:reducePoints(Count).
	% App = dbcs_base:get_app(),
	% ets:update_counter(siteview_elecc,{App,totalPointsUsed},-Count).

getServerConf(Key)->
	server_conf:getServerConf(Key).
%	case ets:lookup(siteview_elecc_conf,Key) of
%		[]->
%			undefined;
%		[{_,V}|_]->
%			V
%	end.

get_monitor_scheduler()->
	SV = get_current_siteview(),
	case SV:get_attribute(monitor_scheduler) of
		{ok,{monitor_scheduler,Sche}}->
			{ok,Sche};
		_->
			{error,not_found_scheduler}
	end.


get_report_scheduler()->
	% [{siteview,SV}|_] = ets:lookup(siteview_elecc,siteview),
	SV = get_current_siteview(),
	case SV:get_attribute(report_scheduler) of
		{ok,{report_scheduler,Sche}}->
			{ok,Sche};
		_->
			{error,not_found_scheduler}
	end.

create_object(Data)->
	SV = get_current_siteview(),
	SV:createObject(Data).

create_object_by_id(Id)->
	case dbcs_monitor:get_monitor(Id) of
		{error,Err}->
			{error,Err};
		MonitorData->
			create_object(MonitorData)
	end.

create_object(Parent,Data)->
	SV = get_current_siteview(),
	SV:createObject(Parent,Data).

get_java_node()->
	server_conf:get_java_node().
%	case ets:lookup(siteview_elecc_conf,javaNode) of
%		[]->
%			undefined;
%		[{javaNode,Node}|_]->
%			Node
%	end.	
	
get_version()->
	case file:read_file_info("conf/server.conf") of
		{ok,Fi}->
			%  io:format("footer render:~p~n",[Fi]),
			case Fi#file_info.mtime of
				{{YY,MM,DD},_}->
					lists:flatten(io_lib:format("~w/~w/~w",[MM,DD,YY]));
				_->
					"9.0"
			end;
		_->
			"9.0"
	end.
	
get_release_date()->
	server_conf:get_release_date().

	
check_install_date()->
	case preferences:get(master_config,install_date) of
		{ok,[_|_]}->
			ok;
		_->
			preferences:set(master_config,install_date,date())
	end.
	
get_serverurl()->
	case inet:gethostname() of
		{ok,Name}->
			case inet:gethostbyname(Name) of
				{ok,{_,_,_,_,_,[Ip|_]}}->
					lists:flatten(io_lib:format("http://~w.~w.~w.~w:~w",[element(1,Ip),element(2,Ip),element(3,Ip),element(4,Ip),nitrogen:get_port()]));
				_->
					lists:flatten(io_lib:format("http://localhost:~w",[nitrogen:get_port()]))
			end;
		_->
			lists:flatten(io_lib:format("http://localhost:~w",[nitrogen:get_port()]))
	end.
	
-ifdef(USER_SPL).
reset()->
	% SV = get_current_siteview(),
	% SV:stop_siteview(),
	try
		case dbcs_group:reset() of
			{ok,_}->
				io:format("group reset -----------OK.~n");
			Err->
				io:format("ERROR:group reset maybe cause a error:~p!~n",[Err])
		end,
		% api_usergroup:reset(),
		case dbcs_user_spl:reset() of
			{ok,_}->
				io:format("user reset -----------OK.~n");
			Err2->
				io:format("ERROR:user reset maybe cause a error:~p!~n",[Err2])
		end,
		io:format("reset database ok please restart service..."),
		{ok,reset_ok}
	catch
		_:{error,group_reset}->{error,group_reset};
		_:{error,user_reset}->{error,user_reset}
	end.
	
-else.
reset()->
	% SV = get_current_siteview(),
	% SV:stop_siteview(),
	dbcs_group:reset(),
	api_usergroup:reset(),
	% SV:start_siteview(),
	ok.
-endif.	


                      
reset(AppName) when is_atom(AppName) ->  reset(atom_to_list(AppName));  

reset(AppName) ->
      case ets:info(siteview_elecc_conf) of
         undefined -> load_conf();
         _ -> ok
      end,  
      put(hostname,list_to_atom(AppName)),
      init_application(AppName),
      dbcs_group:reset(),
	  dbcs_user_spl:reset().
%~ rpc:call(sv3ren@contentstore,app,get,[[{application,"oldhand"}]]).
%~ {ok,{0,100,1,
     %~ [{application,oldhand,<<"oldhand">>,63393414434000000,
                   %~ 63402057181963033,'20snqodspk86x',
                   %~ <<229,138,170,229,138,155,230,137,141,232,131,189,232,174,
                     %~ 169,...>>,
                   %~ <<"http://www.3ren.com/view-source.html?appUrl=oldhand">>,
                   %~ false,false,[],
                   %~ [<<"oldhand">>,<<"erlang">>,<<"linux">>,<<"home">>,
                    %~ <<"3ren">>,<<"wow">>,
                    %~ <<233,173,148,...>>,
                    %~ <<230,143,...>>],
                   %~ [],[],null,true,
                   %~ {<<"http://api.3ren."...>>,<<"icon">>},
                   %~ <<"2795388334018897">>,0,2003}]}}     
               
init_application(AppName) ->     
     case init_profile() of
           {ok,Profileid} ->
                case rpc:call(?DBName,app,get,[[{application,AppName}]]) of 
                {ok,{_,_,_,[]}} -> 
                 Application = #application{ 
                         id=list_to_atom(AppName),updated=null,published=null,				 
                         viewSourceUrl = <<>>,privateSource = null,
                         runOwnAds=false,xn_layoutVersion=null,
                         xn_active=true,seqenceNumber=null,
                         storeLimit=0,link=null,
                         description = <<>>,title=list_to_binary(AppName),
                         author=Profileid,xn_tag=[],                 
                         xn_premiumService=[],
                         xn_category=[],xn_domain=[]},
                  io:format("~p~n",[Application]),   	 
                  case rpc:call(?DBName,app,create,[[{version,2}],Application]) of 
                        {ok,App} -> {ok,App};
                        Result -> io:format("~p~n",[Result])
                  end;
                {ok,{_,_,_,App}} -> 
                    io:format("~p Application is exist ~n",[AppName]),
                    {ok,App};
                Result -> io:format("~p~n",[Result])
          end;
          _ ->
          io:format("init_profile is failure~n")
     end.
    



%~ rpc:call(sv3ren@contentstore,profile,get,[[{application,"oldhand"},{profile,"email=oldhand@sina.com"}]]).
%~ {ok,{0,100,1,
     %~ [{profile,'20snqodspk86x',<<"oldhand">>,<<>>,
               %~ 63383999045000000,63428161395777637,<<"Ning">>,
               %~ {<<"http://api.3ren.com/files/6067b262ce64d59a0e6c97bb2b"...>>,
                %~ <<"icon">>},
               %~ false,null,null,<<"m">>,
               %~ <<230,185,150,229,141,151,231,156,129,...>>,
               %~ <<"CN">>,<<"1974-08-05">>,<<"oldhand@sina.com">>,
               %~ <<"zealot">>,
               %~ [{laoshou,null},{master,null},{oldhand,...},{...}],
               %~ 469}]}}


init_profile() ->   init_profile("oldhand@sina.com").
    
init_profile(UserName) ->      
     case rpc:call(?DBName,profile,get,[[{application,"oldhand"},{profile,"email="++UserName}]]) of 
            {ok,{_,_,_,[]}} -> 
             Profile = #profile{ 
					 id=null, 
                     title = <<"oldhand">>, 
                     summary = <<>>, 
                     published = null, 
                     updated = null, 
                     author = <<"siteview">>, 
                     link = null, 
                     emailVerified = false, 
                     xn_relationship = null,
                     xn_zipcode = null, 
                     xn_gender = <<"m">>, 
                     xn_location = <<>>, 
                     xn_country = <<"CN">>, 
                     xn_birthdate = <<"1974-08-05">>, 
                     xn_email = list_to_binary(UserName), 
                     xn_password = <<"zealot">>, 
                     xn_masterkey = [], point = 0},
              io:format("~p~n",[Profile]),   	 
              case rpc:call(?DBName,profile,create,[[{version,2}],Profile]) of 
                    {ok,App} -> User = App, {ok,User#profile.id};
                    Result -> io:format("~p~n",[Result])
              end;
            {ok,{_,_,_,Profiles}} -> 
                io:format("~p profile is exist ~n",[UserName]),
                User = hd(Profiles), 
                {ok,User#profile.id};               
            Result -> io:format("~p~n",[Result])
      end.     
	  
create_index_process()->
	timer:apply_interval(60*60*1000,index_store,create,[]).