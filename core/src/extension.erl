

-module(extension).
-export([start_link/1,start_link/0,init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% main function 
-export([call_plugins/3, hi/0, proxyPluginRegist/2]).

%% get detail data of ets table
-export([report_status/0, report_plugins/0, report_extension_points/0, report/0 ]).

%% disable and enable functions, it's global scope
-export([disable_plugin/3, enable_plugin/3, disable_extension_point/2, enable_extension_point/2]).

%% only for test
-export([get_start_time/0, gentle_stop/2, exit_test/2]).



-define(GenServer, gen_server).
-behaviour(?GenServer).

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(PointSuffix, "_ErlangExtension").
-define(PluginSuffix, "_ErlangPlugin").
-define(PluginTable, extensions_plugins_table).
-define(PointTable, extensions_points_table).
-define(StatusTable, extensions_status_table).
-define(SPid, supervisor_pid).
-define(ENABLE,enable).
-define(DISABLE,disable).
-define(LOCAL,local).

-record(state, {inited= false,
				?SPid= undefined 
			   }).

start_link() ->
	start_link([]).

start_link(A) ->	
	case catch ets:info(?StatusTable) of
		undefined -> 
			ets:new(?StatusTable, [public, set, named_table]);
		_ ->
			ok
	end,	
    ?GenServer:start_link({local, ?MODULE}, ?MODULE, [{?SPid,self()}|A], []).

report_status() ->
	ets:tab2list(?StatusTable).
report_plugins() ->
	ets:tab2list(?PluginTable).
report_extension_points() ->
	ets:tab2list(?PointTable).
report()->
	[{?StatusTable,report_status()},
	 {?PluginTable,report_plugins()},
	 {?PointTable,report_extension_points()}
	].

%% checking if extension framework is alive  
hi()->
	gen_call(say_hi , []).

call_plugins( Module, Point, Args) ->	
	lists:foldl(
	  	fun(OneP,Acc)->
			case catch {{_Func1,_Module1,_Location},_} = OneP of
				{'EXIT', _} ->				
					[{'EXIT', {undefined,undefined}, "extension framework error!"}|Acc];
				_ ->
					{{F,M,Location},_} = OneP,
					case catch apply_or_rpc(Location,M,F,Args) of
						{'EXIT', Reason} ->
							plugin_statistic( Module, Point, Location,M,F, {'EXIT',Reason} ),
							RetResson= 
							case Reason of
								{badrpc,nodedown} ->
									delete_proxy_node_data(Location),
									"nodedown, all plugins and extension points at this node will be deleted!";
								_ ->
									Reason
							end,
							[{'EXIT', {Location,M,F}, RetResson}|Acc];
						Ret ->
							plugin_statistic( Module, Point, Location,M,F, {ok,ok} ),
							[{ok, {Location,M,F}, Ret}|Acc]
					end
			end
		end,[],ets:select(?PluginTable, getMs(Module, Point, ?PluginSuffix, ?ENABLE))).

apply_or_rpc( local, Module, Func, Args)->
	apply(Module, Func, [Args]);
apply_or_rpc( Location, Module, Func, Args)->
	R= rpc:call( Location, Module, Func, [Args] ),
	case R of		
		{badrpc, Reason}->
			throw({'EXIT',{badrpc, Reason}});
		A->
			A
	end.

%% will disable all sub-plugins
disable_extension_point( ExtensionModule, ExtensionPoint ) -> 
    gen_call(dis_enable_extp_plg ,{?DISABLE, extp, {undefined, ExtensionModule, ExtensionPoint}}).

%% will enable all sub-plugins
enable_extension_point( ExtensionModule, ExtensionPoint ) -> 
    gen_call(dis_enable_extp_plg ,{?ENABLE, extp, {undefined, ExtensionModule, ExtensionPoint}}).

disable_plugin( PluginModule, ExtensionModule, ExtensionPoint ) -> 
    gen_call(dis_enable_extp_plg ,{?DISABLE, plg, {PluginModule, ExtensionModule, ExtensionPoint}}).

enable_plugin( PluginModule, ExtensionModule, ExtensionPoint ) -> 
    gen_call(dis_enable_extp_plg ,{?ENABLE, plg, {PluginModule, ExtensionModule, ExtensionPoint}}).



getMs(Module, Point, Suffix, DisEnable)-> 
	AName= getPNameOfAtom(Module, Point, Suffix), 
	ets:fun2ms(
		   fun({{Pname,M,Location},{Enable,T1,T2}}) when AName=:=Pname andalso Enable=:=DisEnable -> {{Pname,M,Location},{Enable,T1,T2}}
		   end).

getPNameOfAtom(Module, Point, Suffix)->
	Name= atom_to_list(Module) ++ "_" ++ atom_to_list(Point) ++ Suffix, 
	case catch list_to_atom(Name) of
		{'EXIT', _} -> list_to_atom(Name);
		A -> A
	end.	

init(Env) ->
    process_flag(trap_exit, true),	
	{_,{X1,Y1,Z1}}= get_time_now(),
	SPid= proplists:get_value(?SPid, Env, undefined),
	scanThenInit(SPid),
	{_,{X2,Y2,Z2}}=Time2 =get_time_now(),	
	put(start_time, Time2),	
	ets:insert(?StatusTable, {start_time,Time2}),	
	Sec= abs(Z2-Z1 + (Y2-Y1)*60 + (X2-X1)*3600),
	StartCount= case catch  ets:lookup(?StatusTable, start_count) of
		[{start_count, Count}|_] -> Count;
		_ -> undefined
	end,	
	io:format("~nextension framework ~pth startup in ~.3f seconds ...~n",[StartCount,Sec]),
	{ok, #state{inited=true, ?SPid=SPid}}.

proxyPluginRegist(ProxyNode,PluginsInfo)->
	gen_call(proxyPluginRegist , {ProxyNode,PluginsInfo} ).

delete_proxy_node_data(Node)->
	gen_call(delete_proxy_node_data , {Node} ).

scanThenInit(SupPid) ->
	set_start_count(SupPid),
    ets:new(?PluginTable, [protected, ordered_set, named_table]),
    ets:new(?PointTable, [protected, ordered_set, named_table]),	
	
	lists:foreach(
	  fun( {plg,Module,Func} ) -> ets:insert(?PluginTable, {{Func,Module,?LOCAL},{?ENABLE,0,0} });
		 ( {pnt,Module,Func} ) -> ets:insert(?PointTable, {{Func,Module,?LOCAL},{?ENABLE,0,0} });
		 (_) -> ok
	  end, filter_module()),	
    put(inited, true),
	ok.


terminate(Reason, _State) ->
	SCount= case catch  ets:lookup(?StatusTable, start_count) of
		[{start_count, Count}|_] when is_integer(Count) -> 
			Count;
		_ ->
			10000
	end,	
	case SCount<100 of
		true -> catch ets:insert(?StatusTable, {{extension,terminate_reason,SCount},Reason});
		_ -> ok
	end, 		
	ok.

get_start_time()->
	gen_call(get_start_time , []).  	

gentle_stop(Reason, _State) ->
    gen_call(gentle_stop , Reason).

exit_test(Reason, _State) ->
    gen_call(exit_test , Reason).

gen_call(What,Msg)->
  	?GenServer:call(?MODULE, {What,Msg} ).

handle_call({dis_enable_extp_plg, {En, What, {PlgM, ExtM, ExtP}}}, _From, State) ->
	set_dis_enable(En, What, PlgM, ExtM, ExtP),
	{reply, {State, ok}, State};

handle_call({gentle_stop,_Msg}, _From, State) ->
	{stop, shutdown, {State, terminate}, State};

handle_call({exit_test,_Msg}, _From, State) ->
    Reply = {State, exit_test},
	{_A}= Reply,
	{reply, Reply, State};

handle_call({is_inited,_Msg}, _From, State) ->
    Reply = {State, get(inited)},
	{reply, Reply, State};

handle_call({get_start_time,_Msg}, _From, State) ->	
    Reply = {State, get(start_time)},
	{reply, Reply, State};

handle_call({proxyPluginRegist, {ProxyNode,PluginsInfo} }, _From, State) ->
	catch delete_ext_plg_data(ProxyNode),
	lists:foreach(
	  fun( {plg,Module,Func} ) -> ets:insert(?PluginTable,{{Func,Module,ProxyNode},{?ENABLE,0,0} });
		 ( {pnt,Module,Func} ) -> ets:insert(?PointTable, {{Func,Module,ProxyNode},{?ENABLE,0,0} });
		 (_) -> ok
	  end, PluginsInfo),	
	{reply, {"Proxy Plugins registting to MainNode is ok!"}, State};

handle_call({delete_proxy_node_data , {Node} }, _From, State) ->
	catch delete_ext_plg_data(Node),
	{reply, {"Delete proxy node data."}, State};

handle_call({say_hi,_Msg}, _From, State) ->	
	{reply, {'Yes, I am here!'}, State}.


plugin_statistic(Module, Point, Location, M, F, State)->
	?GenServer:cast(?MODULE, {plugin_statistic, {Module, Point, Location, M, F, State} } ).	

handle_cast({plugin_statistic,{ExtModule, Point, PlgLocation,Module,Func,{IsOk,Reason}}}, State) ->
	Ms= ets:fun2ms(
		   fun({{P,Mo,Location},A}) when Func=:=P andalso Module=:=Mo andalso PlgLocation=:=Location -> {{P,Mo,Location},A}
		   end),
	catch lists:map(
	fun({{Pname,M,Location},{Enable,T1,T2}}) ->
		case IsOk of
			ok -> 		
				catch ets:insert(?PluginTable, {{Pname,M,Location},{Enable,T1+1,T2}});
			'EXIT' ->	
				catch ets:insert(?PluginTable, {{Pname,M,Location},{Enable,T1,T2+1}}),
				case T2+1 =< 10 of
					true -> catch ets:insert(?StatusTable, {{Pname,M,Location,'EXIT-Reason',T2+1},Reason});
					_ -> ok
				end
		end;
		(_)-> ok
	end, ets:select(?PluginTable, Ms) ),
	
	AName= getPNameOfAtom(ExtModule, Point, ?PointSuffix), 
	Ms2= ets:fun2ms(
		   fun({{Pname,M,Location},{Enable,T1,T2}}) when AName=:=Pname -> {{Pname,M,Location},{Enable,T1,T2}}
		   end),
	catch lists:map(
	fun({{Pname,M,Location},{Enable,T1,T2}}) ->	
		case IsOk of
			ok -> 		
				catch ets:insert(?PointTable, {{Pname,M,Location},{Enable,T1+1,T2}});
			'EXIT' ->	
				catch ets:insert(?PointTable, {{Pname,M,Location},{Enable,T1,T2+1}})		
		end;
		(_)-> ok
	end, ets:select(?PointTable, Ms2) ),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


filter_module()->
    lists:foldl(
	  	fun(M, Acc) ->
	    	lists:foldl(
	  			fun(F, FAcc) ->
					case catch {_,_}= F of
						{'EXIT', _} -> 	FAcc;
						_ ->
							{Func,_}= F,
							Str= atom_to_list(Func),
							case string:rstr(Str, ?PointSuffix) =/= 0 of
								true -> [{pnt, M, Func}|FAcc];
								_ ->
									case string:rstr(Str, ?PluginSuffix) =/= 0 of
										true -> [{plg, M, Func}|FAcc];
										_ -> FAcc
									end
							end
					end
				end, Acc, M:module_info(exports) )
    	end, [], getAllModules()).	

getAllModules()->
    lists:foldl(fun(D, Acc) ->
        case do_add_directory(D) of
        Mods when is_list(Mods) -> 
            lists:append(Mods,Acc);
        _ -> Acc
        end
    end, [], getAllEbinPath()).

getAllEbinPath()->
	OtpLib= code:lib_dir(),
	lists:foldl(
		fun(E,Last)->	
			case string:rstr(E,OtpLib) of
				0 -> [E|Last];
				_ -> Last
			end
		end, [], code:get_path() ).

do_add_directory(Dir) ->
    {FileNames, _,_,_} =
		scan_directory(Dir, [], [".beam"], [".jam"]),
	lists:foldl(
	  fun(E,Last)->
				Argv= case {_, FileName}=E of
						{'EXIT', _} -> 	Last;
						_ -> split_filename(FileName, ".beam")
					  end,
				case Argv of
					{name,Name} -> [Name|Last];
					_ -> Last
				end
		end, [], FileNames).

scan_directory(File, Recurse, Collect, Watch) ->
    Init = [[] | {[],[],[]}],
    [L | {E,J,U}] = find_files_dir(File, Recurse, Collect, Watch, Init),
    {lists:reverse(L), lists:reverse(E), lists:reverse(J), lists:reverse(U)}.

split_filename(File, Extension) ->
    case catch {name,list_to_atom(filename:basename(File, Extension))} of
	{'EXIT', _} -> false;
	R -> R
    end.


find_files_dir(Dir, Recurse, Collect, Watch, L) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    find_files(lists:sort(Files), Dir, Recurse, Collect, Watch, L);
	{error, Error} ->
	    [B | {E,J,U}] = L,
	    [B | {[file_error(Dir, Error)|E],J,U}]
    end.

find_files([F | Fs], Dir, Recurse, Collect, Watch, L) ->
    File = filename:join(Dir, F),
    L1 = case file_info(File) of
	     {ok, {_, directory, readable, _}} when Recurse ->
		 	find_files_dir(File, Recurse, Collect, Watch, L);
         {ok, {_, directory, _, _}} ->
		 	L;
	     Info ->
		 [B | EJU = {E,J,U}] = L,
		 Ext = filename:extension(File),
		 C = lists:member(Ext, Collect),
		 case C of
		     true ->
			 case Info of
			     {ok, {_, file, readable, _}} ->
				 [[{Dir,F} | B] | EJU];
			     {ok, {_, file, unreadable, _}} ->
				 [B | {E,J,[File|U]}];
			     Error ->
				 [B | {[Error|E],J,U}]
			 end;
		     false ->
			 case lists:member(Ext, Watch) of
			     true -> [B | {E,[File|J],U}];
			     false -> L
			 end
		 end
	 end,
    find_files(Fs, Dir, Recurse, Collect, Watch, L1);
find_files([], _Dir, _Recurse, _Collect, _Watch, L) ->
    L.

file_info(F) ->
    case file:read_file_info(F) of
	{ok, Info} -> 
	    Readable = case Info#file_info.access of
			   Access when Access =:= read; 
                           Access =:= read_write ->
			       readable;
			   _ ->
			       unreadable
		       end,
	    Type = case Info#file_info.type of
		       directory -> directory;
		       regular -> file;
		       _ -> error
		   end,
	    case Type of 
		error -> error({unrecognized_file, F});
		_ -> {ok, {F, Type, Readable, Info#file_info.mtime}}
	    end;
	{error, Error} -> 
	    file_error(F, Error)
    end.


file_error(File, Error) ->
    error({file_error, File, Error}).

error(Error) ->
    {error, ?MODULE, Error}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.


get_time_now()->
	{_,_,MicroSec} = erlang:now(),	
    {{A,B,C},{X,Y,Z}} =  erlang:localtime(),
	{{A,B,C},{X,Y,Z+MicroSec/1000000}}.

set_start_count(SPid)->
	NewCount= case catch  ets:lookup(?StatusTable, start_count) of
		[{start_count, Count}|_] when is_integer(Count) -> 
			Count+1;
		_ ->
			catch ets:new(?StatusTable, [public, set, named_table, {heir,SPid,[]} ]),
			1
	end,
%% 	io:format("~n extension fmwk start_count: ~p  ~n", [NewCount]),
	ets:insert(?StatusTable, {start_count,NewCount}),
	ok.	
		
set_dis_enable(En, plg, PlgM, ExtM, ExtP, ForAllPluginModule)->
	REn= reverse_dis_enable(En),
	AName= getPNameOfAtom(ExtM, ExtP, ?PluginSuffix), 
	Ms= case ForAllPluginModule =:= all of
			true ->
				ets:fun2ms(
			   	fun({{Pname,M,Location},{Enable,T1,T2}}) when AName=:=Pname andalso Enable=:=REn -> {{Pname,M,Location},{Enable,T1,T2}}
		   		end);				
			_ ->
				ets:fun2ms(
			   	fun({{Pname,M,Location},{Enable,T1,T2}}) when AName=:=Pname andalso PlgM=:=M andalso Enable=:=REn -> {{Pname,M,Location},{Enable,T1,T2}}
		   		end)
		end,
	lists:map(
	  	fun({{Pname,Module,Location},{_Enable,T1,T2}})->
			catch ets:insert(?PluginTable, {{Pname,Module,Location},{En,T1,T2}})
		end,ets:select(?PluginTable, Ms)).

set_dis_enable(En, plg, PlgM, ExtM, ExtP)->
	set_dis_enable(En, plg, PlgM, ExtM, ExtP, onlyOne);	
set_dis_enable(En, extp,PlgM, ExtM, ExtP)->
	lists:map(
	  	fun({{Pname,Module,Location},{_Enable,T1,T2}})->
			catch ets:insert(?PointTable, {{Pname,Module,Location},{En,T1,T2}})
		end,ets:select(?PointTable, getMs(ExtM, ExtP, ?PointSuffix, reverse_dis_enable(En)))),
	set_dis_enable(En, plg, PlgM, ExtM, ExtP, all).
	
reverse_dis_enable(?ENABLE)->
	?DISABLE;
reverse_dis_enable(?DISABLE)->
	?ENABLE.
	

delete_ext_plg_data(Node)->
	Ms= ets:fun2ms(
		   fun({{P,Mo,Location},A}) when Node=:=Location -> {{P,Mo,Location},A}
		   end),
	catch lists:map(
	  	fun({{Pname,Module,Location},_})->
			catch ets:delete(?PluginTable, {Pname,Module,Location})
		end,ets:select(?PluginTable, Ms)),	
	catch lists:map(
	  	fun({{Pname,Module,Location},_})->
			catch ets:delete(?PointTable, {Pname,Module,Location})
		end,ets:select(?PointTable, Ms)).	
	
	
	