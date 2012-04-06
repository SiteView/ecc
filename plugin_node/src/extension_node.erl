

-module(extension_node).

-export([io_format_local/2 , global_register_name/0 , get_register_name/0 ]).

%% main function 
-export([findPluginsRegistToMainNode/0, reg/0, getMainNode/0]).

%% get detail data of ets table
-export([report_status/0, report_plugins/0, report_extension_points/0, report/0 ]).

%% disable and enable functions, it's global scope
-export([disable_plugin/3, enable_plugin/3, disable_extension_point/2, enable_extension_point/2]).



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


getMainNode()->
	case catch begin
		NodeList= os:getenv("ExtensionMainNode"),
		{ok, list_to_atom(NodeList)}
		end of
		{ok,Node}->
			Node;
		_->
			undefined
	end.


%% short function name for findPluginsRegistToMainNode()
reg()->
	findPluginsRegistToMainNode().	

findPluginsRegistToMainNode() ->
	MainNode= getMainNode(),
	pong= ping_many_times(MainNode),
	R= rpc:call(MainNode, extension, proxyPluginRegist, [node(),filter_module()]),
	R.


ping_many_times(Node)->
	ping_many_times(Node,5,pang).
ping_many_times(_Node,_Times,pong)->
	pong;
ping_many_times(_Node,0,Ping)->
	Ping;
ping_many_times(Node,Times,_Ping)->
	ping_many_times(Node, Times-1, net_adm:ping(Node)).
	

report_status() ->
	rpc:call(getMainNode(), extension, report_status, []).
report_plugins() ->
	rpc:call(getMainNode(), extension, report_plugins, []).
report_extension_points() ->
	rpc:call(getMainNode(), extension, report_extension_points, []).
report()->
	rpc:call(getMainNode(), extension, report, []).



%% will disable all sub-plugins
disable_extension_point( ExtensionModule, ExtensionPoint ) -> 
    rpc:call(getMainNode(), extension, disable_extension_point, [ExtensionModule, ExtensionPoint]).

%% will enable all sub-plugins
enable_extension_point( ExtensionModule, ExtensionPoint ) -> 
    rpc:call(getMainNode(), extension, enable_extension_point, [ExtensionModule, ExtensionPoint]).

disable_plugin( PluginModule, ExtensionModule, ExtensionPoint ) -> 
    rpc:call(getMainNode(), extension, disable_plugin, [PluginModule, ExtensionModule, ExtensionPoint]).

enable_plugin( PluginModule, ExtensionModule, ExtensionPoint ) -> 
    rpc:call(getMainNode(), extension, enable_plugin, [PluginModule, ExtensionModule, ExtensionPoint]).



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


get_register_name()->
	list_to_atom(atom_to_list(node())++"_global_io_extension_node").

global_register_name()->
	global:register_name(get_register_name(), group_leader()).

io_format_local(Arg1,Arg2)->
	Node= getMainNode(),
	RegName= get_register_name(),
	Lo= rpc:call(Node, global, whereis_name, [RegName]),	 
%% 	io:format("~n~n~n@@@@@@@@@ MainNode: ~p~n@@@@@@@@@ RegName: ~p~n@@@@@@@@@ Lo: ~p~n~n~n",[Node,RegName,Lo]),
	rpc:call(Node, io, format, [Lo, Arg1,Arg2]).

	 
	 
	 
