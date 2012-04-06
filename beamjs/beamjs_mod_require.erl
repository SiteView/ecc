-module(beamjs_mod_require).
-export([exports/1,init/1, require/2]).

-behaviour(beamjs_module).
-include_lib("../include/erlv8.hrl"). 

init(VM) ->
	erlv8_vm:stor(VM,{beamjs_mod_require,mod_tab},ets:new(require_modules,[public,{heir, VM, none}])),
	ok.

exports(VM) ->
	{ok, Cwd} = file:get_cwd(),
	Paths = erlv8_vm:taint(VM,?V8Arr([Cwd])),
	Paths:set_value("__doc__","Array of paths where require() will be looking for modules"),	
	erlv8_fun:new(fun require_fun/2,?V8Obj([
				{"__doc__",
					"`require()` provides a way to load another javascript modules. Not fully compatible with [CommonJS Modules/1.1](http://wiki.commonjs.org/wiki/Modules/1.1) yet, "
					"but eventually [will be](https://github.com/beamjs/beamjs/issues/issue/3)." ++ [10] ++
					"### Synopsis\n"
					"    "
					"    require(ModuleId)"
					"    "
				},
				{"paths", Paths}])).

require(VM, Filename) when is_pid(VM), is_atom(Filename) ->
	erlv8_vm:taint(VM, Filename:exports(VM));
require(VM, Filename) when is_pid(VM) ->
	{_, Ctx} = erlv8_context:get(VM),
	require_fun(#erlv8_fun_invocation{ vm = VM, ctx = Ctx }, [Filename]).


require_fun(#erlv8_fun_invocation{ vm = VM }, [#erlv8_object{}=Opts1]) ->
%% 	io:format("require_fun 2 : ~p~n", [Opts1:proplist()]),
	Opts = [{erlang:binary_to_list(X), erlang:binary_to_list(Y)} || {X, Y} <- Opts1:proplist()],
%% 	Opts = [{X, Y} || {X, Y} <- Opts1:proplist()],
	case Opts of 
		[{"module", Module}] when is_list(Module) ->
			Mod = list_to_atom(Module),
%% 			io:format("require_fun 333 : ~p~n", [Mod:exports(VM)]),
			erlv8_vm:taint(VM, Mod:exports(VM));
		[{"join", Modules}] when is_list(Modules) ->
			NewExports = erlv8_vm:taint(VM, ?V8Obj([])),
			Throws = lists:filter(fun ({throw, _}) ->
										  true;
									  (_) ->
										  false
								  end,
								  lists:map(fun (Module) ->
													Exports = require(VM, Module),
													lists:foreach(fun ({K,V}) ->
																		  NewExports:set_value(K,V)
																  end, Exports:proplist())
											end, Modules)),
			case Throws of
				[Throw|_] -> 
					Throw;
				_ ->
					NewExports
			end;
		_ ->
			{throw, "Unknown key"}
	end;
				

require_fun(#erlv8_fun_invocation{ vm = VM } = Invocation, [FilenameIn]) ->
%% 	io:format("require_fun22  : ~p~n", [FilenameIn]),
	Filename = erlang:binary_to_list(FilenameIn),	
	case require_file(Invocation, Filename) of
 		{throw, E} ->
 			case erlv8_vm:retr(VM, {beamjs_bundle, module, Filename}) of
 				undefined ->
 					{throw, E};
				#erlv8_object{}=O ->
					O
 			end;
 		Exports ->
 			Exports
 	end.

file_reader(Path,Filename) ->
	case file_reader(Path,Filename,".js") of
		not_found ->
			file_reader(Path,Filename,"");
		Result ->
			Result
	end.

file_reader(Path1,Filename,Ext) ->
	Path = erlang:binary_to_list(Path1),
%% 	io:format("file_reader1 :  ~p   ~p  ~p ~n",[Path, Filename, Ext]),	
%% 	io:format("file_reader : ~p~n",[filename:join([Path, Filename ++ Ext])]),	
	case file:read_file(filename:join([Path, Filename ++ Ext])) of
		{error, _} ->
			not_found;
		{ok, B} ->
			{filename:dirname(filename:absname(Path ++ "/" ++ Filename)), Filename ++ Ext, binary_to_list(B)}
	end.


require_file(#erlv8_fun_invocation{ vm = VM } = Invocation, Filename) ->
	Global = Invocation:global(),
	Require = erlv8_vm:taint(VM, fun require_fun/2),  %%Global:get_value("require"),
	RequireObject = Require:object(),
	Paths = (Global:get_value("require")):get_value("paths",erlv8_vm:taint(VM,?V8Arr([""]))),
	RequireObject:set_value("paths", Paths),
	Dirname  = filename:absname(Global:get_value("__dirname")),
%% 	io:format("Dirname:~p~n", [Dirname]),
%% 	io:format("Paths:list():~p~n", [Paths:list()]),
%% 	io:format("Filename:~p~n", [Filename]),
	Sources = lists:filter(fun (not_found) -> 
								 false;
							 (_) ->
								 true
						 end,
						 lists:map(fun (Path) -> file_reader(Path, Filename) end,[Dirname|Paths:list()])),
%% 	io:format("Sources:~p~n", [Sources]),
	case Sources of 
		[] ->
			{throw, {error, lists:flatten(io_lib:format("Cannot find module '~s'",[Filename])) }};
		[{Path,LoadedFilename,S}|_] ->
%% 			io:format("Tab1:~p~n", [S]),
			Tab = erlv8_vm:retr(VM,{beamjs_mod_require,mod_tab}),
%% 			io:format("Tab:~p~n", [Tab]),
			case ets:lookup(Tab,Filename) of
				[{Filename, loading}] ->
					Module = Global:get_value("module"),
					ModuleId = Module:get_value("id"),
					ets:delete(Tab,Filename),
					ets:delete(Tab,ModuleId),
					{throw, {error, lists:flatten(io_lib:format(
													"Dependency cycle detected while attempting to load '~s' from '~s'",
													[Filename, ModuleId]))}};
				[{Filename, Exports}] ->
					Exports;
				[] ->
%% 					io:format("Tab22222222222:~p~n", [Tab]),
					NewCtx = erlv8_context:new(VM),
					NewGlobal = erlv8_context:global(NewCtx),
					lists:foreach(fun ({K,V}) ->
										  NewGlobal:set_value(K,V)
								  end,  Global:proplist()),
					NewGlobal:set_value("require",fun require_fun/2),
					NewRequire = NewGlobal:get_value("require"),
					lists:foreach(fun ({K,V}) ->
										  NewRequire:set_value(K,V)
								  end,  Require:proplist()),
					NewPaths = NewRequire:get_value("paths"),
					NewRequire:set_value("paths",?V8Arr(lists:usort([Dirname|(NewPaths:list())]))),
					case Global:get_value("exports") of
						undefined ->
							NewGlobal:set_value("module",Global:get_value("module"));
						_ ->
							NewGlobal:set_value("module",?V8Obj([]))
					end,
					
					Module = NewGlobal:get_value("module"),
					Module:set_value("id", Filename, [dontdelete,readonly]),
					NewGlobal:set_value("exports",?V8Obj([])),
					NewGlobal:set_value("__dirname",Path),
					NewGlobal:set_value("__filename",filename:join([Path,LoadedFilename])),
					
					ets:insert(Tab,{Filename, loading}),
%% 					io:format("Tabddddddddddddddddddddd:~p~n", [S]),
					case erlv8_vm:run(VM,NewCtx,S,{LoadedFilename,0,0}) of
						{ok, _} ->
							lists:foreach(fun ({"exports",_}) ->
												  ignore;
											  ({"__dirname",_}) ->
												  ignore;
											  ({"__filename",_}) ->
												  ignore;
											  ({"module",_}) ->
												  ignore;
											  ({K,V}) ->
%% 												   io:format("Tab sucess111111111 :~p   ~p ~n", [{K,V}]),
												  Global:set_value(K,V)
										  end,  NewGlobal:proplist()),
%% 							io:format("Tab sucess :~p~n", [NewGlobal:proplist()]),
							Exports = NewGlobal:get_value("exports"),		
%% 							io:format("Tab sucess :~p~n", [Exports]),
							ets:insert(Tab, {Filename, Exports}),
							Exports;
						{_,E} ->
%% 							io:format("Tab error :~p~n", [E]),
							ets:delete(Tab, Filename),
							{throw, {error, E}}
					end
			end
	end.
	
	

