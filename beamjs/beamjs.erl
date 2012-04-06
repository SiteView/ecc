-module(beamjs).
-include_lib("../include/erlv8.hrl"). 
-export([run_jsfun/2]).

%%%

install_require(VM) ->
	Global = erlv8_vm:global(VM),
	beamjs_mod_require:init(VM),
	Global:set_value("require", beamjs_mod_require:exports(VM)).

load_jsfun(VM,Files) ->
	Global = erlv8_vm:global(VM),
	Global:set_value("__dirname",filename:absname("")),
	Global:set_value("module",?V8Obj([{"id","init"},{"exports", ?V8Obj([])}])),
	lists:foreach(fun (File) ->
	  	  io:format("load_jsfun ~p,  ~p~n", [VM, File]),
		  Require = Global:get_value("require"),
		  Global:set_value("module",?V8Obj([])),
		  Module = Global:get_value("module"),
		  Module:set_value("id",File,[dontdelete,readonly]),
		  Require:set_value("main",Module),
		  case Require:call([File]) of
			  {throw, {error, #erlv8_object{}=E}} ->
				  io:format("~s~n",[E:proplist()]);
			  _ ->
				  ignore
		  end
	 end, Files).

run_fun([], Global, Result)->
	Result;
run_fun([H|T], Global, Result)->
	{File, FunName, Param} = H,
	F = Global:get_value(FunName),
	case FunName of 
		"require"-> 
			run_fun(T, Global, Result ++ [{File, FunName,"requiresucessed"}]);
		_->
			ChildResult = F:call(Param),
			run_fun(T, Global, Result++[{File, FunName,ChildResult}])		
	end.
	
	

run_jsfun(Files, Params) ->
	erlv8:start(),
	{ok, VM} = erlv8_vm:start(),
	install_require(VM),
	load_jsfun(VM, Files),
	
	Global = erlv8_vm:global(VM),
	Result = run_fun(Params, Global, []),	
%% 	Global1 = erlv8_vm:global(VM),	
%% 	F = Global1:get_value("f"),
%% 	F1 = Global1:get_value("f2"),
%% 	F2 = Global1:get_value("erlang_run"),
%% 	io:format("load  : ~p ~n ", [F:call([2])]),
%% 	io:format("load1 : ~p ~n ", [F1:call([44])]),
%% 	io:format("load2 : ~p ~n ", [F2:call([5])]),
	erlv8_vm:stop(VM),
	Result.

%% args(VM,jseval) ->
%% 	case init:get_argument(jseval) of
%% 		{ok, [[JS]]} ->			
%% 			io:format("jseval ~p~n", [erlv8_vm:run(VM, erlv8_context:get(VM), JS, {"(command line)",0,0})]);
%% 		_ ->
%% 			false
%% 	end;
%% 
%% args(VM, path) ->
%% 	case init:get_argument(jspath) of
%% 		{ok, [Paths]} ->
%% 			lists:foreach(fun (Path) ->
%% 								  Global = erlv8_vm:global(VM),
%% 								  Require = Global:get_value("require"),
%% 								  RPaths = Require:get_value("paths"),
%% 								  RPaths:unshift(Path)
%% 						  end, Paths);
%% 		_ -> 
%% 			false
%% 	end;
%% 
%% args(VM,bundles) ->
%% 	case init:get_argument(bundles) of
%% 		{ok, [Bundles]} ->
%% 			lists:foreach(fun(Bundle) ->
%% 								  case (catch beamjs_bundle:load(VM, Bundle)) of
%% 									  {'EXIT', {{bundle, {throw, {error, #erlv8_object{}=E}}}, _}} ->
%% 										  io:format("~s~n",[E:proplist()]);
%% 									  _ ->
%% 										  ignore
%% 								  end
%% 						  end, Bundles);
%% 		_ ->
%% 			false
%% 	end;
