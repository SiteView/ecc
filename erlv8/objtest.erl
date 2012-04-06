%% Author: cxy
%% Created: 2012-3-2
%% Description: TODO: Add description to objtest
-module(objtest).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0, test1/0, test_classifier/0, test_jsfun/0, test_jsfun1/0, test_jsfun2/0]).

%%
%% API Functions
%%

%% rr("../include/erlv8.hrl"), %% for use in shell only
-include_lib("../include/erlv8.hrl").  

test()->
	{ok, VM} = erlv8_vm:start(),
	Global = erlv8_vm:global(VM),
	Global:set_value("greeting",  erlv8_object:new([{"English", fun (#erlv8_fun_invocation{}, []) ->
                                                                                                  "f = function (x) { return x * 10 };f(10)" end}])),
	erlv8_vm:run(VM,"greeting.English()").
%% 	erlv8_vm:run(VM,"greeting.English").
%%  erlv8_vm:stop(VM).

test1()->
	{ok, VM} = erlv8_vm:start(),
	Global = erlv8_vm:global(VM),
	Global:set_value("greeting", erlv8_object:new([{"English", 
						       fun  (#erlv8_fun_invocation{}=Invocation, [Arg]) -> 
							    case Invocation:is_construct_call() of  
								    true ->
									 This = Invocation:this(),
									 io:format("Arg:~p~n", [Arg]),
									 This:set_value("greeting",Arg);
									% VM1 = Invocation:vm(),
									 %erlv8_vm:run(VM1,Arg);
								    false ->
									 {throw, {error, "This should always be used as a constructor!"}}
							    end
							   end}])),
%% 	erlv8_vm:run(VM,"greeting.English()"), %% first run
	{ok, Obj} = erlv8_vm:run(VM,"new greeting.English('fffff')"),
	Obj:proplist().%% second run
%% 	erlv8_vm:stop(VM).


%%[{errorClassifier, "errorfun = function(percent) {percent < 100};erorfun(percent)"},
%% {warningClassifier, "warningfun = function(percent) {percent < 75};warningfun(percent)"},
%% {okClassifier, "okfun = function(percent) {percent < 75};okfun(percent)"}] 
test_classifier()->
	Obj = base_monitor:start('testObj'),
	
	object:set(Obj, "percent", 10),
	
%% 	Error_classifier = "classifier.obj_value('percent') > 80",
%% 	Warning_classifier = "classifier.obj_value('percent') < 80 && classifier.obj_value('percent') > 30",
%% 	Ok_classifier = "classifier.obj_value('percent') < 30",

	Error_classifier = "Value('percent') > 80",
	Warning_classifier = "Value('percent') < 80 && Value('percent') > 30",
	Ok_classifier = "Value('percent') < 30",
	
	object:call(Obj, set_classifier, [[{error_classifier, Error_classifier},
	{warning_classifier, Warning_classifier}, {ok_classifier, Ok_classifier}]]),

	object:call(Obj, runClassifiersJs, [Obj]),
	object:delete(Obj).

test_jsfun()->
	{ok, VM} = erlv8_vm:start(),
	Global = erlv8_vm:global(VM),
%% 	Global:set_value("cx", 30),
%% 	Global:set_value("cy", 35),
%%  erlv8_vm:run(VM,"f = function (x, y) { return x * y }; f(cx, cy)"),
%% 	Global:set_value("f2",fun(_,Args) -> Args + 11 end),
	Global:set_value("f",fun(_,Args) -> Args end),
	F = Global:get_value("f"),
%% 	F:call([1,2,3]),% results in
 	F:set_value("x","2*3"),
 	erlv8_vm:run(VM,"f(f.x)"). 

test_jsfun1()->
	{ok, VM} = erlv8_vm:start(),
	Global = erlv8_vm:global(VM),
	Global:set_value("classifier",erlv8_object:new([{"getvalue", fun (#erlv8_fun_invocation{}, [String]) -> io:format("Format:~p~n", [String]), String + 3 end}])),
	erlv8_vm:run(VM,"classifier.getvalue('percent')"),
	erlv8_vm:run(VM,"classifier.getvalue(22) > 10 && classifier.getvalue(2) > 6").

test_jsfun2()->
	{ok, VM} = erlv8_vm:start(),
	Global = erlv8_vm:global(VM),
	erlv8_vm:run(VM, "include 'testjs.js'"),
	erlv8_vm:run(VM, "f(30)").
%% 	Global:set_value("classifier",erlv8_object:new([{"getvalue", fun (#erlv8_fun_invocation{}, [String]) -> io:format("Format:~p~n", [String]), String + 3 end}])),
%% 	erlv8_vm:run(VM,"classifier.getvalue('percent')"),
%% 	erlv8_vm:run(VM,"classifier.getvalue(22) > 10 && classifier.getvalue(2) > 6").


test_beamjs()->
	{ok, VM} = erlv8_vm:start(),
	Global = erlv8_vm:global(VM),  
	Global:set_value("vm", ?V8Obj([{"vm_run", fun vm_run/2}])),
	erlv8_vm:run(VM,"vm.vm_run(3+2>5").
 
vm_run(#erlv8_fun_invocation{ this = This }, [Code]) when is_list(Code) ->
	case This:get_hidden_value("VMServer") of
		undefined ->
			{throw, {error, "VM is not started"}};
		VM ->
			case erlv8_vm:run(VM, Code) of
				{throw, Error} ->
					Error;
				{ok, Result} ->
					Result;
				{compilation_failed, Error} ->
					{throw, Error};
				{exception, Error} ->
					{throw, Error}
			end
	end;

vm_run(#erlv8_fun_invocation{} = I, [Code, #erlv8_fun{}=Callback]) when is_list(Code) ->
	spawn(fun () ->
				  Result = vm_run(I, [Code]),
				  Callback:call([Result])
		  end),
	ok.