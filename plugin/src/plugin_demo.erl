

-module(plugin_demo).
-compile(export_all).

test()->
	t1().

%% test in shell
t1()->
%%  extension:disable_extension_point(plugin_demo, point1).
%%  extension:disable_plugin(plugin2,plugin_demo, point1). 
%%  extension:report().
	
	io:format("hi from extension: ~p", [extension:hi()]),
	myfunction(),
	spawn(fun myfunction/0),
	ok.


myfunction()->
	RetData= plugin_demo_point1_ErlangExtension(123),
	io:format("~n ------------------ call plugins -------------------~n"),
	lists:foreach(
		fun	({State,M,Data}) -> 
				 io:format("state:~p  call:~p ~n return-data:~p ~n",[State,M,Data]);
			(E) -> 
				 io:format("  Should never reach here!  ~p ~n",[E])
		end, RetData),
	io:format("------------------ call plugins -------------------~n~n "),	
	ok.

plugin_demo_point1_ErlangExtension(Args)->
	extension:call_plugins(?MODULE, point1, Args).

plugin_demo_point1_ErlangPlugin(Args)-> 
	{1,Args}.



