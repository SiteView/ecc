

-module(plugin3).
-compile(export_all).

plugin_demo_point1_ErlangPlugin(Args)-> 

	io:format("io:format  Args:~p~n",[Args]),
	extension_node:io_format_local("io_format_local  Args 11112:~p~n",[Args]),

	{3,Args}.

plugin3_pointtest_ErlangExtension(Args)-> 
	{3,Args}.




