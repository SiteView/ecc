

-module(help_plugin_2).
-include ("../../nitrogen/include/wf.inc").
-compile(export_all).

web_help_point1_ErlangPlugin(Args)-> 
	#tablerow {show_if=true,cells=[
		#tablecell{body=#link{text="plugin-framework status",url="plugin_framwork_status"}},
		#tablecell{text="(for debugging only) plugin-framework status"}]}.



