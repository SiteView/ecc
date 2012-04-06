

-module(help_plugin_1).
-include ("../../nitrogen/include/wf.inc").
-compile(export_all).

web_help_point1_ErlangPlugin(Args)-> 
	#tablerow {show_if=true,cells=[
		#tablecell{body=#link{text="plugin-framework doc",url="../edoc/plugin-Read Me.txt"}},
		#tablecell{text="plugin-framework document"}]}.



