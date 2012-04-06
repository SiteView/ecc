%% @copyright 2008-2009 Dragonflow
%% @author shaohua.wang, kaiyang.cheng
%% @version 1.0
%%
%% Description: Monitor IBM DB2
%% %% Versions supported: 8.x
%% Platform: All operating systems with a supported JDBC driver and DB2 snapshot feature
%%
-module(db2_monitor, [BASE]).
-extends(browsable_exe_base).
-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").



%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for db2 database monitor
new() ->
	Base = browsable_exe_base:new(),	
	{?MODULE,Base}.

%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to monitor db2 database performance.
update() ->	
	BASE:update(),
	true.

%% @spec get_template_property() -> List
%% @type List = [term()]
%% @doc get_template_property is the function called by schedule to show user the GUI interface to input data.
get_template_property()->
	BASE:get_template_property().

%% @spec get_classifier(error) -> List
%% @type List = [Tuple]
%% @type Tule = {status, Logic, Value}
%% @type Logic = '!=' | '==' | '>' | '<' | 'contain
%% @type Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error) ->
	BASE:get_classifier(error);
get_classifier(warning) ->
	BASE:get_classifier(warning);
get_classifier(good) -> 
	BASE:get_classifier(good).

