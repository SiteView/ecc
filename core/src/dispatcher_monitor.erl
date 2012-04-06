%% @author lei.lin@dragonflow.com
%% @copyright 2009 siteview
%% @version 1.0
%% @doc monitor websphere performance 

-module(dispatcher_monitor,[BASE]).
-compile(export_all).
-extends(browsable_base).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(MaxCounters,40).
-define(PROPERTY_NAME_COUNTER_VALUE,"browsableValue").
-define(PROPERTY_NAME_COUNTER_NAME,"_browseName").

%% @spec new() -> ok
%% @doc initialization browsable windows counter monitor.
new() ->
    Base = browsable_base:new(),
    {?MODULE,Base}.
    
%% @spec update() -> ok
%% @doc overloading function,monitor update data.
update() ->
	ok.    

%% @spec get_template_property() -> List
%% @doc overloading function.
get_template_property() ->
    BASE:get_template_property() ++ 
    [   
	 	%#property{name=pServerName,title="Counter File",type=text,order=1,description="a Performance Monitor setting file that specifies the counters"},
        #property{name=server,title="Server",type=text,order=1,editable=true,configurable=true,description="the name of the server"}
    ].
