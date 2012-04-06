-module(monitor_mapping_tools).
-compile(export_all).


getMonitorIds()->
	[list_to_atom(Id)||[_,Id]<-[tuple_to_list(Monitor)||Monitor<-api_monitor:get_all_monitors()]].

getMappingMonitors()->
	proplists:get_value(ok,[api_monitor_proxy:get_proxy_mapping_list()]).

delete(List1,List2)->
	case List1 of
		[]->List2;
		[E | List]->
			delete(List,deleteE(E,List2))
	end.

deleteE(E,List)->
	proplists:delete(E,List).

mappingCheck()->
	MapMonitors = getMappingMonitors(),
	Monitors = getMonitorIds(),
	[api_monitor_proxy:remove_proxy_mapping(Monitor) || Monitor<-delete(Monitors,MapMonitors)].
	
