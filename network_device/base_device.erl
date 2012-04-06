-module (base_device).
-compile(export_all).
-include("../../include/object.hrl").

%% modelling the network device:
%%  interface is the difference from servers
%% TODO: modify digraph.erl for the topology
%% need connect monitor to the attribute of the object
%% auto generating a monitor for each attribute

extends () -> nil.

%%@doc the constructor
base_device (Self,Name) ->
	?SETVALUE(model,""),
	?SETVALUE(snmpver,"v2"),
	?SETVALUE(community_string,"nn_mobile"),	
	?SETVALUE(ifnumber,24),
	?SETVALUE(cpu,0),?VALUEMONITOR(cpu),?NEWATTRMONITOR(cpu),
	?SETVALUE(mem,0),?VALUEMONITOR(mem),?NEWATTRMONITOR(mem),
	?SETVALUE(configuration_file,""),?NEWATTRMONITOR(configuration_file),
	eresye:start(Name). %%TODO: need evaluate whether start a rule engine for each monitor or one rule engine for all monitor ?

%%@doc the destructor
base_device_(Self)-> 
	eresye:stop(?VALUE(name)).

create_monitor(Self,AttributeName) ->
	AttributeString = atom_to_list(AttributeName),
	ClassName = list_to_atom(?MODULE_STRING ++ "_" ++ AttributeString ++ "_monitor"), %%network_device_aft_monitor
	ObjectName = list_to_atom(atom_to_list(?VALUE(name)) ++ AttributeString ++"_monitor"),%%
	erlang:apply(ClassName, start,ObjectName).

get_monitor_object(Self,AttributeName) ->
	AttributeString = atom_to_list(AttributeName),
	ObjectName = list_to_atom(atom_to_list(?VALUE(name)) ++ AttributeString ++"_monitor"),
	ObjectName.


start(Name) ->
	case object:get_by_name(Name) of
		[] -> 
				X = object:new(?MODULE,[Name]),
				object:start(X),
				X;
		_ -> atom_to_list(Name) ++ " not available, choose a new name"
	end.