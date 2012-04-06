-module (network_switch).
-compile(export_all).
-include("../../include/object.hrl").

%% modelling the network device:
%%  interface is the difference from servers
%% TODO: modify digraph.erl for the topology
%%    connect monitor to the attribute of the object
%% auto generating a monitor for each attribute

extends () -> base_device.

%%@doc the constructor
network_switch (Self,Name) ->
	?SETVALUE(aft,[]),?NEWATTRMONITOR(aft), %% address forward table or MAC Address table
	?SETVALUE(name,Name),
	eresye:start(Name). %%TODO: need evaluate whether start a rule engine for each monitor or one rule engine for all monitor ?

%%@doc the destructor
network_switch_(Self)-> 
	eresye:stop(?VALUE(name)).

update() ->
	 ?SETVALUE(aft,?ATTRMONITOR(aft)),	 
	 ?SETVALUE(cpu,?ATTRMONITOR(cpu)),
	 ?SETVALUE(mem,?ATTRMONITOR(mem)).

newnode() ->
	[eresys:assert(discover_rule_engine, {newnode,X,now()})||  X <- ?VALUE(aft)]. %% %% 	[{MAC,If,ConnectedMAC}].	

start(Name) ->
	case object:get_by_name(Name) of
		[] -> 
				X = object:new(?MODULE,[Name]),
				object:start(X),
				X;
		_ -> atom_to_list(Name) ++ " not available, choose a new name"
	end.