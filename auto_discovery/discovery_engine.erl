%% @doc  the discovery engine to discover everything in the data center
%%
%% process:
%% 	0. obtaining the initial IP address list: seed or an IP range
%%  1. find the active nodes: ICMP, SNMPPing, NMap, each discovery method is a 'monitor'
%%  2. detect the type of the node and create the typed object: sysoid, nmap identification result
%%  3. processing each type which will generate more nodes to be discovered 

%% modelling:
%%  information about the device itself:Common Information Model (CIM)
%%  relationship between devices
%% 
%%  172.20.16.1,nn_mobile
%% 	172.20.12.1
%%  10.111.111.2 adminsitrator/nnyz
%%
%%  related monitors: 
%% 		Active nodes discovery: fastping, SNMPPing, 
%% 		NMap: NMap host, NMap ports, 
%% 		SNMP:
%% [SWITCH]
%% 	AftInfo
%% 	InterfaceInfo
%% [ROUTE]
%% 	InterfaceInfo
%% 	RouteInfo
%% 	ArpInfo
%% 	OspfInfo
%% 	BgpInfo
%% 	DirectInfo
%% [ROUTE_SWITCH]
%% 	InterfaceInfo
%% 	RouteInfo
%% 	ArpInfo
%% 	OspfInfo
%% 	BgpInfo
%% 	DirectInfo
%% 	AftInfo
%% [gpon]
%% 	InterfaceInfo
%% 	GponOltInfo
%% [FIREWALL]
%% 	InterfaceInfo
%% 	RouteInfo
%% 	ArpInfo
%% 	OspfInfo
%% 	BgpInfo
%% 	DirectInfo
%% 
%% %% 
%% 
%% 
%% 

-module (discovery_engine).
-compile ([export_all]).

-include("object.hrl").

extends () -> nil .

?PATTERN(switch_pattern) ->  {discover_rule_engine, read, {'_',is,switch}}; %% node,is,switch
?PATTERN(router_pattern) ->  {discover_rule_engine, read, {'_',is,router}}; %% node,is,switch
?PATTERN(router_switch_pattern) -> {discover_rule_engine, read, {'_',is,router_switch}}; %% node,is,switch
?PATTERN(gpon_pattern) -> {discover_rule_engine, read, {'_',is,gpon}}; %% node,is,switch
?PATTERN(firewall_pattern) -> {discover_rule_engine, read, {'_',is,firewall}}; %% node,is,switch
?PATTERN(newnode_pattern) -> {discover_rule_engine, read, {newnode,'_','_'}}. %% {node,port, node}, timestamp

?EVENT(newnode_event)-> {eresye,newnode_pattern}.

?ACTION(start) -> {newnode_event,newnode_action}.

start()->
	Discover = object:new(?MODULE),
	object:start(Discover),
	eresye:start(discover_rule_engine),
	load_algorithm(),
	Discover.

discovery_engine(Self)->
	?SETVALUE(name,discover_object),
	eresye:start(?VALUE(name)).

discovery_engine_(Self)->eresye:stop(?VALUE(name)).

%%@doc whenever a new node added, discover the node type and then create it and hand over the further discover processing to the object 
newnode_action(Self,EventType,Pattern,State) -> 
	{newnode,{Node,Port,NewNode},Timestamp} = Pattern,
	NodeInfoLen = length(eresye:query_kb(discover_rule_engine, {NewNode,is,'_'})),
	if 	
		NodeInfoLen == 0 -> %% not discovered before
			DiscoverMethods = discovermethod_monitor:run(NewNode) ,  %snmp,nmap,telnet,ssh,wmi,jdbc,tr069,packet,manual_input
			%%create one object, and assign these DiscoverTypes to this object
			NodeType = discover_node_type(DiscoverMethods,NewNode),
			erlang:apply(NodeType, start, [NewNode]), %%create a node object, e.g. network_switch object
			ok;
		true -> already_discovered 
			end.

%%@doc discover the node type with the Access Methods, in a sequence, if discoverred, then exit
%% rules: is  
discover_node_type(DiscoverMethods,Node)  ->
	case DiscoverMethods of
		snmp -> 
			%%using snmp_ping to obtain the sysoid
			eresye:assert(discover_rule_engine,{Node,is,network_switch}), 
			network_switch; 
		nmap -> eresye:assert(discover_rule_engine,{Node,is,network_switch});
		ssh -> eresye:assert(discover_rule_engine,{Node,is,network_switch}) ;
		telnet -> eresye:assert(discover_rule_engine,{Node,is,network_switch}) 
	end.
	

%% SNMPPing: if yes, get sysoid, if not using nmap
%% id the type:sysoid and nmap
%% sysoid: if known create the model, if not try nmap: if yes create the generic model, if not create the generic model and put in for-manual-input
%% nmap: certain: create the model
%% 		possibility: prompt for-manual-input
%% create and start the typed object

load_algorithm() ->
	ok.

start_scan(Node) when is_atom(Node) ->
	eresys:assert(discover_rule_engine,{newnode,Node,'','',now()});
start_scan([]) -> ok;
start_scan(NodeList) ->
	[Node|T] = NodeList,
	start_scan(T).