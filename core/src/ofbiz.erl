-module(ofbiz).


-export([call/2,callservice/2,callmonitor/3,call_topology/2,log_monitor/1,call/5]).

-define(RECEIVE_TIME_OUT, 10*1000).
-define(MBox,eccadmin).
-define(OfbizNode,server_conf:get_ofbiz_node()).


%%@spec call(ServiceName, InData) -> Response
%%@type ServiceName = atom()
%%@type InData= proplist
%%@doc call ofbiz service, the input data is a proplist.
call(ServiceName, Indata) ->
	call(?OfbizNode,?MBox,"OfbizService",ServiceName, Indata).

callservice(ServiceName, Indata) ->
	call(?OfbizNode,?MBox,"OfbizService",ServiceName, Indata).


%%@spec callmonitor(MonitorClassName, InData) -> Response
%%@type MonitorClassName = atom(), the monitor type
%%@type InData= proplist
%%@doc call ofbiz service, the input data is a proplist.
callmonitor(MonitorClassName,Action,Params) ->
	Msg={self(),"UpdateMonitor",MonitorClassName,Action,Params},
	Result = ofbizrpc(?MBox,?OfbizNode,Msg),
	Result.

%%@spec log_monitor(InData) -> void
%%@type InData= the log, a list of string
%%@doc call log monitor, the input data is a proplist, no return is needed
log_monitor(Params) ->
	Msg={self(),"LogMonitor","OfbizService","monitorListLogger",Params},
	{?MBox,?OfbizNode} ! Msg.


%%@spec call_topology(Action, InData) -> Response
%%@type Action = the action
%%@type InData= the log
%%@doc call toplogy related services, the input data is a proplist.
call_topology(Class, Action) ->
	Msg={self(),"TopologyDispatch",Class, Action},
	{?MBox,?OfbizNode} ! Msg.

%%@spec call(OfbizNode,MBox,ServiceName, Indata) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Type = string()  OfbizService or OfbizDelegator
%%@type ServiceName = atom()
%%@type InData= proplist: [self(),Action,Parameters]
%%@doc call ofbiz service, the input data is a proplist.
call(OfbizNode,MBox,Type,ServiceName, Indata) -> 
	Msg = {self(),Type,"OfbizService",ServiceName,Indata},
	Result = ofbizrpc(MBox,OfbizNode,Msg),
	Result.

%%@spec ofbizrpc(Node,RegName, Msg) -> Response
%%@type RegName = atom()
%%@type Node = atom()
%%@type Msg = [tuple()]
%%@doc remote process calling for the java node with messages.
ofbizrpc(RegName, Node, Msg) ->
	%THIS:set_attribute(?DEBUG_INFO, "remote process call ofbiz erlang node ..."),
	monitor_node(Node,true),
	{RegName, Node} ! Msg,	
	receive
		{error,Err}->
			[{error,Err}];
		{nodedown,Node}->
			{error, node_down};
		{Sender,Ret}->
			%io:format("ofbiz Sender~p~n",[Sender]),
			Ret;
		{ok,_,Ret}->
			qsort(Ret);
        {error, _From, [Ret]} ->
            [{error,Ret}];
		{Ret, _, _}->
			[{error, atom_to_list(Ret)}];
		_ ->
			{error,"*******  Ofbiz erlang node return a unknow error."}
			
		after ?RECEIVE_TIME_OUT ->
			case net_adm:ping(Node) of
				pong ->
					[{error, " ******* time is out. "}];
				pang ->					
					[{error, " *******Fatal error: connect to Ofbiz erlang node Error! "}]
			end
	end.

qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X || X <- T, element(2,X) < element(2, Pivot)])
	++ [Pivot] ++
	qsort([X || X <- T, element(2,X) >= element(2,Pivot)]).	