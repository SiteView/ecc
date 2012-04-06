%%
%% @author SiteView
%% @copyright Dragonflow Networks, Inc.
%% @version 1.0.0
%% @title The resource pool manager
%% @doc The resource pool needs started during system initializing phase.  The resource pool module implement managing the limited system resource for different monitor type.  
%% Limiting the vary large parallel process by queue the resource request when exeeding the resourse limits.

%% when monitor request resource, the resource counter increased by one, if less than limits, assert the resource allocated
%% if exeed the limit, put it into a queue.  Once finishing monitoring, the resource counter decrease by one or taking a queue
%% item  
%% 
%% TODO: if the load is very loarge, need this resource allocation alogorithm, for smaller one, we do not need it.
%% 		handle the counter/max and queue, need an algorithm, goal, constraint ?
%% 		rule1: if queue length keep on increasing, need increase the max
%% 		rule2: set the max based on system resource: mem, cpu and io 
%% 		rule3: set the max based on needs
%% 		rule4: set the max based on resource type mix of the monitors
%%  constraints: cpu, mem, io, monitor waiting length
%%  goal: max the per minute execution, minimize waiting time
%%  steps: 1. gather all variables, 2. formula to inter-relate the variables

%%  formula: QueueLen = Number - Fequency/Update * Max, if nagative, set to zero
%% 			 Wait = Number/Max * Update - Frequency
%%			 when need Wait = 0, can set the Max = Number * Update / Frequency
%%			 Max is constraint by system resource 			
%%  formula: Max x Wait = Num x Update + Max * Frequency, the optimal is Wait = 0.
%% @end

-module(resource_pool).
-compile(export_all).
-include("object.hrl").

extends () -> nil .


%% 	{_Request,Name,Session,RequestType} = Pattern,
?PATTERN(frequency_request_pattern) -> {?POOLNAME,get,{'_','_','_','_',frequency_request,'_','_'}};%%  {ResourceType,Name,Session,Now,RequestType,Counter,QueueLen}
?PATTERN(refresh_request_pattern) -> {?POOLNAME,get,{'_','_','_','_',refresh_request,'_','_'}};%%  {ResourceType,Name,Session,Now,RequestType,Counter,QueueLen}
?PATTERN(release_resource_pattern)-> {?POOLNAME, get, {'_',release_resource}}.

%% can NOT return a list
?EVENT(frequency_request_event) -> {eresye,frequency_request_pattern};
?EVENT(refresh_request_event) -> {eresye,refresh_request_pattern};
?EVENT(release_resource_event)-> {eresye,release_resource_pattern}.

?ACTION(start) -> [
				   {frequency_request_event,frequency_request_action},
				   {refresh_request_event,refresh_request_action},
				   {release_resource_event,release_resource_action}
				  ].

%% ?ACTION(start) -> {test_event,test_action}.

start()->
	Pool = object:new(?MODULE),
	object:start(Pool),
	eresye:start(?POOLNAME),
	Pool.

resource_pool(Self)->
	?SETVALUE(name,?POOLOBJ),
	eresye:start(?VALUE(name)).

resource_pool_(Self)->eresye:stop(?VALUE(name)).

%%@doc request resource from the pool,   insert the resource into its queue
%% 1. check if any list empty
%% 2. if empty, insert into it
%% 3. if no empty list, if the number of lists reaches max
%% 3.1. if not, create a new list, insert into it
%% 3.2. if yes, insert the request into a least length list
%% 4. if more than one list empty, delete one

%%@doc add patterns to monitor,resource pool and log to trigger the next action
%% monitor: trigger update_action in monitor, e.g. ping_monitor:update_action
%% resource pool: create a new resource consumption, not further action
%% log_analyzer: logging, no further action
request(Name,Session,RequestType) ->
	ResourceType = erlang:apply(object:getClass(Name), get_resource_type,[]),
	Max = get_max(ResourceType),
	Counter = length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_'})),
	QueueLen = erlang:length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_',waiting_for_resource,'_'})),
	eresye:assert(?LOGNAME, {Name,Session,now(),RequestType}),
	case length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_'})) < Max of  
	   true -> 
		   if  QueueLen == 0 ->
				   eresye:assert(Name,{Session,resource_allocated}),%%trigger the pattern in monitor, invoking the update_action in indidual [NAME] monitor
				   eresye:assert(?LOGNAME, {Name,Session,now(),allocate_resource}),
	       		   eresye:assert(?POOLNAME,{ResourceType,Name,Session,now()});
			   true ->
				   eresye:assert(?POOLNAME, {ResourceType,Name,Session,now(),waiting_for_resource,RequestType}),
		   		   allocate_next(ResourceType) 		   
		   	   end;
	   _ -> %%add into queue
		   eresye:assert(?POOLNAME, {ResourceType,Name,Session,now(),waiting_for_resource,RequestType})
	end.

%%@doc get the max parallel number for each resource type
%%TODO: need a table to map the resource to max, first static, then dynamically allocate resource
-spec(get_max/1 :: (atom) -> int).
get_max(ResourceType) -> 20.

release(Name,Session) -> 
	ClassName = object:getClass(Name),
	ResourceType = erlang:apply(ClassName, get_resource_type,[]),
	eresye:retract_match(?POOLNAME,{ResourceType,Name,Session,'_'}),
	QueueLen = erlang:length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_',waiting_for_resource,'_'})),

	case erlang:length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_',waiting_for_resource,'_'})) == 0 of
	   true -> nil;
	   _ -> %get the next item to run and drop it from the queue
		   allocate_next(ResourceType) 
		   
%% 	   	   Max = get_max(ResourceType),
%% 	   	   case length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_'})) < Max of 
%% 			   true -> allocate_next(ResourceType) ; 
%% 			   Other-> nil 
%% 		   end
		  %%TODO: if run time out, should still release the resource
	end.

allocate_next(ResourceType) ->
	{NextName,NextSession} = get_next(ResourceType),
	eresye:assert(NextName,{NextSession,resource_allocated}),%%trigger the pattern in monitor, invoking the update_action in indidual [NAME] monitor
	eresye:assert(?POOLNAME,{ResourceType,NextName,NextSession,now()}),
%% 		   io:format('~w~n', [eresye:query_kb(?LOGNAME, {NextName,NextSession,'_',frequency})]),
	[{_,_,StartWait,frequency_request}|_] = eresye:query_kb(?LOGNAME, {NextName,NextSession,'_',fun(X)-> (X == frequency_request) or (X == refresh_request) end}),
	object:set(NextName,wait_time,timer:now_diff(now(), StartWait)/1000000),
	eresye:assert(?LOGNAME, {NextName,NextSession,now(),allocate_resource}).
	

release(Name) -> %%for object:delete()  
	ClassName = object:getClass(Name),
	ResourceType = erlang:apply(ClassName, get_resource_type,[]),
	eresye:retract_match(?POOLNAME,{ResourceType,Name,'_','_'}),  %%delete in pool
	eresye:retract_match(?POOLNAME,{ResourceType,Name,'_','_','_','_'}). %%delete in queue

%%@doc get the next from the queue based on the following priority
%% 1. refresh_request and earliest
%% 2. if no refresh_request, earliest
-spec(get_next/1::(atom) -> atom). 
get_next(ResourceType) ->
	RefreshQueue = lists:keysort(4,eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_',waiting_for_resource,refresh_request})),
	if erlang:length(RefreshQueue) == 0 ->
			[Next|_] = lists:keysort(4,eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_',waiting_for_resource,frequency_request}));
	   true ->
			[Next|_] = RefreshQueue		    
	end,
	{_,NextName,NextSession,StartWait,waiting_for_resource,_} = Next,
	eresye:retract(?POOLNAME,Next), %%remove the item in queue
	{NextName,NextSession}.
	
	
%% 	eresye:assert(?POOLNAME, {ResourceType,Name,Session,Now,waiting_for_resource,RequestType}),
%%@doc register the object's resource type, the same type only once.
%% called in object:new
register(ResourceType) -> 	
	Self = ?POOLOBJ, 
	IsAttribute = object:isAttribute(Self,ResourceType),
	if IsAttribute -> ok;
	   true -> ?SETVALUE(ResourceType,{0,queue:new()})
	end.

get_counter(Name) -> 
	IsValidName = object:isValidName(Name) ,
	if IsValidName->
		ClassName = object:getClass(Name),
		ResourceType = erlang:apply(ClassName, get_resource_type,[]),
		Counter = length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_'})),
		Counter;
	   true -> 0
	end.

get_queue_length(Name) -> 
	IsValidName = object:isValidName(Name) ,
	if IsValidName->
		   	ClassName = object:getClass(Name),
			ResourceType = erlang:apply(ClassName, get_resource_type,[]),
			erlang:length(eresye:query_kb(?POOLNAME, {ResourceType,'_','_','_',waiting_for_resource,'_'}));
	   true -> 0
	end.

get_pools() ->
	PoolList = object:get_defined_attrs(?POOLOBJ),
	[{ResourceType,Counter,erlang:apply(ResourceType, get_max,[]),queue:to_list(Queue)}
			||{ResourceType,{Counter,Queue}}<-PoolList].

%%TODO: check for error	
%% 	if Len == 0 andalso Counter > Max -> % this should never happen, serious error.
%% 		  io:format("[~w:~w] Resource pool SYSTEM ERROR. Queue empty and reach the max limit ~w", [?MODULE,?LINE,{Max,Counter}]);
%% 	   true -> ok
%% 	end,

%%TODO:stat info of the resource pool: number of pool, max, queue max/size/avg, create a monitor for it
%%TODO: create a monitors to monitor the health condition of pool

%% test() ->
%% 	start().