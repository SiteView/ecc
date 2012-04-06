-module(queue_server).
-behaviour(gen_server).
-include("../include/amqp_client.hrl").
-compile(export_all).

-export([start_link/0,stop/0]).

%% -export([asyncCall/1]).

%-record(state, {}).
-record(state, {conn}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE,stop).

init([]) ->
	process_flag(trap_exit, true),
	P = init(),
	{ok, #state{conn = P}}.

init()->
      %% Start a network connection
      {ok, Connection} = amqp_connection:start(network, #amqp_params{}),
	  io:format("Connection:~p~n",[Connection]),
      %% Open a channel on the connection
      {ok, Channel} = amqp_connection:open_channel(Connection),
%% 	  io:format("Channel:~p~n",[Channel]),

%%	1 input
	  Exchange_Input = <<"siteveiw_input_exchange">>,
	  Queue_Input = <<"siteveiw_input_queue">>,
      BindKey_Input = <<"siteveiw_input.#">>,
	   
       #'queue.declare_ok'{queue = Queue_Input}
                = amqp_channel:call(Channel, #'queue.declare'{queue = Queue_Input}),
	  
       #'exchange.declare_ok'{} 
				= amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange_Input, 
																 type = <<"topic">>}),

       #'queue.bind_ok'{} 
				= amqp_channel:call(Channel, #'queue.bind'{queue = Queue_Input, 
														   exchange = Exchange_Input,  
														   routing_key = BindKey_Input}),
      RoutingKey_Input_Default = <<"siteveiw_input.server">>,
%%     Payload = <<"This is a really interesting message!">>,
%%     [send_message(Channel, Exchange_Input, RoutingKey_Input, Payload) || Tag <- lists:seq(1, 100)],
%% 	   setup_consumer(Channel, Queue_Input).
	  
%%	2 output1
	  Exchange_Output1 = <<"siteveiw_output_exchange1">>,
	  Queue_Output1 = <<"siteveiw_output_queue1">>,
      BindKey_Output1 = <<"siteveiw_output1.#">>,
	   
       #'queue.declare_ok'{queue = Queue_Output1}
                = amqp_channel:call(Channel, #'queue.declare'{queue = Queue_Output1}),
	  
       #'exchange.declare_ok'{} 
				= amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange_Output1, 
																 type = <<"topic">>}),

       #'queue.bind_ok'{} 
				= amqp_channel:call(Channel, #'queue.bind'{queue = Queue_Output1, 
														   exchange = Exchange_Output1,  
														   routing_key = BindKey_Output1}),
      RoutingKey_Output1_Default = <<"siteveiw_output1.server1">>,
	  
%%	3 output2 + consumer	  
	  Exchange_Output2 = <<"siteveiw_output_exchange2">>,
	  Queue_Output2 = <<"siteveiw_output_queue2">>,
      BindKey_Output2 = <<"siteveiw_output2.#">>,
	   
       #'queue.declare_ok'{queue = Queue_Output2}
                = amqp_channel:call(Channel, #'queue.declare'{queue = Queue_Output2}),
	  
       #'exchange.declare_ok'{} 
				= amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange_Output2, 
																 type = <<"topic">>}),

       #'queue.bind_ok'{} 
				= amqp_channel:call(Channel, #'queue.bind'{queue = Queue_Output2, 
														   exchange = Exchange_Output2,  
														   routing_key = BindKey_Output2}),
      RoutingKey_Output2_Default = <<"siteveiw_output2.server2">>,

%%	4 parameter	  
	 P = {{connection, Connection}, {chanel, Channel}, 
		{input_routing_key, RoutingKey_Input_Default}, {input_exchange, Exchange_Input},{input_queue, Queue_Input},
		{output1_routing_key, RoutingKey_Output1_Default}, {output1_exchange, Exchange_Output1},{output1_queue, Queue_Output1},
		{output2_routing_key, RoutingKey_Output2_Default}, {output2_exchange, Exchange_Output2},{output2_queue, Queue_Output2}},
	  
%% 	5 consumer 
%% 	setup_consumer(P),
    ConsumerId = spawn_opt(?MODULE, setup_consumer, [P], [{priority, high}]),
	io:format("queue_server init ConProcId:  ~p ~n", [ConsumerId]),
	P.

%% handle_call({asyncCall, Msg}, _, State) ->
%% 	{state,Param} = State,
%%   {{connection, Connection}, {chanel, Channel}, 
%% 	{input_routing_key, RoutingKey_Input_Default}, {input_exchange, Exchange_Input},{input_queue, Queue_Input},
%% 	{output1_routing_key, RoutingKey_Output1_Default}, {output1_exchange, Exchange_Output1},{output1_queue, Queue_Output1},
%% 	{output2_routing_key, RoutingKey_Output2_Default}, {output2_exchange, Exchange_Output2},{output2_queue, Queue_Output2}} = Param,
%% 	Guid = erlang:binary_to_list(uuid()),
%%   	BasicPublish = #'basic.publish'{exchange = Exchange_Input, routing_key = RoutingKey_Input_Default},
%%   	amqp_channel:cast(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = erlang:list_to_binary("Guid_" ++ Guid ++"_"++Msg)}),
%% 	{reply,Guid,State};
handle_call(Req, _, State) ->
    {reply, {error, {unknown_request, Req}}, State}.

setup_consumer(Param) ->
	{{connection, Connection}, {chanel, Channel}, 
	{input_routing_key, RoutingKey_Input_Default}, {input_exchange, Exchange_Input},{input_queue, Queue_Input},
	{output1_routing_key, RoutingKey_Output1_Default}, {output1_exchange, Exchange_Output1},{output1_queue, Queue_Output1},
	{output2_routing_key, RoutingKey_Output2_Default}, {output2_exchange, Exchange_Output2},{output2_queue, Queue_Output2}} = Param,
	setup_consumer(Param, Channel, Queue_Input).

reply_message(Param, Props, Payload) ->
	{{connection, Connection}, {chanel, Channel}, 
	{input_routing_key, RoutingKey_Input_Default}, {input_exchange, Exchange_Input},{input_queue, Queue_Input},
	{output1_routing_key, RoutingKey_Output1_Default}, {output1_exchange, Exchange_Output1},{output1_queue, Queue_Output1},
	{output2_routing_key, RoutingKey_Output2_Default}, {output2_exchange, Exchange_Output2},{output2_queue, Queue_Output2}} = Param,
	io:format("queue_server Payload received: ~p~n", [erlang:binary_to_term(Payload)]),
	{Guid, Type, MsgContent} = erlang:binary_to_term(Payload),
%% 	{Module, Function, Args} = MsgContent,
%% 	{Module, Function, Args} = erlang:binary_to_term(Payload),
%% 	#'P_basic'{correlation_id = Guid, content_type = _, reply_to = Q} = Props,
	#'P_basic'{content_type = _, reply_to = Q} = Props,
%% 	Result = erlang:term_to_binary(erlang:apply(Module, Function, Args)),
	Result = Payload,
	case Q of
		Queue_Output1 ->
		    BasicPublish1 = #'basic.publish'{exchange = Exchange_Output1, routing_key = RoutingKey_Output1_Default},
    		ok = amqp_channel:cast(Channel, BasicPublish1, _MsgPayload = 
					#amqp_msg{props=#'P_basic'{}, payload = erlang:term_to_binary({Guid,Result})});			
%%     		ok = amqp_channel:cast(Channel, BasicPublish1, _MsgPayload = #amqp_msg{payload = erlang:term_to_binary({Guid,Result})});					
		Queue_Output2 ->
		    BasicPublish2 = #'basic.publish'{exchange = Exchange_Output2, routing_key = RoutingKey_Output2_Default},
%%     		ok = amqp_channel:cast(Channel, BasicPublish2, _MsgPayload = #amqp_msg{payload = erlang:term_to_binary({Guid,Result})});
			ok = amqp_channel:cast(Channel, BasicPublish2, _MsgPayload = 
				#amqp_msg{props=#'P_basic'{}, payload = erlang:term_to_binary({Guid,Result})});		
		_->
		    BasicPublish = #'basic.publish'{exchange = Exchange_Output2, routing_key = RoutingKey_Output2_Default},
%%     		ok = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = Payload})
    		ok = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = 
				#amqp_msg{props=#'P_basic'{}, payload = erlang:term_to_binary({Guid,Result})})			
	end.

close(Param) ->
	{{connection, Connection}, {chanel, Channel}, 
	{input_routing_key, RoutingKey_Input_Default}, {input_exchange, Exchange_Input},{input_queue, Queue_Input},
	{output1_routing_key, RoutingKey_Output1_Default}, {output1_exchange, Exchange_Output1},{output1_queue, Queue_Output1},
	{output2_routing_key, RoutingKey_Output2_Default}, {output2_exchange, Exchange_Output2},{output2_queue, Queue_Output2}} = Param, 
	
	log(channel_close,"start"), 
	ok = amqp_channel:close(Channel),

	log(connection_close,"start"),
    ok = amqp_connection:close(Connection),
    log(connection_close,"Demo Completed!"),
    ok.
	
setup_consumer(Param, Channel, Q) ->
    %% Register a consumer to listen to a queue
    log(setup_consumer,"basic.consume"),
    BasicConsume = #'basic.consume'{queue = Q,
                                    consumer_tag = <<"">>,
                                    no_ack = true},
    #'basic.consume_ok'{consumer_tag = ConsumerTag}
                     = amqp_channel:subscribe(Channel, BasicConsume, self()),

    %% If the registration was sucessful, then consumer will be notified
    log(setup_consumer,"basic.consume_ok start receive"),
    receive
        #'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
    end,
    log(setup_consumer,"basic.consume_ok finished"),

    %% When a message is routed to the queue, it will then be delivered to this consumer
    log(read_messages,"start"),
	read_messages(Param).
%%     Msg = read_messages(Param, 0),
%%     io:format("Msg: ~p~n", [Msg]),
%%     log(read_messages,"finish"),
%% 
%%     %% After the consumer is finished interacting with the queue, it can deregister itself
%%     log(basic_cancel,"start"),
%%     BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag},
%%     #'basic.cancel_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel,BasicCancel).

read_messages(Param) ->
    receive
        {#'basic.deliver'{consumer_tag=_ConsumerTag, delivery_tag=_DeliveryTag, redelivered=_Redelivered, exchange=_Exchange, routing_key=RoutingKey}, Content} ->
%%             log(read_messages,"basic.deliver"),
%%             io:format("RoutingKey received: ~p~n", [RoutingKey]),
            #amqp_msg{props = Props, payload = Payload} = Content,
%%             io:format("Payload received: ~p~n", [Payload]),
			reply_message(Param, Props, Payload),
            read_messages(Param);
        Any ->
            io:format("received unexpected Any: ~p~n", [Any]),
            read_messages(Param)
%%     after 1000 ->
%%         case Timeouts of
%%             0 ->
%%                 Timeouts2 = Timeouts + 1,
%%                 read_messages(Param, Timeouts2);
%%             10000 ->
%%                 io:format("~n"),
%%                 io:format("Message timeout exceeded ~n");
%% %% 				read_messages(0);
%%             _ ->
%%                 Timeouts2 = Timeouts + 1,
%%                 io:format("."),
%%                 read_messages(Param, Timeouts2)
%%         end
    end.

log(Key,Value) ->
    io:format("queue_server:~p: ~p~n",[Key,Value]).

uuid() ->
    {A, B, C} = now(),
    <<A:32, B:32, C:32>>.

handle_cast(stop, State) ->	
%% 	pgsql:close(State#state.conn),
	close(State),
	{stop,normal,State};
handle_cast(_, State) ->	
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.