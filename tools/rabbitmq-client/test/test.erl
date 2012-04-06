
-module(test).


-include("amqp_client.hrl").

-compile([export_all]).


-record(publish, {q, x, routing_key, bind_key, payload,
                  mandatory = false, immediate = false}).
	

test() ->
          %% Start a network connection
          {ok, Connection} = amqp_connection:start(network, #amqp_params{}),
          %% Open a channel on the connection
          {ok, Channel} = amqp_connection:open_channel(Connection),

          %% Declare a queue
          #'queue.declare_ok'{queue = Q}
              = amqp_channel:call(Channel, #'queue.declare'{}),
	      
          io:format("Q:~p~n",[Q]),
	  
          %% Publish a message
          Payload = <<"foobar">>,
          Publish = #'basic.publish'{exchange = <<>>, routing_key = Q},
          amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

          %% Get the message back from the queue
          Get = #'basic.get'{queue = Q},
          {#'basic.get_ok'{delivery_tag = Tag}, Content}
               = amqp_channel:call(Channel, Get),

          %% Do something with the message payload
          %% (some work here)
	  io:format("Content:~p~n",[Content]),

          %% Ack the message
          amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

          %% Close the channel
          amqp_channel:close(Channel),
          %% Close the connection
          amqp_connection:close(Connection),

          ok.

		  
start() ->
%~ username	guest
%~ password	guest
%~ virtual_host	/
%~ host	localhost
%~ port	5672
%~ node	the current node - relevant only for the direct type of connection
%~ channel_max	0
%~ frame_max	0
%~ heartbeat	0
%~ ssl_options	none
%~ auth_mechanisms	[fun amqp_auth_mechanisms:plain/3, fun amqp_auth_mechanisms:amqplain/3]
%~ client_properties	[ ]

    Result = basic_ack_test(new_connection()),
    io:format("basic_ack_test:~p~n",[Result]),
    ok.
    
new_connection() ->
    new_connection(#amqp_params{}).

new_connection(AmqpParams) ->
    case amqp_connection:start(network, AmqpParams) of {ok, Conn}     -> Conn;
                                                       {error, _} = E -> E
    end.    
    
basic_ack_test(Connection) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Q} = setup_publish(Channel),
    {#'basic.get_ok'{delivery_tag = Tag}, _}
        = amqp_channel:call(Channel, #'basic.get'{queue = Q, no_ack = false}),
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    teardown(Connection, Channel).    
    
setup_publish(Channel) ->
    Publish = #publish{routing_key = <<"a.b.c.d">>,
                       q = uuid(),
                       x = uuid(),
                       bind_key = <<"a.b.c.*">>,
                       payload = <<"foobar">>},
    setup_publish(Channel, Publish).

setup_publish(Channel, #publish{routing_key = RoutingKey,
                                q = Q, x = X,
                                bind_key = BindKey,
                                payload = Payload}) ->
    ok = setup_exchange(Channel, Q, X, BindKey),
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    ok = amqp_channel:call(Channel, Publish, #amqp_msg{payload = Payload}),
    {ok, Q}.


setup_exchange(Channel, Q, X, Binding) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = X,
                                                   type = <<"topic">>}),
    amqp_channel:call(Channel, #'queue.declare'{queue = Q}),
    Route = #'queue.bind'{queue = Q,
                          exchange = X,
                          routing_key = Binding},
    amqp_channel:call(Channel, Route),
    ok.
    
    
teardown(Connection, Channel) ->
    amqp_channel:close(Channel),
    wait_for_death(Channel),
    amqp_connection:close(Connection),
    wait_for_death(Connection).
    
wait_for_death(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 1000 -> exit({timed_out_waiting_for_process_death, Pid})
    end.
    
uuid() ->
    {A, B, C} = now(),
    <<A:32, B:32, C:32>>.    
    