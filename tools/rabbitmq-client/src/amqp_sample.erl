-module(amqp_sample).
-compile([export_all]).

-include("amqp_client.hrl").



send() ->
    Msg = {ok,"Hello World!"},
    send(Msg).
    
send(Msg) ->
    Binary = term_to_binary(Msg),
    {ok, Connection} = amqp_connection:start(network,
                                             #amqp_params{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"siteview.ecc">>}),

    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = <<"">>,
                        routing_key = <<"siteview.ecc">>},
                      #amqp_msg{payload = Binary}),
    io:format("MQ Sent '~p'~n",[Msg]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.    
    
rec() ->
    {ok, Connection} = amqp_connection:start(network,
                                             #amqp_params{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"siteview.ecc">>}),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"siteview.ecc">>,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).


loop(Channel) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format("MQ Received ~p~n", [binary_to_term(Body)]),
            loop(Channel)
    end.    
