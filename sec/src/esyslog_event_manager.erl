-module(esyslog_event_manager).
-export([start/0]).

start() ->
    {ok, Config} = esyslog_config:parse(verifyetcpath()),
    io:format("Starting event manager~n"),
    {ok, Pid} = gen_event:start({local, esyslog_logger}),
    gen_event:add_handler(esyslog_logger, esyslog_console_logger, []),
    gen_event:add_handler(esyslog_logger, esyslog_standard_logger, Config),
    {ok, Pid}.

 
 
verifyetcpath() ->
    case filelib:is_file("etc/syslog.conf") of
         true -> "etc/syslog.conf";
         _ ->
         case filelib:is_file("sec/etc/syslog.conf") of
             true -> "sec/etc/syslog.conf";
             _ -> "etc/syslog.conf"           
         end
    end.