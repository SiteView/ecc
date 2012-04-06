% Implementation of syslog server protocol (RFC3164)

-module(esyslog).
-behavior(application).
-export([
    start/0,
    start/2, 
    stop/1
]).

-define(UDP_OPTIONS, [binary, {active, false}]).

start() ->
    crypto:start(),   
    application:start(?MODULE).

start(_Type, _Args) ->
    io:format("Starting application~n"),
    esyslog_supervisor:start_link().

stop(_State) ->
    kvs_main:stop(),
    ok.



