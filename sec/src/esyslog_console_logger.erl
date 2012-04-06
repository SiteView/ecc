-module(esyslog_console_logger).
-include("sec_log.hrl").
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, {Priority, Timestamp, Host, Tag, Body}}, State) ->
    %?Log({"Logging: ~p ~p ~p ~p ~p~n", [Priority, Timestamp, Host, Tag, Body]}),
    {Facility, Severity} = esyslog_message:decode_priority(Priority),
    sec_decoder:decode(Facility, Severity, Timestamp, Host, Tag, Body),
    %io:format("Logging: ~p ~p ~p ~p ~p~n", [Priority, Timestamp, Host, Tag, Body]),
    {ok, State};
    
handle_event(Event, State) ->
    ?Log({"catchall: ~p, ~p~n", [Event, State]}),
    {ok, State}.

handle_call(Call, State) ->
    ?Log({"catchall: ~p, ~p~n", [Call, State]}),
    {ok, State}.

handle_info(Info, State) ->
   ?Log({"catchall: ~p, ~p~n", [Info, State]}),
    {ok, State}.

