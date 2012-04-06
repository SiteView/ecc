-module(esyslog_standard_logger).
-include("sec_log.hrl").
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init(Config) ->
    {ok, Config}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, Msg = {Priority, _Timestamp, _Host, _Tag, _Body}}, Config) ->
    Targets = esyslog_config:get_targets(Priority, Config),
    lists:foreach(
        fun(Target) ->
            log(Msg, Target)
        end,
    Targets),
    {ok, Config};
    
handle_event(Event, State) ->
    ?Log({"catchall: ~p, ~p~n", [Event, State]}),
    {ok, State}.

handle_call(Call, State) ->
    ?Log({"Catchall: ~p, ~p~n", [Call, State]}),
    {ok, State}.

handle_info(Info, State) ->
    ?Log({"Catchall: ~p, ~p~n", [Info, State]}),
    {ok, State}.

log(Msg, {local, Filename}) ->
    %?Log({"Writing locally to ~p~n", [Filename]}),
    {ok, IoDevice} = file:open(Filename, [append]),
    try
        io:put_chars(IoDevice, string:concat(esyslog_message:format(Msg), "\n"))
    after
        file:close(IoDevice)
    end,
    ok;
    
log(Msg, {remote, Remote}) ->
    case Remote of
        {Host, Port} ->
            ?Log({"Writing remotely to ~p port ~p~n", [Host, Port]});
    
        Host ->
           ?Log({"Writing remotely to ~p~n", [Host]})
    end,
    ok.
    
    
    
    