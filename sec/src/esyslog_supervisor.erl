-module(esyslog_supervisor).
-behaviour(supervisor).
-include("sec_log.hrl").
-export([start_link/0]).
-export([init/1]).


start_link() ->
    io:format("Starting supervisor~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, Port} = application:get_env(esyslog, port),
    regex:start(),
    iconv:start(),
    Policy = {one_for_one, 1, 60},
    EventManager = {esyslog_event_manager, 
                        {esyslog_event_manager, start, []},
                        permanent, brutal_kill, worker, dynamic},
    Server = {esyslog_server, 
                {esyslog_server, start, [Port]},
                permanent, brutal_kill, worker, [esyslog_server]},
                
    sec_config:init(),
    license:start(),
    kvs_main:start(),
    
    {ok, {Policy, [EventManager, Server]}}.


	