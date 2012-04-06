% Implementation of syslog server protocol (RFC3164)

-module(esyslog_server).
-behaviour(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/1]).

start(Port) ->
    io:format("Starting server~n"),
    Ipaddress = case application:get_env(esyslog, ipaddress) of
                    {ok, Address} -> 
                        case inet_parse:address(Address) of
                            {ok,Parseaddress} -> Parseaddress;
                            _ ->  {0,0,0,0}
                        end;
                    _ ->  {0,0,0,0}
                end, 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Ipaddress,Port], []).

init([Ipaddress,Port]) ->
    io:format("Opening socket at ipaddress ~p and port ~p~n", [Ipaddress,Port]),
    listen_loop({Ipaddress,Port}).

strip(Value) ->
    NewValue = string:strip(Value,right,$\n),
    string:strip(NewValue,right).

listen_loop({Ipaddress,Port}) ->
    try gen_udp:open(Port, [binary, {active, false},{ip,Ipaddress}]) of    
        {ok, Socket} -> 
            case gen_udp:recv(Socket, 0) of
                {ok, {IP, _, Data}} ->
                    %io:format("~p~n",[{IP,Data}]),
                    Message = case esyslog_message:parse(IP,strip(binary_to_list(Data))) of
                          bad_message -> bad_message;
                          Result -> {log,Result}
                    end,
                    gen_event:notify(esyslog_logger, Message);
                Other ->
                    io:format("Nope: ~p~n", [Other])
            end,
    
            gen_udp:close(Socket),
        
            listen_loop({Ipaddress,Port})
    catch
        error:Error ->
            io:format("Error: ~p~n", [Error]),
            {ok, Port}
    end.

handle_cast(Cast, State) ->
    io:format("~p catchall: ~p, ~p~n", [?MODULE, Cast, State]),
    {ok, State}.

handle_call({logged}, _Caller, Ipaddress_Port) ->
    listen_loop(Ipaddress_Port).

handle_info(Info, State) ->
    io:format("~p catchall: ~p, ~p~n", [?MODULE, Info, State]),
    {ok, State}.

terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

