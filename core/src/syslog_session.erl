-module(syslog_session, []).
-compile(export_all).

new()-> 
	Obj = instance(),
	Obj.
    
%% 发送syslog协议包
send_sysloginfo(Host, Port, Facility, Program, Level, Message) ->
    syslog_agent:start(),
    try syslog_agent:send(Host, Port, Facility,Program,Level,Message) of
        {ok, Result} ->
            {ok, success};
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.