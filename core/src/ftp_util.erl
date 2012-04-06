-module(ftp_util).
-compile(export_all).

%%client receive child process function
ftp_util_listen_transfer_connect(Port, Pid, Timeout, RemoteFileSize)->
    proc_lib:init_ack(Pid, {ok, self()}),
	%%listen to one port
    case gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr,true}, {active, true}]) of 
        {ok, ListenSocket}->
			%%listen ok, inform the parent process
            Pid ! {listen, Port},
			%%wait for server to connect
            case gen_tcp:accept(ListenSocket, Timeout) of
                {ok, Socket}->
					%%server come now, set the socket to reuse mode and stop the listen socket
                    inet:setopts(Socket, [{reuseaddr, true}]),
                    gen_tcp:close(ListenSocket),
					%%receive the file use child socket
                    case ftp_util_receive_socket(Socket, [], Timeout, RemoteFileSize, 0) of
						%%receive ok
                        {ok, Bin}->
                            gen_tcp:close(Socket),
                            Pid ! {ok, Bin};
						%%transfer stop  by ftp server
                        {error, "stop"}->
                            gen_tcp:close(Socket);
						%%other error	
                        {error, Reason}->
                            gen_tcp:close(Socket),
                            Pid ! {error, Reason}
                   end;
                {error, Reason}->
                    gen_tcp:close(ListenSocket),
                    Pid ! {error, Reason}
            end;
		%%if listen error choose other port and try to receive again
        {error, _}->
            ftp_util_listen_transfer_connect(Port + 1, Pid, Timeout, RemoteFileSize)
    end.

%%receive the data
ftp_util_receive_socket(Socket, Data, Timeout, RemoteFileSize, CurrentSize)->
    receive
        {tcp, Socket, Bin}->
            Sofar = [Bin|Data],
            RecvLen = size(Bin) + CurrentSize,
            if
                 RecvLen =:= RemoteFileSize ->
                    BFile = list_to_binary(lists:reverse(Sofar)),
                    {ok, BFile};
                true ->
                    ftp_util_receive_socket(Socket, Sofar, Timeout, RemoteFileSize, RecvLen)
            end;
        {tcp_closed, Socket}->
            {error, "tcp closed"};
        {stop}->
            {error, "stop"}
    after Timeout->
        {error, "timeout"}
    end.
	