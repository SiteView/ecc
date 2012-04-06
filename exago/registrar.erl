-module(registrar).
-export([start/0, stop/1]).
-export([register_file/3, list/2, advance_line/3, advance_file/3]).
-export([streamer/2, registrar/1]).

%% Public registrar API
start() ->
    Registrar = spawn(registrar, registrar, [[]]),
    Registrar.

stop(Registrar) ->
    Registrar ! quit.

register_file(Registrar, Path, Options) ->
    register_file_stream(Registrar, Path, Options).

list(Registrar, ClientPid) ->
    Registrar ! {ClientPid, list_registry}.

advance_line(Registrar, Streamer, ClientPid) ->
    Registrar ! {ClientPid, {advance_line, Streamer}}.

advance_file(Registrar, Streamer, ClientPid) ->
    Registrar ! {ClientPid, {advance_until_eof, Streamer}}.

register_file_stream(ServerPid, Path, FileOptions) ->
    case file:open(Path, FileOptions) of
	{ok, IoDevice} ->
	    ClientPid = spawn(registrar, streamer, [ServerPid, IoDevice]),
	    ServerPid ! {add_source, ClientPid},
	    ClientPid;
	{error, Error} ->
	    io:format("[FILE-STREAM] Error spawning file_stream: ~p\n", [Error]),
	    {error, spawning_file_stream}
    end.

%% @doc Responsible for streaming data to a process. At the moment it only works with
%% file streams, but can be extended to support other kinds of streams as well.
streamer(ServerPid, IoDevice) ->
    receive
	%% advance_char is obviously pretty slow and should only be used when the data 
	%% to be read is minimalist, or depends solely on a small number of characters.
	{ClientPid, advance_char} -> 
	    ClientPid ! {char, file:read_char(IoDevice)},
	    streamer(ServerPid, IoDevice);
	{ClientPid, advance_line} ->
	    io:format("[FILE-STREAM] Advancing stream by line\n", []),
	    ClientPid ! {line, file:read_line(IoDevice)},
	    streamer(ServerPid, IoDevice);
	{ClientPid, advance_until_eof} ->
	    io:format("[FILE-STREAM] Advancing stream until eof\n", []),
	    ClientPid ! {file_contents, util:read_file(IoDevice)},
	    file:close(IoDevice),
	    streamer(ServerPid, IoDevice);
	close_stream ->
	    io:format("[FILE-STREAM] Closing file stream\n", []),
	    file:close(IoDevice),
	    streamer(ServerPid, IoDevice);
	quit ->
	    io:format("[FILE-STREAM] Quitting file_streamer process\n", []);
	_Other ->
	    io:format("[FILE-STREAM-ERROR] Received an unrecognized message\n", []),
	    streamer(ServerPid, IoDevice)
    end.

%% @doc The registrar is charged with feeding data in a demand driven way to the rest of
%% the subsystems. 
-spec(registrar/1 :: (list()) -> pid()).
registrar(ProcessRegistry) ->
    receive
	{add_source, Pid}    ->
	    io:format("[REGISTRAR] Adding source process to the registry\n", []),
	    registrar([Pid|ProcessRegistry]);
	{remove_source, Pid} ->
	    io:format("[REGISTRAR] Removing process from the registry\n", []),
	    Pid ! close_stream,
	    Pid ! quit,
	    registrar(lists:delete(Pid, ProcessRegistry));
	{ClientPid, list_registry} ->
	    io:format("[REGISTRAR] Listing files currently known to the registrar\n", []),
	    ClientPid ! {ok, ProcessRegistry},
	    registrar(ProcessRegistry);
    %% The following functions are responsible for asking the file stream for its contents,
    %% and subsequently sending the data back to some arbitrary client process.
	{ClientPid, {advance_char, Pid}} -> 
	    Pid ! {ClientPid, advance_char},
	    registrar(ProcessRegistry);
	{ClientPid, {advance_line, Pid}} ->
	    io:format("[REGISTRAR] Demanding next line from Pid: ~p\n", [Pid]),
	    Pid ! {ClientPid, advance_line},
	    registrar(ProcessRegistry);
	%% This message should only be sent if the Pid refers to a file stream
	{ClientPid, {advance_until_eof, Pid}} -> 
	    io:format("[REGISTRAR] Demanding next file stream to be read directly at Pid: ~p\n", [Pid]),
	    Pid ! {ClientPid, advance_until_eof},
	    registrar(ProcessRegistry);
	{_ClientPid, quit} ->
	    io:format("[REGISTRAR] Quitting\n", []);
	_Other ->
	    io:format("[REGISTRAR-ERROR] Received an unrecognized message\n", []),
	    registrar(ProcessRegistry)
    end.

