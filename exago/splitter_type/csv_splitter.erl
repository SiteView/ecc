-module(csv_splitter).
-behaviour(splitter).
-export([split_file/2, split_event/2]).
-export([file_splitter/2, event_splitter/2]).

split_event(Registrar, Streamer) ->
    spawn(csv_splitter, event_splitter, [Registrar, Streamer]).

split_file(Registrar, Streamer) ->
    spawn(csv_splitter, file_splitter, [Registrar, Streamer]).

%% split_file API

%% @doc split a message into fields
event_splitter(Registrar, Streamer) ->
    registrar:advance_line(Registrar, Streamer, self()),
    receive
	{line, Line} ->
	    lists:map(fun (BinaryString) ->
			      binary_to_list(BinaryString)
		      end, re:split(Line, ","));
	_ ->
	    io:format("[SPLITTER] Error occured whilst demanding line\n")
    end.

%% @doc Split a file into rows. This can be used only on file streams.
%% This should never be used for the demand driven side of the system,
%% since data is streamed by line by default anyway. However, in some
%% cases, we want to read in many files efficiently, rather than stream,
%% so this functionality provides the same efficiency as the old exago system.
%% Short answer: If you are live streaming data, use the event_splitter,
%% if you are bulk reading log files, use the file splitter.
file_splitter(Registrar, Streamer) ->
    registrar:advance_file(Registrar, Streamer, self()),
    receive
	{file_contents, FileContents} ->
	    SplitContent = 
		lists:map(fun (BinaryString) ->
				  binary_to_list(BinaryString)
			  end, re:split(FileContents, "\n")),
	    lists:map(fun (BinaryString) ->
			      binary_to_list(BinaryString)
		      end, re:split(lists:flatten(SplitContent), ","));
	_ ->
	    io:format("[SPLITTER] Error occured whilst demanding file contents\n")
    end.

