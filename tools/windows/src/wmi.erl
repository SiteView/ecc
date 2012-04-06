-module(wmi).
-behaviour(gen_server).

-include("../include/define.hrl").

-define(TIMEOUT, 60000).

-export([start/0, stop/0]).
-export([cpu/4, memory/4, disk/4, disk/5, service/4, service/5, process/4, process/5, network/4, network/5, directory/7]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

check(Parameters) when is_list(Parameters) andalso length(Parameters) =:= 5->
	case lists:last(Parameters) of
		Last when is_list(Last) andalso length(Last) >= 1 ->
			{ok, "parameter format right"};
		_ ->
			{error, "last parameter error"}
	end;
check(_)->
	{error, "parameter format error"}.

convert([H|R], Acc)->
	case H < 0 of
		true->
			convert(R, [256 + H|Acc]);
		_ ->
			convert(R, [H|Acc])
	end;
convert([], Acc)->
	%lists:reverse(Acc).
	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert(Msg)->
	convert(Msg, []).

start() ->
    iconv:start(),
    case gen_server:start_link({global, wmi}, wmi, self(), []) of
    {ok, Pid} ->
        {ok, Pid};
    Error ->
        Error
    end.

cpu(OS, Host, User, Password)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?CPU]], ?TIMEOUT).

memory(OS, Host, User, Password)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?MEMORY]], ?TIMEOUT).

disk(OS, Host, User, Password)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?DISK]], ?TIMEOUT).

disk(OS, Host, User, Password, Disk)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?DISK, Disk]], ?TIMEOUT).


service(OS, Host, User, Password)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?SERVICE]], ?TIMEOUT).

service(OS, Host, User, Password, Service)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?SERVICE, Service]], ?TIMEOUT).


process(OS, Host, User, Password)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?PROCESS]], ?TIMEOUT).

process(OS, Host, User, Password, Process)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?PROCESS, Process]], ?TIMEOUT).

network(OS, Host, User, Password)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?NETWORK]], ?TIMEOUT).

network(OS, Host, User, Password, Network)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?NETWORK, Network]], ?TIMEOUT).

directory(OS, Host, User, Password, Path, Recursive, Match)->
	gen_server:call({global, wmi}, [OS, Host, User, Password, [?DIRECTORY, string:strip(Path, right, $\\) ++ "\\\\", Recursive, Match]], ?TIMEOUT).

%%----------------------------------------------------------------------
%% Purpose: Close the file. Is performed asynchronously.
%% Returns: ok
%%----------------------------------------------------------------------
stop() ->
    Result = gen_server:cast({global, wmi}, stop),
    %receive 
    %    after 2000 ->
    %        done
    %end,
    catch unlink(wmi),
    Result.

% server functions
init(_) ->
	case wmiproxy:init() of
		ok->
			{ok, wmi};
		Error ->
			Error
        end.

terminate(_Reason, _Pid) ->
    ok.

handle_call(Parameters, _From, State) ->
	case check(Parameters) of
		{ok, _}->
			{Flag, MSG} = wmiproxy:execute(Parameters),
			case os:type() of
				{win32, _}->
					{reply, {Flag, convert(MSG)}, State};
				_ ->
					{reply, {Flag, MSG}, State}
			end;
		Error->
			{reply, Error, State}
	end.

handle_cast(stop, State) ->
    {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State}.

