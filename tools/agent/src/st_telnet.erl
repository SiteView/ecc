
-module(st_telnet).

-compile(export_all).

%% Tool internals
-export([silent_teln_expect/5]).
-export([connect/4,cmd/2]).

-define(username,"login: ").
-define(password,"Password: ").
-define(PRX,"login: |Password: |\\\$ |# |>").
-define(ErrorPRX,"Login incorrect| login incorrect").
-define(TIMEOUT,30000).

-define(RECONNS,3).
-define(RECONN_TIMEOUT,5000).
-define(DEFAULT_TIMEOUT,30000).
-define(DEFAULT_PORT,23).

%~ list_to_string([<<>>], Space,Acc) -> list_to_string([], Space,Acc);
%~ list_to_string([<<>>|R], Space,Acc) ->  list_to_string(R, Space,Acc);
%~ list_to_string([[]], Space,Acc) -> list_to_string([], Space,Acc);
%~ list_to_string([[]|R], Space,Acc) ->  list_to_string(R, Space,Acc);
list_to_string([H], Space,Acc) when is_binary(H) -> list_to_string([], Space,[binary_to_list(H)|Acc]);
list_to_string([H|R], Space,Acc) when is_binary(H) ->  list_to_string(R, Space,[Space,binary_to_list(H)|Acc]);
list_to_string([H], Space,Acc) when is_list(H)  -> list_to_string([], Space,[H|Acc]);
list_to_string([H|R], Space,Acc) when is_list(H) ->  list_to_string(R, Space,[Space,H|Acc]);
list_to_string([H], Space,Acc) when is_atom(H) -> list_to_string([], Space,[atom_to_list(H)|Acc]);
list_to_string([H|R], Space,Acc) when is_atom(H) ->  list_to_string(R, Space,[Space,atom_to_list(H)|Acc]);
list_to_string([H], Space,Acc) -> list_to_string([], Space,[H|Acc]);
list_to_string([H|R], Space,Acc) ->  list_to_string(R, Space,[Space,H|Acc]);
list_to_string([], _,Acc) -> lists:reverse(Acc).
list_to_string(List,Space) ->
  lists:flatten(list_to_string(List,Space,[])).


strip([13|Rest], Acc) ->  strip(Rest, Acc);
strip([10|Rest], Acc) ->  strip(Rest, Acc);
strip([C|Rest], Acc) ->  strip(Rest, [C|Acc]);
strip([], Acc) ->    lists:reverse(Acc).

cmd(Pid,Cmd) ->
   st_telnet_client:send_data(Pid,Cmd),
   case teln_receive_until_prompt(Pid,?PRX,?TIMEOUT) of
       {ok,Data,PromptType,_} ->   
	  %~ io:format("teln_receive_until_prompt:~p~n",[{Data,Cmd}]),      
          Newdata =  case strip(hd(Data),[]) of
			Command when  Command =:= Cmd -> lists:nthtail(1, Data);			
			_ -> Data
		     end,
	  case Newdata of
		  [] -> {ok,[]};
		  _ ->
			  Taildata = lists:nth(length(Newdata), Newdata),
			  FinalData = case string:str(Taildata,PromptType) of
				0 ->  Newdata;
				_ -> lists:sublist(Newdata,1,length(Newdata)-1)
			  end,	  
			  {ok,lists:flatten(list_to_string(FinalData,"\r\n"))}
          end;		  
       Result -> Result
   end.
   
sleep(Time)->
    receive 
	after Time ->   ok
    end.   

%%%-----------------------------------------------------------------
%%% @hidden
%%% @spec connect(Ip,Port,Timeout,Extra) -> {ok,Handle} | {error,Reason}
%%%      Ip = string() | {integer(),integer(),integer(),integer()}
%%%      Port = integer()
%%%      Username = string()
%%%      Password = string()
%%%      Handle = ct_telnet:handle()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Setup telnet connection to a UNIX host.</p>
connect(Ip,Port,Username,Password)  -> connect(Ip,Port,?TIMEOUT,Username,Password).
    

connect(Ip,Port,Timeout,Username,Password) ->
	case st_telnet_client:open(Ip,Port,Timeout) of
	    {ok,Pid} ->	
		case st_telnet:silent_teln_expect(Pid,[],[prompt],?PRX,[]) of
		    {ok,{prompt,?username},_} ->		        
			ok = st_telnet_client:send_data(Pid,Username),			
			case st_telnet:silent_teln_expect(Pid,[],prompt,?PRX,[]) of
			    {ok,{prompt,?password},_} ->
				ok = st_telnet_client:send_data(Pid,Password),
				%~ Stars = lists:duplicate(length(Password),$*),				
				ok = st_telnet_client:send_data(Pid,""),
				case st_telnet:silent_teln_expect(Pid,[],prompt,?PRX,[]) of
				    {ok,{prompt,Prompt},_}  when Prompt=/=?username, Prompt=/=?password -> {ok,Pid};
				    {ok,{prompt,_},_} -> {ok,Pid};
				    Error ->st_telnet_client:close(Pid),Error
				end;
			    {ok,{prompt,_},_} -> {ok,Pid};
			    Error ->st_telnet_client:close(Pid), Error
			end;
		    {ok,[{prompt,_OtherPrompt1},{prompt,_OtherPrompt2}],_} -> {ok,Pid};
		    Error -> st_telnet_client:close(Pid),Error
		end;
	    Error -> Error
	end.



%% Expect options record
-record(eo,{teln_pid,
	    prx,
	    timeout,
	    haltpatterns=[],
	    seq=false,
	    repeat=false,
	    found_prompt=false}).

%% @hidden
%% @doc Externally the silent_teln_expect function shall only be used
%% by the TargetModule, i.e. the target specific module which
%% implements connect/2 and get_prompt_regexp/0.
silent_teln_expect(Pid,Data,Pattern,Prx,Opts) ->
    Old = put(silent,true),    
    Result = teln_expect(Pid,Data,Pattern,Prx,Opts),
    put(silent,Old),
    Result.

%% teln_expect/5 
%%
%% This function implements the expect functionality over telnet. In
%% general there are three possible ways to go:
%% 1) Single: One or more patterns are given, and the function return
%% when one of the patterns are matched.
%% 2) Sequence: Several patterns are given, and they are matched in
%% the order they appear in the pattern list.
%% 3a) Repeat (single): 1) is repeated either N times or until a halt
%% condition is fullfilled.
%% 3b) Repeat (sequence): 2) is repeated either N times or until a
%% halt condition is fullfilled.
teln_expect(Pid,Data,Pattern0,Prx,Opts) -> 
    HaltPatterns = case get_ignore_prompt(Opts) of 
	    true -> get_haltpatterns(Opts); 
	    false -> [prompt | get_haltpatterns(Opts)]
	    end,

    Seq = get_seq(Opts),
    Pattern = convert_pattern(Pattern0,Seq),

    Timeout = get_timeout(Opts),
 
    EO = #eo{teln_pid=Pid,
	     prx=Prx,
	     timeout=Timeout,
	     seq=Seq,
	     haltpatterns=HaltPatterns},
    
    case get_repeat(Opts) of
	false ->
	    case teln_expect1(Data,Pattern,[],EO) of
		{ok,Matched,Rest} ->
		    {ok,Matched,Rest};
		{halt,Why,Rest} ->
		    {error,Why,Rest};
		{error,Reason} ->
		    {error,Reason}
	    end;
	N ->
	    EO1 = EO#eo{repeat=N},
	    repeat_expect(Data,Pattern,[],EO1)
    end.

convert_pattern(Pattern,Seq) 
  when is_list(Pattern) and not is_integer(hd(Pattern)) ->
    case Seq of
	true -> Pattern;
	false -> rm_dupl(Pattern,[])
    end;
convert_pattern(Pattern,_Seq) ->
    [Pattern].

rm_dupl([P|Ps],Acc) ->
    case lists:member(P,Acc) of
	true ->
	    rm_dupl(Ps,Acc);
	false ->
	    rm_dupl(Ps,[P|Acc])
    end;
rm_dupl([],Acc) ->
    lists:reverse(Acc).

get_timeout(Opts) ->
    case lists:keysearch(timeout,1,Opts) of
	{value,{timeout,T}} -> T;
	false -> ?DEFAULT_TIMEOUT
    end.
get_repeat(Opts) ->
    case lists:keysearch(repeat,1,Opts) of
	{value,{repeat,N}} when is_integer(N) ->
	    N;
	false ->
	    case lists:member(repeat,Opts) of
		true ->
		    -1;
		false ->
		    false
	    end
    end.
get_seq(Opts) ->
    lists:member(sequence,Opts).
get_haltpatterns(Opts) ->
    case lists:keysearch(halt,1,Opts) of
	{value,{halt,HaltPatterns}} ->
	    convert_pattern(HaltPatterns,false);
	false ->
	    []
    end.
get_ignore_prompt(Opts) ->    
    lists:member(ignore_prompt,Opts).
	
%% Repeat either single or sequence. All match results are accumulated
%% and returned when a halt condition is fulllfilled.
repeat_expect(Rest,_Pattern,Acc,#eo{repeat=0}) ->
    {ok,lists:reverse(Acc),done,Rest};
repeat_expect(Data,Pattern,Acc,EO) ->
    case teln_expect1(Data,Pattern,[],EO) of
	{ok,Matched,Rest} ->
	    EO1 = EO#eo{repeat=EO#eo.repeat-1},
	    repeat_expect(Rest,Pattern,[Matched|Acc],EO1);
	{halt,Why,Rest} ->
	    {ok,lists:reverse(Acc),Why,Rest};
	{error,Reason} ->
	    {error,Reason}
    end.

teln_expect1(Data,Pattern,Acc,EO) ->
    
    ExpectFun = case EO#eo.seq of
		    true -> fun() -> seq_expect(Data,Pattern,Acc,EO) end;
		    false -> fun() -> one_expect(Data,Pattern,EO) end
		end,
    case ExpectFun() of
	{match,Match,Rest} ->
	    {ok,Match,Rest};
	{halt,Why,Rest} ->
	    {halt,Why,Rest};
	NotFinished ->
	    %% Get more data
	    Fun = fun() -> get_data1(EO#eo.teln_pid) end,
	    case ct_gen_conn:do_within_time(Fun, EO#eo.timeout) of
		{error,Reason} -> 
		    %% A timeout will occur when the telnet connection
		    %% is idle for EO#eo.timeout milliseconds.
		    {error,Reason};
		{ok,Data1} ->
		    case NotFinished of
			{nomatch,Rest} ->
			    %% One expect
			    teln_expect1(Rest++Data1,Pattern,[],EO);
			{continue,Patterns1,Acc1,Rest} ->
			    %% Sequence
			    teln_expect1(Rest++Data1,Patterns1,Acc1,EO)
		    end
	    end
    end.

get_data1(Pid) ->
    case st_telnet_client:get_data(Pid) of
	{ok,[]} ->
	    get_data1(Pid);
	{ok,Data} ->
	    {ok,Data}
    end.

%% 1) Single expect.
%% First the whole data chunk is searched for a prompt (to avoid doing
%% a regexp match for the prompt at each line).
%% If we are searching for anyting else, the datachunk is split into
%% lines and each line is matched against each pattern.

%% one_expect: split data chunk at prompts
one_expect(Data,Pattern,EO) ->
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    case Pattern of 
		[Prompt] when Prompt==prompt; Prompt=={prompt,PromptType} ->
		    %% Only searching for prompt
		    log_lines(UptoPrompt),		   
		    {match,{prompt,PromptType},Rest};
		[{prompt,_OtherPromptType}] ->
		    %% Only searching for one specific prompt, not thisone
		    log_lines(UptoPrompt),
		    {nomatch,Rest};
		_ ->
		    one_expect1(UptoPrompt,Pattern,Rest,
				EO#eo{found_prompt=PromptType})
	    end;
	noprompt ->
	    case Pattern of
		[Prompt] when Prompt==prompt; element(1,Prompt)==prompt ->
		    %% Only searching for prompt
		    LastLine = log_lines_not_last(Data),
		    {nomatch,LastLine};
		_ ->
		    one_expect1(Data,Pattern,[],EO#eo{found_prompt=false})
	    end
    end.

remove_zero(List) ->
    [Ch || Ch <- List, Ch=/=0, Ch=/=13].

%% one_expect1: split data chunk at lines
one_expect1(Data,Pattern,Rest,EO) ->
    case match_lines(Data,Pattern,EO) of
	{match,Match,MatchRest} ->
	    {match,Match,MatchRest++Rest};
	{nomatch,prompt} ->
	    one_expect(Rest,Pattern,EO);
	{nomatch,NoMatchRest} ->
	    {nomatch,NoMatchRest++Rest};
	{halt,Why,HaltRest} ->
	    {halt,Why,HaltRest++Rest}
    end.
    

%% 2) Sequence.
%% First the whole data chunk is searched for a prompt (to avoid doing
%% a regexp match for the prompt at each line).
%% If we are searching for anyting else, the datachunk is split into
%% lines and each line is matched against the first pattern in the list.
%% When a match is found, the match result is accumulated, and we keep
%% searching for the next pattern in the list.

%% seq_expect: Split data chunk at prompts
seq_expect(Data,[],Acc,_EO) ->
    {match,lists:reverse(Acc),Data};
seq_expect([],Patterns,Acc,_EO) ->
    {continue,Patterns,lists:reverse(Acc),[]};
seq_expect(Data,Patterns,Acc,EO) ->
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    seq_expect1(UptoPrompt,Patterns,Acc,Rest,
			EO#eo{found_prompt=PromptType});
	noprompt ->
	    seq_expect1(Data,Patterns,Acc,[],EO#eo{found_prompt=false})
    end.

%% seq_expect1: For one prompt-chunk, match each pattern - line by
%% line if it is other than the prompt we are seaching for.
seq_expect1(Data,[prompt|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Data),
	    %% Rest==[] because no prompt is found
	    {continue,[prompt|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Data),	    
	    seq_expect(Rest,Patterns,[{prompt,PromptType}|Acc],EO)
    end;
seq_expect1(Data,[{prompt,PromptType}|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Data),
	    %% Rest==[] because no prompt is found
	    {continue,[{prompt,PromptType}|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Data),	   
	    seq_expect(Rest,Patterns,[{prompt,PromptType}|Acc],EO);
	_OtherPromptType ->
	    log_lines(Data),
	    seq_expect(Rest,[{prompt,PromptType}|Patterns],Acc,EO)
    end;
seq_expect1(Data,[Pattern|Patterns],Acc,Rest,EO) ->
    case match_lines(Data,[Pattern],EO) of
	{match,Match,MatchRest} ->
	    seq_expect1(MatchRest,Patterns,[Match|Acc],Rest,EO);
	{nomatch,prompt} ->
	    seq_expect(Rest,[Pattern|Patterns],Acc,EO);
	{nomatch,NoMatchRest} when Rest==[] ->
	    %% The data did not end with a prompt
	    {continue,[Pattern|Patterns],Acc,NoMatchRest};
	{halt,Why,HaltRest} ->
	    {halt,Why,HaltRest++Rest}
    end;
seq_expect1(Data,[],Acc,Rest,_EO) ->
    {match,lists:reverse(Acc),Data++Rest}.

%% Split prompt-chunk at lines
match_lines(Data,Patterns,EO) ->
    FoundPrompt = EO#eo.found_prompt,
    case one_line(Data,[]) of
	{noline,Rest} when FoundPrompt=/=false ->
	    %% This is the line including the prompt
	    case match_line(Rest,Patterns,FoundPrompt,EO) of
		nomatch ->
		    {nomatch,prompt};
		{Tag,Match} ->
		    {Tag,Match,[]}
	    end;
	{noline,Rest} ->
	    {nomatch,Rest};
	{Line,Rest} ->
	    case match_line(Line,Patterns,false,EO) of
		nomatch ->
		    match_lines(Rest,Patterns,EO);
		{Tag,Match} ->
		    {Tag,Match,Rest}
	    end
    end.
    

%% For one line, match each pattern
match_line(Line,Patterns,FoundPrompt,EO) ->
    match_line(Line,Patterns,FoundPrompt,EO,match).

match_line(Line,[prompt|Patterns],false,EO,RetTag) ->
    match_line(Line,Patterns,false,EO,RetTag);
match_line(_Line,[prompt|_Patterns],FoundPrompt,_EO,RetTag) ->
    {RetTag,{prompt,FoundPrompt}};
match_line(_Line,[{prompt,PromptType}|_Patterns],FoundPrompt,_EO,RetTag) 
  when PromptType==FoundPrompt ->
    {RetTag,{prompt,FoundPrompt}};
match_line(Line,[{prompt,PromptType}|Patterns],FoundPrompt,EO,RetTag) 
  when PromptType=/=FoundPrompt ->
    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
match_line(Line,[{Tag,Pattern}|Patterns],FoundPrompt,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
	{match,Match} ->
	    {RetTag,{Tag,Match}}
    end;
match_line(Line,[Pattern|Patterns],FoundPrompt,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
	{match,Match} ->
	    {RetTag,Match}
    end;
match_line(Line,[],FoundPrompt,EO,match) ->
    match_line(Line,EO#eo.haltpatterns,FoundPrompt,EO,halt);
match_line(_Line,[],_FoundPrompt,_EO,halt) ->
    nomatch.

one_line([$\n|Rest],Line) ->
    {lists:reverse(Line),Rest};
one_line([$\r|Rest],Line) ->
    one_line(Rest,Line);
one_line([0|Rest],Line) ->
    one_line(Rest,Line);
one_line([Char|Rest],Line) ->
    one_line(Rest,[Char|Line]);
one_line([],Line) ->
    {noline,lists:reverse(Line)}.

debug_log_lines(String) ->
    Old = put(silent,true),
    log_lines(String),
    put(silent,Old).

log_lines(String) ->
    case log_lines_not_last(String) of
	[] ->
	    ok;
	_LastLine -> ok
    end.

log_lines_not_last(String) ->
    case add_tabs(String,[],[]) of
	{[],LastLine} ->
	    LastLine;
	{_String1,LastLine} ->
	    LastLine
    end.

add_tabs([0|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,LastLine);
add_tabs([$\r|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,LastLine);
add_tabs([$\n|Rest],Acc,LastLine) ->
    add_tabs(Rest,[$\n|LastLine] ++ [$\s,$\s,$\s,$\s,$\s,$\s,$\s|Acc],[]);
add_tabs([Ch|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,[Ch|LastLine]);
add_tabs([],[$\n|Acc],LastLine) ->
    {lists:reverse(Acc),lists:reverse(LastLine)};
add_tabs([],[],LastLine) ->
    {[],lists:reverse(LastLine)}.


backspace([], Acc) ->    lists:reverse(Acc);
backspace([8|Rest], Acc) ->  Backspace =  lists:nthtail(1,Acc),  backspace(Rest, Backspace);
backspace([C|Rest], Acc) ->  backspace(Rest, [C|Acc]).

%%% @hidden
teln_receive_until_prompt(Pid,Prx,Timeout) ->
    Fun = fun() -> teln_receive_until_prompt(Pid,Prx,[],[]) end,
    ct_gen_conn:do_within_time(Fun, Timeout).

teln_receive_until_prompt(Pid,Prx,Acc,LastLine) ->
    {ok,Data} = st_telnet_client:get_data(Pid),
    Newdata = backspace(Data,[]),
    case check_for_prompt(Prx,LastLine ++ Newdata) of
	{prompt,Lines,PromptType,Rest} ->
	    Return = lists:reverse(lists:append([Lines|Acc])),
	   {ok,Return,PromptType,Rest};
	{noprompt,Lines,LastLine1} ->
	    %~ io:format("Cmd:~p~n",[Newdata]),
	    case string:rstr(Newdata,"Siteview_Ready") of
		0 -> teln_receive_until_prompt(Pid,Prx,[Lines|Acc],LastLine1);
		Pos  ->  {ok,[lists:sublist(Newdata,1,Pos-1)],"",""}
	    end
   end.

check_for_prompt(Prx,Data) ->
    case match_prompt(Data,Prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    {RevLines,LastLine} = split_lines(UptoPrompt),
	    {prompt,[LastLine|RevLines],PromptType,Rest};
	noprompt ->
	    {RevLines,Rest} = split_lines(Data),
	    {noprompt,RevLines,Rest}
    end.

split_lines(String) ->
    split_lines(String,[],[]).
split_lines([$\n|Rest],Line,Lines) ->
    split_lines(Rest,[],[lists:reverse(Line)|Lines]);
split_lines([$\r|Rest],Line,Lines) ->
    split_lines(Rest,Line,Lines);
split_lines([0|Rest],Line,Lines) ->
    split_lines(Rest,Line,Lines);
split_lines([Char|Rest],Line,Lines) ->
    split_lines(Rest,[Char|Line],Lines);
split_lines([],Line,Lines) ->
    {Lines,lists:reverse(Line)}.


match_prompt(Str,Prx) ->
    match_prompt(Str,Prx,[]).
match_prompt(Str,Prx,Acc) ->
    case re:run(Str,Prx) of
	nomatch ->
	    noprompt;
	{match,[{Start,Len}]} ->
	    case split_prompt_string(Str,Start+1,Start+Len,1,[],[]) of
		{noprompt,Done,Rest} ->
		    match_prompt(Rest,Prx,Done);
		{prompt,UptoPrompt,Prompt,Rest} ->
		    {prompt,lists:reverse(UptoPrompt++Acc),
		     lists:reverse(Prompt),Rest}
	    end
    end.

split_prompt_string([Ch|Str],Start,End,N,UptoPrompt,Prompt) when N<Start ->
    split_prompt_string(Str,Start,End,N+1,[Ch|UptoPrompt],Prompt);
split_prompt_string([Ch|Str],Start,End,N,UptoPrompt,Prompt) 
  when N>=Start, N<End->
    split_prompt_string(Str,Start,End,N+1,UptoPrompt,[Ch|Prompt]);
split_prompt_string([Ch|Rest],_Start,End,N,UptoPrompt,Prompt) when N==End ->
    case UptoPrompt of
	[$",$=,$T,$P,$M,$O,$R,$P|_] ->
	    %% This is a line from "listenv", it is not a real prompt
	    {noprompt,[Ch|Prompt]++UptoPrompt,Rest};
	[$\s,$t,$s,$a|_] when Prompt==":nigol" ->
	    %% This is probably the "Last login:" statement which is
	    %% written when telnet connection is openend.
	    {noprompt,[Ch|Prompt]++UptoPrompt,Rest};
	_ ->
	    {prompt,[Ch|Prompt]++UptoPrompt,[Ch|Prompt],Rest}
    end.
