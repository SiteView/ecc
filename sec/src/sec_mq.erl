-module(sec_mq).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").
-include("xmerl.hrl").
-compile(export_all).


utc_random() ->
    Now = {_, _, Micro} = now(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix).

new_uuid() ->
    to_hex(crypto:rand_bytes(16)).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.


nowseconds() ->
    {_, _, Micro} = now(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    NowSeconds * 1000000 + Micro.

insert(Record) ->
    NowSeconds = nowseconds(),
	New_Key = list_to_atom(new_uuid()),
	ets:insert(siteview_syslog_mq, {New_Key,{NowSeconds,Record}}).
    
getall() -> 
    Data = ets:tab2list(siteview_syslog_mq),
    Fun = fun(X, Y)-> element(1,element(2, X)) > element(1,element(2, Y)) end,
    Sortdata = lists:sort(Fun, Data),
    Sortdata.
   
getlastest() ->
    getlastest(getall()).

getlastest([]) -> [];
getlastest(All) -> hd(All).


getlastestbyid(ID) -> getlastestbyid(getall(),ID,[]).

getlastestbyid([{ID,_}|_],ID,Acc) -> lists:reverse(Acc);
getlastestbyid([{Key,Value}|L],ID,Acc) ->  getlastestbyid(L,ID,[{Key,Value}|Acc]);
getlastestbyid([_|L],ID,Acc) ->  getlastestbyid(L,ID,Acc);
getlastestbyid([],_,Acc) -> lists:reverse(Acc).


