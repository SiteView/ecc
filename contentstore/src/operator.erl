-module(operator).
-compile(export_all).

%L ->数据记录的值
%R ->查询条件的值
    
always(_, _)->true.

lessE(L, R) -> L =< R.
less(L,R) -> L < R.

equal(L, R) -> L =:= R.
nequal(L, R) -> L =/= R.

greater(L, R)-> L > R.
greaterE(L,R) -> L >= R.

eic(<<LC, LR/binary>>, <<RC, RR/binary>>)->
    case string:to_upper(LC) =:= RC of
            true ->
                eic(LR, RR);
            _ ->
                false
    end;
%此条件子句主要是为了匹配如null之类的极端条件
eic(L, R)->L =:= R.
    
neic(L, R) -> not eic(L,R).

%in(_, null)-> false;
%这种情况不会出现，因为R参数为查询条件，解析之后最多为一个空的List
in(L, R)-> lists:member(L, R).

%skipSpace(<<32, Rest/binary>>)->
%    skipSpace(Rest);
%skipSpace(Rest)->
%    Rest.
%
%nextWord(<<32, Rest/binary>>)->
%    skipSpace(Rest);
%nextWord(<<_, Rest/binary>>)->
%    nextWord(Rest);
%nextWord(<<>>)->
%    <<>>.

%sublike(<<C, LRest/binary>>, R, <<C, RRest/binary>>)->
%    sublike(LRest, R, RRest);
%sublike(<<32, LRest/binary>>, R, <<_, _/binary>>)->
%    binarylike(skipSpace(LRest), R);
%sublike(<<_, LRest/binary>>, R, <<_, _/binary>>)->
%    binarylike(nextWord(LRest), R);
%sublike(<<32, _/binary>>, _, <<>>)->
%    true;
%sublike(<<>>, _, <<>>)->
%    true;
%sublike( _, _, _)->
%    false.
%
%binarylike(<<C, LRest/binary>>, <<C, RRest/binary>> = R)->
%    sublike(LRest, R, RRest);
%binarylike(<<_, _/binary>> = Rest, R)->
%    binarylike(nextWord(Rest), R);
%binarylike(<<>>, _)->
%    false.
%
%like(L, R)when is_binary(L), is_binary(R) ->
%     binarylike(skipSpace(L), R);
%like(L, R)->L =:= R.

sublike(<<C, LRest/binary>>, R, <<C, RRest/binary>>)->
    sublike(LRest, R, RRest);
sublike(<<_, LRest/binary>>, R, <<_, _/binary>>)->
    binarylike(LRest, R);
sublike(_, _, <<>>)->
    true;
sublike( _, _, _)->
    false.

binarylike(<<C, LRest/binary>>, <<C, RRest/binary>> = R)->
    sublike(LRest, R, RRest);
binarylike(<<_, LRest/binary>>, R)->
    binarylike(LRest, R);
binarylike(<<>>, _)->
    false.

like(L, R)when is_binary(L), is_binary(R) ->
     binarylike(L, R);
like(L, R)->L =:= R.

%sublikeic(<<LC, LRest/binary>>, R, <<RC, RRest/binary>>)->
%    case string:to_upper(LC) =:= RC of
%        true->
%            sublikeic(LRest, R, RRest);
%        _ ->
%            case LC of
%                32 ->
%                    binarylikeic(skipSpace(LRest), R);
%                _ ->
%                    binarylikeic(nextWord(LRest), R)
%            end
%    end;
%sublikeic(<<32, _/binary>>, _, <<>>)->
%    true;
%sublikeic(<<>>, _, <<>>)->
%    true;
%sublikeic(_, _, _)->
%    false.
%
%binarylikeic(<<LC, LRest/binary>>, <<RC, RRest/binary>> = R)->
%    case string:to_upper(LC) =:= RC of
%        true ->
%            sublikeic(LRest, R, RRest);
%        _ ->
%            binarylikeic(nextWord(LRest), R)
%    end;
%binarylikeic(<<>>, _)->
%    false.
%
%likeic(L, R)when is_binary(L), is_binary(R) ->
%     binarylikeic(skipSpace(L), R);
%likeic(L, R)->L =:= R.

sublikeic(<<LC, LRest/binary>>, R, <<RC, RRest/binary>>)->
    case string:to_upper(LC) =:= RC of
        true->
            sublikeic(LRest, R, RRest);
        _ ->
            binarylikeic(LRest, R)
    end;
sublikeic(_, _, <<>>)->
    true;
sublikeic(_, _, _)->
    false.

binarylikeic(<<LC, LRest/binary>>, <<RC, RRest/binary>> = R)->
    case string:to_upper(LC) =:= RC of
        true ->
            sublikeic(LRest, R, RRest);
        _ ->
            binarylikeic(LRest, R)
    end;
binarylikeic(<<>>, _)->
    false.

likeic(L, R)when is_binary(L), is_binary(R) ->
     binarylikeic(L, R);
likeic(L, R)->L =:= R.

nlike(L, R)-> not like(L, R).