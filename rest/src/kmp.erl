-module(kmp).
-compile(export_all).

%string_next(String)->
%	T = list_to_tuple(String),
%	Length = tuple_size(T),
%	io:format("T:~p,   Length:~p ~n", [T, Length]),
%	Array = array:new([{size,  Length + 1}, {default, 0}]),
%	string_next(T,  Array, Length, 1,  0).
%
%string_next( _, Array,  Length, Length, _)->
%	[_|R] = array:to_list(Array),
%	R;
%string_next(Tuple,  Array,  Length,  I,  0)->
%	NewI = I + 1,
%	NewJ = 1,
%	TI = element(NewI, Tuple),
%	TJ = element(NewJ, Tuple),
%	io:format("sting_next===>NewI:~p, NewJ:~p, TI:~p, TJ:~p ~n", [NewI, NewJ, TI, TJ]),
%	string_next_1(Tuple, Array, Length, NewI, NewJ, TI, TJ);
%string_next(Tuple,  Array,  Length,  I,  J)->
%	TI = element(I, Tuple),
%	TJ = element(J, Tuple),
%	io:format("sting_next===>I:~p, J:~p, TI:~p, TJ:~p ~n", [I, J, TI, TJ]),
%	string_next_2(Tuple, Array, Length, I, J, TI, TJ).
%
%string_next_1(Tuple, Array, Length, I, J,  T,  T)->
%	io:format("string_next_1===>I:~p, J:~p, T:~p~n", [I, J, T]),
%	string_next(Tuple, array:set(I, array:get(J, Array), Array), Length, I, J);
%string_next_1(Tuple, Array, Length, I, J,  _,  _)->
%	io:format("string_next_1===>I:~p, J:~p~n", [I, J]),
%	string_next(Tuple, array:set(I, J, Array), Length,  I, J).
%
%string_next_2(Tuple, Array, Length, I, J, T, T)->
%	NewI = I + 1,
%	NewJ = J + 1,
%	TI = element(NewI, Tuple),
%	TJ = element(NewJ, Tuple),
%	io:format("string_next_2===>NewI:~p, NewJ:~p, TI:~p, TJ:~p ~n", [NewI, NewJ, TI, TJ]),
%	string_next_1(Tuple, Array, Length, NewI, NewJ, TI, TJ);
%string_next_2(Tuple, Array, Length, I, J,  _,  _)->
%	io:format("string_next_2===>I:~p, J:~p~n", [I, J]),
%	string_next(Tuple,  Array, Length,  I,  array:get(J, Array)).
%


string_next(String)->
	T = list_to_tuple(String),
	Length = tuple_size(T),
	Array = array:new([{size,  Length + 1}, {default, 0}]),
	string_next(T,  Array, Length, 1,  0).

string_next( _, Array,  Length, Length, _)->
	[_|R] = array:to_list(Array),
	R;
string_next(Tuple,  Array,  Length,  I,  0)->
	string_next_0(Tuple, Array, Length, I + 1, 1);
string_next(Tuple,  Array,  Length,  I,  J)->
	string_next_2(Tuple, Array, Length, I, J, element(I, Tuple), element(J, Tuple)).

string_next_0(Tuple, Array, Length, I, J)->
	string_next_1(Tuple, Array, Length, I, J, element(I, Tuple), element(J, Tuple)).

string_next_1(Tuple, Array, Length, I, J,  T,  T)->
	string_next(Tuple, array:set(I, array:get(J, Array), Array), Length, I, J);
string_next_1(Tuple, Array, Length, I, J,  _,  _)->
	string_next(Tuple, array:set(I, J, Array), Length,  I, J).

string_next_2(Tuple, Array, Length, I, J, T, T)->
	string_next_0(Tuple, Array, Length, I + 1, J + 1);
string_next_2(Tuple, Array, Length, I, J,  _,  _)->
	string_next(Tuple,  Array, Length,  I,  array:get(J, Array)).

%-------------------------------------------------------------------------------
binary_element(Binary, 1)->
	<<Element, _/binary>> = Binary,
	Element;
binary_element(Binary, Index)->
	Skip = Index - 1,
	<<_:Skip/binary, Element, _/binary>> = Binary,
	Element.

elements(Binary, Indices)->
	elements(Binary, Indices, []).

elements(_, [], Acc)->
	Acc;
elements(Binary, [Index|Rest], Acc)->
	Element = binary_element(Binary, Index),
	elements(Binary, Rest, [Element|Acc]).
	

binary_next(Binary)->
	Length = byte_size(Binary),
	Array = array:new([{size,  Length + 1}, {default, 0}]),
	binary_next(Binary,  Array, Length, 1,  0).

binary_next( _, Array,  Length, Length, _)->
	[_|R] = array:to_list(Array),
	R;
binary_next(Binary,  Array,  Length,  I,  0)->
	binary_next_0(Binary, Array, Length, I + 1, 1);
binary_next(Binary,  Array,  Length,  I,  J)->
	[TI, TJ] = elements(Binary, [J, I]),
	binary_next_2(Binary, Array, Length, I, J, TI, TJ).

binary_next_0(Binary, Array, Length, I, J)->
	[TI, TJ] = elements(Binary, [J, I]),
	binary_next_1(Binary, Array, Length, I, J, TI, TJ).

binary_next_1(Binary, Array, Length, I, J,  T,  T)->
	binary_next(Binary, array:set(I, array:get(J, Array), Array), Length, I, J);
binary_next_1(Binary, Array, Length, I, J,  _,  _)->
	binary_next(Binary, array:set(I, J, Array), Length,  I, J).

binary_next_2(Binary, Array, Length, I, J, T, T)->
	binary_next_0(Binary, Array, Length, I + 1, J + 1);
binary_next_2(Binary, Array, Length, I, J,  _,  _)->
	binary_next(Binary,  Array, Length,  I,  array:get(J, Array)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

contact(Acc, [H|R])->
	contact([H|Acc],R);
contact(Acc, [])->
	Acc.

string_next_tuple(String, Next)->
	string_next_tuple(String, Next, []).

string_next_tuple(_, [], Acc)->
	list_to_tuple(lists:reverse(Acc));
string_next_tuple(String, [0|R], Acc)->
	string_next_tuple(String, R, [{0, null}|Acc]);
string_next_tuple(String, [Index|R], Acc)->
	string_next_tuple(String, R, [{Index, lists:nthtail(Index - 1, String)}|Acc]).

replace(List, [], _)->List;
replace(List, Find, Replace)->
	Next = string_next(Find),
	NextTuple = string_next_tuple(Find, Next),
	Length = tuple_size(NextTuple),
	Indices = string_indices(List, Find, Length, 1, 1, Find, NextTuple, []),
	replace(List, Replace, Length, Indices).

string_indices([], _, _, _, _, _, _, Acc)->
	lists:reverse(Acc);
string_indices(Rest, [], Length, I, _, Find, NextTuple, Acc)->
	string_indices(Rest, Find, Length, I, 1, Find, NextTuple, [ I - Length|Acc]); 
string_indices([_|SR], _, Length, I, 0, Find, NextTuple, Acc)->
	string_indices(SR, Find, Length, I + 1, 1, Find, NextTuple, Acc);
string_indices([H|SR], [H|PR], Length, I, J, Find, NextTuple, Acc)->
	string_indices(SR, PR, Length, I + 1, J + 1, Find, NextTuple, Acc);
string_indices(Rest, _, Length, I,  J, Find, NextTuple, Acc)->
	{Next, Tail} = element(J, NextTuple),
	string_indices(Rest, Tail, Length, I, Next, Find, NextTuple, Acc).

string_skip(Rest, 0)->
	Rest;
string_skip([_|R], Length)->
	string_skip(R, Length - 1).

replace(String, _, _, [])->
	String;
replace(String, Replace, Length, Indices)->
	replace(String, Replace, Length, Indices, 1, []).

replace([], _, _, _, _, Acc)->
	lists:reverse(Acc);
replace(String, Replace, Length, [I|Rest], I, Acc)->
	replace(string_skip(String, Length), Replace, Length,  Rest, I + Length, contact(Acc, Replace));
replace([H|R], Replace, Length, Indices, I, Acc)->
	replace(R, Replace, Length, Indices, I + 1, [H|Acc]).
      
%=======================================================

sub_binary(_, 0)->
	null;
sub_binary(Binary, 1)->
	Binary;
sub_binary(Binary, Index)->
	Skip = Index - 1,
	<<_:Skip/binary, Rest/binary>> = Binary,
	Rest.

binary_next_tuple(Binary, Next)->
	binary_next_tuple(Binary, Next, []).

binary_next_tuple(_, [], Acc)->
	list_to_tuple(lists:reverse(Acc));
binary_next_tuple(Binary, [Index|R], Acc)->
	binary_next_tuple(Binary, R, [{Index, sub_binary(Binary, Index)}|Acc]).

binary_like(<<>>,  _, _, _, _)->
	false;
binary_like(_, <<>>, _, _, _)->
	true; 
binary_like(<<_, SR/binary>>,  _, 0, Pattern, Next)->
	binary_like(SR, Pattern, 1, Pattern, Next);
binary_like(<<H, SR/binary>>, <<H, PR/binary>>, J, Pattern, Next)->
	binary_like(SR, PR, J + 1, Pattern, Next);
binary_like(Rest, _,  J, Pattern, Next)->
	{NewJ, Tail} = element(J, Next),
	binary_like(Rest, Tail,  NewJ, Pattern, Next).

like(L, R, Next)when is_binary(L), is_binary(R), is_tuple(Next)->
     binary_like(L, R, 1, R, Next);
like(L, R, _)->L =:= R.

binary_find(_, <<>>)->false;
binary_find(Binary, Pattern)->
	Next = binary_next(Pattern),
	NextTuple = binary_next_tuple(Pattern, Next),
	binary_like(Binary, Pattern, 1, Pattern, NextTuple).

%%%%---------------------------------------------------------------------------

binary_likeic(<<>>,  _, _, _, _)->
	false;
binary_likeic(_, <<>>, _, _, _)->
	true; 
binary_likeic(<<_, SR/binary>>,  _, 0, Pattern, Next)->
	binary_likeic(SR, Pattern, 1, Pattern, Next);
binary_likeic(<<SH, SR/binary>> = S, <<PH, PR/binary>>, J, Pattern, Next)->
	case string:to_upper(SH) =:= PH of
		true ->
		    binary_likeic(SR, PR, J + 1, Pattern, Next);
		_ ->
		    {NewJ, Tail} = element(J, Next),
		    binary_likeic(S, Tail,  NewJ, Pattern, Next)
	    end;
binary_likeic(Rest, _,  J, Pattern, Next)->
	{NewJ, Tail} = element(J, Next),
	binary_likeic(Rest, Tail,  NewJ, Pattern, Next).

likeic(L, R, Next)when is_binary(L), is_binary(R), is_tuple(Next)->
     binary_likeic(L, R, 1, R, Next);
likeic(L, R, _)->L =:= R.

to_upper([C|R])-> [string:to_upper(C)|to_upper(R)];
to_upper([])-> [].

binary_to_upper(Binary)->
	List = binary_to_list(Binary),
	list_to_binary(to_upper(List)).

binary_find_ic(_, <<>>)->false;
binary_find_ic(Binary, Pattern)->
	UpperPattern = binary_to_upper(Pattern),
	Next = binary_next(UpperPattern),
	NextTuple = binary_next_tuple(UpperPattern, Next),
	binary_likeic(Binary, UpperPattern, 1, UpperPattern, NextTuple).