-module(license_lib).
-export([get_Activation/0]).

get_Activation() ->
	license:start(),
	case license:mac() of
		{ok,MacList} -> split_mac(MacList);
		_ -> {error,"Get License Error!"}
	end.

split_mac(T) ->
	T1 = string:tokens(T,";"),
	Fmac = lists:flatmap(fun(X) -> [format_mac(X)] end,T1),
	md5(encode_mac(Fmac)).

format_mac(Mac) ->
	MacSubList = string:tokens(Mac,"-"),
	random_mac(element(3,list_to_tuple(MacSubList)),element(4,list_to_tuple(MacSubList))) ++
	random_mac(element(2,list_to_tuple(MacSubList)),element(5,list_to_tuple(MacSubList))) ++
	random_mac(element(1,list_to_tuple(MacSubList)),element(6,list_to_tuple(MacSubList))).

random_mac(A,B) ->
	A1 = list_to_tuple(A),
	B1 = list_to_tuple(B),
	tuple_to_list({element(1,A1),element(1,B1)}) ++ tuple_to_list({element(2,A1),element(2,B1)}).

encode_mac(Mac) -> 
	case Mac of
	[] -> "";
	[One|Other] ->
		Tmp = One ++ encode_mac(Other),
		md5(Tmp)
	end.
	
md5(S) ->        
    Md5_bin =  erlang:md5(S), 
    Md5_list = binary_to_list(Md5_bin), 
    lists:flatten(list_to_hex(Md5_list)). 
 
list_to_hex(L) -> 
    lists:map(fun(X) -> int_to_hex(X) end, L). 

int_to_hex(N) when N < 256 -> 
    [hex(N div 16), hex(N rem 16)];
int_to_hex(N) -> N.

hex(N) when N < 10 , N >= 0 -> $0+N; 
hex(N) when N >= 10, N < 16 -> $A + (N-10);
hex(N) -> N.
 