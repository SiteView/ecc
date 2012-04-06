%
%textutils.erl
%author:lei.lin@dragonflow.com
%

-module(textutils).
-compile(export_all).
-include("monitor.hrl").

%get Value from tuple of list,Key is atom,
getValue(List,Key) ->
getValue_t(List,Key,"",length(List)).
getValue_t(_L,_K,R,0) -> R;
getValue_t(L,K,_Re,Num) ->
	[A|B] = L,
	Cos = element(1,A),
	if Cos == K ->
		getValue_t(L,K,element(2,A),0);
	true ->
		getValue_t(B,K,"",Num-1)
	end. 		

toInt(Value) ->
	case is_integer(Value) of
	true ->
		Value;
	_ ->
		list_to_integer(Value)
	end.

findLong(String1,String2,String3) ->
	Index1 = string:str(String1,String2),
	if Index1 == 0 ->
		-1;
	true ->
		if String3 == null ->
			Index2 = findLong_util(String1,Index1+length(String2)+1),
			lists:sublist(String1,Index1+length(String2)+1,Index2-(Index1+length(String2)));
		true ->
			Index3 = string:str(String1,String3),
			if Index3 == 0 ->
				-1;
			true ->
				Index2 = findLong_util(String1,Index1+length(String2)+1),
				lists:sublist(String1,Index1+length(String2)+1,Index2-(Index1+length(String2))) 
			end			
		end		
	end.

			

%N is index 	
findLong_util(String,N) ->
	findLong_util_t(String,true,N).
findLong_util_t(_S,false,Num) -> Num;
findLong_util_t(S,F,Nu) ->
	[A|B] = S,
	case isDigit(lists:sublist(S,Nu,1)) of
	true ->
		findLong_util_t(S,F,Nu+1);
	_ ->
		findLong_util_t(S,false,Nu)
	end.

	


	
stringContainsSubstringFromArray(String,List) ->
	Sn = (length(String) == 0),
	Ln = (length(List) == 0),
	if Sn ->
		if Ln ->
			false;
		true ->
			false
		end;
	true ->
		if Ln ->
			false;
		true ->
			stringContainsSubstringFromArray_util(String,List)   			
		end
	end.

stringContainsSubstringFromArray_util(S,L) ->
	stringContainsSubstringFromArray_util_t(S,L,length(L),false).
stringContainsSubstringFromArray_util_t(_S,_T,0,R) -> R;
stringContainsSubstringFromArray_util_t(St,Li,Nu,_R) ->
	[A|B] =Li,
	N = (length(A) /= 0),
	K = (string:rstr(St,A) == 1),
	if N,K ->
		stringContainsSubstringFromArray_util_t(St,Li,0,true);
	true ->
		stringContainsSubstringFromArray_util_t(St,B,Nu-1,_R)
	end.

foundIPAddress(IpStr,Ip) ->
	I = string:str(IpStr,Ip),
	J = I + length(Ip),
	IsL = length(IpStr),
	if  I > 0 ->		     		
		true;
	true ->
		false
	end.		

% replace string 	
replaceString(String1,String2,String3) ->
	if String2 /= String3 ->
		I = string:str(String1,String2),
		replaceString_util(String1,String2,String3);
	true ->
		String1
	end.	
		
replaceString_util(S,S1,S2) ->
	replaceString_util_t(1,S,S1,S2,1). 		
replaceString_util_t(_I,_S,_S1,_S2,0) -> _S;
replaceString_util_t(I,Str,Str1,Str2,N) ->
	T = string:substr(Str,I),		
	case  T of
	[] ->
		replaceString_util_t(I,Str,Str1,Str2,0);
	_ ->
		S3 = string:substr(Str,1,I) ++ Str2 ++string:substr(Str,I+length(Str1)),
		T2 = length(Str2),
		if T2 > 1 ->
			N = I + length(Str2),
			replaceString_util_t(N,S3,Str1,Str2,1);
		true ->		
			replaceString_util_t(I,S3,Str1,Str2,0)
		end
	end.		
		
isRegularExpression(String) ->		
	case regexp:parse(String) of
	{ok,_} ->
		true;
	_ ->
		false
	end.
  
			
%"ismcdwa"
onlyChars(String1,String2) ->
	onlyChars_t(String1,String2,length(String1),true).
onlyChars_t(_S1,_F,0,R) -> R;
onlyChars_t(Str1,Str2,Num,Re) ->
	A = string:substr(Str1,1,1),
	T = string:str(Str2,A),
	if T == 0 ->
		onlyChars_t(Str1,Str2,0,false);
	true ->
		onlyChars_t(string:substr(Str1,2,Num-1),Str2,Num-1,Re)
	end.	


%% String string string which contains SubString
contains(String, SubString) ->
    Int = string:str(String, SubString),
    if
        Int > 0 ->
            true;
        true ->
            false
    end.

	
stripLineTerminators(String) ->
	String.

endsWithIgnoreCase(Str1,Str2) ->
	Len1 = length(Str1),
	Len2 = length(Str2),
	if Len1 >= Len2 ->
		Str = string:substr(Str1,Len1-Len2),
		string:to_lower(Str) == string:to_lower(Str2);
	true ->
		false
	end.		

match(String,Mat) ->
	case is_list(Mat) of
	true ->
		case re:run(String,Mat) of
		{match,_} ->
			true;
		_ ->
			false
		end;
	_ ->
		false
	end.
		

matchExpression(String1,String2,Array,"","matched ") ->  ok.  

substitute(String) ->
	substitute(String,null).

isSubstituteExpression(String) ->
	length(substitutionExpressionDelimiter(String)) > 0.

substitute(String,Object) ->
	S1 = substitutionExpressionDelimiter(String).

substitutionExpressionDelimiter(String) ->
	Len = length(String),
	Index1 = string:str(String,"s/"),
	Index2 = string:rstr(String,"/"),
	Index3 = string:str(String,"s|"),
	Index4 = string:str(String,"|"),
	if Index1 == 1, Index2 == Len ->
		"/";
	true ->
		if Index3 == 1 , Index4 == Len ->
			"|";
		true ->
			""
		end
	end.		

%is real getValue(),but hashmap change over to ets table. 	
getValue_ets(EtsTables,Key) ->
	Res = ets:lookup(EtsTables,Key),
	case Res of
	[] ->
		"";
	[{_K,V}] ->
		V  
	end.	
	 
isDigit(NumString) ->    
	case regexp:match(NumString,"^[0-9]+$") of
	{match,_,_} ->
		true; 
	nomatch ->
		false 
	end.	
 
isIpaddr(IpString) ->
	case  regexp:match(IpString,"^(([0-9]+).([0-9]+).([0-9]+)).([0-9]+)$")  of
	{match,_,_} ->
		true; 
	nomatch ->
		false 
	end.	    

%"^((([0-9]+)-([0-9]+))-([0-9]+))|((([0-9]+) ([0-9]+)) ([0-9]+))$|((([0-9]+)/([0-9]+))/([0-9]+))"
isDate(DateString,MP) ->
	case re:run(DateString,MP) of 
	{match,_} ->
		true;
	_ ->
		false 
	end.

%"^(?:19|20)[0-9][0-9]$"
isYear(DateString,MP) ->
	case re:run(DateString,MP) of 
	{match,_} ->
		true;
	_ ->
		false 
	end. 

floatToString(Float ,Int) ->
	ok.

readLong(String,Int) ->
	Len = length(String),
	NumStr = readLong_util(String,Int,Len),
	NumStr.
	
readLong_util(String,Int,Length) ->
	readLong_util_t(String,Int,Length,true,"").
readLong_util_t(_Str,_Int,_Length,false,R) -> R;
readLong_util_t(Str,I,Len,Flag,R) ->
	if I > Len ->
		readLong_util_t(Str,I,Len,false,R);
	true ->   
		Char = string:substr(Str,I,1),
		Bool = isDigit(Char),
		if Bool  ->
			readLong_util_t(Str,I+1,Len,Flag,string:concat(R,Char));
		true ->   
			readLong_util_t(Str,I,Len,false,R)
		end
	end.        
	 
	 
%
hasChars(Str,Str1) ->
	hasChars_util_t(Str,Str1,1,length(Str),false).
hasChars_util_t(_S,_S1,N,0,F) -> F;
hasChars_util_t(String,String1,Num,Len,False) ->
	Length = length(String),
	if Num > Length  ->
		hasChars_util_t(String,String1,Num,0,False);
	true ->        
		Char = string:substr(String,Num,1),
		Bool = (string:str(String1,Char) >= 1),    
		if Bool ->
			hasChars_util_t(String,String1,Num,Len,true);
		true ->
			hasChars_util_t(String,String1,Num+1,Len-1,False) 
		end
	end.        


removeChars(Str,Str1) ->
	removeChars_t(1,Str,Str1,length(Str),"").
removeChars_t(N,S,_S1,0,R) -> R;
removeChars_t(Num,String,String1,Len,Re) ->
	Char = string:substr(String,Num,1),
	Bool = (string:str(String1,Char) == 0),
	if Bool ->
		removeChars_t(Num+1,String,String1,Len-1,string:concat(Re,Char));
	true ->
		removeChars_t(Num+1,String,String1,Len-1,Re)
	end.


min(A,B) ->
	if A =< B ->
		A;
	true ->
		B
	end.
	
max(A,B) ->
	if A >= B ->
		A;
	true ->
		B
	end.




trim(String) ->
	string:strip(String,both,$ ).



isMultiLineRegularExpression(String) ->
	Index = string:rstr(String,"/"),
	if Index > 0 ->
		SubStr = string:substr(String,Index+1),
		Index1 = string:str(SubStr,"s"),
		if Index1 >0 ->
			true;
		true ->
			false
		end;
	true ->
		false
	end.        


			
split2(String) ->      
	split2_t(String,"",[]).
split2_t([13|SubStr1],F1,List) ->
	[C|D] = SubStr1, 
	split2_t(D,"",[lists:reverse(F1,[])|List]);
split2_t([10|SubStr2],F2,List) ->
	split2_t(SubStr2,"",[lists:reverse(F2,[])|List]);   
split2_t([A|B],F3,List) ->
	split2_t(B,[A|F3],List);
split2_t("",F4,List) -> if length(F4) > 0 -> lists:reverse([lists:reverse(F4,[]) | List ],[]);true -> lists:reverse(List,[]) end.



lookupStatus(Int) ->
	case Int of
	201 ->
		"created";
	202 ->
		"accepted";
	203 ->
		"non-authoratative";
	204 ->
		"no content";
	205 ->
		"reset content";
	206 ->
		"partial content";
	301 ->
		"document moved";
	302 ->
		"document moved";
	303 ->
		"document moved";
	307 ->
		"document moved";
	400 ->
		"bad request";
	401 ->
		"unauthorized";
	403 ->
		"forbidden";
	404 ->
		"not found";
	407 ->
		"proxy authentication required";
	500 ->
		"server error";
	501 ->
		"not implemented";
	502 ->
		"proxy received invalid response";
	503 ->
		"server busy";
	504 ->
		"proxy timed out";
	999 ->
		"time out";   
	_ ->
		"unknown error"           
	end.
 
%replaceParameters(AboriginalString,ReplaceStirng,ReplaceList) ->
%    if ReplaceList == null ->
%        TempReplaceList = ["$","%"];
%    true ->
%        TempReplaceList = ReplaceList
%    end,
%    if ReplaceStirng /= null ->
					


delete_bar(String) ->
	List  = string:tokens(String,"_"),
 delete_bar_t(List,length(List),"").
 delete_bar_t(_List,0,E) -> E;
 delete_bar_t(Li,Num,En) ->
	[A|B] = Li,
	delete_bar_t(B,Num-1,En ++ A).    
 
tokenize(String) ->
	{Array,StringBuff} = tokenize_util(String),
	if length(StringBuff) > 0 ->
		NewArray = Array ++ [StringBuff];
	true ->
		NewArray =  Array
	end.
	
tokenize_util(String) ->
	tokenize_util_rec(String,length(String)," "," ",[],"").
tokenize_util_rec(_S,0,_C,_C1,Array,StringBuff) -> {Array,StringBuff};
tokenize_util_rec(Str,Len,Char,Char1,Arra,StrBuf) ->
	Char2 = string:substr(Str,length(Str)-Len+1,1),
	if Char /= " " ->
		if (Char2 == Char) and (Char1 /= "\\") ->
			NewArray = Arra ++ [Char ++ StrBuf ++ Char],
			NewStrBuf = "",
			NewChar = " ";
		true ->
			NewStrBuf = StrBuf ++ Char2,
			NewChar = Char,
			NewArray = Arra            
		end;
	true ->
		if Char2 /= " " ->
			if  Char2 == "\"" ->
				NewArray = Arra,
				NewStrBuf = StrBuf,                
				NewChar = Char2;
			true ->
				NewStrBuf = StrBuf ++ Char2,
				NewArray = Arra,
				NewChar = Char           
			end;
		true ->
			if length(StrBuf) >0 ->
				NewArray = Arra ++ [StrBuf ],  
				NewStrBuf = "",
				NewChar = Char;
			true ->
				NewArray = Arra,
				NewStrBuf = StrBuf, 
				NewChar = Char                
			end 
		end  
	end,
	NewChar1 = Char2,
	tokenize_util_rec(Str,Len-1,NewChar,NewChar1,NewArray,NewStrBuf).    
	  

hasSpaces(String) ->
	case re:run(String," ") of
	nomatch ->
		true;
	_ ->
		false 
	end. 
  
%Replace spaces 
replacespace(String) ->
	List = string:tokens(String," "),
	replacespace_t(List,length(List),"").
replacespace_t(_L,0,R) -> R;
replacespace_t([A|B],Len,Res) ->
	replacespace_t(B,Len-1,Res++A).  
	

%Place a string of special characters ¡°\ / : * ? " < > |¡±
replacechar(String) ->
	replacechar_t(String,length(String),"").
replacechar_t(_S,0,Str) -> lists:reverse(Str);
replacechar_t([A|B],Len,St) ->
	if (A == 92) or (A==47) or (A==58) or (A==42) or (A==63) or (A==34) or (A==60) or (A==62)or (A==124) or(A==32) ->
		replacechar_t(B,Len-1,St);
	true ->
		replacechar_t(B,Len-1,[A|St]) 
	end.

		
%Space replaced with a colon
space2spechar(String) ->
	space2spechar_t(String,length(String),"").
space2spechar_t(_S,0,Str) -> lists:reverse(Str);
space2spechar_t([A|B],Len,St) ->
	if A==32 ->
		space2spechar_t(B,Len-1,[63|St]);
	true ->
		space2spechar_t(B,Len-1,[A|St]) 
	end.

%Colon with spaces
spechar2space(String) ->
	spechar2space_t(String,length(String),"").
spechar2space_t(_S,0,Str) -> lists:reverse(Str);
spechar2space_t([A|B],Len,St) ->
	if A==63 ->
		spechar2space_t(B,Len-1,[32|St]);
	true ->
		spechar2space_t(B,Len-1,[A|St]) 
	end.
    

%Replace the empty space
space2empty(String) ->
	space2empty_t(String,length(String),"").
space2empty_t(_S,0,Str) -> lists:reverse(Str);
space2empty_t([A|B],Len,St) ->
	if A==32 ->
		space2empty_t(B,Len-1,St);
	true ->
		space2empty_t(B,Len-1,[A|St]) 
	end.

%% Update feature in the device will upload the file to all folders and file name into a valid file system name (_: 95), (??&: 38), (!: 33), (+: 43)
biastoEfficfilename(String) ->
	biastoEfficfilename_t(String,length(String),"").
biastoEfficfilename_t(_S,0,Str) -> lists:reverse(Str);
biastoEfficfilename_t([A|B],Len,St) ->
	Blen = string:len(B),
	if 
	   Blen == 0 ->
		if
			A == 46 ->
				biastoEfficfilename_t(B,Len-1,[95,43,33,38|St]);
			true ->
				biastoEfficfilename_t(B,Len-1,[A|St])
		end;
	   A==47 ->     %% /
		biastoEfficfilename_t(B,Len-1,[43,33,38,95|St]);
	   A==58 ->       %% :
		biastoEfficfilename_t(B,Len-1,[33,43,38,95|St]);
	   A==42 ->       %% *
		biastoEfficfilename_t(B,Len-1,[43,38,33,95|St]);
	   A==63 ->       %% ?
		biastoEfficfilename_t(B,Len-1,[38,43,33,95|St]);
	   A==34 ->       %% "
		biastoEfficfilename_t(B,Len-1,[38,33,43,95|St]);
	   A==60 ->       %% <
		biastoEfficfilename_t(B,Len-1,[33,38,43,95|St]);
	   A==62 ->       %% >
		biastoEfficfilename_t(B,Len-1,[43,33,95,38|St]);
	   A==124 ->       %% |
		biastoEfficfilename_t(B,Len-1,[33,43,95,38|St]);
	   A==92 ->       %% \
		biastoEfficfilename_t(B,Len-1,[43,95,33,38|St]);
	   A==32 ->       %% space
		biastoEfficfilename_t(B,Len-1,[95,33,43,38|St]);
	   true ->
		biastoEfficfilename_t(B,Len-1,[A|St]) 
	end.


%% Processing date and time string

%% @doc The world standard date time format string to date {{Y, M, D}, {HH, MM, SS}
datetimestr_to_datetime(Str) ->
	DateTime = string:tokens(Str, " "),
    [DateStr,TimeStr] = case length(DateTime) of
                        1   ->[DateStr1] =  DateTime,
                               [DateStr1,"00:00:00"];
                        2   -> DateTime;
                        _   -> ["1970-01-01","00:00:00"]
                        end,
    Date = string:tokens(DateStr, "-"),
    [Year,Month,Day] = case length(Date) of
                       1  ->  [Y] = Date,
                              [list_to_integer(Y),1,1];
                       2  ->  [Y,M] = Date,
                              Y_Int = list_to_integer(Y),
                              M_Int = list_to_integer(M),
                              M_Int_change = case M_Int > 12 of
                                                true -> 
                                                    12;
                                                _   ->     
                                                    case M_Int < 1 of
                                                        true -> 
                                                             1;
                                                        _ ->
                                                           M_Int
                                                    end
                                             end,                              
                              [Y_Int,M_Int_change,1];
                       3  ->  [Y,M,D] = Date,
                              Y_Int = list_to_integer(Y),
                              M_Int = list_to_integer(M),
                              D_Int = list_to_integer(D),
                              LastDayOfMonths = lastdatOfMonths(Y_Int,M_Int),
                              D_Int_Change = case D_Int > LastDayOfMonths of
                                                true ->
                                                    LastDayOfMonths;
                                                _ ->
                                                    case D_Int < 1 of
                                                        true ->
                                                            1;                                                            
                                                        _ ->
                                                            D_Int
                                                    end
                                            end,
                              M_Int_Change =  case M_Int > 12 of
                                                true -> 
                                                    12;
                                                _   ->     
                                                    case M_Int < 1 of
                                                        true -> 
                                                             1;
                                                        _ ->
                                                           M_Int
                                                    end
                                             end,             
                              [Y_Int,M_Int_Change,D_Int_Change];
                       _  ->  [1970,1,1]
                       end,
    Time = string:tokens(TimeStr, ":"),
    [Hour,Minute,Second] = case length(Time) of
                       1  -> [H] = Time,
                             [list_to_integer(H),0,0];
                       2  -> [H,Mi] = Time,
                             [list_to_integer(H),list_to_integer(Mi),0];
                       3  -> [H,Mi,S] = Time,
                             [list_to_integer(H),list_to_integer(Mi),list_to_integer(S)];
                       _  -> [0,0,0]
                       end,
   %% io:format("~n~n~p-~p-~p ~p:~p:~p~n~n",[Year,Month,Day,Hour,Minute,Second]),
	{{Year,Month,Day},{Hour,Minute,Second}}.

%% @doc Get the last day of month
lastdatOfMonths(Year,Month) ->  
    case Month of
            1 -> 31;
            2 -> case calendar:is_leap_year(Year) of
                    true ->
                        29;
                    false ->
                        28
                 end;
            3 -> 31;
            4 -> 30;
            5 -> 31;
            6 -> 30;
            7 -> 31;
            7 -> 31;
            9 -> 30;
            10 -> 31;
            11 -> 30;
            12 -> 31;
            _ -> 31
           end.
    %%	[YearStr,MonthStr,DayStr] = string:tokens(DateStr, "-"),
    %%	[HourStr, MinuteStr, SecondStr] = string:tokens(TimeStr, ":"),
    %%	Year = erlang:list_to_integer(YearStr),
    %%	Month = erlang:list_to_integer(MonthStr),
    %%	Day = erlang:list_to_integer(DayStr),
    %%	Hour = erlang:list_to_integer(HourStr),
    %%	Minute = erlang:list_to_integer(MinuteStr),
    %%	Second = erlang:list_to_integer(SecondStr),
	%%{{Y,M,D},{HH,MM,SS}
	

%% The world standard date time format string into milliseconds
dtstr_to_microseconds(Str) ->
	DateTime = datetimestr_to_datetime(Str),
	sv_datetime:time(DateTime).

%% The world standard date time format string into a string of milliseconds
dtstr_to_microsecondStr(Str) ->
	MSecondStr = dtstr_to_microseconds(Str),
	erlang:integer_to_list(MSecondStr).

%% validate_datetimeYYYY-MM-DD HH:mm:ss
validate_datetimestr(Str) ->
	%%Regex = "((\d{2}(([02468][048])|([13579][26]))[\-\/\s]?((((0?[13578])|(1[02]))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\-\/\s]?((0?[1-9])|([1-2][0-9])))))|(\d{2}(([02468][1235679])|([13579][01345789]))[\-\/\s]?((((0?[13578])|(1[02]))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\-\/\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\-\/\s]?((0?[1-9])|(1[0-9])|(2[0-8]))))))", %%data part
	%%Regex1 = "(\s(((0?[0-9])|([1-2][0-3]))\:([0-5]?[0-9])((\s)|(\:([0-5]?[0-9])))))?", %%time part
	Regex = "[0-9][0-9][0-9][0-9]" ++ "-" ++ "[0-9][0-9]" ++ "-" ++ "[0-9][0-9]",   %%date
	ReC = " ",
	RegexTime = "[0-9][0-9]" ++ ":" ++ "[0-9][0-9]" ++ ":" ++ "[0-9][0-9]",   %%time
	Rss = "|",
	RegExp = Regex ++ ReC ++ RegexTime,
	io:format("RegExp = ~p~n", [RegExp]),
	case Str of
		[] ->
			true;
		_ ->
			case regexp:match(Str, RegExp) of
				{match,1,19} ->
					true;
				_ ->
					false
			end
	end.
	
% guid/0 - Return a guid like object.
guid() ->
	MD5 = erlang:md5(term_to_binary(make_ref())),
	MD5List = lists:nthtail(8, binary_to_list(MD5)),
	F = fun(N) -> f("~2.16.0B", [N]) end,
	L = [F(N) || N <- MD5List],
	lists:flatten(L).

%% wf:f
f(S) -> f1(S).
f(S, Args) -> f1(S, Args).

%% wf_utils:f
f1(S) -> f1(S, []).
f1(S, Args) -> lists:flatten(io_lib:format(S, Args)).

%% Get the right length of string, from the beginning of the string length Length of string specified intercept

get_fitlength_str(Str, Length) ->
	StrLength = string:len(Str),
	if
		StrLength =< Length ->
            Str;
		true ->
            string:substr(Str, 1, Length) ++ "..."
	end.

%% From the list of key-value pairs, for the specified key value (key for the atoms), get the value if the value is returned, otherwise false
get_bool_kvl(Key, KvList) ->
    case lists:keysearch(Key, 1, KvList) of
        {value, {Key, Value}} ->
            Value;
        _ ->
            false
    end.
    
%% ex. ["1","2","3"] -> "[1,2,3]"
strlist_to_str(StrList) ->
    "["++strlist_to_str_t(StrList)++"]".
strlist_to_str_t([]) ->
    [];
strlist_to_str_t([Str|T]) ->
    case Str of
        CStr when erlang:is_list(CStr) ->
            case T of
                [] ->
                    CStr ++ strlist_to_str_t(T);
                _ ->
                    CStr++"," ++strlist_to_str_t(T)
            end;
        _ ->
            strlist_to_str_t(T)
    end.

%% according labelid Get labels, nt
get_nt_label_name(LabelId) ->
    case lists:prefix("nt_", LabelId) of
        true ->
            string:substr(LabelId, 4);
        _ ->
            LabelId
    end.

%% According labelid get labels, unix
get_unix_label_name(LabelId) ->
    case lists:prefix("unix_", LabelId) of
        true ->
            string:substr(LabelId, 6);
        _ ->
            LabelId
    end.


%% All the string "/" replaced by "-"
bias2horizontal(String) ->
    bias2horizontal_t(String, []).
bias2horizontal_t([], Results) ->
    lists:reverse(Results);
bias2horizontal_t([A|B], Results) ->
    if
        A == 47 ->
            bias2horizontal_t(B, [45|Results]);
        true ->
            bias2horizontal_t(B, [A|Results])
    end.
    

%% All members of the list into a string, the connection of all members and then type a string into a list

listToStr(List) ->
    "["++listToString(List)++"]".

%% All members of the list into a string, the connection and then all members of the string into a comma-separated
listToString(List) ->
    SList = listEveryToString(List),
    string:join(SList, ",").

%% List of all members of the edges into a string
listEveryToString(List) ->
    Fun = 
        fun(X) ->
            any_to_list(X)
        end,
    SList = lists:map(Fun, List).
    
%% Erlang string converted to IP format ip (for example: "192.168.0.1" -> {192,168,0,1})
ipStr2ErIp(IPStr) ->
    IPs =
        case IPStr of
            "\\\\"++IP ->
                IP;
            _ ->
                IPStr
        end,
    StrList = string:tokens(IPs, "."),
    IntList = [erlang:list_to_integer(X)||X<-StrList, erlang:is_list(X)],
    erlang:list_to_tuple(IntList).
    
%% erlang convert the format string format ip ip   
erIp2IpStr(ErIP) ->
    IntList = erlang:tuple_to_list(ErIP),
    StrList = [erlang:integer_to_list(X)||X<-IntList, erlang:is_integer(X)],
    string:join(StrList, ".").

%% ----------------The following is the basic character and string operations side------------------------

%% ------------------------------------------------------------------------


%% String replace characters inside the Old New Character
replace(String, Old, New) ->
    replace_t(String, Old, New, []).
replace_t([], Old, New, Results) ->
    lists:reverse(Results);
replace_t([A|B], Old, New, Results) ->
    if
        A == Old ->
            bias2horizontal_t(B, [New|Results]);
        true ->
            bias2horizontal_t(B, [A|Results])
    end.
    

%% String is the beginning of the S
startsWith(String, S) ->
    case string:str(String, S) of
        1 ->
            true;
        _ ->
            false
    end.
    
%% Char character is a digit (0-9)
isdigit(Char) ->
    if
        Char >= 48, Char =< 57 ->
            true;
        true ->
            false
    end.

%% Convert any type of list
any_to_list(AnyType) ->
    if
        is_tuple(AnyType) == true ->
            tuple_to_list(AnyType);
        is_integer(AnyType) == true ->
            integer_to_list(AnyType);
        is_float(AnyType) == true ->
            io_lib:format("~.2f", [AnyType]);
        is_list(AnyType) == true ->
            AnyType;
        is_atom(AnyType) ->
            atom_to_list(AnyType);
        is_binary(AnyType) ->
            binary_to_list(AnyType);
        is_bitstring(AnyType) ->
            io:format("is bitstring~n"),
            bitstring_to_list(AnyType);
        is_boolean(AnyType) ->
            atom_to_list(AnyType);
        is_pid(AnyType) ->
            pid_to_list(AnyType);
        is_port(AnyType) ->
            erlang:port_to_list(AnyType);
        true ->
            AnyType
    end.
    
    
appendString(Stringbuffer, S, I, Flag, Flag1) ->
    SLength = string:len(S),
    if
        I < 1 ->
            lists:append([Stringbuffer, S]);
        I < SLength ->
            lists:append([Stringbuffer, string:substr(S, 1, I - 1)]);
        true ->
            J = I - SLength,
            {Jj,Flag1List} =
            if
                Flag1 =:= true ->
                    {0, lists:duplicate(J, " ")};
                true ->
                    {J,[]}
            end,
            FlagList =
            if
                Flag =:= true ->
                    lists:duplicate(Jj, " ");
                true ->
                    []
            end,
            lists:append([Stringbuffer] ++ Flag1List ++ [S] ++FlagList)
    end.
    
    
appendStringRightJustify(Stringbuffer, S, I) ->
    appendString(Stringbuffer, S, I, false, true).
    

%% String index from the Index which begin to take to a string of numbers
getNumFromStr(String, Index) ->
    SubString = string:substr(String, Index),
    io:format("SubString !!!!!!!!!!!!!!!~n~p~n", [SubString]),
    Str = getNumFromStr_t(SubString, []),
    lists:reverse(Str).
getNumFromStr_t([], Result) ->
    Result;
getNumFromStr_t([A|B], Result) ->
    Begin = A >= 48,
    End = A =< 57,
    if
        Begin =:= true, End =:= true ->
            getNumFromStr_t(B, [A|Result]);
        true ->
            Result
    end.
    
%% ------- The following is the xml report generation solution --------------
    
isEscaped(String, S, I) ->
    Ss = string:substr(S, I),
    IsStartGt = startsWith(Ss, "&gt;"),
    IsStartLt = startsWith(Ss, "&lt;"),
    if
        IsStartGt =:= true ->       %% ">"
            {4, lists:append(String, "&gt;")};
        IsStartLt ->       %% "<"
            {4, lists:append(String, "&lt;")};
        true ->
            IsStartOldAmp = startsWith(Ss, "&#"),        %% "&#"
            IsStartAmp = startsWith(Ss, "&amp;#"),        %% "&amp;#"
            if
                IsStartOldAmp =:= true; IsStartAmp =:= true ->
                    Byte0 = 
                        if
                            IsStartOldAmp =:= true ->
                                2;
                            true ->
                                6
                        end,
                    J = I + Byte0,
                    Jj = forEscaped(S, J),
                    SLength = string:len(S),
                    SChar = lists:nth(Jj, S),
                    if
                        Jj < SLength, SChar == $; ->
                            Length = Jj + 1 - (I + Byte0),
                            {(Jj-i)+1, lists:append([String, "&#", string:substr(String, I + Byte0, Length)])};
                        true ->
                            {0, String}
                    end;
                true ->
                    IsStartAmpR = startsWith(Ss, "&amp;"),
                    if
                        IsStartAmpR =:= true ->             %% "&"
                            {5, lists:append(String, "&amp;")};
                        true ->
                            {0, String}
                    end
            end
    end.
    
escapeSpecial(C, Stringbuffer) ->
    if
        C == 62 ->
            {true, lists:append(Stringbuffer, "&gt;")};
        C == 60 ->
            {true, lists:append(Stringbuffer, "&lt;")};
        C == 38 ->
            {true, lists:append(Stringbuffer, "&amp;")};
        true ->
            {false, Stringbuffer}
    end.
    
forEscaped(S, J) ->
    Length = string:len(S),
    IsDigit = isdigit(lists:nth(J, S)),
    if
        J < Length, IsDigit =:= true ->
            forEscaped(S, J+1);
        true ->
            J
    end.
    
    
%% According to property names and values ??to construct the xml property
escapeXML(S, S1) ->
    Ss = replace(S, $/, $-),
    BeginPropName = lists:append(["<",Ss,">"]),
    Content = forEscapeXML(S1, 1, []),
    EndPropName = lists:append(["</",Ss,">"]),
    lists:append([BeginPropName, Content, EndPropName]).
    
    
forEscapeXML(S1, I, Stringbuffer) ->
    S1Length = string:len(S1),
    if
        I > S1Length ->
            lists:append(Stringbuffer);
        true ->
            {Ii, Stringbuffer1} = isEscaped(lists:append(Stringbuffer), S1, I),
            I1 = I + Ii,
            if
                I1 > S1Length ->
                    Stringbuffer1;
                true ->
                    C = lists:nth(I1, S1),
                    I2 = I1 + 1,
                    {IsSpecial, Stringbuffer2} = escapeSpecial(C, Stringbuffer1),
                    if
                        IsSpecial =:= false ->
                            forEscapeXML(S1, I+1, [Stringbuffer2] ++ [string:chars(C, 1)]);
                        true ->
                            forEscapeXML(S1, I+1, [Stringbuffer2])
                    end
            end
    end.
    
%% -----------------------------------------
