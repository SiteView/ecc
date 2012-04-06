-module(httputils).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

-define(GET_PROPERTY,"$value-").
-define(GET_PRIVATE,"$private-").

keepChars(Source,Keep) ->
    keepChars(Source,Keep,[]).
    
keepChars([],_,S) ->lists:flatten(S);
keepChars([F|R],Keep,S) ->
    Exist = lists:member(F,Keep),
    if
        Exist ->
            keepChars(R,Keep,S++[F]);
        true ->
            keepChars(R,Keep,S)
    end.

escapeHTML(S) ->
    S.
    

is_lock(Index) ->
	global:set_lock({Index, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).	
					
release_lock(Index) ->	
	global:del_lock({Index, atom_to_list(node()) ++ pid_to_list(self())}, [node()|nodes()]).

floatToString(Float,Precision) ->
    P = "~."++integer_to_list(Precision)++"f",
    lists:flatten(io_lib:format(P,[(Float/1)])).

pageEncode() ->
    "utf-8".

hasSpaces([]) ->false;
hasSpaces([F|R]) ->
    if
        F=:=32 ->
            true;
        true ->
            hasSpaces(R)
    end.

readInteger(S,Index)->
    Num = readLong(S,Index),
    if
        Num=<16#7fffffff ->
            Num;
        true ->
            -1
    end.
    
readLong(S,Index) ->
    NumString = readNum(S,Index,length(S),[]),
    if
        length(NumString)>0 ->
            list_to_integer(NumString);
        true ->
            -1
    end.
    
readNum(S,Index,Len,NumString) ->
    if
        Index>=Len ->
            NumString;
        true ->
            C = lists:nth(Index,S),
            Isdigit = isDigit(lists:nth(Index,S)),
            if
                Isdigit ->
                    readNum(S,Index+1,Len,NumString++[C]);
                true ->
                    NumString
            end
    end.
    
isDigit(Char)->
    ((Char>=48) and (Char=<57)).

getMachineEncode(Machine)->
    if
        length(Machine)>0 ->  %%远程机器
            Record = machine:getMachine(Machine),
            if
                length(Record)>0 ->
                    R = hd(Record),
                    R#machine.remoteencoding;
                true ->
                    platform:getLocalCode()
            end;
        true ->
            platform:getLocalCode()
    end.

readIntegerFromEnd(Name) ->
    Len = length(Name),
    NumIndex = findint(Len,Name),
    if
        ((NumIndex>=1) and (NumIndex=/=Len)) ->
            list_to_integer(string:sub_string(Name,NumIndex+1,length(Name)));
        true ->
            -1
    end.

findint(Len,Name)->
    Flag = ((Len>=0) and ((lists:nth(Len,Name)>=48) and (lists:nth(Len,Name)=<57))),
    if
        Flag ->
            findint(Len-1,Name);
        true ->
            Len
    end.

len(null) ->0;
len(undefined) ->0;
len(String)->
    if
        is_list(String) ->
            length(String);
        true ->
            0
    end.
    

addLinkToCount(LinkCount,Link) ->
    case proplists:get_value(Link,LinkCount) of
        undefined ->
            NewLinkCount = LinkCount++[{Link,1}],
            {1,NewLinkCount};
        CurrIndex ->
            NewLinkCount = lists:keyreplace(Link, 1, LinkCount, {Link,CurrIndex+1}),
            {CurrIndex+1,NewLinkCount}
    end.
    
addIndexToLink(Link,Index)->
    if
        Index>1 ->
            Link++"\\link_sep/"++integer_to_list(Index);
        true ->
            Link
    end.


toInt(S)->
    case is_integer(S) of
        true ->
            S;
        _->
            case string:to_integer(S) of
                {error,_}->
                    0;
                {N,_} ->
                    N
            end
    end.
        

shiftNthElementInList(N,Elem,List)->
    Len = length(List),
    if
        N>Len ->
            List;
        true ->
            if
                N=:=1 ->
                    [_|R] = List,
                    [Elem]++R;
                N=:=Len ->
                    lists:sublist(List, 1, Len-1)++[Elem];
                true ->
                    L1 = lists:sublist(List, 1, N-1),
                    L2 = lists:sublist(List, N+1,Len-N),
                    L1++[Elem]++L2
            end
    end.
    
    
lookupStatus(State) ->
    case proplists:get_value(State,?statusMapping) of
        undefined ->
            "unknown error (" ++ integer_to_list(State) ++ ")";
        S->
            S
    end.

isSubstituteExpression(S) ->
	string:len(substitutionExpressionDelimiter(S))>0.
	
substitutionExpressionDelimiter(S) ->
	Index1 = string:str(S,"s/"),
	Index2 = string:rstr(S,"/"),
	Index3 = string:str(S,"s|"),
	Index4 = string:rstr(S,"|"),
	Index5 = string:len(S),
	if
		((Index1=:=1) and (Index2=:=Index5)) ->
			"/";
		((Index3=:=1) and (Index4=:=Index5)) ->
			"|";
		true->
			""
	end.
	
substitute(S) ->
	substitute(S,"").
	
substitute(S,Obj) ->
	S1 = substitutionExpressionDelimiter(S),
	case string:len(S1) of
		N when N>0 ->
			I = string:str(S,S1),
			J = string:rstr(S,S1),
				if
					((I>0) and (J>0))->
						doSubstitution(string:sub_string(S,I+1,J-1),Obj);
					true ->
						S
				end;
		_->	
			S
	end.
	
doSubstitution(S) ->
	doSubstitution(S,"").
	
doSubstitution(S,Obj)->
	F1 = string:str(S,"$")=/=0,
	if
		F1->
			%%以下步骤暂缺
			S;
		true ->
			S
	end.
	
replaceAll(S,S1,S2)->
	case replace(S,S1,S2) of
		[]->
			S;
		R->
			R
	end.
	
replace(S,S1,S2)->
	F1 = string:str(S,S1)=/=0,
	if
		F1->
			string:sub_string(S,1,string:str(S,S1)-1)++S2++replace(string:sub_string(S,string:str(S,S1)+string:len(S1),string:len(S)),S1,S2);
		true->
			case S of
				""->
					"";
				_->
					S
			end
	end.

startsWithIgnoreCase(S,S1) ->
	F1 = (string:len(S) >= string:len(S1)),
	if
		F1->
			string:to_lower(string:sub_string(S,1,string:len(S1))) =:= string:to_lower(S1);
		true ->
			false
	end.
	
%%从第n个字符处开始在S中寻找S1,返回这个字符在整个字符串中的位置,若未找到或索引位置超出字符串长度返回0.
indexOf(S,S1,N) ->
	Len = string:len(S),
	if
		N>Len ->
			0;
		true ->
			Rs = string:sub_string(S,N,string:len(S)),
			INRS = string:str(Rs,S1),
			if
				INRS=:=0 ->
					0;
				true ->
					INRS+N-1
			end
	end.
	
endsWith(S,S1)->
	ES = string:sub_string(S,string:len(S)-string:len(S1)+1,string:len(S)),
	if
		ES=:=S1 ->
			true;
		true ->
			false
	end.
	
startsWith(S,S1) ->
	case string:str(S,S1) of
		1->
			true;
		_->
			false
	end.
	
%%直接照SiteView写的，可能存在字符串的问题
replaceMatchValues(S,Array,Array1) ->
	{SS,A,A1,N} = replaceMatchValues(S,Array,Array1,0),
	F1 = string:str(SS,"{$")=:=0,
	if
		F1 and A=/=[] ->
			for1(A,SS,1);
		true ->
			SS
	end.
	
replaceMatchValues(S,Array,Array1,N) ->
	F1 = string:str(S,"{$$")=:=0,
	F2 = string:str(S,"{$$")=:=N,
	F3 = string:str(S,".")=:=0,
	F4 = string:str(S,"}")=:=0,
	F5 = Array1=:=[],
	if
		(F1 or F2 or F3 or F4 or F5) ->
			{S,Array,Array1,N};
		true ->
			I = string:str(S,"{$$"),
			J = I+3,
			L = string:str(string:sub_string(S,J,string:len(S)),".")+J,
			I1 = string:str(string:sub_string(S,L,string:len(S)),"}")+L,
			SS = if
				((J<L) and (L<I1)) ->
					J1 = list_to_integer(string:sub_string(S,J,L)),
					K1 = list_to_integer(string:sub_string(S,L+1,I1)),
					Nth = length(Array),
					if
						(Nth>=K1) and (K1>0) and (K1<30) ->
							Array2=lists:nth(J1,Array1),
							Len = length(Array2),
							if
								Len>=K1 ->
									S3 = lists:nth(K1,Array2),
									replace(S,string:sub_string(S,J-3,I1+1),S3);
								true ->
									S
							end;
						true ->
							S
					end;
				true ->
					S
			end,
			replaceMatchValues(SS,Array,Array1,I)
	end.

for1([],S,_) ->S;
for1([F|R],S,N) ->
	S1 = "{$"++integer_to_list(N)++"}",
	SS = case string:str(S,S1) of
		0->
			S;
		_->
			S2 = lists:nth(N),
			replace(S,S1,S2)
	end,
	for1(R,SS,N+1).

bytesToString(N,Pre) ->
	if
		N<1024 ->
            floatToString(N,Pre)++"bytes";
		N<1048576 ->
			floatToString(N/1024,Pre)++"K";
		true ->
			N1 = N/1048576,
			floatToString(N1,Pre)++" MB"
	end.

bpsToString(N,Pre) ->
	if
		N<1000 ->
            floatToString(N,Pre)++"bps";
		N<1000000 ->
			floatToString(N/1000,Pre)++"Kbps";
		true ->
			N1 = N/1000000,
			floatToString(N1,Pre)++" Mbps"
	end.

createProfile(Class) ->
    {A,B,C} = erlang:now(),
	random:seed(A,B,C),
	Rand=random:uniform(100000),
    list_to_atom(atom_to_list(Class)++" - "++integer_to_list(Rand)).
    
matchExpressionForWebServiceMonitor(S,S1,Array,StringBuffer) ->
    S2 = "matched ",
    {Status,Array1} = matchExpression(S,S1,[]),
    SB1 = if
        Array1=/=[] ->
            StringBuffer++S2;
        true ->
            StringBuffer
    end,
    NewS = SB1++loop1(Array1),
    {Status,NewS,Array1}.
    

match(S,S1)->
    element(1,matchExpression(S,S1)) =:= 200.

matchExpression(S,S1)->
    matchExpression(S,S1,[],"").

matchExpression(S,S1,Array,StringBuffer) ->
    matchExpression(S,S1,Array,StringBuffer,"matched ").
    
matchExpression(S,S1,Array,StringBuffer,S2) ->
    {I,Array1} = matchExpression(S,S1,[]),
    Len = length(Array1),
    StringBuffer1 = if
        Len>=1 ->
            StringBuffer ++ S2;
        true ->
            StringBuffer
    end,
    {NewS,NewArray} = loop2(Array1,StringBuffer1,Array,1),
    {I,NewS,NewArray}.
    
loop2([],StringBuffer,Array,_)->{StringBuffer,Array};
loop2([F|R],StringBuffer,Array,N)->
    S = if
        N=/=1 ->
            StringBuffer++", ";
        true ->
            StringBuffer
    end,
    S1 = S++F,
    loop2(R,S1,Array++[F],N+1).

matchExpression(Body,Regexp,Array) ->
    I = string:str(Regexp,"/"),
    J = string:rstr(Regexp,"/"),
	Flag1 = startsWith(Regexp,"/"),
	Flag2 = endsWith(Regexp,"/"),
    if
        Flag1 and Flag2 ->
            Regexp1 = string:sub_string(Regexp,I+1,J-1),
            case re:run(Body,Regexp1) of
                nomatch ->
                    State = -999,
                    Array1 = [];
                {match,L} ->
                    State = 200,
                    Array1 = Array++matchregexp(lists:nthtail(1,L),Body)
            end,
            {State,Array1};
        true ->
            Index = string:str(Body,Regexp),
            if
                Index =/= 0 ->
                    {200,[]};
                true ->
                    {-999,[]}
            end
    end.
    

matchregexp([],_)->[];
matchregexp([F|R],Body)->
    case F of
        {N,M}->
            [string:substr(Body,N+1,M)]++matchregexp(R,Body);
        _->
            matchregexp(R,Body)
    end.
	
matchregexp(S,S1,S2) ->
	{State,Array} = matchregexp(S,S1),
	Len = length(Array),
	Sb = if
		Len>=2 ->
			S2++loop1(Array);
		true ->
			loop1(Array)
	end,
	{State,Array,Sb}.
	
loop1([])->[];
loop1([F|R]) ->
	if
		R=/=[] ->
			F++","++loop1(R);
		true ->
			F++loop1(R)
	end.
	
isValueExpression(S) ->
	F1 = startsWith(S,"xml."),
	F2 = isRegularExpression(S),
	F1 or F2.
	
isRegularExpression(S) ->
	F1 = startsWith(S,"/"),
	if
		F1 ->
			I = string:str(S,"/"),
			J = string:rstr(S,"/"),
			if
				((I=/=J) and (J=/=0)) ->
                    true;
				true ->
					false
			end;
		true ->
			false
	end.

%%从begin的位置开始寻找下一行，从第一个“\r\n”以后的字符串开始算起（适用于log文件）给出所有的行
readLine(File,Begin) ->
	{ok,FileInfo} = file:read_file_info(File),
	L = element(2,FileInfo),
	if
		L=<Begin ->
			[];
		true ->
			{ok,F} = file:read_file(File),
			Content = binary_to_list(F),
			I = indexOf(Content,"\r\n",Begin),
			S = string:sub_string(Content,I+2,string:len(Content)),
			string:tokens(S,"\r\n")
	end.
    
%%system time
timeMillis() ->
    {T,_} = statistics(wall_clock),
    T.

currentTimeMillis() ->
    %%calendar:datetime_to_gregorian_seconds({date(), time()}).
    {T,_} = statistics(wall_clock),
    T.


dateToString({Date,Time}) ->
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute)++":"++integer_to_list(Second)++" "++
    integer_to_list(Month)++"/"++integer_to_list(Day)++"/"++integer_to_list(Year).

unescapeHTML(URL) ->
    S1 = replaceAll(URL,"&amp;","&"),
    S2 = replaceAll(S1,"&AMP;","&"),
    S2.
    
readStringFromStart(S,S1) ->
    I = string:str(S,S1),
    S2 = if
        I=/=0 ->
            string:sub_string(S,1,I-1);
        true ->
            S
    end,
    S2.

exception_to_String(Exp)->
    if
        is_list(Exp)=:=true ->
            Exp;
        true ->
            lists:flatten(io_lib:format("~p",[Exp]))
    end.
    
removeChars(S,S1)->
    removeChars(S,S1,[]).
    
removeChars([],S1,List)->lists:reverse(List);
removeChars([F|_Rest],S1,List)->
    case string:chr(S1,F) of
        0 ->
            removeChars(_Rest,S1,[F|List]);
        _->
            removeChars(_Rest,S1,List)
    end.
    
onlyChars([],S1)->true;
onlyChars([F|_Rest],S1)->
    case string:chr(S1,F) of
        0 ->
            false;
        _->
            onlyChars(_Rest,S1)
    end.

%%修正http_util 转换日期中存在的问题
to_upper(Str) ->
    string:to_upper(Str).

to_lower(Str) ->
    string:to_lower(Str).

convert_netscapecookie_date([_D,_A,_Y, $,, _SP,
			     D1,D2,_DA,
			     M,O,N,_DA,
			     Y1,Y2,Y3,Y4,_SP,
			     H1,H2,_Col,
			     M1,M2,_Col,
			     S1,S2|_Rest]) -> 
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=list_to_integer([D1,D2]),
    Month=convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

convert_netscapecookie_date([_D,_A,_Y, _SP,
			     D1,D2,_DA,
			     M,O,N,_DA,
			     Y1,Y2,Y3,Y4,_SP,
			     H1,H2,_Col,
			     M1,M2,_Col,
			     S1,S2|_Rest]) -> 
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=list_to_integer([D1,D2]),
    Month=convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

%%增加单数日期的匹配
convert_netscapecookie_date([_D,_A,_Y, _SP,
			     D1,_DA,
			     M,O,N,_DA,
			     Y1,Y2,Y3,Y4,_SP,
			     H1,H2,_Col,
			     M1,M2,_Col,
			     S1,S2|_Rest]) -> 
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=list_to_integer([D1]),
    Month=convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};
    
convert_netscapecookie_date([_D,_A,_Y, $,, _SP,
			     D1,_DA,
			     M,O,N,_DA,
			     Y1,Y2,Y3,Y4,_SP,
			     H1,H2,_Col,
			     M1,M2,_Col,
			     S1,S2|_Rest]) -> 
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=list_to_integer([D1]),
    Month=convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

convert_netscapecookie_date(_)->
    {error,"wrong date"}.

hexlist_to_integer([])->
    empty;
%%When the string only contains one value its eaasy done.
%% 0-9
hexlist_to_integer([Size]) when Size >= 48 , Size =< 57 ->
   Size - 48;
%% A-F
hexlist_to_integer([Size]) when Size >= 65 , Size =< 70 ->
    Size - 55;
%% a-f
hexlist_to_integer([Size]) when Size >= 97 , Size =< 102 ->
    Size - 87;
hexlist_to_integer([_Size]) ->
    not_a_num;

hexlist_to_integer(Size) ->
    Len = string:span(Size, "1234567890abcdefABCDEF"),
    hexlist_to_integer2(Size, 16 bsl (4 *(Len-2)),0).

integer_to_hexlist(Num)->
    integer_to_hexlist(Num, get_size(Num), []).

convert_month("Jan") -> 1;
convert_month("Feb") -> 2;
convert_month("Mar") -> 3; 
convert_month("Apr") -> 4;
convert_month("May") -> 5;
convert_month("Jun") -> 6;
convert_month("Jul") -> 7;
convert_month("Aug") -> 8;
convert_month("Sep") -> 9;
convert_month("Oct") -> 10;
convert_month("Nov") -> 11;
convert_month("Dec") -> 12.

is_hostname(Dest) ->
    inet_parse:domain(Dest).

%%%========================================================================
%%% Internal functions
%%%========================================================================
hexlist_to_integer2([],_Pos,Sum)->
    Sum;
hexlist_to_integer2([HexVal | HexString], Pos, Sum) 
  when HexVal >= 48, HexVal =< 57 ->
    hexlist_to_integer2(HexString, Pos bsr 4, Sum + ((HexVal-48) * Pos));

hexlist_to_integer2([HexVal | HexString], Pos, Sum) 
  when HexVal >= 65, HexVal =<70 ->
    hexlist_to_integer2(HexString, Pos bsr 4, Sum + ((HexVal-55) * Pos));

hexlist_to_integer2([HexVal | HexString], Pos, Sum)
  when HexVal>=97, HexVal=<102 ->
    hexlist_to_integer2(HexString, Pos bsr 4, Sum + ((HexVal-87) * Pos));

hexlist_to_integer2(_AfterHexString, _Pos, Sum)->
    Sum.

integer_to_hexlist(Num, Pot, Res) when Pot<0 ->
    convert_to_ascii([Num | Res]);

integer_to_hexlist(Num,Pot,Res) ->
    Position = (16 bsl (Pot*4)),
    PosVal = Num div Position,
    integer_to_hexlist(Num - (PosVal*Position), Pot-1, [PosVal | Res]).

get_size(Num)->
    get_size(Num, 0).

get_size(Num, Pot) when Num < (16 bsl(Pot *4))  ->
    Pot-1;

get_size(Num, Pot) ->
    get_size(Num, Pot+1).

convert_to_ascii(RevesedNum) ->
    convert_to_ascii(RevesedNum, []).

convert_to_ascii([], Num)->
    Num;
convert_to_ascii([Num | Reversed], Number) when Num > -1, Num < 10 ->
    convert_to_ascii(Reversed, [Num + 48 | Number]);
convert_to_ascii([Num | Reversed], Number) when Num > 9, Num < 16 ->
    convert_to_ascii(Reversed, [Num + 55 | Number]).
    
changeCountersName([])->[];
changeCountersName([F|R])->
    NewName = case F of
        {ID,Name}->
            [{ID,httputils:replaceAll(Name,"/","\\")}];
        _->
            []
    end,
    NewName++changeCountersName(R).

getStrippedButtonName(S)->
    EndOfForm = string:str(S,"}"),
    StartOfFormNumber = indexOf(S,"\\",EndOfForm),
    if
        StartOfFormNumber=/=0 ->
            string:sub_string(S,EndOfForm+1,StartOfFormNumber-1);
        EndOfForm=/=0 ->
            string:sub_string(S,EndOfForm+1,string:len(S));
        true ->
            S
    end.
