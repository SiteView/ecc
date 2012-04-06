%
%lineReader.erl
%author:lei.lin@dragonflow.com
%
%Os is string,Data is list
-module(lineReader,[Data,Os,Tid,CmdId]).
-compile(export_all).


%CmdId is atom,like pageFault,return {ok,Value} or {error,Res}
getStartLine() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSettingAsInteger(CmdId, startLine,0),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        0
    end.		
	
getEndLine() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSettingAsInteger(CmdId, endLine,16#7fffffff),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        16#7fffffff
    end.	
	
getHeaderLine() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSettingAsInteger(CmdId, headerLine,1),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        1
    end.	
	
getStartMatch() ->	
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSetting(CmdId, startMatch),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        ""
    end.	

getStart() ->
	Val = getStartMatch(),
	Len = length(Val),
	if Len /= 0 ->
	    false;
    true ->
        true
    end.		
	
getEndMatch() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSetting(CmdId, endMatch),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        ""
    end.	

getMachLine() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSetting(CmdId, matchLine),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        ""
    end.	

getSkipLineMatch() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSetting(CmdId, skipLine),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        ""
    end.	
	
getReverseColumns() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSetting(CmdId, reverseColumns),
	case Value of
	{ok,Val} ->
	    Val;
	_ ->
        ""
    end.	

getReverseLines() ->
    OsaI = osAdapter:new(list_to_atom(Os)),	
	OsaI:initEts(),
	Value = OsaI:getCommandSetting(CmdId,reverseLines),
	case Value of
	{ok,V} ->
	    {ok,V};
	_ ->
        ""
    end.		
	
%return List or {error,R}	
tryReverseLines() ->
    Value  = getReverseLines(),
    case Value of
    {ok,_Val} ->
        {ok,reverse(Data)};
	"" ->
        {error,undefined}	
	end.	

reverse(List) ->
    reverse_t(List,length(List),[]).
reverse_t(_L,0,R) -> R;
reverse_t(L,N,Re) ->
    reverse_t(L,N-1,lists:append(Re,lists:sublist(L,N,1))).	

getColumnNames() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
	Value = OsaI:getMatchedCommandSettings(CmdId,"ColumnName"),
    case Value of
	{ok,V} ->
	    V;
	_ ->
        ""
    end.

%get subkey's value, return {ok,Val} or {error,Res}
getColumnLabels() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
	Value = OsaI:getMatchedCommandSettings(CmdId,"ColumnName"),
    case Value of
	{ok,V} ->
	    util_getColumnLabels(V,CmdId); 		
	_ ->
        ""
    end.        
% parameter is list of atom	
util_getColumnLabels(List,CmdId) ->	
    util_getColumnLabels_t(CmdId,List,length(List),[]).
util_getColumnLabels_t(_C,_L,0,R) -> R;
util_getColumnLabels_t(C,L,N,Re) ->
    [A|B] = L,
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
    Value = OsaI:getCommandSetting(C,A),
    case Value of
	{ok,V} ->
	    util_getColumnLabels_t(C,B,N-1,lists:append(Re,[V]));
    _ ->
        util_getColumnLabels_t(C,B,N-1,Re)
    end.		
	
getColumnStartIndex() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
	Value = OsaI:getMatchedCommandSettings(CmdId,"ColumnName"),
    case Value of
	{ok,V} ->
	    {ok,lists:duplicate(length(V),0)}; 		
	_ ->
        {error,undefined}
    end.

getcolumnEndIndex() ->
    OsaI = osAdapter:new(list_to_atom(Os)),
    OsaI:initEts(),
	Value = OsaI:getMatchedCommandSettings(CmdId,"ColumnName"),
    case Value of
	{ok,V} ->
	    {ok,lists:duplicate(length(V),-1)}; 		
	_ ->
        {error,undefined}
    end.

checkhead() ->
	%~ io:format("=== ~p====~n",[{ets:lookup(Tid,processhead),ets:lookup_element(Tid,processhead,2)}]),
	ets:lookup_element(Tid,processhead,2).
	

			
processLine() ->
    HeaderLine = getHeaderLine(),
    ColumnNames = getColumnNames(),
	ColumnLabels = getColumnLabels(),
	EndMatch = getEndMatch(),
	EndLine = getEndLine(),
	StartMatch = getStartMatch(),
	MatchLine = getMachLine(),
	SkipLineMatch = getSkipLineMatch(),
    TLineNumber = ets:lookup_element(Tid,lineNumber,2),
    %HeaderLine = ets:lookup_element(Tid,headerLine,2),
    %StartLine = ets:lookup_element(Tid,startLine,2),
    StartLine = getStartLine(),
    %EndLine = ets:lookup_element(Tid,endLine,2),	
	ets:insert(Tid,{start,length(StartMatch) == 0}),
    case tryReverseLines() of
    {error,R} ->
        if  TLineNumber > length(Data) ->
            ets:insert(Tid,{reading,false});
	    true ->            
	        Reading = ets:lookup_element(Tid,reading,2),
            if  Reading ->
		        ets:insert(Tid,{shouldSkipLine,false}),
			[CurrentLine] = lists:sublist(Data,TLineNumber,1),
			ets:insert(Tid,{currentLine,CurrentLine}),
			ets:insert(Tid,{lineNumber,TLineNumber+1}),
			LineNumber = TLineNumber+1,
			ets:insert(Tid,{processhead,false}),
			if ColumnNames /= "",TLineNumber == HeaderLine ->				
				%% oldhand modify
				parseHeader(Data,TLineNumber,CmdId,CurrentLine),
				ets:insert(Tid,{processhead,true});				
			        %%parseHeader(CmdId,CurrentLine);  
			    true ->
				nothing
			end,
			if 	LineNumber == 1 ->
			    nothing;
			true ->
			    nothing
			end,
			
			if 	LineNumber < StartLine	 ->
					ets:insert(Tid,{shouldSkipLine,true});
				    true ->
			    nothing
			end,
			if 	LineNumber > EndLine ->
			    ets:insert(Tid,{reading,false});
			true ->
			    nothing
			end,
			Len1 = length(EndMatch),
			if  Len1 > 0 ->
			    Bool = match(CurrentLine,EndMatch),  
			    case Bool of 
			    true ->
				ets:insert(Tid,{reading,false});
			    _ ->
				 nothing 
			    end;  
			true ->
			    nothing
			end,
			TStart = ets:lookup_element(Tid,start,2),
			Len2 = length(StartMatch),
			if  ((TStart /= true) and (Len2 > 0)) ->
			   % io:format("3~n"),                
				    Start = match(CurrentLine,StartMatch),
			    ets:insert(Tid,{start,Start});
			true ->
			    nothing
			end,
                Len3 = length(MatchLine),
                %io:format("00~n"),
                if  Len3 > 0 ->
                     Bool1 = match(CurrentLine,MatchLine),
                    case Bool1 of
                    true ->       
                        nothing;
                    _ ->                         
                        ets:insert(Tid,{shouldSkipLine,true})
                    end;    
          	    true ->
                    nothing
                end,
                Len4 = length(SkipLineMatch),                
                if Len4 > 0 ->
                   % io:format("5~n"), 
                    Bool2 = match(CurrentLine,SkipLineMatch),
                    case  Bool2 of
                    true ->
                        ets:insert(Tid,{shouldSkipLine,true});
                    _ ->
                        nothing  
                    end; 
                true ->
                    nothing
                end,
                Start2 = ets:lookup_element(Tid,start,2),
                if Start2 == false	->
                    ets:insert(Tid,{shouldSkipLine,true});
                true ->
                    nothing
                end
		    end	        			
        end;
    {ok,TData} ->
        if  TLineNumber > length(TData) ->
            ets:insert(Tid,{reading,false});
	    true ->
	        Reading = ets:lookup_element(Tid,reading,2),
            if  Reading ->
		        ets:insert(Tid,{shouldSkipLine,false}),
                [CurrentLine] = lists:sublist(TData,TLineNumber,1),
			    ets:insert(Tid,{currentLine,CurrentLine}),
                ets:insert(Tid,{lineNumber,TLineNumber+1}),
			    LineNumber = TLineNumber+1,
		ets:insert(Tid,{processhead,false}),
                if ColumnNames /= "",LineNumber == HeaderLine ->				
				%% oldhand modify
				parseHeader(TData,TLineNumber,CmdId,CurrentLine),
				ets:insert(Tid,{processhead,true});	
			        %%parseHeader(CmdId,CurrentLine);  
			    true ->
                    nothing
                end,
                if 	LineNumber == 1 ->
                    nothing;
                true ->
                    nothing
                end,
		
                if 	LineNumber < StartLine	 ->
			        ets:insert(Tid,{shouldSkipLine,true});
			    true ->
                    nothing
                end,
                if 	LineNumber > EndLine ->
                    ets:insert(Tid,{reading,false});
                true ->
                    nothing
                end,
                Len1 = length(EndMatch),			    
                if Len1 > 0->
                   Bool = match(CurrentLine,EndMatch),
                    case Bool of
                    true ->                     
                        ets:insert(Tid,{reading,false});
                    _ ->
                        nothing 
                    end; 
                true ->
                    nothing
                end,
                TStart = ets:lookup_element(Tid,start,2),
                Len2 = length(StartMatch),
                if  ((TStart /= true) and (Len2 > 0)) ->         
        		    Start = match(CurrentLine,StartMatch),
                    ets:insert(Tid,{start,Start});
                true ->
                    nothing
                end,
                Len3 = length(MatchLine),               
                if  Len3 > 0 ->
                    Bool1 = match(CurrentLine,MatchLine), 
                    case Bool1 of
                    true ->
                        nothing;
                    _ ->                        
                        ets:insert(Tid,{shouldSkipLine,true})
                    end;    
          	    true ->
                    nothing
                end,
                Len4 = length(SkipLineMatch),
                
                if Len4 > 0  ->
                    Bool2 = match(CurrentLine,SkipLineMatch), 
                    case Bool2 of
                    true ->                      
                        ets:insert(Tid,{shouldSkipLine,true});
                    _ ->
                        nothing  
                    end; 
                true ->
                    nothing
                end,
                Start2 = ets:lookup_element(Tid,start,2),
                if Start2 == false	->
                    ets:insert(Tid,{shouldSkipLine,true});
                true ->
                    nothing
                end
		    end	        			
        end   
    end, 
    ets:lookup_element(Tid,reading,2).	

match(String,Reg) ->
    case regexp:match(String,Reg) of
	{match,_Start,_Length} ->
	    true;
	_ ->
        false
    end.	
%%%%%%%%%%%%% oldhand modify    
parseHeader(TData,TLineNumber,_CmdId,_CurrentLine) when length(TData) - 1 < TLineNumber -> error;	
parseHeader(TData,TLineNumber,CmdId,CurrentLine) ->

    case parseHeader(CmdId,CurrentLine) of
          error -> 
	   [NewCurrentLine] = lists:sublist(TData,TLineNumber+1,1),
	   ets:insert(Tid,{currentLine,NewCurrentLine}),
	   ets:insert(Tid,{lineNumber,TLineNumber+2}),
	   parseHeader(TData,TLineNumber+1,CmdId,NewCurrentLine);
	  Result ->  Result
    end.
%%% oldhand modify end

%get ColumnStartIndex,return list,[{ColumnLabel,ColumnStartIndex}]
parseHeader(CmdId,CurrentLine) ->
    ColumnNames = getColumnNames(),
    ColumnLabels = getColumnLabels(),   
    parseHeader_t(ColumnNames,ColumnLabels,CurrentLine,length(ColumnNames)).
    
    
parseHeader_t(CN,CL,CuL,0) -> ok;
parseHeader_t(TColumnNames,TColumnLabels,TCurrentLine,Num) ->    
    [A|B] = TColumnLabels,
    [C|D] = TColumnNames,
    K = string:str(TCurrentLine,A),
	if K == 0 -> error;
	    %~  oldhand modify
	    %~ parseHeader_t(TColumnNames,B,TCurrentLine,Num-1);
    true -> 		
        L = K,
	I1 = (K + length(A)) - 1,
	%List = string:tokens(TCurrentLine," "),
	%Numt = match_str(List,A),
	%timer:sleep(3000), 
        {Start,End} = str_index(TCurrentLine,A),
        Len = length(TCurrentLine),
        if End >= Len -> 	   	
            ets:insert(Tid,{C,{Start,-1}}),           
			parseHeader_t(D,B,TCurrentLine,Num-1);
        true ->	   
            ets:insert(Tid,{C,{Start,End}}),           
			parseHeader_t(D,B,TCurrentLine,Num-1)
        end
    end. 		

% String is CurrentLine
str_index(String,Str) ->
    Index = string:str(String,Str),
    if Index /= 0 ->
        Start = index_start_utils(String,Index),
        End = index_end_utils(String,Index+length(Str)-1),
        {Start,End};
    true ->
        {-1,-1}
    end.   
         

index_start_utils(Str,Index) ->
    index_start_utils_t(Str,true,Index).
index_start_utils_t(_Str,false,_Ind) ->  _Ind;
index_start_utils_t(S,F,In) ->
    if In == 1 ->
        index_start_utils_t(S,false,In); 
    true ->    
        Char = string:substr(S,In-1,1),
        if (Char == " ")or (Char == []) ->
            index_start_utils_t(S,F,In-1);
        true ->
            index_start_utils_t(S,false,In)
        end        
    end.

index_end_utils(Str,Index) ->
    index_end_utils_t(Str,length(Str),true,Index,-1).
index_end_utils_t(_S,_L,false,_In,End) -> End;
index_end_utils_t(S,L,F,I,E) ->
    if I == L ->        
        index_end_utils_t(S,L,false,I,I);
    true ->
        Char = string:substr(S,I+1,1),
        if Char == " ",Char == [] ->
           index_end_utils_t(S,L,F,I+1,E);
        true ->
           index_end_utils_t(S,L,false,I,I) 
        end        
    end.   
    

match_str(List,String) ->
   match_str_t(List,String,length(List),0).
match_str_t(_L,_S,0,N) -> N + 1;
match_str_t(L,S,Len,Num) ->
    [A|B] = L,
    if A == S ->
        match_str_t(B,S,0,Num);
    true ->
        match_str_t(B,S,Len-1,Num+1)
    end.		

skipLine() ->
    ets:lookup_element(Tid,shouldSkipLine,2).
    

readColumn(I) ->
    readColumn_Flag(I,false).	
	

readColumn(I,String) ->
    ReverseColumns = ets:lookup_element(Tid,reverseColumns,2),
	Len = length(ReverseColumns),
	if  Len > 0 ->
	    Line = string:tokens(ReverseColumns,","),
		{Flag,_N} = readColumn_util(String,Line);
	true ->
        Flag = false
    end,	
   % io:format("___________________________~p~n",[{ReverseColumns,Len,I,String}]),    
    readColumn_Flag(I,Flag).
		
		
readColumn_util(String,List) ->
	readColumn_util_t(String,list,length(List),{false,1}).
readColumn_util_t(_S,_L,0,R) -> R;
readColumn_util_t(S,L,Num,Re) ->
    {F,J} = Re,	
    Len = length(L),
	if Re >= Len ->
	    readColumn_util_t(S,L,0,Re);
	true ->
        Str = lists:sublist(L,J+1,1),
        if 	S == Str ->
	        
            readColumn_util_t(S,L,0,{true,J});
        true -> 		
            readColumn_util_t(S,L,Num-1,{F,J+1})
        end
    end.		
	
readColumn_Flag(Num,Flag) ->
    List = string:tokens(ets:lookup_element(Tid,currentLine,2)," "),
   %  io:format("___________________________~p~n",[{    List,Num,Flag}]),    
    if Flag ->
        I = (length(List)-Num)+1;
    true ->
        I = Num
    end,
    Len = length(List),
	if 	Len >= I, I > 0 ->
	    lists:sublist(List,I,1);
	true ->
        if I == 999 ,Len > 0 ->
            lists:sublist(List,Len,1);
        true ->
            [""]
        end			
	end.	

readColumnByName(String) ->
    ColumnNames = getColumnNames(),    
    Con = count(ColumnNames,String),
    case Con of
    "" ->
        "";
    _ ->    
        TT = ets:lookup(Tid,Con),
        %Column  = ets:lookup_element(Tid,Con,2),
        {Start,End} = ets:lookup_element(Tid,Con,2),
        %List = string:tokens(ets:lookup_element(Tid,currentLine,2)," "),
        CurrentLine = ets:lookup_element(Tid,currentLine,2),        
	    Len = length(CurrentLine),
        if End < 0 ->
            string:substr(CurrentLine,Start,Len);
        true ->
            string:substr(CurrentLine,Start,End-Start+1)
        end
    end.		

    

count(List,Str) ->
   	count_t(List,Str,length(List),"").
count_t(_L,_S,0,R) ->R;
count_t(L,S,N,Re) ->
    [A|B] = L,
    Bool = (string:str(atom_to_list(A),S) == 1),
    if Bool ->
        count_t(B,S,0,A);
	true ->
        count_t(B,S,N-1,Re)
    end.		
 	

getCurrentLine() ->
    ets:lookup_element(Tid,currentLine,2).



	
 	
	