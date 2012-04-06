
%IoDevice is file:open()'s return value
-module(braf,[Object,IoDevice,CurrentPos,Tid]).
-compile(export_all).
-include("monitor.hrl").

-define(BUF_SIZE,32768).

close() ->
    ets:delete(Tid),  
    file:close(IoDevice).

read() ->
    %Fileb = fillBuffer(),
    %io:format("Fileb:~p~n",[Fileb]),
    EndObjectList = ets:lookup(Tid,buf_end),
    %io:format("EndObjectList:~p~n",[EndObjectList]),
    if length(EndObjectList)>0  ->
        [{buf_end,Buf_end}] = EndObjectList;
    true ->
        Buf_end = 0    
    end,
    PosObjectList = ets:lookup(Tid,buf_pos),
    if length(PosObjectList)>0  ->
        [{buf_pos,Buf_pos}] = PosObjectList;
    true ->
        Buf_pos = 1    
    end, 
    BufferObjectList = ets:lookup(Tid,buffer),
    % io:format("%%%%PosObjectList:~p~n",[PosObjectList]), 
     %io:format("BufferObjectList:~p~n",[BufferObjectList]),
    if length(BufferObjectList)>0  ->
        [{buffer,Buffer}] = BufferObjectList;
    true ->
        Buffer = ""    
    end,      
    if Buf_pos >= Buf_end  ->
        -1;
    true ->
        if Buf_end == 0 ->
            -1;
        true ->
            %ets:insert(Tid,{buf_pos,Buf_pos+1}),
            %io:format("@#$%^&*&^%$#$%^&*~n"),
            ets:insert_new(Tid,{buf_pos,Buf_pos+1}),        
            string:substr(Buffer,Buf_pos,1)
        end
    end.    
    
lastLineLength() ->
    ets:lookup_element(Tid,lineLength,2).
    %{ok,{_,LineLength}} = THIS:get_property(lineLength),
    %LineLength.
    
fillBuffer() ->
    Filebuffer = file:pread(IoDevice,CurrentPos,?BUF_SIZE),
    case Filebuffer of
    {ok,Data} ->
        case ets:insert_new(Tid,[{buffer,Data},{buf_end,length(Data)},{buf_pos,1}]) of
        true ->
            length(Data);
         _ ->
            -1 
        end;
    _ ->
        ets:insert_new(Tid,[{buffer,""},{buf_end,0},{buf_pos,1}]),  
        -1
    end.        
          
readLine() ->
    readLine(false).
    
readLine(Flag) ->
    String = readWholeLine(),
    if String /= "" ->
        ets:insert(Tid,{lineLength,length(String)});
        %THIS:set_property(lineLength,length(String));
    true ->
        null
    end,    
    if Flag == false , String /= "" ->
        Len = length(String),    
        Substring = readLine_util(String);
    true ->
        String
    end.   
        
readLine_util(String) ->
    readLine_util_t(String,length(String)).
readLine_util_t(EStr,0) -> EStr;
readLine_util_t(Str,Num) ->
    if Num =< 0 ->
        readLine_util_t(Str,0);
    true ->
        Char = string:substr(Str,Num,1),
        if Char /= "\n" , char /= "\r" ->
            readLine_util_t(Str,0); 
        true ->
            SubStr = string:substr(Str,1,Num-1),
            readLine_util_t(SubStr,length(SubStr))
        end    
    end.

readWholeLine() ->
    TempBuf_end = ets:lookup_element(Tid,buf_end,2),
    TempBuf_pos = ets:lookup_element(Tid,buf_pos,2),
    TempBuf = ets:lookup_element(Tid,buffer,2),
    %{ok,{_,TempBuf_end}} = THIS:get_property(buf_end),
    %{ok,{_,TempBuf_pos}} = THIS:get_property(buf_pos),
    %{ok,{_,TempBuf}} = THIS:get_property(buffer),    
    if (TempBuf_end - TempBuf_pos) =< 0 -> 
        EndInt = fillBuffer(),
        if EndInt < 0 ->
           "";
        true ->
            Buf_end = ets:lookup_element(Tid,buf_end,2),
            Buf_pos = ets:lookup_element(Tid,buf_pos,2),
            Buf = ets:lookup_element(Tid,buffer,2),
            LineEndPos = readWholeLine_util(Buf_pos,Buf_end),
            if  LineEndPos > Buf_pos ->
                K = (LineEndPos - Buf_pos) +1,
                ets:insert(Tid,{buf_pos,LineEndPos+1}),
                Object:set_property(buf_pos,LineEndPos+1),
                String = string:substr(Buf,Buf_pos,K);
            true ->
                readWholeLine_util_1(0) 
            end
        end;
    true ->
        Buf_end = TempBuf_end,
        Buf_pos = TempBuf_pos,
        Buf = TempBuf,
        LineEndPos = readWholeLine_util(Buf_pos,Buf_end), 
        if  LineEndPos > Buf_pos ->
            K = (LineEndPos - Buf_pos) +1,
            ets:insert(Tid,{buf_pos,LineEndPos+1}),
            Object:set_property(buf_pos,LineEndPos+1),
            String = string:substr(Buf,Buf_pos,K);
        true ->
            readWholeLine_util_1(0) 
        end        
    end. 
        
readWholeLine_util(Pos,End) ->
    Buf = ets:lookup_element(Tid,buffer,2),
    %{ok,{_,Buf}} = THIS:get_property(buffer),
    readWholeLine_util_t(Pos,End,string:substr(Buf,Pos),true).
readWholeLine_util_t(NowPos,_End,_Buf,false) -> NowPos;
readWholeLine_util_t(Pos1,End1,[SubChar|Buf1],Flag) ->
    if Pos1 >= End1 ->
        %ets:insert_new(Tid,{buf_pos,Pos1}),  
        readWholeLine_util_t(Pos1,End1,Buf1,false); 
    true ->
        if SubChar == 10 ->
            %ets:insert_new(Tid,{buf_pos,Pos1}),    
            readWholeLine_util_t(Pos1,End1,Buf1,false);
        true ->            
            readWholeLine_util_t(Pos1+1,End1,Buf1,Flag)
        end 
    end.   
    
    
readWholeLine_util_1(Int) ->
    readWholeLine_util_1_t(Int,"",true).
readWholeLine_util_1_t(_Int,Str,false) -> Str;
readWholeLine_util_1_t(Int1,Str1,Flag) ->
    Char = read(), % L
    %io:format("#####Char:~p~n",[Char]),
    if (Int1 >= 4096) or (Char == -1) ->
        readWholeLine_util_1_t(Int1,Str1,false);
    true ->
        StringBuffer =  Str1 ++ Char,
        if (Char == "\n") or (Char < 0)  ->
            readWholeLine_util_1_t(Int1,StringBuffer,false);
        true ->
            readWholeLine_util_1_t(Int1+1,StringBuffer,Flag) 
        end        
    end.
        
        
        
        

