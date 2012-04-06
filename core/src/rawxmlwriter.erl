%%
%%
-module(rawxmlwriter,[StringBuffer]).
-compile(export_all).

-define(RawXmlWriter,rawxmlwriter).

new(StringBuffer) ->
	ID = ets:new(?RawXmlWriter, []),
	put(?RawXmlWriter,ID),
	ets:insert(ID, {buf, StringBuffer}),
	{?MODULE,StringBuffer}.

delete() ->
    ets:delete(get(?RawXmlWriter)).

writeSOAPHeader(Array)->
    [{_,S}] = ets:lookup(get(?RawXmlWriter), buf),
    NewS = if
        Array=:=[] ->
            S1=string:concat(S,"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"),
            S2=string:concat(S1,"<SOAP-ENV:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "),
            S3=string:concat(S2,"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" "),
            string:concat(S3,"xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\">\n");
        true ->
            appendstring(Array,S)
    end,
    ets:insert(get(?RawXmlWriter), {buf, NewS}).

appendstring([],S)->S;
appendstring([F|R],S)->
    appendstring(R,string:concat(S,F)).


startElement(S)->
    [{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
	S1=string:concat(Buf,"<"),
	S2=string:concat(S1,S),
	S3=string:concat(S2,">"),
	ets:insert(get(?RawXmlWriter), {buf, S3}).
	
endElement(S)->
	[{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
	S1=string:concat(Buf,"</"),
	S2=string:concat(S1,S),
	S3=string:concat(S2,">"),
	ets:insert(get(?RawXmlWriter), {buf, S3}).
	
emptyElement(S)->
	[{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
	S1=string:concat(Buf,"<"),
	S2=string:concat(S1,S),
	S3=string:concat(S2,"/>"),
	ets:insert(get(?RawXmlWriter), {buf, S3}).
	
startElementEnc(S)->
	[{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
	S1=string:concat(Buf,xmlEncode("<")),
	S2=string:concat(S1,xmlEncode(S)),
	S3=string:concat(S2,xmlEncode(">")),
	ets:insert(get(?RawXmlWriter), {buf, S3}).
	
endElementEnc(S)->
	[{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
	S1=string:concat(Buf,xmlEncode("</")),
	S2=string:concat(S1,xmlEncode(S)),
	S3=string:concat(S2,xmlEncode(">")),
	ets:insert(get(?RawXmlWriter), {buf, S3}).
	
emptyElementEnc(S)->
	[{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
	S1=string:concat(Buf,xmlEncode("<")),
	S2=string:concat(S1,xmlEncode(S)),
	S3=string:concat(S2,xmlEncode("/>")),
	ets:insert(get(?RawXmlWriter), {buf, S3}).
	
chardata([])->ok;
chardata([F|R]) ->
    [{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
    S1 = case F of
        60 ->   %%'<'
            "&lt;";
        62 ->   %%'>'
            "&gt;";
        34 ->   %%'"'
            "&quot;";
        38 ->   %%'&'
            "&amp;";
        _ ->
            [F]
    end,
    ets:insert(get(?RawXmlWriter), {buf, Buf++S1}),
    chardata(R).

xmlEncode(S)->
    S1 = "&",
    S2 = "&amp;",
    S3 = S,
    NewS3 = for1(S1,S2,S3,0),
    S4 = httputils:replaceString(NewS3,"\"","&quot;"),
    S5 = httputils:replaceString(S4,">","&gt;"),
    httputils:replaceString(S5,"<","&lt;").

for1(S1,S2,S3,I) ->
    I1 = httputils:indexOf(S3,S1,I+1),
    if
        I1=/=0 ->
            NewS3 = string:sub_string(S3,1,I1-1)++S2++string:sub_string(S3,I1+string:len(S1),string:len(S3)),
            for1(S1,S2,NewS3,I1);
        true ->
            S3
    end.

write(Char,char)->
    [{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
    ets:insert(get(?RawXmlWriter), {buf, Buf++[Char]});
write(String,str)->
    [{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
    ets:insert(get(?RawXmlWriter), {buf, Buf++String}).
    
toString() ->
    [{_,Buf}] = ets:lookup(get(?RawXmlWriter), buf),
    Buf.

enXMLElement(S)->
    S.

enCodeElement(S) ->
    S1 = "&",
    S2 = "&amp;",
    S3 = S,
    NewS3 = for1(S1,S2,S3,0),
    S4 = httputils:replaceString(NewS3,"\"","&quot;"),
    S5 = httputils:replaceString(S4,">","&gt;"),
    httputils:replaceString(S5,"<","&lt;").

for2(S1,S2,S3,I) ->
    I1 = httputils:indexOf(S3,S1,I+1),
    if
        I1=/=0 ->
            NewS3 = string:sub_string(S3,1,I1-1)++S2++string:sub_string(S3,I1+string:len(S1),string:len(S3)),
            for2(S1,S2,NewS3,I1);
        true ->
            S3
    end.
    
	