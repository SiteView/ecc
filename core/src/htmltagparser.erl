%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc parse html tags.
%% 
%% Description: Use yaws_html to parse html into a html tags tree,it can find special tags type
-module(htmltagparser).
-compile(export_all).
-include("monitor.hrl").

-record(begin_tag,{type,content,sequence}).
-record(end_tag,{type,content,sequence}).
-record(data,{content,sequence}).

%%Note that when the analysis is owned by some non-html errors may occur when

process(HTML)->
    try yaws_html:parse(HTML) of
		Tags ->
			Tags
	catch
	error:X->X,
	[]
	end.
    
%%test
test(URL)->
    inets:start(),
    {ok,{_,_,B}} = http:request(URL),
    Tree = process(B),
    findTags(Tree,a).

%%Tag itself, only to take
findSingleTags([],_)->[];
findSingleTags([#begin_tag{type=Type,content=Content,sequence=Sequence}|R],Type) ->
    [[#begin_tag{type=Type,content=Content,sequence=Sequence}]]++findSingleTags(R,Type);
findSingleTags([_|R],Type) ->
    findSingleTags(R,Type).
        
%%This function can find the complete tag (including its internal and other tags), for only the start tag of the html tag returns only its own, type a string in the form required
findTags(Tree,a) when is_list(Tree) ->
    finda(Tree,[],1,false,[],a);
findTags(Tree,Type) when is_list(Tree)->
    Tags = [],
    TagList = [],
    SingleTagType = [input,meta,option],
    case lists:member(Type,SingleTagType) of
        true ->
            findSingleTags(Tree,Type);
        _->
            findTags(Tree,Type,Tags,TagList,false)
    end;
findTags(Html,Type) ->
    Tree = process(Html),
    findTags(Tree,Type).

findTags([],_,_,TagList,_) ->TagList;
%%For the terminator is <form> (forgot to add / case) to do fault-tolerant
findTags([#begin_tag{type=form,content=[],sequence=Sequence}|R],form,Tags,TagList,true)->
    FinalTag = Tags++[#end_tag{type=form,content=[],sequence=Sequence}],
    NewTag = [],
    findTags(R,form,NewTag,TagList++[FinalTag],false);
findTags([#begin_tag{type=Type,content=Content,sequence=Sequence}|R],Type,Tags,TagList,_)->
%%This tag is only beginning to determine whether the marker
    OnlyBeginTag = lists:member("/",Content),
    if
        OnlyBeginTag ->
            findTags(R,Type,Tags,TagList++[[#begin_tag{type=Type,content=Content,sequence=Sequence}]],false);
        true ->
            findTags(R,Type,Tags++[#begin_tag{type=Type,content=Content,sequence=Sequence}],TagList,true)
    end;
findTags([#end_tag{type=Type,content=Content,sequence=Sequence}|R],Type,Tags,TagList,_)->
    FinalTag = Tags++[#end_tag{type=Type,content=Content,sequence=Sequence}],
    NewTag = [],
    findTags(R,Type,NewTag,TagList++[FinalTag],false);
findTags([F|R],Type,Tags,TagList,true)->
    NewTags = Tags++[F],
    findTags(R,Type,NewTags,TagList,true);
findTags([_|R],Type,Tags,TagList,Flag)->
    findTags(R,Type,Tags,TagList,Flag).


finda([],_,_,_,A,_) ->A;
finda([#begin_tag{type=Type,content=Content,sequence=Sequence}|R],Con,N,_,A,Type)->
    finda(R,Con++[#begin_tag{type=a,content=Content,sequence=Sequence}],N+1,true,A,Type);
finda([#end_tag{type=Type,content=Content,sequence=Sequence}|R],Con,N,true,A,Type) when N=<6 ->
    finda(R,[],1,false,A++[Con++[#end_tag{type=Type,content=Content,sequence=Sequence}]],Type);
finda([F|R],Con,7,_,A,Type)->
    finda(R,[],1,false,A++[Con++[F]++[#end_tag{type=a,content=[],sequence=1}]],Type);
finda([F|R],Con,N,true,A,Type)->
    finda(R,Con++[F],N+1,true,A,Type);
finda([_|R],Con,N,Flag,A,Type)->
    finda(R,Con,N,Flag,A,Type).

%%Non-key values ??of the form
get([#begin_tag{content=Content}|_],Type) when is_list(Type)->
    NotKV = [string:to_lower(X)||X<-Content,is_list(X)],
    if
        length(NotKV)>0 ->
            Exist = lists:member(string:to_lower(Type),NotKV),
            if
                Exist ->
                    Type;
                true ->
                    null
            end;
        true ->
            null
    end;
get([#begin_tag{content=Content}|R],Type)->
    get([#begin_tag{content=Content}|R],atom_to_list(Type));
get(_,_)->null.
    

%%type must be atom
getValue([],_)->"";
getValue(Tag,href) ->
    href(Tag,href);
getValue([],tag) ->"";
getValue([#begin_tag{type=Type}|_],tag) ->
    atom_to_list(Type);
getValue(Tag,contents) ->
    Len = length(Tag),
    if
        Len<3 -> %%Only a start tag or the tag with no other content
            "";
        true ->
            unfoldTags(lists:sublist(Tag,2,Len-2))
    end;
getValue([#begin_tag{content=Content}|_],Type) ->
    case proplists:get_value(Type,Content) of
        undefined ->
            "";
        Value ->
            Value
    end;
getValue([F|R],Type) when is_list(F) ->
    case proplists:get_value(atom_to_list(Type),F) of
        undefined ->
            getValue(R,Type);
        Other ->
            Other
    end;
getValue(_,_)->"".

href([#begin_tag{content=Content}|_],Type) ->
    case proplists:get_value(Type,Content) of
        undefined ->
            "";
        Value ->
            Value++lists:append(["="++V||{K,V}<-Content,K=:=''])
    end;
href(_,_)->"".

%% As the span will display all possible html tags into the corresponding function in the treatment of html so this changes the display contents when the contents of
%% Change it from an html form

unfoldTags1([])->"";
unfoldTags1([#begin_tag{type=Type,content=Content}|R])->
%%This tag is only beginning to determine whether the marker
    OnlyBeginTag = lists:member("/",Content),
    EndDot = if
        OnlyBeginTag ->
            "/)";
        true ->
            ")"
    end,
    Tag = if
        Content=:=[] ->
            "("++atom_to_list(Type)++EndDot;
        true ->
            "("++atom_to_list(Type)++" "++content2string(Content)++EndDot
    end,
    Tag++unfoldTags1(R);
unfoldTags1([#data{content=Content}|R])->
    Content++unfoldTags1(R);
unfoldTags1([#end_tag{type=Type}|R])->
    EndTag = "(/"++atom_to_list(Type)++")",
    EndTag++unfoldTags1(R);
unfoldTags1([_|R])->
    unfoldTags1(R).
    


%%Html put together again
unfoldTags([])->"";
unfoldTags([#begin_tag{type=Type,content=Content}|R])->
%%To determine whether this tag is only a start tag
    OnlyBeginTag = lists:member("/",Content),
    EndDot = if
        OnlyBeginTag ->
            "/>";
        true ->
            ">"
    end,
    Tag = if
        Content=:=[] ->
            "<"++atom_to_list(Type)++EndDot;
        true ->
            "<"++atom_to_list(Type)++" "++content2string(Content)++EndDot
    end,
    Tag++unfoldTags(R);
unfoldTags([#data{content=Content}|R])->
    Content++unfoldTags(R);
unfoldTags([#end_tag{type=Type}|R])->
    EndTag = "</"++atom_to_list(Type)++">",
    EndTag++unfoldTags(R);
unfoldTags([_|R])->
    unfoldTags(R).
    
content2string([])->"";
content2string([F|R])->
    Content = case F of
        {K,V}->
            atom_to_list(K)++"="++"\""++V++"\"";
        _ ->
            ""
    end,
    Dot = if
        R=/=[] ->
            " ";
        true ->
            ""
    end,
    Content++Dot++content2string(R).
    
getVariables1(FormTag,ButtonName,ButtonValue) ->
    Len = length(FormTag),
    %%array is in the middle of all the other nodes formtag
    Array = if
        Len<3 ->
            [];
        true ->
            lists:sublist(FormTag,2,Len-2)
    end,
    getVariables2(Array,ButtonName,ButtonValue).
    
getVariables2(FormTag,ButtonName,ButtonValue) ->
    Variables = getFormInputs(FormTag,ButtonName,ButtonValue),
    getValueFromVariables(Variables,[]).
    
getValueFromVariables([],Result)->Result;
getValueFromVariables([F|R],Result)->
    Value = case proplists:get_value("_value",F) of
        undefined ->
            null;
        Other ->
            Other
    end,
    getValueFromVariables(R,Result++[Value]).

getFormInputs(FormTag,ButtonName,ButtonValue) ->
    Inputs = htmltagparser:findTags(FormTag,input)++htmltagparser:findTags(FormTag,button),
    {FirstRadio,Variables} = parseInputs(Inputs,ButtonName,ButtonValue,[],[]),
    Radios = [K||{K,_}<-FirstRadio],
    Variables1 = parseRadios(Radios,FirstRadio,Variables),
    Textareas = findTags(FormTag,textarea),
    Variables2 = parseTextareas(Textareas,Variables1),
    Selects = findTags(FormTag,select),
    parseSelects(Selects,Variables2).

parseInputs([],_,_,FirstRadio,Variables)->{FirstRadio,Variables};
parseInputs([InputTag|R],ButtonName,ButtonValue,FirstRadio,Variables) ->
    Type = case string:to_lower(getValue(InputTag,type)) of
        "" ->
            "text";
        Tp->
            Tp
    end,
    if
        Type=:="text" orelse Type=:="hidden" orelse Type=:="password" ->
            Value = getValue(InputTag,value),
            NewVariables = addValue(Variables,InputTag,getValue(InputTag,name),Value),
            NewFirstRadio = FirstRadio;
        Type=:="radio" ->
            RadioName = getValue(InputTag,name),
            NewFirstRadio = case proplists:get_value(RadioName,FirstRadio) of
                undefined ->
                    FirstRadio++[{RadioName,InputTag}];
                _->
                    FirstRadio
            end,
            case getValue(InputTag,checked) of
                "" ->
                    NewVariables = Variables;
                _->
                    NewVariables = addValue(Variables,InputTag,getValue(InputTag,name),getValue(InputTag,value))
            end;
        Type=:="checkbox" ->
            NewFirstRadio = FirstRadio,
            case getValue(InputTag,checked) of
                "" ->
                    NewVariables = Variables;
                _->
                    NewVariables = addValue(Variables,InputTag,getValue(InputTag,name),getValue(InputTag,value))
            end;
        Type=:="submit" ->
            NewFirstRadio = FirstRadio,
            case getValue(InputTag,name) of
                "" ->
                    NewVariables = Variables;
                _->
                    if
                        ButtonName=:=null ->
                            NewVariables = addValue(Variables,InputTag,getValue(InputTag,name),getValue(InputTag,value));
                        true ->
                            Flag = ((getValue(InputTag,name)=:=ButtonName) and (getValue(InputTag,value)=:=ButtonValue)),
                            if
                                Flag ->
                                    NewVariables = addValue(Variables,InputTag,ButtonName,ButtonValue);
                                true ->
                                    NewVariables = Variables
                            end
                    end       
            end;
        Type=:="image" ->
            NewFirstRadio = FirstRadio,
            case getValue(InputTag,name) of
                "" ->
                    NewVariables = Variables;
                _->
                    Flag = getValue(InputTag,name)=:=ButtonName,
                    if
                        ButtonName=:=null ->
                            Variables1 = addValue(Variables,InputTag,getValue(InputTag,name)+".x","1"),
                            NewVariables = addValue(Variables1,InputTag,getValue(InputTag,name)+".y","1");
                        Flag ->
                            Variables1 = addValue(Variables,InputTag,ButtonName+".x","1"),
                            NewVariables = addValue(Variables1,InputTag,getValue(InputTag,name)+".y","1");
                        true ->
                            NewVariables = Variables
                    end
            end;
        true ->
            NewFirstRadio = FirstRadio,
            NewVariables = Variables
    end,
    parseInputs(R,ButtonName,ButtonValue,NewFirstRadio,NewVariables).

parseRadios([],_,Variables)->Variables;
parseRadios([RadioName|R],FirstRadio,Variables)->
    Radio = proplists:get_value(RadioName,FirstRadio),
    Flag = findVariable(Variables,RadioName)=:=null,
    if
        Flag ->
            NewVariables = addValue(Variables,Radio,RadioName,getValue(Radio,value));
        true ->
            NewVariables = Variables
    end,
    parseRadios(R,FirstRadio,NewVariables).
    
findVariable(Variables,Key)->
    Key1 = Key++"=",
    forfindVariable(Variables,Key1).
    
forfindVariable([],_)->null;
forfindVariable([Variable|R],Key)->
    Value = getValue(Variable,'_value'),
    Flag = httputils:startsWith(Value,Key),
    if
        Flag ->
            Variable;
        true ->
            forfindVariable(R,Key)
    end.
    
parseTextareas([],Variables) ->Variables;
parseTextareas([TextareaTag|R],Variables) ->
    Value = getValue(TextareaTag,contents),
    addValue(Variables,TextareaTag,getValue(TextareaTag,name),Value),
    parseTextareas(R,Variables).
    
parseSelects([],Variables) ->Variables;
parseSelects([SelectTag|R],Variables) ->
    SelectName = getValue(SelectTag,name),
    Multiple = getValue(SelectTag,multiple)=/="",
    Options = findTags(SelectTag,option),
    {NewVariables,FoundSelection,SelectedOption} = parseOptions(Options,Variables,false,Multiple,SelectName,null),
    NewVariables1 = if
        Multiple ->
            Flag1 = (not FoundSelection),
            if
                Flag1 ->
                    addValue(NewVariables,SelectedOption,SelectName,"");
                true ->
                    NewVariables
            end;
        true ->
            if
                SelectedOption=/=null ->
                    Flag2 = getValue(SelectedOption,value)=:="",
                    ValueSource1 = if
                        Flag2 ->
                            contents;
                        true ->
                            value
                    end,
                    addValue(NewVariables,SelectedOption,SelectName,getValue(SelectedOption,ValueSource1));
                true ->
                    NewVariables
            end
    end,
    parseSelects(R,NewVariables1).

parseOptions([],Variables,FoundSelection,_,_,SelectedOption)->{Variables,FoundSelection,SelectedOption};
parseOptions([OptionTag|R],Variables,FoundSelection,Multiple,SelectName,SelectedOption)->
    Flag = getValue(OptionTag,selected)=/="",
    if
        Flag ->
            FoundSelection1 = true,
            if
                Multiple ->
                    Flag1 = getValue(OptionTag,value)=:="",
                    ValueSource = if
                        Flag1 ->
                            contents;
                        true ->
                            value
                    end,
                    NewVariables = addValue(Variables,OptionTag,SelectName,getValue(OptionTag,ValueSource)),
                    NewSelectedOption = SelectedOption;
                true ->
                    NewVariables = Variables,
                    NewSelectedOption = OptionTag
            end;
        true ->
            NewSelectedOption = if
                SelectedOption=:=null ->
                    OptionTag;
                true ->
                    SelectedOption
            end,
            NewVariables = Variables,
            FoundSelection1 = FoundSelection
    end,
    parseOptions(R,NewVariables,FoundSelection1,Multiple,SelectName,NewSelectedOption).

addValue(Variables,Tag,Name,Value) ->
    NewName = if
        Name=:=null ->
            [];
        true ->
            Name
    end,
    NewValue = if
        Value=:=null ->
            [];
        true ->
            Value
    end,
    Index = string:str(NewName,"="),
    Name1 = if
        Index=/=0 ->
            httputils:replaceAll(NewName,"=","\\eq.");
        true ->
            NewName
    end,
    NewTag = Tag++[{"_value",Name1++"="++NewValue}],
    Variables++[NewTag].
        
findVar(Variables,Key) ->
    Key1 = Key++"=",
    findkey(Variables,Key1).

findkey([],_)->null;
findkey([F|R],Key) ->
    case httputils:startsWith(F,Key) of
        true ->
            F;
        _->
            findkey(R,Key)
    end.


