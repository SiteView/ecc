-module(url_sequence_client).
-compile(export_all).
-include("monitor_template.hrl").
-include("monitor.hrl").

-define(STEPPROPERTIES,["referenceType","reference","contentMatch","errorContent","postData","userName","password","domain","whenToAuthenticate","stepDelay","stepName","encoding","encodePostData"]).
-define(URLPREFIX,"http://").
-define(displayMax,80).
-define(formNameMax,30).


%nextStep
get_step(Params) ->
    get_step(Params,1).
    
get_step(Params,Step) ->
    case proplists:get_value("reference"++Step,Params) of
        undefined ->
            Step-1;
        _ ->
            get_step(Params,Step+1)
    end.

isVariableCountProperty(Name) ->
    Fun = fun(X) ->
        case re:run(Name,X) of
            nomatch ->
                false;
            _ ->
                true
        end
    end,
    case length(lists:filter(Fun,?STEPPROPERTIES)) of
        0 ->
            false;
        _ ->
            true
    end.

shouldPrintVariableCountProperty(Step,Name)->
    J = httputils:readIntegerFromEnd(Name),
    ((J=/=-1) and (J=<Step)).


get_classifier() ->
    M = url_sequence_monitor:new(),
    Good = hd(M:get_classifier(good)),
    Warn = hd(M:get_classifier(warning)),
    Error = hd(M:get_classifier(error)),
    [Good,Warn,Error].

add_monitor(Id, Params, Classifier) when is_atom(Id)->
    M = url_sequence_monitor:new(),
    Properties = api_monitor_template:get_template(url_sequence_monitor),
    %整理页面部分取到的所有值
    Config = prepare_params_for_add(Properties,Params,[]),
    M:delete(),
    ClassifierData = build_classifier(Classifier,Properties,[]),
    api_monitor:create(Id, [{id,undefined},{class,url_sequence_monitor}]++ClassifierData++Config);
add_monitor(_,_,_) ->{error, params_error}.

%要求：referenceN已经设好了,有关checkbox的property值若选中则为“on”
request(Params) ->
    %try
    M = url_sequence_monitor:new(),
    Properties = api_monitor_template:get_template(url_sequence_monitor),
    %整理页面部分取到的所有值
    Config = prepare_params(Properties,Params,M,[]),
    {Result,LastStepResult} = M:checkURLSequence(Config,"",null,"","","",M),
    Status = lists:nth(1,Result),
    Time = lists:nth(2,Result),
    ErrorMessage = LastStepResult#urlresults.errorMessage,
    {ok,{_,SequenceBuffer}} = M:get_attribute(sequenceBuffer),
    M:delete(),
    ResultText = if
        Status=:=200 ->
            Duration = io_lib:format("~.2f",[(Time/1000)])++" sec",
            "ok, "++Duration;
        true ->
            if
                ErrorMessage=/="" ->
                    ErrorMessage;
                true ->
                    httputils:lookupStatus(Status)
            end
    end,
    if
        Status=/=200 ->
            {error, ResultText};
        true ->
            {ok,[{time,Time},{body,LastStepResult#urlresults.body},{sequenceBuffer,SequenceBuffer}]}
    end.
    %catch
        %_:_  -> {error, "request error"}
    %end.
    
build_classifier([],_,Result) ->Result;
build_classifier([{K,V}|R],Props,Result) ->
%每个classifier可能含有多个值
    Classifier = parse_each_classifier(V,Props,[]),
    build_classifier(R,Props,[{list_to_atom(K),Classifier}|Result]);
build_classifier([_|R],Props,Result) ->
    build_classifier(R,Props,Result).
    
parse_each_classifier([],_,Result) -> lists:reverse(Result);
parse_each_classifier([{N,O,V}|R],Props,Result) ->
    Type = get_type(Props,list_to_atom(N)),
    V1 = change_value(Type,V),
    parse_each_classifier(R,Props,[{list_to_atom(N),list_to_atom(O),V1}|Result]);
parse_each_classifier([Oper|R],Props,Result) ->
    parse_each_classifier(R,Props,[list_to_atom(Oper)|Result]).
    
get_type([],_) ->notype;
get_type([#property{name=Name,type=Type}|R],Name) ->
    Type;
get_type([_|R],Name) ->
    get_type(R,Name).

prepare_params([],_,_,Result)->lists:reverse(Result);
prepare_params([F|R],Params,M,Result) ->
    Name = F#property.name,
    Type = F#property.type,
    Key = atom_to_list(Name),
    V = proplists:get_value(Key,Params),
    if
        V==undefined ->
            prepare_params(R,Params,M,Result);
        true ->
            Value = change_value(Type,V),
            case re:run(Key,"reference") of
                nomatch ->
                    M:set_property(Name,Value);
                _ ->
                    do_nothing
            end,
            prepare_params(R,Params,M,[{Name,Value}|Result])
    end.

prepare_params_for_add([],_,Result) ->Result;
prepare_params_for_add([F|R],Params,Result)->
    Name = F#property.name,
    Type = F#property.type,
    Key = atom_to_list(Name),
    V = proplists:get_value(Key,Params),
    Value = if
        V==undefined ->
            if
                Type==numeric ->
                    0;
                true ->
                    []
            end;
        true ->
            change_value(Type,V)
    end,
    prepare_params_for_add(R,Params,[{Name,Value}|Result]).

change_value(frequency,V) ->try list_to_integer(V) catch _:_ ->V end;
change_value(numeric,V) ->try list_to_integer(V) catch _:_ ->V end;
change_value(bool,V) ->try list_to_atom(V) catch _:_ ->V end;
change_value(_,V) ->V.
    
process_html(Body) ->
    HTMLTree = htmltagparser:process(Body),
    {FormList,FormInput} = getForms(HTMLTree),
    Links = getLinks(HTMLTree),
    Frames = getFrames(HTMLTree),
    Metas = getRefresh(HTMLTree),
    Fun = fun({K,V}) ->{K,lists:filter(fun(X)->case X of {[],[]} ->false;_ ->true end end, V)} end,
    lists:map(Fun, [{link,Links},{frame,Frames},{refresh,Metas},{formList,FormList}])++[{formInput,FormInput}].
    
makeTitle(S) ->
    if
        length(S)=<?displayMax ->
            S;
        true ->
            string:sub_string(S,1,?displayMax)
    end.
    
%find links
getLinks(Tree)->
    Links = htmltagparser:findTags(Tree,a),
    L1 = parse_link(Links,[]),
    Areas = htmltagparser:findTags(Tree,area),
    L2 = parse_area(Areas,[]),
    L1++L2.

parse_link([],Result)->lists:reverse(Result);
parse_link([Tag|R],Result) ->
    Value = string:strip(htmltagparser:getValue(Tag,contents)),
    Title = makeTitle(Value),
    parse_link(R,[{Value,Title}|Result]).
    
parse_area([],Result)->lists:reverse(Result);
parse_area([Tag|R],Result)->
    V = string:strip(htmltagparser:getValue(Tag,contents)),
    ContainImg = string:rstr(string:to_lower(V),"<img")=/=0,
    V1 = if
        ((length(V)==0) or ContainImg) ->
            string:strip(htmltagparser:getValue(Tag,href));
        true ->
            V
    end,
    Title = makeTitle(V1),
    parse_area(R,[{V1,Title}|Result]).
%    

%find frames
getFrames(Tree)->
    Frames = htmltagparser:findTags(Tree,frame),
    IFrames = htmltagparser:findTags(Tree,iframe),
    getFrames(Frames++IFrames,[]).

getFrames([],Result) ->lists:reverse(Result);
getFrames([F|R],Result) ->
    V = case string:strip(htmltagparser:getValue(F,name)) of
        "" ->
            string:strip(htmltagparser:getValue(F,src));
        Other ->
            Other
    end,
    Title = makeTitle(V),
    getFrames(R,[{V,Title}|Result]).
%

%find refresh
getRefresh(Tree) ->
    Metas = htmltagparser:findTags(Tree,meta),
    parse_meta(Metas,[]).

parse_meta([],Result) ->lists:reverse(Result);
parse_meta([F|R],Result) ->
    Value = string:strip(htmltagparser:getValue(F,'http-equiv')),
    IsRefresh = string:to_lower(Value)=:="refresh",
    if
        IsRefresh ->
            URL = string:strip(htmltagparser:getValue(F,content)),
            Title = makeTitle(URL),
            parse_meta(R,[{URL,Title}|Result]);
        true ->
            parse_meta(R,Result)
    end.
%

%find forms
getForms(Tree)->
    Forms = htmltagparser:findTags(Tree,form),
    parse_form(Forms,1,[],[]).
    
parse_form([],_,PostData,Result) ->{Result, PostData};
parse_form([F|R],Index,PostData,Result) ->
    Name = case string:strip(htmltagparser:getValue(F,name)) of
        "" ->
            string:strip(htmltagparser:getValue(F,action));
        Other ->
            Other
    end,
    FP = "[" ++ integer_to_list(Index) ++ "]" ++ Name,
    FormPrefix = case length(FP) of
        Len when Len>?formNameMax ->
            string:sub_string(FP,1,?formNameMax);
        _ ->
            FP
    end,
    Inputs = htmltagparser:findTags(F,input)++htmltagparser:findTags(F,select)++htmltagparser:findTags(F,button),
    {NR,NPd} = parse_input(Inputs,Index,0,FormPrefix,PostData,Result),
    parse_form(R,Index+1,NPd,NR).
    
parse_input([],FormCnt,ButtonCnt,FormPrefix,PostData,Result) ->
    V = if 
        ButtonCnt==0 ->
            ButtonName = "[" ++ integer_to_list(FormCnt) ++ "]",
            [{"{" ++ FormPrefix ++ "}" ++ ButtonName,"{" ++ FormPrefix ++ "}" ++ ButtonName}];
        true ->
            []
    end,
    {Result++V, PostData};
parse_input([F|R],FormCnt,ButtonCnt,FormPrefix,PostData,Result) ->
    Tag = string:to_lower(htmltagparser:getValue(F,tag)),
    if
        Tag=="select" ->
            Options = htmltagparser:findTags(F,option),
            InputField = htmltagparser:getValue(F, name),
            InputValue = parse_option(Options,null,null),
            BC = ButtonCnt,
            NewResult = Result;
        true ->
            Type = string:to_lower(htmltagparser:getValue(F,type)),
            Ischecked = htmltagparser:getValue(F,checked)=/="",
            if
                Type=="submit" -> %提交的控件也会作为formselect的内容之一
                    BC = ButtonCnt+1,
                    BN = if
                        Tag=="button" ->
                            htmltagparser:getValue(F,name);
                        true ->
                            htmltagparser:getValue(F,value)
                    end,
                    ButtonName = if
                        length(BN)==0 ->
                            "Submit";
                        true ->
                            BN
                    end,
                    InputField = "",
                    InputValue = "";
                Type=="image" ->
                    BC = ButtonCnt+1,
                    BN = htmltagparser:getValue(F,name),
                    BN1 = if
                        length(BN)==0 ->
                            htmltagparser:getValue(F,alt);
                        true ->
                            BN
                    end,
                    BN2 = if
                        length(BN1)==0 ->
                            htmltagparser:getValue(F,src);
                        true ->
                            BN1
                    end,
                    ButtonName = if
                        length(BN2)==0 ->
                            "["++ integer_to_list(BC) ++"]";
                        true ->
                            BN2
                    end,
                    InputField = "",
                    InputValue = "";
                ((Type=="text") or (Type=="password") or (Type=="hidden") or (Type=="checkbox") or (Type=:="")) ->
                    BC = ButtonCnt,
                    ButtonName = "",
                    InputField = htmltagparser:getValue(F,name),
                    InputValue = htmltagparser:getValue(F,value);
                ((Type=="radio") and Ischecked) ->
                    BC = ButtonCnt,
                    ButtonName = "",
                    InputField = htmltagparser:getValue(F,name),
                    InputValue = htmltagparser:getValue(F,value);
                true ->
                    BC = ButtonCnt,
                    ButtonName = "",
                    InputField = "",
                    InputValue = ""
            end,
            if
                length(ButtonName)>0 ->
                    AddFormNum = if
                        FormCnt>1 ->
                            parseResult(Result,ButtonName,"",FormCnt);
                        true ->
                            ""
                    end,
                    Len = string:len(ButtonName),
                    HalfLen = round(?displayMax/2),
                    NewButtonName = if
                        (Len>HalfLen) ->
                            string:sub_string(ButtonName,1,HalfLen);
                        true ->
                            ButtonName
                    end,
                    NewResult = Result++[{"{"++FormPrefix++"}"++NewButtonName++AddFormNum,"{"++FormPrefix++"}"++NewButtonName}];
                true ->
                    NewResult = Result
            end
    end,
    %增加postdata的内容
    NewPostData = if
        length(InputField)>0 ->
            Dot = if
                length(PostData)>0 ->
                    "\n";
                true ->
                    ""
            end,
            PostData++Dot++"{"++FormPrefix++"}"++InputField++"="++InputValue;
        true ->
            PostData
    end,
    parse_input(R,FormCnt,BC,FormPrefix,NewPostData,NewResult).      
        
parseResult([],_,AddFormNum,_)->AddFormNum;
parseResult([{_,V}|R],ButtonName,AddFormNum,FormCnt)->
    SButtonName = try httputils:getStrippedButtonName(V) of
        Value ->
            Value
    catch error:X->X,
        null
    end,
    if
        ButtonName=:=SButtonName ->
            "["++integer_to_list(FormCnt)++"]";
        true ->
            parseResult(R,ButtonName,AddFormNum,FormCnt)
    end.


parse_option([],SelectedOption,FirstOption) ->
    if
        SelectedOption==null ->
            FirstOption;
        true ->
            SelectedOption
    end;
parse_option([F|R],SelectedOption,FirstOption) ->
    Isselected = (htmltagparser:get(F,selected)/=null),
    if
        FirstOption==null ->
            FO = htmltagparser:getValue(F,value),
            SO = SelectedOption;
        ((SelectedOption==null) and Isselected) ->
            FO = FirstOption,
            SO = htmltagparser:getValue(F, value);
        true ->
            FO = FirstOption,
            SO = SelectedOption
    end,
    parse_option(R,SO,FO).
%
    