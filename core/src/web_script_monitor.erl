%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2010 dragonflow, Inc.
%% @version 1.0
%% @doc web script monitor.
%%
%% Description: Test scripts created by selenium ide
-module(web_script_monitor,[BASE]).   
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for web script monitor
new()->
	Base = atomic_monitor:new(),
	{?MODULE,Base}.
    
%% @spec defaultTitle(Params) ->string()
%%  Params = [term()]
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	File = proplists:get_value(file,Params),
    F = case File of
        "templates.webscripts/"++T ->
            T;
        O ->
            O
    end,
	if
		length(File)>0->
			BASE:defaultTitle(Params) ++":" ++ F;
		true ->
			BASE:defaultTitle(Params)
	end.
    
%% @spec verify(Params) -> ErrorList
%% where
%% Params = [{atom(),integer()|string()}]
%% ErrorList = [string()]
%% @doc page check.
verify(Params)->
	Errs =
	case proplists:get_value(server,Params) of
		""->
			[{url,"Selenium server host is missing"}];
		_ ->
			[]
	end ++
	case proplists:get_value(port,Params) of
		""->
			[{port,"Selenium server port is missing"}];
		Port when is_integer(Port)->
            [];
        _ ->
            [{port,"Port must be a integer"}]
	end ++
	case BASE:verify(Params) of
		{error,E}->
			E;
		_->
			[]
	end,
	if
		length(Errs)>0 ->
			{error,Errs};
		true ->
			{ok,""}
	end.
    
%% @spec update() -> ok;
%% @doc update is the run function called by schedule to test the  web script monitor
update()->
    {ok,{_,File}} = THIS:get_property(file),
    {ok,{_,Browser}} = THIS:get_property(browser),
    {ok,{_,Install}} = THIS:get_property(installDirectory),
    {ok,{_,Server}} = THIS:get_property(server),
    {ok,{_,Port}} = THIS:get_property(port),
    BrowserContent = case Browser of
        "*"++_ ->
            if
                length(Install)==0 ->
                    Browser;
                true ->
                    Browser++" "++Install
            end;
        _ ->
            "*custom "++Browser
    end,
    case read_script(File) of
        {ok, Script} ->
            {Commands,URL} = parse_script(Script),
            io:format("com:~p, base url:~p~n",[Commands, URL]),
            if
                length(URL)==0 ->
                    THIS:set_attribute(round_trip_time,"n/a"),
                    THIS:set_attribute(?NO_DATA, true),
                    THIS:set_attribute(state,400),
                    THIS:set_attribute(?STATE_STRING,"script missing base url");
                true ->
                    Session = [{server,{Server,Port}},{browser,BrowserContent},{url,URL}],
                    T1 = httputils:currentTimeMillis(),
                    try selenium:run(Session,Commands) of
                        R ->
                            io:format("web script result:~p~n",[R]),
                            T2 = httputils:currentTimeMillis(),
                            THIS:set_attribute(round_trip_time,T2 - T1),
                            State = parse_result(R,""),
                            if
                                length(State)>0 ->
                                    THIS:set_attribute(state,400),
                                    THIS:set_attribute(?STATE_STRING, State);
                                true ->
                                    THIS:set_attribute(state,200),
                                    THIS:set_attribute(?STATE_STRING, "No error detected")
                            end
                    catch _:Error ->
                        T2 = httputils:currentTimeMillis(),
                        THIS:set_attribute(round_trip_time,T2 - T1),
                        THIS:set_attribute(state,400),
                        THIS:set_attribute(?NO_DATA, true),
                        ErrorStatus = case Error of
                            {_,{_, ER}} when is_atom(ER) ->
                                atom_to_list(ER);
                            {_,{_, ER}} when is_list(ER) ->
                                ER;
                            O ->
                                lists:flatten(io_lib:format("~p",[O]))
                        end,
                        THIS:set_attribute(?STATE_STRING, ErrorStatus)
                    end
            end;
        {error, Reason} ->
            THIS:set_attribute(round_trip_time,"n/a"),
            THIS:set_attribute(?NO_DATA, true),
            THIS:set_attribute(state,400),
            THIS:set_attribute(?STATE_STRING,Reason)
    end.
            
parse_result([],Result) ->Result;
parse_result([{_,{_,{ok,_}}}|R],Result) ->
    parse_result(R,Result);
parse_result([{{Action,_Params},{_,{error,Reason}}}|R],Result) ->
    parse_result(R,Result++"<br>"++"Action: "++atom_to_list(Action)++" Result: "++Reason);
parse_result([_|R],Result) ->
    parse_result(R,Result).

read_script(File) ->
    case file:read_file(File) of
        {ok,B} ->
            {ok, binary_to_list(B)};
        _ ->
            {error, "read file: "++File++" error"}
    end.

parse_script(Script) ->
    %取得Encode
    M = url_monitor:new(),
    Encode = M:getPageEncoding(Script),
    io:format("script encode:~p~n",[Encode]),
    M:delete(),
    %使用encode进行编码
    EncodedScript = case string:to_lower(Encode) of
        "utf-8" ->
            io:format("no encode~n"),
            Script;
        _ ->
            iconv:convert(Encode, httputils:pageEncode(), Script)
    end,
    Tree = htmltagparser:process(EncodedScript),
    %取得baseURL
    BaseURL = get_baseURL(Tree),
    io:format("base url:~p~n",[BaseURL]),
    %除第一个tr外，其他每个tr都是一个动作
    TR = htmltagparser:findTags(Tree,tr),
    Commands = if
        length(TR)>1 ->
            parse_tr(lists:nthtail(1, TR),[]);
        true ->
            []
    end,
    {Commands,BaseURL}.

get_baseURL(Tree) ->
    Link = htmltagparser:findTags(Tree, link),
    parse_link(Link).
    
parse_link([]) ->"";
parse_link([F|R]) ->
    case htmltagparser:getValue(F,rel) of
        "selenium.base" ->
            htmltagparser:getValue(F,href);
        _ ->
            parse_link(R)
    end.

parse_tr([],Result) ->lists:reverse(Result);
parse_tr([F|R],Result) ->
    C = each_tr(F,false,[]),
    parse_tr(R,[C|Result]).

each_tr([],_,Td) ->
    build_command(lists:reverse(Td));
each_tr([{end_tag,td,_,_}|R],_,Td) ->
    each_tr(R,false,Td);
each_tr([{begin_tag,td,_,_}|R],_,Td) ->
    each_tr(R,true,Td);
each_tr([{data,F,_}|R],true,Td) ->
    each_tr(R,true,[F|Td]);
each_tr([_|R],Save,Td) ->
    each_tr(R,Save,Td).
    
build_command(Action) ->
    if
        length(Action)<1 ->
            [];
        true ->
            %bug1: ie浏览器对style attribute的定位必须为大写
            %bug2: 从浏览器中读到的“为&quot;要替换成"\""
            Fun2 = fun(X) ->httputils:replaceAll(X,"&quot;","\"") end,
            {list_to_atom(hd(Action)), lists:map(Fun2, lists:nthtail(1,Action))}
    end.

getScalarValues(Prop,Params)->
	case Prop of
		file->
			get_script();
		browser->
			[{"IE 6","*iehta"},{"IE 7,8,9","*iexploreproxy"},{"Firefox(Version 3.x.x)","*firefox3"},{"Google Chrome","*googlechrome"},{"Safari","*safari"},{"Opera","*opera"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

get_script() ->
    File = filelib:wildcard("templates.webscripts/*"),
    Fun = fun(X) ->
        case X of 
            "templates.webscripts/"++T ->
                {T,X};
            _ ->
                {X,X}
        end
    end,
    lists:map(Fun,File).
    
%% @spec get_classifier(Flag) -> Threshold
%% where
%% Flag = (error|warning|good)
%% Threshold = [{Attribute,Opera,Value}]
%% Attribute = atom()
%% Opera = atom()
%% Value = (integer()|atom()|string())
%% @doc Set default threshold value.
get_classifier(error)->
	case THIS:get_property(error_classifier) of
		{ok,{error_classifier,Classifier}}->
			Classifier;
		_->
			[{state,'==',400}]
	end;
get_classifier(warning)->
	case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[{state,'==',400}]
	end;
get_classifier(good)->
	case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier; 
		_->
			[{state,'==',200}]
	end.

%% @spec get_template_property() ->list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++ 
    [
	    #property{name=file,title="Script File",type=scalar,order=1,description="Chose a script to monitor"},
		#property{name=browser,title="Browser",type=scalar,allowother=true,order=2,description="Select a browser to run the script,if you browser isn't in the list, set you browser install location in the textbox below"},
		#property{name=installDirectory,title="Install Directory",type=text,order=3,description="if selected browser isn't installed by default,set the location ie(root\\Mozilla Firefox\\firefox.exe)"},
        #property{name=server, title="Server",type=text,default="127.0.0.1", order=4,description="Selenium server host"},
        #property{name=port, title="Port",type=numeric,default=4444,order=5,description="Selenium server port"},
        #property{name=state,title="State",type=numeric,state=true,configurable=false,baselinable=true},
        #property{name=round_trip_time,title="round trip time(milliseconds)",type=numeric,state=true,configurable=false,baselinable=true}
	].