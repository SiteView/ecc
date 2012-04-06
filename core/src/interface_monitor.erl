%% ---
%% interface_monitor
%%
%%---
-module(interface_monitor,[BASE]).
-extends(browsable_snmp_base).
-compile(export_all).
-export([new/0, getBrowseData/1, update/0, getStateProperties/2, buildClassifer/2, getDevClassify/1, getIfDevTypeName/1, getClassifyByBrowse/1, getIfCounterName/2, oid2dots/1, inc_count_error/0, get_classifier/1, getScalarValues/2, verify/1, get_template_property/0]).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_ecc.hrl").

-define(COMMON, "1").
-define(THOUSAND, "2").

-define(SIGN_INOCTETS, "ifcIndex_InOctets").        %% 输入字节数
-define(SIGN_OUTOCTETS, "ifcIndex_OutOctets").        %% 输出字节数
-define(SIGN_INDISCARDS, "ifcIndex_InDiscards").        %% 输入丢包数
-define(SIGN_OUTDISCARDS, "ifcIndex_OutDiscards").        %% 输出丢包数

%% @spec new() -> Obj
%% Obj = term()
%% @doc create a new instance for network_bandwidth monitor
new()->
	Base = browsable_snmp_base:new(),
	{?MODULE,Base}.

%% @spec defaultTitle(Params) -> List
%% Params = [term()]
%% List = string()
%% @doc defaultTitle is the function called by schedule to set default title of monitor
defaultTitle(Params)->
	%%BASE:defaultTitle(Params) ++ "(" ++ case proplists:get_value(disk,Params) of undefined->"";V->V end ++ ")".
    Browse = 
        case THIS:get_property(browse) of
            {ok,{_,V2}}->
                V2;
            _->
                []
        end,
    Names = [NValue||{Key, NValue}<-Browse],
    MNames = string:join(Names, ","),	
    BASE:defaultTitle(Params) ++ "(" ++ MNames ++ ")".


%% @spec getBrowseData(Params)-> Obj
%% Params = [Tuple]
%% Obj = list()
%% @doc get counters of network_bandwidth_monitor's oids
getBrowseData(Params)->
	io:format("Params:~p~n", [Params]),
	NewParams=case lists:keymember("%snmp_version%", 2, Params) of
				 false ->
					 Params;
				  true ->
					 One=lists:keyreplace(snmpversion, 1, Params, {snmpversion,"v1"}),
					 lists:keyreplace(community, 1, One, {community,"public"})
			  end,
	io:format("NewParams:~p~n", [NewParams]),		  
    Session = THIS:obtainSession(NewParams),
    THIS:set_attribute(session, Session),
    InterfaceType = 
        case proplists:get_value(interfaceType,Params) of
			undefined->
				?COMMON;
			Num->
				Num
		end,
    case InterfaceType of
        ?COMMON ->
            Oid = dots2oid("1.3.6.1.2.1.2.2.1.1"),
%% 			io:format("oid:~p~n", [Oid]),
            IIdexVar = Session:get_table_col(Oid),
            DesList = getDes(InterfaceType, IIdexVar, Session),
			io:format("DesList: ~p~n", [DesList]),
            % [{erlang:integer_to_list(X),lists:sublist(Y, length(Y)-5) }||{X, Y}<-DesList]; 
			[{erlang:integer_to_list(X),Y}||{X, Y}<-DesList];
        _ ->
            Oid = dots2oid("1.3.6.1.2.1.2.2.1.1"),
            IIdexVar = Session:get_table_col(Oid),
            DesList = getDes(InterfaceType, IIdexVar, Session),
            [{erlang:integer_to_list(X),Y}||{X, Y}<-DesList]
    end.
convert1([H|R], Acc)->
	case H > 255 of
		true->
			<<H1:8, H2:8>> = <<H:16>>,
			List = iconv:convert("utf-32", "utf-8", [0, 0, H1, H2]),
			convert1(R, lists:reverse(List) ++ Acc);
		_ ->
			convert1(R, [H|Acc])
	end;
convert1([], Acc)->
lists:reverse(Acc).
%%  io:format("convert1:~p ~n", [L]),
%%  L.
%% %%   io:format("convert1:~p ~n", [L]),
%%  	iconv:convert("utf-8", "gbk", lists:reverse(Acc)).

convert1(Msg)->
%% 	io:format("oldstatus:~p ~n", [Msg]),
	convert1(Msg, []).

getDes(Type, [], Session) ->
    [];
getDes(Type, [V=#varbind{}|T], Session) ->
    Index = V#varbind.value,
    Des =
    case Type of
        "1" ->
            Var = Session:g([1, 3, 6, 1, 2, 1, 2, 2, 1, 2]++[Index]),
            case Var of
                {ok,{noError,_,[Vb|_]},_} ->
                    Vb#varbind.value;
                _ ->
                    []
            end;
        _ ->
            Var = Session:g([1, 3, 6, 1, 2, 1, 2, 2, 1, 2]++[Index]),
            case Var of
                {ok,{noError,_,[Vb|_]},_} ->
                    Vb#varbind.value;
                _ ->
                    []
            end
    end,
    NStatus =
    case Session:g([1, 3, 6, 1, 2, 1, 2, 2, 1, 8]++[Index]) of
        {ok,{noError,_,[VbS|_]},_} ->
            case lists:keysearch(VbS#varbind.value, 1, ?INTERFACE_STATUS) of
                {value, {_, Status}} ->
                    lists:append(["(",Status,")"]);
                _ ->
                    ""
            end;
        _ ->
            ""
    end,
    NDes = lists:append([Des, NStatus]),
    [{Index, NDes}] ++
    getDes(Type, [Index|T], Session);
getDes(Type, [Index|T], Session) ->
    getDes(Type, T, Session).


test_infStatus(Index) ->
	io:format("Index: ~p~n", [Index]),
    IntIndex = erlang:list_to_integer(Index),
    Session = THIS:obtainSession(),
    NStatus =
    case Session:g([1, 3, 6, 1, 2, 1, 2, 2, 1, 8]++[IntIndex]) of
        {ok,{noError,_,[VbS|_]},_} ->
            %%io:format("VbS#varbind.value: ~p~n", [VbS#varbind.value]),
            VbS#varbind.value;
        _ ->
            4
    end.

%% @spec update() -> Result
%% Result = term()
%% @doc update is the run function called by schedule to test the network bandwidth monitor info
update()->
    THIS:set_attribute(pLastRunTime, sv_datetime:now()),
	THIS:set_attribute(countersInError,0),
	
    Sess = THIS:obtainSession(),
    case Sess:test_snmp() of
        true ->		
            InterfaceType = 
                case THIS:get_property(interfaceType) of
                    undefined->
                        ?COMMON;
                    {ok, {_, DType}} ->
                        DType;
                    _ ->
                        ?COMMON
                end,
		io:format("interface  ~p~n",[InterfaceType]),
            case InterfaceType of
                ?COMMON ->
                    runComm();
                _ ->
                    runThousand()
            end,
            THIS:set_attribute(?NO_DATA,false);
			% THIS:set_attribute(?CATEGORY,error);
        _ ->
            THIS:set_attribute(?NO_DATA,true),
			THIS:set_attribute(?CATEGORY,error),
			THIS:set_attribute(status,"error"),
            inc_count_error(),
			THIS:set_attribute(?STATE_STRING,"Application Server not available on host")
    end.

%% 一般接口
runComm() ->
    Ifs = 
        case THIS:get_property(browse) of
            {ok,{_,V2}}->
                V2;
            _->
                []
        end,	
	% io:format("runComm Ifs ~p~n",[Ifs]),
    String = runByIndex(Ifs,?COMMON),
    % io:format("runComm String ~p~n",[String]),
    THIS:set_attribute(?STATE_STRING,String),
    ok.

%% 千兆接口
runThousand() ->
    Ifs = 
        case THIS:get_property(browse) of
            {ok,{_,V2}}->
                V2;
            _->
                []
        end,
    String = runByIndex(Ifs,?THOUSAND),
    io:format("runComm Ifs ~n~p",[Ifs]),
    io:format("runComm String ~n~p",[String]),
    THIS:set_attribute(?STATE_STRING,String),
    ok.


runByIndex([], Type) ->
    [];
runByIndex([{Index,Name}|T], Type) when erlang:is_list(Index) ->
    case test_infStatus(Index) of
        1 ->
            Re = 
            case Type of
                ?COMMON ->
                    %% 获取接收的字节数(一般接口)
                    %% 1.3.6.1.2.1.2.2.1.10
                    case statifspeed([1,3,6,1,2,1,2,2,1,10], Index, ?SIGN_INOCTETS) of
                        {ok, V} ->
							io:format("ok1 ~p~n",[V]),
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inOctets),V),
                            Name ++" InOctets = "++erlang:integer_to_list(V) ++ "(bytes)";
                        {error, E} ->							
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inOctets),0),
                            Name ++" InOctets Error: "++E;
                        O ->							
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inOctets),0),
							Name ++" InOctets Error: "++O ++ "(bytes)"
							
                    end ++ "<br>" ++
                    %% 获取发送的字节数(一般接口)
                    %% 1.3.6.1.2.1.2.2.1.16
                    case statifspeed([1,3,6,1,2,1,2,2,1,16], Index, ?SIGN_OUTOCTETS) of
                        {ok, V1} ->
							io:format("ok2 ~p~n",[V1]),
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outOctets),V1),
                            Name ++" OutOctets = "++erlang:integer_to_list(V1) ++ "(bytes)";
                        {error, E1} ->
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outOctets),0),
                            Name ++" OutOctets Error: "++E1;
                        O1 ->
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outOctets),0),
                            Name ++" OutOctets Error: "++O1 ++ "(bytes)"
							 
                    end;
                _ ->
                    %% 获取接收的字节数(千兆接口) 64位版本, 到达最大会清0
                    %% 1.3.6.1.2.1.31.1.1.1.6
                    case statifspeed([1,3,6,1,2,1,31,1,1,1,6], Index, ?SIGN_INOCTETS) of
                        {ok, V} ->
							io:format("ok3 ~p~n",[V]),
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inOctets),V),
                            Name ++" InOctets = "++erlang:integer_to_list(V) ++ "(bytes)";
							
                        {error, E} ->
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inOctets),0),
                            Name ++" InOctets Error: "++E;
                        O ->
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inOctets),0),
                            Name ++" InOctets Error: "++O ++ "(bytes)"
							
                    end ++ "<br>" ++
                    %% 获取发送的字节数(千兆接口) 64位版本
                    %% 1.3.6.1.2.1.31.1.1.1.10
                    case statifspeed([1,3,6,1,2,1,31,1,1,1,10], Index, ?SIGN_OUTOCTETS) of
                        {ok, V1} ->
							io:format("ok4 ~p~n",[V1]),
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outOctets),V1),
                            Name ++" OutOctets = "++erlang:integer_to_list(V1) ++ "(bytes)";
							
                        {error, E1} ->
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outOctets),0),
                            Name ++ " OutOctets Error: "++E1;
                        O1 ->
                            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outOctets),0),
                            Name ++" OutOctets Error: "++O1 ++ "(bytes)"
							
                    end
            end ++ "<br>" ++
            %% 获取端口状态
            %% 1.3.6.1.2.1.2.2.1.8
            case getValue([1,3,6,1,2,1,2,2,1,8], Index) of
                {ok,V2} ->
				io:format("ok5 ~p~n",[V2]),
                    Sta = 
                    case V2 of
                        1 ->
                            "up";
                        _ ->
                            "down"
                    end,
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},operStatus),V2),
                    Name ++" OperStatus = "++Sta;
					
                {error, E2} ->
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},operStatus),0),
                    Name ++ " OperStatus Error: "++E2;
                O2 ->
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},operStatus),0),
                    Name ++" OperStatus Error: "++O2
					
            end ++ "<br>" ++
            %% 输入丢包数量
            %% 1.3.6.1.2.1.2.2.1.13
            case statifspeed([1,3,6,1,2,1,2,2,1,13], Index, ?SIGN_INDISCARDS) of
                {ok, V3} ->
					io:format("ok6 ~p~n",[V3]),
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},inDiscards),V3),
                    Name ++" InDiscards = "++erlang:integer_to_list(V3);
					
                {error, E3} ->
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},inDiscards),0),
                    Name ++" InDiscards Error: "++E3;
                O3 ->
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},inDiscards),0),
                    Name ++" InDiscards Error: "++O3
            end ++ "<br>" ++
            %% 输出丢包数量
            %% 1.3.6.1.2.1.2.2.1.19
            case statifspeed([1,3,6,1,2,1,2,2,1,19], Index, ?SIGN_OUTDISCARDS) of
                {ok, V4} ->
					io:format("ok7 ~p~n",[V4]),
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},outDiscards),V4),
                    Name ++" OutDiscards = "++erlang:integer_to_list(V4);
                {error, E4} ->
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},outDiscards),0),
                    Name ++" OutDiscards Error: "++E4;
                O4 ->
                    THIS:set_attribute(THIS:getIfCounterName({Index,Name},outDiscards),0),
                    Name ++" OutDiscards Error: "++O4
            end;
        _ ->
            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inOctets),0),
            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outOctets),0),
            THIS:set_attribute(THIS:getIfCounterName({Index,Name},operStatus),0),
            THIS:set_attribute(THIS:getIfCounterName({Index,Name},inDiscards),0),
            THIS:set_attribute(THIS:getIfCounterName({Index,Name},outDiscards),0),
            inc_count_error(),
            lists:append([Name, " is not up"])
    end  ++ "<br>" ++
    runByIndex(T, Type);
runByIndex([H|T], Type) ->
    runByIndex(T, Type).

%% *****************************
%% ******** 取监测指标 *********
%% *****************************

%% 直接获取值
getValue(Oid, Index) ->
    Idx = erlang:list_to_integer(Index),
    Session = THIS:obtainSession(),
    CurValue =
    case Session:g(Oid++[Idx]) of
        {ok,{noError,_,[Vb|_]},_} ->
            {ok, Vb#varbind.value};
        _ ->
            {error, "get value error"}
    end.

%% 获取速率算法
statifspeed(Oid, Index, Sign) ->
    Idx = erlang:list_to_integer(Index),
    AtomIdx = erlang:list_to_atom(Index),
    IdIdx = erlang:list_to_atom(Sign++"_"++Index),
    IdIdxTime = erlang:list_to_atom(Sign++Index),
    Session = Sess = THIS:obtainSession(),
    CurValue =
    case Session:g(Oid++[Idx]) of
        {ok,{noError,_,[Vb|_]},_} ->
            Vb#varbind.value;
        _ ->
            -1
    end,
    if
        CurValue >= 0 ->
            Now = sv_datetime:now(),
            Result = 
            case THIS:get_attribute(IdIdx) of
				{ok,{_,V3}}->
					case erlang:is_integer(V3) of
                        true ->
                            if
                                CurValue >= V3 ->
                                    case THIS:get_attribute(IdIdxTime) of
                                        {ok,{_,V4}} ->
                                            case erlang:is_integer(V4) of
                                                true ->
                                                    {ok, round((CurValue - V3)/((Now - V4)/1000))};
                                                _ ->
                                                    {error, "get value error"}
                                            end;
                                        _ ->
                                            {error, "get value error"}
                                    end;
                                true ->
                                    {error, "get value error"}
                            end;
                        _ ->
                            {error, "get value error"}
                    end;
				_->
					{error, "get value error"}
            end,
            THIS:set_attribute(IdIdx, CurValue),
            THIS:set_attribute(IdIdxTime, Now),
            case Result of
                {ok, V} ->
                    {ok, V};
                _ ->
                    platform:sleep(1000),
                    statifspeed(Oid, Index, Sign)
            end;
        true ->
            THIS:set_attribute(IdIdx, undefine),
            THIS:set_attribute(IdIdxTime, undefine),
            {error, "get value error"}
    end.









    
    
%% *****************************

%% @spec getStateProperties(This,Params)-> Obj
%% This = object
%% Params = [Tuple]
%% Obj = list()
%% @doc do something when after update or edit
getStateProperties(This,Params)->
    io:format("Params: ~p~n", [Params]),
    Class = buildClassifer(This, Params),
    io:format("Class: ~p~n", [Class]),
    Class.

%% @spec buildClassifer(This, Params)-> Obj
%% This = object
%% Params = [Tuple]
%% Obj = list()
%% @doc build Classifer by runtime data
buildClassifer(This, Params) ->
    Properties = THIS:get_properties(),
    io:format("Properties: ~p~n", [Properties]),
    Ifs = case THIS:get_property(browse) of
				{ok,{_,V2}}->
                    io:format("V2: ~p~n", [V2]),
					V2;
				Ot->
                    io:format("Ot: ~p~n", [Ot]),
					case lists:keysearch(browse, 1, Params) of
                        {value, {browse, Browse}} ->
                            Browse;
                        _ ->
                            []
                    end
	    end,
    io:format("Ifs: ~p~n", [Ifs]),
    Temp = This:get_template_property(),
	T = [X || X<-Temp,X#property.state=:=true],
    Cls =
    if 
        Ifs =/= [] ->
            getClassifyByBrowse(Ifs);
        Ifs =:= [] ->
            case get('sv_classfier') of
                undefined ->
                    [];
                Values ->
                    Values
            end;
        true ->
            []
    end,
    %%put('sv_classfier', Cls),
    %%io:format("*************sv_sndev_classfier: ~p~n", [DevCls]),
    %%最终构造阀值
    Classifier = T ++ Cls,
    %%io:format("*************Classifier: ~p~n", [Classifier]),
    Classifier.


%%得到指定设备类型的参数的阀值
%% @spec getDevClassify(Ifs)-> Obj
%% Ifs = list()
%% Obj = list()
%% @doc build device type Classifer
getDevClassify([]) ->
    [];
getDevClassify([If|T]) ->
    [#property{name=THIS:getIfDevTypeName(If),title=atom_to_list(THIS:getIfDevTypeName(If)),type=text,state=true,configurable=false}
     ] ++
     getDevClassify(T).
     
%% @spec getIfDevTypeName(If)-> Obj
%% If = tuple()
%% Obj = atom()
%% @doc switch device type name to atom
getIfDevTypeName(If) ->
    list_to_atom(element(1,If)).

%%得到接口参数的阀值
%% @spec getClassifyByBrowse(Ifs)-> Obj
%% Ifs = list()
%% Obj = list()
%% @doc get classfier by browse counters
getClassifyByBrowse([]) ->
    [];
getClassifyByBrowse([If|T]) ->
    
    [#property{name=THIS:getIfCounterName(If,inOctets),title=atom_to_list(THIS:getIfCounterName(If,inOctets)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,outOctets),title=atom_to_list(THIS:getIfCounterName(If,outOctets)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,operStatus),title=atom_to_list(THIS:getIfCounterName(If,operStatus)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,inDiscards),title=atom_to_list(THIS:getIfCounterName(If,inDiscards)),type=text,state=true,configurable=false},
     #property{name=THIS:getIfCounterName(If,outDiscards),title=atom_to_list(THIS:getIfCounterName(If,outDiscards)),type=text,state=true,configurable=false}] ++
     getClassifyByBrowse(T).


getLogProperties(This)->
	Ifs = case THIS:get_property(browse) of
				{ok,{_,V2}}->
					V2;
				_->
					[]
	    end,
    Temp = This:get_template_property() ++ getClassifyByBrowse(Ifs),
	[X#property.name || X<-Temp,X#property.state=:=true].

%% @spec getIfCounterName(If,Measure)-> Obj
%% If = tuple()
%% Measure = atom()
%% @doc build browse counters string and switch to atom
getIfCounterName(If,Measure)->
	list_to_atom(element(2,If)++":"++ atom_to_list(Measure)). 

%% @spec oid2dots(Oids)-> Obj
%% Oids = list()
%% Obj = list()
%% @doc switch oid list to oid string
oid2dots([])->"";
oid2dots([H|T])->
	case T of
		[]->
			integer_to_list(H);
		_->
			integer_to_list(H) ++ "." ++ oid2dots(T)
	end.
    
dots2oid(OidStr) ->
    dots2oid_t(string:tokens(OidStr, ".")).
dots2oid_t([]) ->
    [];
dots2oid_t([H|T]) when erlang:is_list(H) ->
    [erlang:list_to_integer(H)] ++
    dots2oid_t(T);
dots2oid_t([H|T]) ->
    dots2oid_t(T).

%% @spec inc_count_error()-> Obj
%% Obj = term()
%% @doc count error
inc_count_error()->
	case THIS:get_attribute(countersInError) of
		{ok,{_,C}}->
			THIS:set_attribute(countersInError,C+1);
		_->
			THIS:set_attribute(countersInError,1)
	end.

parse_classifier([], Results) ->
    lists:reverse(Results);
parse_classifier([{Key, Oper, Value}|T], Results) ->
    NKey =
    case Key of
        VKey when erlang:is_list(Key) ->
            erlang:list_to_atom(VKey);
        _ ->
            Key
    end,
    parse_classifier(T, [{NKey, Oper, Value}|Results]);
parse_classifier([H|T], Results) ->
    parse_classifier(T, [H|Results]).


%% @spec get_classifier(error) -> List
%% List = [Tuple]
%% Tule = {status, Logic, Value}
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error
get_classifier(error)->
	
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					parse_classifier(Classifier, []);
				_->
					[{countersInError,'>',0}]
			end,
	if 
		length(Cls) < 10 ->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			parse_classifier(Classifier, []);
		_->
			[{countersInError,'>',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end;
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			parse_classifier(Classifier, []);
		_->
			[{countersInError,'==',0}]
	end,
	if 
		length(Cls)<10->
			Cls ++ lists:map(fun(_)->{'N/A','',''} end,lists:seq(1,THIS:getMaxCounter() - length(Cls)));
		true ->
			Cls
	end.

%% @spec getScalarValues(Prop,Params)-> Result
%% Result = list()
%% @doc get version list, oid list, percentageBase list, scale list for ui's combobox
getScalarValues(Prop,Params)->
	case Prop of
		interfaceType->
			[{"common","1"},{"thousands million","2"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

%% @spec verify(Params)-> Result
%% Params = [Tuple]
%% Result = list()
%% @doc verify data of ui, ex. host,percentageBase,oid,oidIndex,timeout,scale and son
verify(Params)->
	Errs=
	case proplists:get_value(maxRTDataWindow,Params) of
		""->
			[{maxRTDataWindow,"Real-Time Data Time Window missing"}];
		MaxRTDataWindow when not is_number(MaxRTDataWindow) orelse MaxRTDataWindow<1 orelse MaxRTDataWindow > 24 ->
			[{maxRTDataWindow,"The Real-Time Data Time Window must be a number between 1 and 24"}];
		_->
			[]
	end ++
	case proplists:get_value(maxRTDataVerticalAxis,Params) of
		""->
			[];
		MaxRTDataVerticalAxis when not is_number(MaxRTDataVerticalAxis) orelse MaxRTDataVerticalAxis < 0 ->
			[{maxRTDataVerticalAxis,"The Real-Time Data Vertical Axis must be a number greater than 0"}];
		_->
			[]
	end ++
	case BASE:verify(Params) of
		{error,Be}->
			Be;
		_->
			[]
	end,
	if
		length(Errs)>0->
			{error,Errs};
		true->
			{ok,""}
	end.

%% @spec get_template_property()-> Result
%% Result = [Record]
%% Record = property
%% @doc get monitor template of network_bandwidth_monitor monitor
get_template_property()->
	Tp = BASE:get_template_property(),
	[X||X<-Tp,X#property.name=/=mibfile]++
	[
    #property{name=interfaceType,title="Interface Type", description="",type=scalar,editable=true,advance=true,order=1},
    #property{name=maxRTDataWindow,title="Real-Time Data Time Window", description="the number of hours for which real-time graph data should be stored",type=numeric,editable=true,advance=true,default=24,order=2},
	#property{name=maxRTDataVerticalAxis,title="Real-Time Data Vertical Axis", description="the maximum value on the vertical axis for real-time graphs (leave blank to have this automatically calculated)",type=numeric,editable=true,advance=true,order=3}
	].
