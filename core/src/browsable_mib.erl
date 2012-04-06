%% ---
%% browsable_mib
%%
%%---
-module(browsable_mib).
-export([getCompiledMIBs/0,get_mid_oids/2,containsMIB/1,check_oids/3]).
-compile(export_all).

-define(MIB_PATH,"templates.mib").
-define(ETSNAME,snmp_mib_files_node).
-include_lib("snmp/include/snmp_types.hrl").
%% mib record
-record(mmib, {mibname, nodes, source, redmib}).
%% mibnode record
-record(mibnode, {children, mib, name, oid, parent, value, type}).

getCompiledMIBs()->
	Files = filelib:wildcard(?MIB_PATH ++ "/*.mib"),
	case compile_mibs(Files) of
		[]->
			["No MIBs Available"];
		R->
			% ["All-MIBs"] ++ R
			R
	end.

compile_mibs([])->[];
compile_mibs([F|T])->
	case filelib:is_file(filename:rootname(F) ++ ".bin") of
		true->
			[filename:basename(F)] ++ compile_mibs(T);
		_->
			case snmpc:compile(F,[{i,[?MIB_PATH]},{il,["snmp/priv/mibs/","snmp/mibs", ?MIB_PATH]},{outdir,?MIB_PATH},{group_check, false}, {deprecated, false},module_identity]) of
				{ok,_}->
					[filename:basename(F)] ++ compile_mibs(T);
				_->
					compile_mibs(T)
			end
	end.
	
get_mid_oids(Mib,Info)->
	Server = proplists:get_value(server,Info),
	Ver = proplists:get_value(snmpversion,Info),
	Community = proplists:get_value(community,Info),
	AuthType = proplists:get_value(snmpv3authtype,Info),
	User = proplists:get_value(snmpv3username,Info),
	Passwd = proplists:get_value(snmpv3authpassword,Info),
	PrivPasswd = proplists:get_value(snmpv3privpassword,Info),
	EngineID = proplists:get_value(contextEngineID,Info),
	ContextName = proplists:get_value(contextName,Info),
	Timeout = case proplists:get_value(timeout,Info) of
					undefined->
						5;
					"0"->
						5;
					Time->
						case string:to_integer(Time) of
							{error,_}->
								5;
							{Val,_}->
								Val
						end
				end,
    io:format("Ver = ~p~n", [Ver]),
	io:format("getproperty~n"),
	%S = list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")]),
	S = case ip_utils:check_ip(Server) of
					{error,_}->
						if
							length(Server) =<0->
								"";
							true ->
								case inet:gethostbyname(Server) of
									{error,_}->
										"";
									{ok,{_,_,_,_,_,[Ip|_]}}->
										Ip;
									_->
										""
								end
						end;
					_->
						list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")])
				end,
	if
		S == ""->
			{error,"can't connect to snmp agent!"};
		true ->
            io:format("$$ S: ~p~n", [S]),
            io:format("$$ Port: ~p~n", [161]),
            io:format("$$ Ver: ~p~n", [Ver]),
            io:format("$$ Community: ~p~n", [Community]),
            io:format("$$ AuthType: ~p~n", [AuthType]),
            io:format("$$ User: ~p~n", [User]),
            io:format("$$ Passwd: ~p~n", [Passwd]),
            io:format("$$ PrivPasswd: ~p~n", [PrivPasswd]),
            io:format("$$ EngineID: ~p~n", [EngineID]),
            io:format("$$ ContextName: ~p~n", [ContextName]),
            io:format("$$ Timeout: ~p~n", [Timeout*1000]),
			Session = snmp_session:new(S,161,Ver,Community,AuthType,
										User,Passwd,PrivPasswd,
										EngineID,ContextName,Timeout*1000),
            %% Initialization, the main re-load this Counters per call, if there would ets do nothing, if there is no school out of all the mib files, construct mibs, and mibnode_lookup_tree, this method calls for operations really do Counters
            %%browsablemib(),    
            case Session:test_snmp() of
                true ->
                    MibName = filename:rootname(Mib,".mib"),
                    Counters = getMibTree(MibName, true, Info),
                    io:format("ok~n"),
                    Counters;
                _ ->
                    []
            end
             
            
            
            %% Take all the Oid mib
            %%List = Session:get_table_col([1, 3]),
            %%io:format("****Read All Oid Ok~n"),
            %%case Mib of
			%%	"All-MIBs"->
            %%        check_multi_mid_oid(Mib,getCompiledMIBs(),List);
			%%	_->
			%%		check_mib_oids(Mib, List)
			%%end,
            %%case get(snmp_browsable_counters) of
            %%    undefined ->
            %%        io:format("****undefined"),
            %%        [];
            %%    [] ->
            %%        io:format("****[]"),
            %%        [];
            %%    Counters ->
            %%        io:format("****Counters Length = ~p~n", [string:len(Counters)]),
             
            %%        put(snmp_browsable_counters, []),
             %%       Counters
            %%end
            
			%%case Mib of
			%%	"All-MIBs"->
			%%		get_mid_oid(Mib,getCompiledMIBs(),Session);
			%%	_->
			%%		get_oids(Mib,Session)
			%%end
	end.


%%**************************************************************************************************
%%************The new method of taking Mib Counters*************************************************************
%%**************************************************************************************************

%% ^^^^^^^New method added^^^^^^^

getCompiledMIB(MibName)->
	Files = filelib:wildcard(?MIB_PATH ++ "/"++MibName++".mib"),
    %%io:format("Files: ~p~n", [Files]),
	case compile_mibs(Files) of
		[]->
			["No MIBs Available"];
		R->
			R
	end.
    
%% Read a single mib file
read_simple_mib("All-MIBs")->[];
read_simple_mib("No MIBs Available")->[];
read_simple_mib(Mib)->
	Bin = ?MIB_PATH ++ "/" ++ filename:rootname(Mib,".mib") ++ ".bin",
	case snmp_misc:read_mib(Bin) of
		{ok,M}->
            M;
		_->
			[]
	end.

%% Mib file to read from the database root
getRootMe(Mes) ->
    getRootMe_t(Mes).
getRootMe_t([]) ->
    [];
getRootMe_t([Me=#me{}|T]) ->
    case Me#me.entrytype of
        internal ->
            Me;
        _ ->
            getRootMe_t(T)
    end;
getRootMe_t([H|T]) ->
    getRootMe_t(T).
    
getMibTree(MibName, IsAllChild, Info) ->
    Server = proplists:get_value(server,Info),
	Ver = proplists:get_value(snmpversion,Info),
	Community = proplists:get_value(community,Info),
	AuthType = proplists:get_value(snmpv3authtype,Info),
	User = proplists:get_value(snmpv3username,Info),
	Passwd = proplists:get_value(snmpv3authpassword,Info),
	PrivPasswd = proplists:get_value(snmpv3privpassword,Info),
	EngineID = proplists:get_value(contextEngineID,Info),
	ContextName = proplists:get_value(contextName,Info),
	Timeout = case proplists:get_value(timeout,Info) of
					undefined->
						5;
					"0"->
						5;
					Time->
						case string:to_integer(Time) of
							{error,_}->
								5;
							{Val,_}->
								Val
						end
				end,
	S = case ip_utils:check_ip(Server) of
					{error,_}->
						if
							length(Server) =<0->
								"";
							true ->
								case inet:gethostbyname(Server) of
									{error,_}->
										"";
									{ok,{_,_,_,_,_,[Ip|_]}}->
										Ip;
									_->
										""
								end
						end;
					_->
						list_to_tuple([list_to_integer(X) || X <- string:tokens(Server,".")])
				end,
    Session = snmp_session:new(S,161,Ver,Community,AuthType,
										User,Passwd,PrivPasswd,
										EngineID,ContextName,Timeout*1000),
    browsablemib(),         
    %% Began to get the data, and tree construction
    Flag = MibName =:= "All-MIBs",
    io:format("begin get table....~n"),
    %%
    FirstMe =
    case read_simple_mib(MibName) of
        Mib=#mib{} ->
            Mes = Mib#mib.mes,
            %%lists:nth(1, Mes);
            getRootMe(Mes);
        _ ->
            []
    end,
    POid =
    case FirstMe of
        F=#me{} ->
            F#me.oid;
        _ ->
            [1]
    end,
    io:format("RootOid: ~p~n", [POid]),
    %
    List = getVariblesByMib(POid, Session, IsAllChild),
    %%io:format("List: ~p~n", [List]),
        %%case IsAllChild of
        %%    true ->
        %%        io:format("POid: ~p~n", [POid]),
        %%        Session:get_table_col_item([1],POid);
                %%Session:get_table_col_item_bulk(POid, 0, 120);
        %%    _ ->
        %%        getVariblesByMib(POid, Session, )
        %%end,
    %%io:format("List: ~p~n", [List]),
    io:format("end get tabel...~n"),
    io:format("Counters Length = ~p~n", [string:len(List)]),
    io:format("Build Tree...~n"),
    
    %%cyc_variablebinding(List, MibName, Flag),
    cyc_varriable(List, MibName, Flag, POid, FirstMe),
    Counters = get_all_dispar_counters(),
    NCounters = all_parent_counters() ++ Counters,
    NewCounters = lists:keysort(1, NCounters),
    io:format("End Build Tree...~n"),
    NewCounters.
    
cyc_varriable(List, MibName, Flag, RootOid, FirstMe) ->
    %%FirstMe =
    %%case read_simple_mib(MibName) of
    %%    Mib=#mib{} ->
    %%        Mes = Mib#mib.mes,
    %%        lists:nth(1, Mes);
    %%    _ ->
    %%        []
    %%end,
    cyc_variablechild(List, MibName, Flag, FirstMe, RootOid).
    
cyc_variablechild([], MibName, Flag, FirstMe, RootOid) ->
    [];
cyc_variablechild([Vb=#varbind{}|T], MibName, Flag, FirstMe, RootOid) ->
    Oid = Vb#varbind.oid,
    MibNode = get_from_treemap(Oid),
    NewMibNode =
    if 
        Vb#varbind.value =:= noSuchObject ->
            is_continue;
        Vb#varbind.value =:= noSuchInstance ->
            is_continue;
        Vb#varbind.value =:= endOfMibView ->
            is_continue;
        Vb#varbind.value =:= 'NULL' ->
            is_continue;
        Vb#varbind.value =:= noObject ->
            is_continue;
        true ->
            %%snmp_trap_data:log(oid2dots(Oid)),
            if
                MibNode =:= [] ->
                    Close_Ancestor = getClosestAncestor(Oid),
                    if 
                        Close_Ancestor =:= [] ->
                            is_continue;
                        true ->
                            Close_Ancestor
                            %%{S2, Close_Ancestor}
                    end;
                true ->
                    %%S2 = get_fullname(MibNode, M#mib.mes),
                    %%{S2, MibNode}
                    MibNode
            end
    end,
    Vbs =
    case NewMibNode of
        is_continue ->
            is_continue;
        %%{S22, NMibNode} ->
        _ ->
            Is_Continue = (Flag =/= true) andalso (NewMibNode#mibnode.mib =/= MibName),
            %%Is_Continue = 
            %%case Flag of
            %%    true ->
            %%        false;
            %%    _ ->
            %%        case FirstMe of
            %%            MyMe=#me{} ->
            %%                Tem = (lists:prefix(NewMibNode#mibnode.oid, MyMe#me.oid)) or (NewMibNode#mibnode.mib =:= MibName),
            %%                case Tem of
            %%                    true ->
            %%                        false;
            %%                    _ ->
            %%                        true
            %%                end;
            %%            _ ->
            %%                Tem = (NewMibNode#mibnode.mib =:= MibName),
            %%                case Tem of
            %%                    true ->
            %%                        false;
            %%                    _ ->
            %%                        true
            %%                end
            %%        end
            %%end,
            if
                Is_Continue =:= true ->
                    %%{S22, NVbs, NMibNode};
                    is_continue;
                true ->
                    {Vb, NewMibNode}
            end
    end,
    case Vbs of
        is_continue ->
            cyc_variablechild(T, MibName, Flag, FirstMe, RootOid);
        %%{S2s, NVb, NMMibNode} ->
        {NVb, NMMibNode} ->
            NMes =
            case get_mesdict() of
                {ok, Mes} ->
                    Mes;
                {error, Reason} ->
                    [];
                Other ->
                    []
            end,
            %%IsStackOper = is_stack_empty() oralso 
            
            outputCounters(NMMibNode, NVb, NMes)  ++
            cyc_variablechild(T, MibName, Flag, FirstMe, RootOid);
        _ ->
            io:format("continue ____~n"),
            cyc_variablechild(T, MibName, Flag, FirstMe, RootOid)
    end;
cyc_variablechild([Vb|T], MibName, Flag, FirstMe, RootOid) ->
    cyc_variablechild(T, MibName, Flag, FirstMe, RootOid).
    

getVariblesByMib_t(POid, Session) ->
    NMes =
            case get_mesdict() of
                {ok, Mes} ->
                    Mes;
                {error, Reason} ->
                    [];
                Other ->
                    []
            end,
    Vbs =
    case dict:find(POid, NMes) of
        {ok, Tuple=#me{}} ->
            case Tuple#me.entrytype of
                internal ->     
                    List = dict:to_list(NMes),
                    getVariblesByMe(List, POid);
                variable ->     
                    [];
                table ->       
                    List = dict:to_list(NMes),
                    getVariblesByMe(List, POid);
                table_entry ->  
                    List = dict:to_list(NMes),
                    getVariblesByMe(List, POid);
                table_column ->  
                    %%Session:get_table_col_item_bulk(POid, 0, 120)
                    Session:get_table_col_item(POid,POid)
            end;
        _ ->
            []
    end.

getVariblesByMib(POid, Session, IsAllChild) ->
    Vbs = getVariblesByMib_t(POid, Session),
    %%io:format("Vbs: ~p~n", [Vbs]),
    case IsAllChild of
        true ->
            for_getVariblesByMib(Vbs, Session, IsAllChild);
        _ ->
            Vbs
    end.

for_getVariblesByMib([], Session, IsAllChild) ->
    [];
for_getVariblesByMib([Vb=#varbind{}|T], Session, IsAllChild) ->
    case Vb#varbind.org_index of
        variable ->
            %%io:format("Vb#varbind.org_index: ~p~n", [Vb#varbind.org_index]),
            %%io:format("Vb#varbind.org_index: ~p~n", [Vb#varbind.oid]),
            [Vb];
        Value when erlang:is_integer(Value) ->
            [Vb];
        _ ->
            []
    end ++
    getVariblesByMib(Vb#varbind.oid, Session, IsAllChild) ++
    for_getVariblesByMib(T, Session, IsAllChild);
for_getVariblesByMib([H|T], Session, IsAllChild) ->
    for_getVariblesByMib(T, Session, IsAllChild).

getVariblesByMe([], POid) ->
    [];
getVariblesByMe([{_,Me=#me{}}|T], POid) ->
    Last = lists:last(Me#me.oid),
    Oid = POid++[Last],
    if
        Oid =:= Me#me.oid ->       
            %%io:format("Oid: ~p~n", [Oid]),
            case Me#me.entrytype of
                internal ->     
                    [#varbind{oid=Me#me.oid, variabletype='Integer32', org_index=internal}];
                variable ->     
                    [#varbind{oid=Me#me.oid++[0], variabletype='Integer32', org_index=variable}];
                table ->        
                    [#varbind{oid=Me#me.oid, variabletype='Integer32', org_index=table}];
                table_entry ->  
                    [#varbind{oid=Me#me.oid, variabletype='Integer32', org_index=table_entry}];
                table_column ->  
                    [#varbind{oid=Me#me.oid, variabletype='Integer32', org_index=table_column}]
            end ++
            getVariblesByMe(T, POid);
        true ->
            getVariblesByMe(T, POid)
    end;
getVariblesByMe([H|T], POid) ->
    getVariblesByMe(T, POid).
    
%% ^^^^^^^^^^^^^^^^^^^^^^^


%% take the Counters
get_mib_oid(Session, MibName) ->
    Flag = MibName =:= "All-MIBs",
    %%io:format("G = ~p~n", [snmp_ex2_manager:sync_get(Server,Port,[Oid++[0]])]),
    %%List = Session:get_table_col([1,3]),
    io:format("begin get table....~n"),
    List = Session:get_table_col_item_bulk([1,0], 0, 120),
    io:format("end get tabel...~n"),
    %%io:format("Counters Length = ~p~n", [string:len(List)]),
    io:format("Build Tree...~n"),
    cyc_variablebinding(List, MibName, Flag, []),
    %%io:format("****parent counters = ~p~n", [all_parent_counters()]),
    Counters = get_all_dispar_counters(),
    NCounters = all_parent_counters() ++ Counters,
    NewCounters = lists:keysort(1, NCounters),
    io:format("End Build Tree...~n"),
    NewCounters.
    %%make_tree(Counters).

    

%% Oid in the implementation of the statistics of all this is Mib Oid of the MIB corresponding to [{OidStr, Name}] list
cyc_variablebinding([], MibName, Flag, FirstMe) ->
    [];
cyc_variablebinding([Vb=#varbind{}|T], MibName, Flag, FirstMe) ->
    Oid = Vb#varbind.oid,
    MibNode = get_from_treemap(Oid),
    NewMibNode =
    if 
        Vb#varbind.value =:= noSuchObject ->
            is_continue;
        Vb#varbind.value =:= noSuchInstance ->
            is_continue;
        Vb#varbind.value =:= endOfMibView ->
            is_continue;
        Vb#varbind.value =:= 'NULL' ->
            is_continue;
        Vb#varbind.value =:= noObject ->
            is_continue;
        true ->
            %%snmp_trap_data:log(oid2dots(Oid)),
            if
                MibNode =:= [] ->
                    Close_Ancestor = getClosestAncestor(Oid),
                    if 
                        Close_Ancestor =:= [] ->
                            is_continue;
                        true ->
                            Close_Ancestor
                            %%{S2, Close_Ancestor}
                    end;
                true ->
                    %%S2 = get_fullname(MibNode, M#mib.mes),
                    %%{S2, MibNode}
                    MibNode
            end
    end,
    Vbs =
    case NewMibNode of
        is_continue ->
            is_continue;
        %%{S22, NMibNode} ->
        _ ->
            %%Is_Continue = (Flag =/= true) andalso (NewMibNode#mibnode.mib =/= MibName),
            Is_Continue = 
            case Flag of
                true ->
                    false;
                _ ->
                    case FirstMe of
                        MyMe=#me{} ->
                            Tem = (lists:prefix(NewMibNode#mibnode.oid, MyMe#me.oid)) or (NewMibNode#mibnode.mib =:= MibName),
                            case Tem of
                                true ->
                                    false;
                                _ ->
                                    true
                            end;
                        _ ->
                            Tem = (NewMibNode#mibnode.mib =:= MibName),
                            case Tem of
                                true ->
                                    false;
                                _ ->
                                    true
                            end
                    end
            end,
            if
                Is_Continue =:= true ->
                    %%{S22, NVbs, NMibNode};
                    is_continue;
                true ->
                    {Vb, NewMibNode}
            end
    end,
    case Vbs of
        is_continue ->
            cyc_variablebinding(T, MibName, Flag, FirstMe);
        %%{S2s, NVb, NMMibNode} ->
        {NVb, NMMibNode} ->
            NMes =
            case get_mesdict() of
                {ok, Mes} ->
                    Mes;
                {error, Reason} ->
                    [];
                Other ->
                    []
            end,
            %%IsStackOper = is_stack_empty() oralso 
            
            outputCounters(NMMibNode, NVb, NMes)  ++
            cyc_variablebinding(T, MibName, Flag, FirstMe);
        _ ->
            io:format("continue ____~n"),
            cyc_variablebinding(T, MibName, Flag, FirstMe)
    end;
cyc_variablebinding([Vb|T], MibName, Flag, FirstMe) ->
    cyc_variablebinding(T, MibName, Flag, FirstMe).
    
outputCounters(MibNode, Vb, All) ->
    Oid = Vb#varbind.oid,
    CurrFullName = MibNode#mibnode.name,
    COid = Oid,
    POid = MibNode#mibnode.oid,
    I = string:len(POid),
    J = string:len(COid),
    DisparOid = 
    if 
        J - I >= 1 ->
            lists:sublist(COid, string:len(POid) + 1, string:len(COid));
        true ->
            []
    end,
    {KOid, Name} =
    if
        DisparOid =:= [0] ->
            {parent_oid(Oid),[]};
        DisparOid =:= [] ->
            {Oid,CurrFullName};
        true ->
            {Oid,CurrFullName ++ "." ++ oid2dots(DisparOid)}
    end,
    OidStr = oid2dots(KOid),
    NodeName = mibnode_get_fullname(POid) ++ "/" ++ CurrFullName,
    %% Counters of all father nodes
    %%io:format("****POid = ~p~nNodeName = ~p~n", [POid, NodeName]),
    get_parent_and_self(POid, NodeName, MibNode),
    FullName = NodeName ++ "/" ++ Name,
    %% Counters from the server to take to remove the front "/"
    IsBegin = string:substr(FullName, 1, 1) =:= "/",
    FuName =
    if
        IsBegin =:= true ->
            string:substr(FullName, 2, string:len(FullName));
        true ->
            FullName
    end,
    %% Counters from the server to take to remove the back "/"
    IsEnd = string:substr(FuName, string:len(FuName), 1) =:= "/",
    FulName =
    if
        IsEnd =:= true ->
            string:substr(FuName, 1, string:len(FuName) - 1);
        true ->
            FuName
    end,
    build_disparCounters(OidStr, DisparOid, FulName, POid, Name, NodeName),
    [].
    %%[{OidStr, FulName}].


%% *************Construction DisparCounters********************

build_disparCounters(OidStr, DisparOid, FulName, POid, Name, NodeName) ->
    Tab = 
    case get(dispar_counters) of
        undefined ->
            make_a_set_temp_counters();
        [] ->
            make_a_set_temp_counters();
        T ->
            T
    end,
    put(dispar_counters, Tab),
    get_DisparParent(OidStr, DisparOid, FulName, POid, Name, NodeName, Tab).

make_a_set_temp_counters() ->
    ets:new(snmp_temp_build_tree, [public]). 
    
    
get_tempbuild_attribute(Item, Tab) ->
    case ets:lookup(Tab, Item) of
        undefined ->
            {error, undefine_item};
        [{Item, Value}] ->
            {ok, Value};
        Other ->
            {error, Other}
    end.
    
set_tempbuild_attribute(Item, Value, Tab) ->
    ets:insert(Tab,{Item, Value}).

set_dispar_counters(OidStr, TTDisPar, Tab) ->
    %%io:format("OidStr = ~p~nName = ~p~n", [OidStr, TTDisPar]),
    IsBegin = string:substr(TTDisPar, 1, 1) =:= "/",
    FuName =
    if
        IsBegin =:= true ->
            string:substr(TTDisPar, 2, string:len(TTDisPar));
        true ->
            TTDisPar
    end,
    set_tempbuild_attribute(OidStr, FuName, Tab).
    
    
get_DisparParent(OidStr, DisparOid, FulName, POid, Name, NodeName, Tab) ->
    
    if
        DisparOid =:= [0] ->
            set_dispar_counters(OidStr, FulName, Tab);
        DisparOid =:= [] ->
            set_dispar_counters(OidStr, FulName, Tab);
        true ->
            DisparOidStr = oid2dots(DisparOid),
            case string:rstr(FulName, DisparOidStr) of
                0 ->
                    [];
                _ ->
                    exe_DisparParent(DisparOid, NodeName, POid, Name, Tab)
            end
    end.
    

exe_DisparParent([], TmpDisPar, TmpPOid, Name, Tab) ->
    [];
exe_DisparParent([H|T], TmpDisPar, TmpPOid, Name, Tab) ->
    case string:len([H|T]) of
        1 ->
            TTPOid = TmpPOid ++ [H],
            TTDisPar = TmpDisPar ++ "/" ++ Name,
            OidStr = oid2dots(TTPOid),
            set_dispar_counters(OidStr, TTDisPar, Tab),
            exe_DisparParent(T, TTDisPar, TTPOid, Name, Tab);
        _ ->
            TTPOid = TmpPOid ++ [H],
            TTDisPar = TmpDisPar ++ "/" ++ integer_to_list(H),
            OidStr = oid2dots(TTPOid),
            set_dispar_counters(OidStr, TTDisPar, Tab),
            exe_DisparParent(T, TTDisPar, TTPOid, Name, Tab)
    end.

get_all_dispar_counters() ->
    Counters =
    try all_dispar_counters() of
        Cts ->
            case get(dispar_counters) of
                undefined ->
                    [];
                [] ->
                    [];
                Tab ->
                    ets:delete(Tab)
            end,
            erlang:put(dispar_counters, []),
            Cts
    catch
        _:_ ->
            []
    after
        %%ets:delete()
        ok
    end,
    Counters.

%% Get all Disparent of Counters
all_dispar_counters() ->
    NCounters =
    case get(dispar_counters) of
        undefined ->
            [];
        [] ->
            [];
        Tab ->
            ets:tab2list(Tab)
    end,
    NCounters.
%% **********************************************************

%% If you get the value of the variable binding list ends with 0, then remove the 0 , orthwrise do not remove 
getOctetString(Str)->
	case lists:last(Str) of
		0->
			string:substr(Str,1,length(Str)-1);
		_->
			Str
	end.
 

make_tree(List) ->
    make_tree_t(List,length(List),[]).
make_tree_t(_L,0,E) -> E;
make_tree_t(Li,Num,En) ->
    [A|B] = Li,
    Index = string:rstr(A,"/"),
    Substr= string:substr(A,1,Index-1),    
    {List1,Leve} = get_sublist(Substr,Li),
    L = make_tree_util(A,En),    
    make_tree_t(Leve,length(Leve),lists:append(En,lists:append(L,List1))).
        
make_tree_util(String,List) ->
    Index = string:str(String,"/"), 
    make_tree_util_t(String,List,Index,[]).
make_tree_util_t(_Str,_Li,0,E) -> E;     
make_tree_util_t(Str,Li,Num,En) ->
    Bool = lists:keymember(Str,1,Li),
    Index = string:rstr(Str,"/"),
    if Index == 0 ->
        if Bool ->
            make_tree_util_t(Str,Li,0,En);
        true ->
            make_tree_util_t(Str,Li,0,[{Str,Str}|En])
        end; 
    true ->
        if Bool ->                 
            make_tree_util_t(string:substr(Str,1,Index-1),Li,string:str(Str,"/"),En);
        true ->
            make_tree_util_t(string:substr(Str,1,Index-1),Li,string:str(Str,"/"),[{Str,Str}|En]) 
        end
    end.
    

get_sublist(Substr,List) ->
    [A|B] = List, 
    get_sublist_t(Substr,B,length(B),[],[]).
get_sublist_t(_Subs,_L,0,E,Lev) -> {E,Lev};
get_sublist_t(SubS,Li,Num,En,Le) ->
    [A|B]  = Li,
    Index = string:rstr(A,"/"),
    Substr= string:substr(A,1,Index-1),
    if SubS ==  Substr -> 
        get_sublist_t(SubS,B,Num-1,lists:append(En,[{A,A}]),Le);
    true ->
        get_sublist_t(SubS,B,Num-1,En,lists:append(Le,[A]))
    end.     

%% Remove the top stack element, but not removed from the stack
stack_peek() ->
    case get(temp_internal_stack) of
        undefined ->
            [];
        [] ->
            [];
        Stack ->
            lists:last(Stack)
    end.
    
%% Remove the top stack element,and  removed from the stack
stack_pop() ->
    case get(temp_internal_stack) of
        undefined ->
            [];
        [] ->
            [];
        Stack ->
            erlang:put(temp_internal_stack,lists:sublist(Stack, 1, string:len(Stack) - 1)),
            lists:last(Stack)
    end.
    
%% An element of the stack
stack_push(StackElement) ->
    case get(temp_internal_stack) of
        undefined ->
            erlang:put(temp_internal_stack, StackElement);
        [] ->
            erlang:put(temp_internal_stack, StackElement);
        Stack ->
            erlang:put(temp_internal_stack, Stack ++ StackElement)
    end.
    
%% Sentence of stack null
is_stack_empty() ->
    case get(temp_internal_stack) of
        undefined ->
            true;
        [] ->
            true;
        Stack ->
            false
    end.

%% stackclear
stack_clear() ->
    erlang:put(temp_internal_stack, []).

%% Obtained from the process stack object dictionary
get_stack() ->
    case get(temp_internal_stack) of
        undefined ->
            [];
        [] ->
            [];
        Stack ->
            Stack
    end.
%%**********
    
%% Bind variables combined to counter 
outputCounter(MibNode, Vb, All) ->
    Oid = Vb#varbind.oid,
    Me = element(2, MibNode),
    CurrFullName = find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname),
    COid = Oid,
    POid = Me#me.oid,
    I = string:len(POid),
    J = string:len(COid),
    DisparOid = 
    if 
        J - I >= 1 ->
            lists:sublist(COid, string:len(POid) + 1, string:len(COid));
        true ->
            []
    end,
    OidStr = oid2dots(Oid),
    Name = CurrFullName ++ "/" ++ oid2dots(DisparOid),
    TreeMaps =
    case get(treeMapTemp) of
        undefined ->
            [];
        [] ->
            [];
        TreeMap ->
            TreeMap
    end,
    case get(snmp_browsable_counters) of
        undefined ->
            builder_counters(TreeMaps, [], {OidStr, Name}, All);
        [] ->
            builder_counters(TreeMaps, [], {OidStr, Name}, All);
        Counters ->
            builder_counters(TreeMaps, Counters, {OidStr, Name}, All)
    end.
    
%% Get node from MIBNodeLookupTree
get_from_treemap(Oid) ->
    case get_attribute(Oid) of
        {ok, MibNode} ->
            MibNode;
        {error, Reason} ->
            [];
        Other ->
            []
    end.

%% Find the full name of the father by MibNode
mibnode_get_fullname([]) ->
    [];
mibnode_get_fullname(MOid) ->
    Oid = parent_oid(MOid),
    NMibNode =
    case get_attribute(Oid) of
        {ok, MibNode} ->
            MibNode;
        {error, Reason} ->
            [];
        Other ->
            []
    end,
    IsCyc = (NMibNode =:= []) andalso (string:len(Oid) > 0),
    if
        IsCyc =:= true ->
            mibnode_get_fullname(Oid);
        true ->
            NMibNode,
            Length = string:len(Oid),
            if
                (Length =< 0 andalso NMibNode =:= []) ->
                    [];
                (Length > 0 andalso NMibNode =/= []) ->
                    mibnode_get_fullname(Oid) ++ "/" ++
                    NMibNode#mibnode.name;
                (Length =< 0 andalso NMibNode =/= []) ->
                    NMibNode#mibnode.name;
                true ->
                    mibnode_get_fullname(Oid) ++ "/" ++
                    NMibNode#mibnode.name
            end
    end.

%% Counters of all parent
all_parent_counters() ->
    NCounters =
    case get(parent_counters) of
        undefined ->
            [];
        [] ->
            [];
        Counters ->
            Counters
    end,
    erlang:put(parent_counters, []),
    NCounters.

get_parent_and_self(Oid, FullName, MibNode) ->
    if 
        MibNode#mibnode.type =:= variable ->
            [];
        true ->
            PFName =
            case string:substr(FullName, 1, 1) of
                "/" ->
                    string:substr(FullName, 2);
                _ ->
                    FullName
            end,
            OidStr = oid2dots(Oid),
            case get(parent_counters) of
                undefined ->
                    erlang:put(parent_counters,lists:keystore(OidStr, 1, [], {OidStr,PFName}));
                [] ->
                    erlang:put(parent_counters,lists:keystore(OidStr, 1, [], {OidStr,PFName}));
                Counters ->
                    erlang:put(parent_counters,lists:keystore(OidStr, 1, Counters, {OidStr,PFName}))
            end
    end,
    get_all_parent(Oid, FullName).
    

%% Counters of all parent
get_all_parent(Oid, FullName) ->
    case get(parent_counters) of
        undefined ->
            cyc_parent_node(Oid, FullName, []);
        [] ->
            cyc_parent_node(Oid, FullName, []);
        Counters ->
            cyc_parent_node(Oid, FullName, Counters)
    end.

cyc_parent_node(Oid, FullName, Counters) ->
    case exe_parent_node(Oid, FullName) of
        [] ->
            [];
        [{POidStr,PName}] ->
            erlang:put(parent_counters,lists:keystore(POidStr, 1, Counters, {POidStr,PName})),
            get_all_parent(dots2oid(POidStr), PName);
        _ ->
            []        
    end.
    
exe_parent_node(Oid, FullName) ->
    case getClosestAncestor(Oid) of
        [] ->
            [];
        MibNode ->
            %% If you do not / will complain, do not know will not appear
            ParentName = string:substr(FullName, 1, string:rstr(FullName, "/") - 1),
            FName =
            case string:substr(ParentName, 1, 1) of
                "/" ->
                    string:substr(ParentName, 2);
                _ ->
                    ParentName
            end,
            
            [{oid2dots(MibNode#mibnode.oid), FName}]
    end.


%% Get Mibnode Oid corresponding full name specified in the full name of a father before
get_fullname(Mibnode, All) ->
    CurrFullName = mibnode_get_fullname(Mibnode),
    I = string:rstr(CurrFullName,"."),
    if
        I > 0 ->
            string:sub_string(CurrFullName, 1, I - 1);
        true ->
            []
    end.
    
 %% Get Mibnode corresponding to the full name specified Oid
get_fullname(PMibnode, All, COid) ->
    Me = element(2, PMibnode),
    CurrFullName = mibnode_get_fullname(PMibnode),
    POid = Me#me.oid,
    I = string:len(POid),
    J = string:len(COid) - 1,
    DisparOid = 
    if 
        J - I >= 1 ->
        lists:sublist(COid, string:len(POid) + 2, string:len(COid));
            true ->
        []
    end,
    CurrFullName ++ "." ++ oid2dots(DisparOid).


%% Process from treeMap dictionary for the closest ancestor with Oid Oid node, it returns no []
getClosestAncestor([]) ->
    [];
getClosestAncestor(Oid) ->
    POid = parent_oid(Oid),
    NMibNode =
    case get_attribute(POid) of
        {ok, MibNode} ->
            MibNode;
        {error, Reason} ->
            [];
        Other ->
            []
    end,
    IsCyc = (NMibNode =:= []) andalso (string:len(POid) > 0),
    if
        IsCyc =:= true ->
            getClosestAncestor(POid);
        true ->
            NMibNode
    end.
    
%% Oid of the last
parent_oid(Oid) ->
    Len = string:len(Oid),
    if 
        Len > 0 ->
            lists:sublist(Oid, 1, string:len(Oid) -1);
        true ->
            []
    end.

%% Initialize something
browsablemib() -> 
    case is_exist_mib_node() of
        false ->
            make_a_set(),
            Mibs = getCompiledMIBs(),
            MibObjects = read_multi_mid(Mibs),
            set_attribute(mibs, MibObjects),
            MIBNodeLookupTree = populateLookupTree();
        _ ->
            do_nothing
    end.

%% Construction mibnode dictionary
populateLookupTree() ->
    case get_attribute(mibs) of
        {ok, Mibs} ->
            cyc_mibs(Mibs);
        {error, Reason} ->
            [];
        Other ->
            []
    end.
%% Mib walk
cyc_mibs([]) ->
    [];
cyc_mibs([Mib=#mmib{}|T]) ->
    cyc_mibnode(Mib#mmib.nodes) ++
    cyc_mibs(T);
cyc_mibs([H|T]) ->
    cyc_mibs(T).

%% 遍历mibnode
cyc_mibnode([]) ->
    [];
cyc_mibnode([Node=#mibnode{}|T]) ->
    set_attribute(Node#mibnode.oid, Node),
    %% test...
    if
        Node#mibnode.mib =:= "CISCOWORKS-MIB" ->
            %%io:format("Node: ~p~n", [Node]);
            ok;
        true ->
            ok
    end,
    cyc_mibnode(T);
cyc_mibnode([H|T]) ->
    cyc_mibnode(T).

%% Mib file read more
read_multi_mid([])->[];
read_multi_mid([M|T])->
	read_mib(M) ++ read_multi_mid(T).

%% Read a single mib file
read_mib("All-MIBs")->[];
read_mib("No MIBs Available")->[];
read_mib(Mib)->
	Bin = ?MIB_PATH ++ "/" ++ filename:rootname(Mib,".mib") ++ ".bin",
	case snmp_misc:read_mib(Bin) of
		{ok,M}->
            MibName = filename:rootname(Mib,".mib"),
            %%io:format("Oids = ~p~n", [M#mib.mes]),
            set_mesdict(M#mib.mes),
            %%set_attribute(all_mes, NMes ++ M#mib.mes),
            MibObject = #mmib{mibname = MibName, nodes = build_mibnode(M#mib.mes, MibName), source = ?MIB_PATH ++ "/" ++ Mib, redmib = M},
            [MibObject];
		_->
			[]
	end.

get_mesdict() ->
    get_attribute(all_mes_dict).

set_mesdict([]) ->
    ok;
set_mesdict([Me=#me{}|T]) ->
    NMes =
            case get_attribute(all_mes_dict) of
                {ok, Mes} ->
                    Mes;
                {error, Reason} ->
                    dict:new();
                Other ->
                    dict:new()
            end,
    DictMes = dict:store(Me#me.oid, Me, NMes),
    set_attribute(all_mes_dict, DictMes),
    set_mesdict(T);
set_mesdict([H|T]) ->
    set_mesdict(T).

%% Mib node structure
build_mibnode([], MibName) ->
    [];
build_mibnode([Me=#me{}|T], MibName) ->
    MibNode = #mibnode{mib = MibName, name = atom_to_list(Me#me.aliasname), oid = Me#me.oid, type = Me#me.entrytype},
    [MibNode] ++
    build_mibnode(T, MibName);
build_mibnode([H|T], MibName) ->
    build_mibnode(T, MibName).
    

%% The existence of the ets table this mib_node
is_exist_mib_node() ->
    lists:member(?ETSNAME, ets:all()).

make_a_set() ->
    ets:new(?ETSNAME, [public,named_table]). 
    
    
get_attribute(Item) ->
    case ets:lookup(?ETSNAME, Item) of
        undefined ->
            {error, undefine_item};
        [{Item, Value}] ->
            {ok, Value};
        Other ->
            {error, Other}
    end.
    
set_attribute(Item, Value) ->
    ets:insert(?ETSNAME,{Item, Value}).

%%*********************************************************





%% Combination of all Mib Counters
check_multi_mid_oid(_,[],_)->[];
check_multi_mid_oid(Mib,[M|T],List)->
	check_mib_oids(M,List), 
    check_multi_mid_oid(Mib,T,List).
    

%% Inside of the Mib file, generate a list of the dictionary into treeMap process
build_mib_oids([]) ->
    [];
build_mib_oids([Me=#me{}|T]) ->
    case get(treeMap) of
        undefined ->
            NewTreeMap = lists:append([], [{oid2dots(Me#me.oid), Me}]),
            erlang:put(treeMap, NewTreeMap);
        [] ->
            NewTreeMap = lists:append([], [{oid2dots(Me#me.oid), Me}]),
            erlang:put(treeMap, NewTreeMap);
        TreeMap ->
            case lists:keymember(Me#me.oid, 1, TreeMap) of
                true ->
                    NewTreeMap = lists:keystore(Me#me.oid, 1, TreeMap, {oid2dots(Me#me.oid), Me}),
                    erlang:put(treeMap, NewTreeMap);
                _ ->
                    NewTreeMap = lists:append(TreeMap, [{oid2dots(Me#me.oid), Me}]),
                    erlang:put(treeMap, NewTreeMap)
            end
    end,
    build_mib_oids(T);
build_mib_oids([Me|T]) ->
    build_mib_oids(T).



%% Access to and from the process dictionary Oid treeMap the same node, if not, return []

    

check_mib_oids(Mib, List) ->
    Bin = ?MIB_PATH ++ "/" ++ filename:rootname(Mib,".mib") ++ ".bin",
    erlang:put(treeMap, []),
    erlang:put(treeMapTemp, []),
	case snmp_misc:read_mib(Bin) of
		{ok,M=#mib{}}->
            build_mib_oids(M#mib.mes),
            %% Mib node of the tree to make a temporary cache
            case get(treeMap) of
                undefined ->
                    do_nothing;
                [] ->
                    do_nothing;
                TreeMap ->
                    erlang:put(treeMapTemp, TreeMap)
            end,
            exe_check_mib_oids(List, M);
		_->
            []
	end.


%% Oid in the implementation of the statistics of all this is Mib Oid of the MIB corresponding to [{OidStr, Name}] list
exe_check_mib_oids([], M) ->
    [];
exe_check_mib_oids([Vb=#varbind{}|T], M) ->
    Oid = Vb#varbind.oid,
    MibNode = get_from_treemap(Oid),
    NewMibNode =
    if 
        Vb#varbind.value =:= noSuchObject ->
            is_continue;
        Vb#varbind.value =:= noSuchInstance ->
            is_continue;
        Vb#varbind.value =:= endOfMibView ->
            is_continue;
        Vb#varbind.value =:= 'NULL' ->
            is_continue;
        Vb#varbind.value =:= noObject ->
            is_continue;
        true ->
            %%snmp_trap_data:log(oid2dots(Oid)),
            if
                MibNode =:= [] ->
                    Close_Ancestor = getClosestAncestor(Oid),
                    if 
                        Close_Ancestor =:= [] ->
                            is_continue;
                        true ->
                            %%io:format("****Oid = ~p~n", [Oid]),
                            S2 = get_fullname(Close_Ancestor, M#mib.mes, Oid),
                            {S2, Close_Ancestor}
                    end;
                true ->
                    S2 = get_fullname(MibNode, M#mib.mes),
                    {S2, MibNode}
            end
    end,
    Vbs =
    case NewMibNode of
        is_continue ->
            is_continue;
        {S22, NMibNode} ->
            case is_belong_to_mib(Vb, M#mib.mes) of
                [] ->
                    is_continue;
                [NVbs] ->
                    {S22, NVbs, NMibNode};
                _ ->
                    is_continue
            end;
        _ ->
            is_continue
    end,
    case Vbs of
        is_continue ->
            exe_check_mib_oids(T, M);
        {S2s, NVb, NMMibNode} ->
            outputCounter(NMMibNode, NVb, M#mib.mes),
            exe_check_mib_oids(T, M);
        _ ->
            exe_check_mib_oids(T, M)
    end;
exe_check_mib_oids([Vb|T], M) ->
    exe_check_mib_oids(T, M).


%% Bind variables {OidStr, Name} construct to go inside Counters
builder_counters([], Counters, {Oid, Name}, All) ->
    NewCounters = Counters ++ [{Oid, Name}],
    erlang:put(snmp_browsable_counters, NewCounters);
builder_counters([{OidS,Me}|T], Counters, {Oid, Name}, All) ->
    
    case lists:prefix(OidS, Oid) of
        true ->
            NewCounters = Counters ++ [{OidS, find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}],
            erlang:put(treeMapTemp, T),
            builder_counters(T, NewCounters, {Oid, Name}, All);
        _ ->
            builder_counters(T, Counters, {Oid, Name}, All)
    end;
builder_counters([H|T], Counters, {Oid, Name}, All) ->
    builder_counters(T, Counters, {Oid, Name}, All).


%% Oid whether this mib file corresponding to the Oid
is_belong_to_mib(Vb, []) ->
    [];
is_belong_to_mib(Vb, [Me=#me{}|T]) ->
    Oid = Vb#varbind.oid,
    IsVariable = is_Variable(Vb),
    if
        IsVariable =:= true ->
            is_belong_to_mib(Vb, T);
        Me#me.imported =:= false ->
            case lists:prefix(Me#me.oid,Oid) of
                true->
                    [{oid2dots(Me#me.oid), atom_to_list(Me#me.aliasname)}],
                    [Vb];
                _->
                    is_belong_to_mib(Vb, T)
            end;
        true ->
            is_belong_to_mib(Vb, T)
    end;
is_belong_to_mib(Vb, [Me|T]) ->
    is_belong_to_mib(Vb, T).
    

%% Variable type variable to determine is not binding, the oid is not ending with zero
is_Variable(Vb=#varbind{}) ->
    if 
        Vb#varbind.oid =:= [] ->
            false;
        true ->
            case lists:last(Vb#varbind.oid) of
                0 ->
                    true;
                _ ->
                    false
            end
    end;
is_Variable(Vb) ->
    false.
%%**************************************************************************************************

get_mid_oid(_,[],_)->[];
get_mid_oid(Mib,[M|T],Session)->
	get_oids(M,Session) ++ get_mid_oid(Mib,T,Session).

get_oids("All-MIBs",_)->[];
get_oids("No MIBs Available",_)->[];
get_oids(Mib,Session)->
	Bin = ?MIB_PATH ++ "/" ++ filename:rootname(Mib,".mib") ++ ".bin",
	case snmp_misc:read_mib(Bin) of
		{ok,M}->
            
            %%io:format("~n***********MIB**********~n~p", [M]),
			%%[{oid2dots(X#me.oid),find_parent_full_name(X#me.oid,M#mib.mes,M#mib.mes) ++ atom_to_list(X#me.aliasname)}||X<-M#mib.mes];
			Oids = check_oids(M#mib.mes,Session,M#mib.mes),
            %%io:format("***********Oids = ~p~n", [Oids]),
            Oids;
		_->
			[]
	end.
    
to_OidString(T) ->
    Temp = lists:map(fun(X) -> integer_to_list(X) end, T),
    string:join(Temp, ".").

%%Check OId exists on the target machine
%%
check_oids([],_,_)->[];
check_oids([Me|T],Session,All)->
	%%io:format("check_oids:~p~n",[Me]),
	case Me#me.entrytype of
		table->
			case Me#me.assocList of
				[]->
					%%[{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}]
					%%++ 
					check_mib_table_row(T,Session,All,0);
				_->
					[{table_info,Ti}|_]=Me#me.assocList,
					Col = Ti#table_info.nbr_of_cols,
					%%[{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}]
					%%++ 
					Tables = check_mib_table_row(T,Session,All,Col),
                    [{oid2dots(Me#me.oid), find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}] ++
                    Tables
			end;
		internal->			
		 	%	{ok,{noError,_,[Vb|_]},_}->
			%		%%io:format("check_oids:~p~n",[Vb#varbind.variabletype]),
			%		case Vb#varbind.variabletype of
			%			'NULL'->
			%				check_oids(T,Session,All);
			%			_->
            
                            
							[{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}]
							++ check_oids(T,Session,All);
			%		end;
			%	_->
			%		check_oids(T,Session,All)
			% end;
            %%InternalCt = [{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}] ++ 
            %%check_mib_table_item(Me#me.oid,Me#me.oid,Session,find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)),
            %%io:format("***********InternalCt = ~p~n", [InternalCt]),
            %%InternalCt ++ check_oids(T,Session,All);
            
		variable->
			case Session:g(Me#me.oid) of
				{ok,{noError,_,[Vb|_]},_}->
					%%io:format("check_oids:~p~n",[Vb#varbind.variabletype]),
					case Vb#varbind.variabletype of
						'NULL'->
							check_oids(T,Session,All);
						_->
							[{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}]
							++ check_oids(T,Session,All)
					end;
				_->
					check_oids(T,Session,All)
			end;
		_->
			%%io:format("check_oids:~p~n",[Me]),
			%%[{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}]
			%%				++ check_oids(T,Session,All)
			check_oids(T,Session,All)
	end.


check_mib_table_row([Me=#me{entrytype=table_entry}|T],Sess,All,Col)->
	%%io:format("check_mib_table_row:~p~n",[Me]),
	%%[{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}]++ 
    [{oid2dots(Me#me.oid), find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}] ++
	check_mib_table_col(T,Sess,All,Col,0);
check_mib_table_row(Mes,Sess,All,_)->check_oids(Mes,Sess,All).

check_mib_table_col(Mes,Sess,All,Col,Col)->
	check_mib_table_row(Mes,Sess,All,Col);
check_mib_table_col([Me=#me{entrytype=table_column}|T],Sess,All,Col,I)->
	%%io:format("check_mib_table_col:~p~n",[Me]),
	[{oid2dots(Me#me.oid),find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname)}] ++
	check_mib_table_item(Me#me.oid,Me#me.oid,Sess,find_parent_full_name(Me#me.oid,All,All) ++ atom_to_list(Me#me.aliasname))++
	check_mib_table_col(T,Sess,All,Col,I+1);
check_mib_table_col(Mes,Sess,All,Col,_)->
	check_mib_table_row(Mes,Sess,All,Col).

check_mib_table_item(Prefix,Oid,Sess,ParentName)->
	%%io:format("check_mib_table_item:~p~n",[Oid]),
	case Sess:gn(Oid) of
		{ok,{noError,_,[Vb|_]},_}->
			case Vb#varbind.variabletype of
				'NULL'->
					[];
				_->
					%%io:format("check_mib_table_item VB:~p~n",[Vb]),
					case lists:prefix(Prefix,Vb#varbind.oid) of
						true->
							[{oid2dots(Vb#varbind.oid),ParentName ++ "/" ++ oid2dots(lists:subtract(Vb#varbind.oid,Prefix))}] ++
							check_mib_table_item(Prefix,Vb#varbind.oid,Sess,ParentName);
						_->
							[]
					end
			end;
		_->
			[]
	end.
    
check_mib_table_it(Prefix, Oid, Sess, ParentName) ->
    case Sess:gb(0, 50, Oid) of
		{ok,{noError,_,[Vb|_]},_}->
			case Vb#varbind.variabletype of
				'NULL'->
					[];
				_->
					%%io:format("check_mib_table_item VB:~p~n",[Vb]),
					case lists:prefix(Prefix,Vb#varbind.oid) of
						true->
							[{oid2dots(Vb#varbind.oid),ParentName ++ "/" ++ oid2dots(lists:subtract(Vb#varbind.oid,Prefix))}] ++
							check_mib_table_item(Prefix,Vb#varbind.oid,Sess,ParentName);
						_->
							[]
					end
			end;
		Other->
			Other
	end.
							

find_parent_full_name(_,[],_)->"";
find_parent_full_name(OId,[Me=#me{oid=undefined}|T],Mes)->
	find_parent_full_name(OId,T,Mes);
find_parent_full_name(OId,[Me|T],Mes)->
	Len = length(OId)-1,
	case length(Me#me.oid) of
		 Len ->
			case lists:suffix(Me#me.oid, lists:sublist(OId,1,length(Me#me.oid))) of
				true ->
					find_parent_full_name(Me#me.oid,Mes,Mes) ++ atom_to_list(Me#me.aliasname) ++ "/";
				false->
					find_parent_full_name(OId,T,Mes)
			end;
		_->
			find_parent_full_name(OId,T,Mes)
	end.

oid2dots([])->"";
oid2dots([H|T])->
	case T of
		[]->
			integer_to_list(H);
		_->
			integer_to_list(H) ++ "." ++ oid2dots(T)
	end.
    
dots2oid(OidStr)->
	S = string:tokens(OidStr, "."),
    [list_to_integer(X) || X <- S].

%%
%% Check the file exists the corresponding mib
%%
containsMIB(File)->
	BaseName = filename:basename(File),
	Bins = getCompiledMIBs(),
	lists:member(BaseName,Bins).