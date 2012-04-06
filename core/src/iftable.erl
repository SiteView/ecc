%%
%% iftable
%%
%%
-module(iftable,[Session]).
-compile(export_all).
-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_ecc.hrl").

-define(INCOMING_OID,[1,3,6,1,2,1,2,2,1,10]).       %%八位字节组数量
-define(OUTGOING_OID,[1,3,6,1,2,1,2,2,1,16]).
-define(SPEED_OID,[1,3,6,1,2,1,2,2,1,5]).
-define(OPSTATUS_OID,[1,3,6,1,2,1,2,2,1,8]).        %%设备接口当前操作状态
-define(DESC_OID,[1,3,6,1,2,1,2,2,1,2]).            %%设备接口描述
-define(PHYS_ADDR_OID,[1,3,6,1,2,1,2,2,1,6]).
-define(INDISCARDS_OID,[1,3,6,1,2,1,2,2,1,13]).     %%被选择丢失的包的数量，即使没有发生错误
-define(INERRORS_OID,[1,3,6,1,2,1,2,2,1,14]).       %%发生错误的包的数量（组织交给上层协议）
-define(OUTDISCARDS_OID,[1,3,6,1,2,1,2,2,1,19]).
-define(OUTERRORS_OID,[1,3,6,1,2,1,2,2,1,20]).
-define(INUCAST_OID,[1,3,6,1,2,1,2,2,1,11]).        %%包的数量(不包括广播和多点传送地址)
-define(OUTUCAST_OID,[1,3,6,1,2,1,2,2,1,17]).
-define(OUTQLEN_OID,[1,3,6,1,2,1,2,2,1,21]).
-define(NAME_OID,[1,3,6,1,2,1,31,1,1,1,1]).
-define(ALIAS_OID,[1,3,6,1,2,1,31,1,1,1,18]).
-define(oids,[?INCOMING_OID,?OUTGOING_OID,?SPEED_OID,?OPSTATUS_OID,?DESC_OID,?PHYS_ADDR_OID,?INDISCARDS_OID
				,?INERRORS_OID,?OUTDISCARDS_OID,?OUTERRORS_OID,?INUCAST_OID,?OUTUCAST_OID,?OUTQLEN_OID,?NAME_OID,?ALIAS_OID]).

-define(OBJECT_NOT_AVAILABLE,-1).
-define(INTERFACE_UP,1).
-define(INTERFACE_DOWN,2).

refreshTable()->
	[].

getActiveInterfaces(I)->
	OperStatus = Session:get_table_col(?OPSTATUS_OID),
	F = fun(X)->
		Index = lists:last(X#varbind.oid),
		case Session:g(?DESC_OID ++ [Index]) of
			{ok,{noError,_,[Vb|_]},_}->
				NewVb = Vb#varbind{value = "Interface " ++ integer_to_list(Index) ++ " (" ++ getOctetString(Vb#varbind.value) ++ ")" ++ getStatus(Index)};
                %%Vb;
			_->
				[]
		end
	end,
	case I of
		1->
			lists:map(F,lists:filter(fun(X)->X#varbind.value=:=?INTERFACE_UP end,OperStatus));
		_->
			lists:map(F,OperStatus)
	end.

getPhysicalAddress(I)->
	case Session:g(?PHYS_ADDR_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.

getInterfaceName(I)->
	case Session:g(?NAME_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.
    


lookupColValIndex([],_)-> 0;
lookupColValIndex([Item|T],Val)->
	%%io:format("lookupColValIndex:~p,~p~n",[Item,Val]),
	case THIS:getOctetString(Item#varbind.value) of
		Val->

			lists:last(Item#varbind.oid);
		_->
			lookupColValIndex(T,Val)
	end.

getOctetString(Str)->
	case lists:last(Str) of
		0->
			string:substr(Str,1,length(Str)-1);
		_->
			Str
	end.

buildDesc(Ret) ->
    F = fun(X)->
            Str = getOctetString(X#varbind.value),
            Index = lists:last(X#varbind.oid),
            NewVb = X#varbind{value = "Interface " ++ integer_to_list(Index) ++ " (" ++ Str ++ ")"},
            NewVb
            %%X
        end,
	lists:map(F,Ret).
    

getRowFromPhysicalAddress(Addr)->
	Addrs = Session:get_table_col(?PHYS_ADDR_OID),
	lookupColValIndex(Addrs,Addr).

getRowFromDescription(Desc)->
	Ret = Session:get_table_col(?DESC_OID),
    %%io:format("*********************~nRet = ~p~n    Desc = ~p~n", [Ret, Desc]),
    NewRet = buildDesc(Ret),
    %%io:format("*********************~nNewRet = ~p~n    Desc = ~p~n", [NewRet, Desc]),
	Index = lookupColValIndex(NewRet,Desc),
    %%io:format("Match Index = ~p~n", [Index]),
    Index.

getRowFromName(Name)->
	Ret = Session:get_table_col(?NAME_OID),
	lookupColValIndex(Ret,Name).


getInDiscards(I)->
	%%io:format("getInDiscards:~p,~p~n",[I,Session:g(?INDISCARDS_OID ++ [I])]),
	case Session:g(?INDISCARDS_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.

getInErrors(I)->
	case Session:g(?INERRORS_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.

getInOctets(I)->
	case Session:g(?INCOMING_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.

getInUCastPackets(I)->
	case Session:g(?INUCAST_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.
    
getOutUCastPackets(I)->
	case Session:g(?OUTUCAST_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.

getOutDiscards(I)->
	case Session:g(?OUTDISCARDS_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.

getOutErrors(I)->
	case Session:g(?OUTERRORS_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.

getOutOctets(I)->
	case Session:g(?OUTGOING_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.


getOperStatus(I)->
	case Session:g(?OPSTATUS_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		Other->
            io:format("***********Error: ~p~n", [Other]),
			error
	end.
	
getStatus(I)->
	case Session:g(?OPSTATUS_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			case lists:keysearch(Vb#varbind.value, 1, ?INTERFACE_STATUS) of
                {value, {_, Status}} ->
                    lists:append(["(",Status,")"]);
                _ ->
                    ""
            end;
		Other->
            io:format("***********Error: ~p~n", [Other]),
			error
	end.
	
    
getSpeed(I) ->
    case Session:g(?SPEED_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.
    
getOutQLen(I) ->
    case Session:g(?OUTQLEN_OID ++ [I]) of
		{ok,{noError,_,[Vb|_]},_}->
			Vb;
		_->
			error
	end.