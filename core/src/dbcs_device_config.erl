%% ---
%% dbcs_device_config
%%
%%---

-module(dbcs_device_config).
-compile(export_all).
-export([create/1,getbyId/1,get_Where/1,get_Where/2,update/1,delete/1]).
-define(Device_Config,"device_config").
-include("monitor.hrl").
-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(_)->
	false.

%%----
%% The device configuration table data structure
%% [{id,ID},{time,Time},{deviceid,DeviceID},{config,Config}]                                
%%----

build([], _, Result) ->lists:reverse(Result);
build([F|R], Count, Result) ->
    NF = case F of
        {_,App,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,atom_to_list(Id)},{app_,atom_to_list(App)},{total,Count}] ++ [conver(K,T,V)||{K,T,V}<- Adv];
		_->
			[]
	end,
    build(R, Count, [NF|Result]).

data_to_db(Data)->
    {value,{id,ID}} = lists:keysearch(id,1,Data),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Data,base_property(K) =:= false],
	case [is_atom(ID)] of
		[true]->
            {content,list_to_atom(?Device_Config), ID, <<"device_config">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
		_->
			{}
	end.

db_to_data(Data)->
    case Data of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [conver(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.

 conver(K,T,V)->
    {Key,Value}=dbcs_base:db2term(K,T,V),
    case Key of
    time->
        {Key,sv_datetime:now2str(Value)};
    _->
        {Key,Value}
    end.

%% @doc create Config
create(Config) when is_list(Config) ->
    case lists:keysearch(id,1,Config) of
        {value,{id,_}}  ->
             case db_ecc:insert_data(?DBName,?Device_Config,data_to_db(Config)) of
                {ok,Ret}->
                    {ok,db_to_data(Ret)};
                {error,Err}->
                    {error,Err};
                Err2->
                    {error,Err2}
            end;
        _ ->
            case db_ecc:insert_data(?DBName,?Device_Config,data_to_db(lists:append(Config,[{id,dbcs_base:get_id()}]))) of
                {ok,Ret}->
                    {ok,db_to_data(Ret)};
                {error,Err}->
                    {error,Err};
                Err2->
                    {error,Err2}
            end
     end;
create(_)-> {error,parameter_error}.

%% @doc Based on ID Read
getbyId(ID)when is_list(ID)->
    Ret = db_ecc:get_data(?DBName,?Device_Config,"type=device_config & id="++ID),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_data};
				_->
					[Data|_] = Ret,
					db_to_data(Data)
			end
	end;
getbyId(ID)when is_atom(ID)->
    getbyId(atom_to_list(ID));
getbyId(_) ->{error,parameter_error}.

%% @doc Where to read Based on combined conditions
get_Where(Condition=#query_condition1{}) ->
    %%HostName = erlang:list_to_atom(Hostname),
    BeamCondition = build_condition(Condition),
    Where = BeamCondition#query_beam_condition1.where,
    Order = BeamCondition#query_beam_condition1.order,
    get_Where(Where, Order).
get_Where(Where, Order)->     
    Ret = db_ecc:get_data_stat(?DBName, ?Device_Config, Where,Order),
    case Ret of
        {_Start, _End, Count, Contents} ->
            build(Contents, Count, []);
        _ ->
            []
    end.
%% @doc Update a data
update(NewData) when is_list(NewData)->
    case lists:keysearch(id,1,NewData) of
    {value,{id,ID}}  ->
        Where = "id = " ++ atom_to_list(ID),	
        db_ecc:update_data(?DBName, ?Device_Config, Where, data_to_db(NewData));
     _  ->
        {error,not_found_id}
     end;
update(_)  ->  {error,parameter_error}.

%% @doc Based on ID delete a data 
delete(ID) when is_list(ID) ->
    db_ecc:delete_data(?DBName,?Device_Config,"id = "++ID);
delete(ID) when is_atom(ID) ->
    db_ecc:delete_data(?DBName,?Device_Config,"id = "++atom_to_list(ID));
delete(_) -> {error,parameter_error}.


%% *****************************************************************************
%% **********General treatment, (Where a database and assembled into Order)
%% *****************************************************************************
build_condition(Condition=#query_condition1{}) ->
    Index = Condition#query_condition1.index,
    Count = Condition#query_condition1.count,
    Where = 
        case Condition#query_condition1.where of
            ConWhere = #query_condition_where{} ->
                ParseWhere = parse_where_condition(ConWhere#query_condition_where.where),
                ParseWhere1 = string:strip(ParseWhere),
                LastStr = 
                    case ParseWhere1 of
                        [] ->
                            [];
                        _ ->
                            string:substr(ParseWhere1, string:len(ParseWhere1))
                    end,
                %%io:format("LastStr = ~p~n", [LastStr]),
                if
                    LastStr=:="&" ->
                        string:substr(ParseWhere1, 1, string:len(ParseWhere1)-2);
                    LastStr=:="|" ->
                        string:substr(ParseWhere1, 1, string:len(ParseWhere1)-2);
                    true ->
                        ParseWhere1
                end;
            OWhere ->
                OWhere
        end,
    %%io:format("Where = ~p~n", [Where]),
    Sort = Condition#query_condition1.sort,
    SortType = Condition#query_condition1.sortType,
    SortContent =
    case Sort of
        [] ->
            [];
        _V4 when erlang:is_list(Sort) ->
            "&order=my." ++ Sort++SortType;
        _ ->
            []
    end,
    IndexStr = 
        try erlang:integer_to_list(Index) of
            V when erlang:is_list(V) ->
                V;
            _ ->
                "0"
        catch
            _:_ ->
                "0"
        end,
    CountStr = 
        try erlang:integer_to_list(Index+Count) of
            V1 when erlang:is_list(V1) ->
                V1;
            _ ->
                "0"
        catch
            _:_ ->
                "0"
        end,
    Order = "from="++IndexStr++"&to="++CountStr++SortContent,%%++"&order=my."++Order,
    #query_beam_condition1{
        where=Where,
        order=Order
    }.

%% parsing query "where"
parse_where_condition([]) ->
    [];
parse_where_condition([ConditionWhere=#query_condition_where{}|T]) ->
    "(" ++ parse_where_condition(ConditionWhere#query_condition_where.where) ++ ")" ++ " | " ++
    parse_where_condition(T);
parse_where_condition([{Field,Operation,Value,Relation}|T]) ->
    case string:strip(Relation) of
        "" ->
            lists:append(lists:append([Field," ",Operation," ", Value]),
                            parse_where_condition(T));
        Re ->
            lists:append(lists:append([Field," ",Operation, " ", Value, " ", Re, " "]),
                            parse_where_condition(T))
    end;
parse_where_condition([_|T]) ->
    parse_where_condition(T).

