-module(dbcs_view).

-compile(export_all).

-define(Table,"view").
-define(Author,<<"lei.lin@dragonflow.com">>).
-define(DBName,server_conf:get_db_node()).

-record(view,{viewname}).

insert(Viewname) ->
    Record = #view{viewname=Viewname},
    db_ecc:insert_data(?DBName, ?Table, {content, list_to_atom(?Table),view, <<"view">>,null,null,null,null,?Author,null,null,null,null,null,view_to_db(Record)}). 

update(Viewname) ->
    Where =  "id=view",
	NewRecord = {content, list_to_atom(?Table),view, <<"view">>, null, null, null, null, ?Author, null, null, null, null, null, view_to_db(#view{viewname=Viewname})},
	db_ecc:update_data(?DBName, ?Table, Where, NewRecord).    

get() ->
    Where = "id=view", 
    Ret = db_ecc:get_data(?DBName,?Table, Where),
    io:format("Ret:~p~n",[Ret]),
	case is_list(Ret) of
		false ->
			[];
		true ->
			[db_to_view(Advance) || {content, _, _, _, _, _, _, _, _, _, _, _, _, _, Advance} <- Ret] 			
	end.        

db_to_view(Advance) ->
	Data = [db2term(K, T, V) || {K, T, V} <- Advance],
    io:format("Data:~p~n",[Data]),
	#view{               
			 viewname=proplists:get_value(viewname, Data)
			  }. 


    
view_to_db(Mach) ->
	[
        {viewname,string,list_to_binary(Mach#view.viewname)}        
	 ].


db2term(K,T,V) when not is_binary(V)->{K,V};
db2term(K,T,V) when T=:= number ->
	NV = binary_to_list(V),
	case string:to_float(NV) of
		{error,_}->
            if NV /= [] -> 
			    {K,list_to_integer(NV)};
            true ->
                {K,0} 
            end; 
		_->
			{K,list_to_float(NV)}
	end;
db2term(K,T,V) when T=:= string ->  {K,binary_to_list(V)};
db2term(K,_,V)->{K,binary_to_term(V)}.     
