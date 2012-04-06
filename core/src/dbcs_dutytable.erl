%% ---
%% dbcs_dutytable
%%
%%---

-module(dbcs_dutytable).
-compile(export_all).
-export([create_dutytable/1,get_dutytableByID/1,get_dutytable/1,update_dutytable/1,delete_dutytableByID/1,create_dutydetail/1,get_dutydetailByID/1,get_dutydetail/1,update_dutydetail/1,delete_dutydetailByID/1]).
-define(DutyTable,"dutytable").
-define(DutyDetail,"dutydetail").
-include("monitor.hrl").
-include("dbcs_common.hrl").

base_property(id)->
	true;
base_property(_)->
	false.

%%----
%% Information Sheet on duty operation

%% [{id,ID},{name,Name},{type,Type},{desc,Desc}] 
%% id = atom()                                                        
%% name = list()                                                 
%% type = list()       type= "day"|"week"|"monnth"           
%% desc = list()                                        
%%----

data_to_db(Table,Data)->
    {value,{id,ID}} = lists:keysearch(id,1,Data),
	Advance = [dbcs_base:term2db(K,V)||{K,V}<- Data,base_property(K) =:= false],
	case [is_atom(ID)] of
		[true]->
            case Table =:= "dutytable" of
                true->
                    {content,list_to_atom(?DutyTable), ID, <<"dutytable">>,null,null,null,null,?Author,null,null,null,null,null,Advance};
                 false->
                    {content,list_to_atom(?DutyDetail), ID, <<"dutydetail">>,null,null,null,null,?Author,null,null,null,null,null,Advance}
             end;
		_->
			{}
	end.

db_to_data(Data)->
    case Data of
		{_,_,Id,_,_,_,_,_,_,_,_,_,_,_,Adv}->
			[{id,Id}] ++ [dbcs_base:db2term(K,T,V)||{K,T,V}<-Adv];
		_->
			[]
	end.
    
%% @doc create a table
create_dutytable(DutyData) when is_list(DutyData) ->
    case lists:keysearch(id,1,DutyData) of
        {value,{id,_}}  ->
             case db_ecc:insert_data(?DBName,?DutyTable,data_to_db("dutytable",DutyData)) of
                {ok,Ret}->
                    {ok,db_to_data(Ret)};
                {error,Err}->
                    {error,Err};
                Err2->
                    {error,Err2}
            end;
        _  ->
            {error,not_found_id}
     end;
create_dutytable(_)-> {error,parameter_error}.

%% @doc Read on duty under the ID table
get_dutytableByID(ID)when is_list(ID)->
    Ret = db_ecc:get_data(?DBName,?DutyTable,"type=dutytable & id="++ID),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_data};
				_->
					[DutyData|_] = Ret,
					db_to_data(DutyData)
			end
	end;
get_dutytableByID(ID)when is_atom(ID)->
    get_dutytableByID(atom_to_list(ID));
get_dutytableByID(_) ->{error,parameter_error}.

%% @doc Where duty Based on table to read combination of conditions
get_dutytable(Where)when is_list(Where)->
    Ret = db_ecc:get_data(?DBName,?DutyTable,Where),
	case Ret of
		{error,Error}->{error,Error};
		_->
			[db_to_data(X)||X <- Ret]
	end.
    
%% @doc Update a data
update_dutytable(NewDutyData) when is_list(NewDutyData)->
    case lists:keysearch(id,1,NewDutyData) of
    {value,{id,ID}}  ->
        Where = "id = " ++ atom_to_list(ID),	
        db_ecc:update_data(?DBName, ?DutyTable, Where, data_to_db("dutytable",NewDutyData));
     _  ->
        {error,not_found_id}
     end;
update_dutytable(_)  ->  {error,parameter_error}.

%% @doc Based on  ID delete a data
delete_dutytableByID(ID) when is_list(ID) ->
    db_ecc:delete_data(?DBName,?DutyTable,"id = "++ID);
delete_dutytableByID(ID) when is_atom(ID) ->
    db_ecc:delete_data(?DBName,?DutyTable,"id = "++atom_to_list(ID));
delete_dutytableByID(_) -> {error,parameter_error}.


%%-----
%% For more information on duty operating table
%% Watch for more information table data structure
%% [{id,ID},{tableid,TableID},{phone,Phone},{email,EMail},{starttime,{Hour,Minute}},{endtime,{Hour,Minute}},{day,Day}] 
%% id = atom()                
%% tableid = list()          
%% phone = list()            
%% email = list()             
%% starttime = time()  
%% endtime = time()     
%% day = list()                      
%%----

%% @doc Details create a duty
create_dutydetail(DutyDetail)when is_list(DutyDetail) ->
    case lists:keysearch(id,1,DutyDetail) of
        {value,{id,_}}  ->
            case lists:keysearch(tableid,1,DutyDetail) of
                {value,{tableid,TableID}}  ->
                    case get_dutytableByID(TableID) of
                        {error,_}->{error,bad_tableid};
                        _->
                            case db_ecc:insert_data(?DBName,?DutyDetail,data_to_db("dutydetail",DutyDetail)) of
                                {ok,Ret}->
                                    {ok,db_to_data(Ret)};
                                {error,Err}->
                                    {error,Err};
                                Err2->
                                    {error,Err2}
                             end
                    end;         
                _  ->
                    {error,not_found_tableid}
            end;
        _  ->
            {error,not_found_id}
    end;
create_dutydetail(_)-> {error,parameter_error}.

%% @doc For more information read the duty under the ID
get_dutydetailByID(ID)when is_list(ID)->
    Ret = db_ecc:get_data(?DBName,?DutyDetail,"type=dutydetail & id="++ID),
	case Ret of
		{error,Resean}->
			{error,Resean};
		_->
			case length(Ret) of
				0->
					{error,not_found_data};
				_->
					[DutyDetail|_] = Ret,
					db_to_data(DutyDetail)
			end
	end;
get_dutydetailByID(ID)when is_atom(ID)->
    get_dutydetailByID(atom_to_list(ID));
get_dutydetailByID(_) ->{error,parameter_error}.

%% @doc Where to read on duty Based on combined conditions for more information
get_dutydetail(Where)when is_list(Where)->
    Ret = db_ecc:get_data(?DBName,?DutyDetail,Where),
	case Ret of
		{error,Error}-> {error,Error};
		_->
			[db_to_data(X)||X <- Ret]
	end.
    
%% @doc Update a duty details
update_dutydetail(NewDutyDetail)when is_list(NewDutyDetail)->
    case lists:keysearch(id,1,NewDutyDetail) of
    {value,{id,ID}}  ->
        Where = "id = " ++ atom_to_list(ID),	
        db_ecc:update_data(?DBName, ?DutyDetail, Where, data_to_db("dutydetail",NewDutyDetail));
     _  ->
        {error,not_found_id}
     end;
update_dutydetail(_)  ->  {error,parameter_error}.

%% @doc Delete a information duty under the ID 
delete_dutydetailByID(ID)when is_list(ID) ->
    db_ecc:delete_data(?DBName,?DutyDetail,"id = "++ID);
delete_dutydetailByID(ID)when is_atom(ID) ->
    db_ecc:delete_data(?DBName,?DutyDetail,"id = "++atom_to_list(ID));
delete_dutydetailByID(_) -> {error,parameter_error}.
    
    
    
    