%% ---
%%  api_dutytable
%%
%%---

-module(api_dutytable).
-compile(export_all).
-export([create_duty/1,get_dutyByID/1,get_all_duty/0,is_dutyname_exist/1,update_duty/1,delete_duty/1,delete_duty_list/1,create_dutydetail/1,
get_dutydetailByID/1,get_dutydetailByTableid/1,update_dutydetail/1,delete_dutydetail/1,delete_dutydetail_list/1]).
-include("monitor.hrl").

%% @spec create_dutytable (Data)->({ok,Result} | {error, Reason})
%% where
%%	Data = list() = [{id,ID},{name,Name},{type,Type},{desc,Desc}] 
%%	Reason = not_found_id | parameter_error
%%	Result = [{key,value}]
%%	
%% @docInformation to create a shift table
%%
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_id</dt><dd>Parameter is no id</dd>
%%		<dt>parameter_error</dt><dd>Parameter type error</dd>
%%	</dl>
%%  <br>Data struct type:</br>
%%	<dl>
%%		<dt>ID</dt><dd>Duty table id</dd>
%%		<dt>Name</dt><dd>Duty table name</dd>
%%		<dt>Type</dt><dd>Duty table type = "day"|"week"|"monnth"  </dd>
%%		<dt>Desc</dt><dd>Duty table description</dd>
%%	</dl>
create_duty(Data)->
    dbcs_dutytable:create_dutytable(Data).

%% @spec create_dutytable (ID)->(Result | {error, Reason})
%% where
%%	ID = list() 
%%	Reason = parameter_error
%%	Result = list() =  [{id,ID},{name,Name},{type,Type},{desc,Desc}] 
%%	
%% @doc Read on duty under the ID table
get_dutyByID(ID)->
    dbcs_dutytable:get_dutytableByID(ID).
  
%% @spec get_all_duty()
%% @doc Read on duty under the ID table
get_all_duty()->
    dbcs_dutytable:get_dutytable("").

%% @spec get_all_duty_for_alert()
%% @doc Duty List for the alarm to generate drop-down list option
get_all_duty_for_alert()->
    All_Duty = get_all_duty(),
    [{proplists:get_value(name,Duty),atom_to_list(proplists:get_value(id,Duty))} || Duty <-All_Duty].

%% @spec update_duty (NewData)->(Result | {error, Reason})
%% where
%%	NewData = list() = [{id,ID},{name,Name},{type,Type},{desc,Desc}] 
%%	Reason = parameter_error | not_found_id
%%	
%% @doc A duty to modify table data
update_duty(NewData)->
    dbcs_dutytable:update_dutytable(NewData).

%% @spec is_dutyname_exist (Name)->(true|false)
%% where
%%	Name = list() 
%%	Reason = parameter_error 
%%	
%% @doc Duty table name already exists
is_dutyname_exist(Name) when is_list(Name)->
    Result = dbcs_dutytable:get_dutytable("my.name ="++Name),
    length(Result)>0;
is_dutyname_exist(_)->{error,parameter_error}.

%% @spec update_duty (ID)->({ok,deleted} | {error, Reason})
%% where
%%	NewData = list() 
%%	
%% @doc Delete a data table on duty
delete_duty(ID) ->
    Ret = dbcs_dutytable:delete_dutytableByID(ID),
    case Ret of
        {ok,_}->
            DetailInfo = get_dutydetailByTableid(ID),
            Detail_Ids = [ ID1 || [{id,ID1},_,_,_,_,_,_] <- DetailInfo ],
            delete_dutydetail_list(Detail_Ids);
        {error,Reason}->
            {error,Reason};
        _->
            {error,unknow_error} 
    end.

%% @spec update_duty (IDs)->({ok,deleted} | {error, Reason})
%% where
%%	IDs = list() =[ID1,ID2,ID3,...]
%%	
%% @doc Delete a data table on duty
delete_duty_list([])->{ok,deleted};
delete_duty_list([ID|IDs]) ->
    case delete_duty(ID) of
        {ok,_}->
                delete_duty_list(IDs);
         Reason->
                {error,Reason}
    end.

%% @spec create_dutydetail (Data)->({ok,Result} | {error, Reason})
%% where
%%	Data = list() = [{id,ID},{tableid,TableID},{phone,Phone},{email,EMail},{starttime,{Hour,Minute}},{endtime,{Hour,Minute}},{day,Day}] 
%%	Reason = not_found_id | parameter_error | not_found_tableid | bad_tableid
%%	Result = [{key,value}]
%%	
%% @doc create a data table on duty
%%
%%  <br>error reason:</br>
%%	<dl>
%%		<dt>not_found_id</dt><dd>Parameter is no id</dd>
%%		<dt>parameter_error</dt><dd>Parameter type error</dd>
%%		<dt>not_found_tableid</dt><dd>Parameter is not tableid</dd>
%%		<dt>bad_tableid</dt><dd>tableid error</dd>
%%	</dl>
%%  <br>Data struct type:</br>
%%	<dl>
%%		<dt>ID</dt><dd>Duty table id</dd>
%%		<dt>TableID</dt><dd>The main information table corresponding to the id</dd>
%%		<dt>Phone</dt><dd> Receive alarm calls </dd>
%%		<dt>EMail</dt><dd>Receiving alarms eMail </dd>
%%		<dt>starttime</dt><dd>sart time {Hour,Minute}</dd>
%%		<dt>endtime</dt><dd>end time{Hour,Minute}</dd>
%%		<dt>Day</dt><dd>Set weekly or monthly(The first few days) </dd>
%%	</dl>
create_dutydetail(Data)->
    dbcs_dutytable:create_dutydetail(Data).
    
%% @spec get_dutydetailByID (ID)->(Result | {error, Reason})
%% where
%%	ID = list() =  [{id,ID},{tableid,TableID},{phone,Phone},{email,EMail},{starttime,{Hour,Minute}},{endtime,{Hour,Minute}},{day,Day}] 
%%	Reason = parameter_error
%%	Result = list() =  [{id,ID},{name,Name},{type,Type},{desc,Desc}] 
%%	
%% @doc For more information read the duty under the ID
get_dutydetailByID(ID)->
    dbcs_dutytable:get_dutydetailByID(ID).

%% @spec get_dutydetailByTableid()
%% @doc According to the main table id to read all the details on duty2011-8-11
get_dutydetailByTableid(TableId)->
    dbcs_dutytable:get_dutydetail("my.tableid =" ++TableId).

%% @spec update_dutydetail (NewData)->(Result | {error, Reason})
%% where
%%	NewData = list() = [{id,ID},{tableid,TableID},{phone,Phone},{email,EMail},{starttime,{Hour,Minute}},{endtime,{Hour,Minute}},{day,Day}] 
%%	Reason = parameter_error | not_found_id
%%	
%% @doc A duty to modify details
update_dutydetail(NewData)->
    dbcs_dutytable:update_dutydetail(NewData).
    
%% @spec delete_dutydetail (ID)->({ok,deleted} | {error, Reason})
%% where
%%	NewData = list() 
%%	
%% @doc Remove a watch for more information
delete_dutydetail(ID) ->
    dbcs_dutytable:delete_dutydetailByID(ID).

%% @spec delete_dutydetail_list (IDs)->({ok,deleted} | {error, Reason})
%% where
%%	IDs = list() =[ID1,ID2,ID3,...]
%%	
%% @doc Delete a duty details
delete_dutydetail_list([])->{ok,deleted};
delete_dutydetail_list([ID|IDs]) ->
    case dbcs_dutytable:delete_dutydetailByID(ID) of
        {ok,_}->
                delete_dutydetail_list(IDs);
         Reason->
                {error,Reason}
    end.

get_duty_info(ID,Info) when ID=/=[]->%%Info ->email | phone
     Id = case is_atom(ID) of
        true->atom_to_list(ID);
        _   -> ID
        end,
     case dbcs_dutytable:get_dutytableByID(Id) of
     {error,_}->[];
     Duty->
        Type = proplists:get_value(type,Duty),
        Day =case string:str(Type,"week")>0 of
        true->
            sv_datetime:get_day();
        _->
            case string:str(Type,"month")>0 of
            true->
            {{_,_,D},_} = calendar:local_time(),
            D;
            _->
            0
            end
        end,
        case dbcs_dutytable:get_dutydetail("my.tableid =" ++Id++"&my.day ="++integer_to_list(Day)) of
            []->[];
            {error,_}->[];
            DutyDetail ->
            io:format("~nDutyDetail:~p~n",[DutyDetail]),
            {_Date,Time} = calendar:local_time(),
            Fun=fun(EL)->
                {SH,SM} = proplists:get_value(starttime,EL),
                {EH,EM} = proplists:get_value(endtime,EL),
                case sv_datetime:time_compare(Time,{SH,SM,0}) =:= -1 orelse sv_datetime:time_compare(Time,{EH,EM,0}) =:= 1 of
                true -> false;
                _   ->true
                end
            end,
            FilterDutyDetail = lists:filter(Fun,DutyDetail),
            case length(FilterDutyDetail)>0 of
            true->
            [ proplists:get_value(Info,M)|| M<-FilterDutyDetail];
            _->[]
            end
        end    
    end;
get_duty_info(_,_)->[].
    
    


    