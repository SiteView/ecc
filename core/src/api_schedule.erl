%% ---
%%api_schedule
%%
%%---
-module(api_schedule).

-export([create/1,update/2,delete/1,get_info/1,get_infos/0,get_schedulename/1,get_info_by_name/1,name_existed/1,schedule_used/1]).
%-compile(export_all).


%% @spec create({Name, Type, SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) -> ({ok,Result} | {error,Resean})
%% where
%%	Name = string()
%%	Type = string()
%%	SunData = string() | {On,string(),string()}
%%	On = string()
%%	Result = string()
%%	@doc create a schedule,return schedule id.
%%	<br>Type is "hours","range" or "absolute".</br>
%%	<br>On is "enabled" or "disabled".</br>
%%	<br>when Type is "range",SunData is like {"enabled","10:00","20:00"}</br>
%%	<br>when Type is "hours",SunData is like {"enabled","10:00,12:00","11:00,13:00"}</br>
%%	<br>when Type is "absolute",SunData is like "10:00"</br>
create({Name, "range", SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) ->
	Data = [
			{name, string, list_to_binary(Name)},
			{type, string, <<"range">>},
			{sunday, tuple, term_to_binary(SunData)},
			{monday, tuple, term_to_binary(MonData)},
			{tuesday, tuple, term_to_binary(TueData)},
			{wednesday, tuple, term_to_binary(WedData)},
			{thursday, tuple, term_to_binary(ThuData)},
			{friday, tuple, term_to_binary(FriData)},
			{saturday, tuple, term_to_binary(SatData)}
			],
	create_schedule(Data);
create({Name, "hours", SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) ->
	Data = [
			{name, string, list_to_binary(Name)},
			{type, string, <<"hours">>},
			{sunday, tuple, term_to_binary(SunData)},
			{monday, tuple, term_to_binary(MonData)},
			{tuesday, tuple, term_to_binary(TueData)},
			{wednesday, tuple, term_to_binary(WedData)},
			{thursday, tuple, term_to_binary(ThuData)},
			{friday, tuple, term_to_binary(FriData)},
			{saturday, tuple, term_to_binary(SatData)}
			],
	create_schedule(Data);
create({Name, "absolute", SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) ->
	Data = [
			{name, string, list_to_binary(Name)},
			{type, string, <<"absolute">>},
			{sunday, string, list_to_binary(SunData)},
			{monday, string, list_to_binary(MonData)},
			{tuesday, string, list_to_binary(TueData)},
			{wednesday, string, list_to_binary(WedData)},
			{thursday, string, list_to_binary(ThuData)},
			{friday, string, list_to_binary(FriData)},
			{saturday, string, list_to_binary(SatData)}
			],
	create_schedule(Data);
create(_)->{error,error_parameter}.	
	
create_schedule(Data) ->	
	%Id = dbcs_schedule:get_nextId(),
	case dbcs_base:uuid2() of
		error ->
			{error, "0"};
		Id ->
			% Num = list_to_integer(StrN) + 1,
			% Id = integer_to_list(Num),
			case dbcs_schedule:insert_schedule(Id, Data) of
				{ok, _} ->
					% dbcs_schedule:update_maxId(Id),
					{ok, Id};
				{error, Err} ->
					{error,Err}
			end
	end.	


%% @spec delete(Id) -> (ok | error)
%% where
%%	Id = string()
%% @doc delete a schedule
delete(Id) ->
	case dbcs_schedule:delete_schedule(Id) of
		{ok, _} ->
			ok;
		_ ->
			error
	end.
	
%% @spec update(Id, {Name, Type, SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) -> (ok | error)
%% where
%%	Id = string()
%%	Name = string()
%%	Type = string()
%%	SunData = string() | {On,string(),string()}
%%	On = string()
%% @doc update a schedule
%%	<br>Type is "range","hours" or "absolute".</br>
%%	<br>On is "enabled" or "disabled".</br>
%%	<br>see {@link create/1}</br>
update(Id, {Name, "range", SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) ->
	Data = [
			{name, string, list_to_binary(Name)},
			{type, string, <<"range">>},
			{sunday, tuple, term_to_binary(SunData)},
			{monday, tuple, term_to_binary(MonData)},
			{tuesday, tuple, term_to_binary(TueData)},
			{wednesday, tuple, term_to_binary(WedData)},
			{thursday, tuple, term_to_binary(ThuData)},
			{friday, tuple, term_to_binary(FriData)},
			{saturday, tuple, term_to_binary(SatData)}
			],
	update_schedule(Id, Data);
update(Id, {Name, "hours", SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) ->
	Data = [
			{name, string, list_to_binary(Name)},
			{type, string, <<"hours">>},
			{sunday, tuple, term_to_binary(SunData)},
			{monday, tuple, term_to_binary(MonData)},
			{tuesday, tuple, term_to_binary(TueData)},
			{wednesday, tuple, term_to_binary(WedData)},
			{thursday, tuple, term_to_binary(ThuData)},
			{friday, tuple, term_to_binary(FriData)},
			{saturday, tuple, term_to_binary(SatData)}
			],
	update_schedule(Id, Data);	
update(Id, {Name, "absolute", SunData, MonData, TueData, WedData, ThuData, FriData, SatData}) ->
	Data = [
			{name, string, list_to_binary(Name)},
			{type, string, <<"absolute">>},
			{sunday, string, list_to_binary(SunData)},
			{monday, string, list_to_binary(MonData)},
			{tuesday, string, list_to_binary(TueData)},
			{wednesday, string, list_to_binary(WedData)},
			{thursday, string, list_to_binary(ThuData)},
			{friday, string, list_to_binary(FriData)},
			{saturday, string, list_to_binary(SatData)}
			],
	update_schedule(Id, Data).

update_schedule(Id, Data) ->
	case dbcs_schedule:update_schedule(Id, Data) of
		{ok, _} ->
			ok;
		_ ->
			error
	end.
	
%% @spec get_info(Id) -> ([] | Data)
%% where
%%	Id = string()
%%	Data = [{key,value}]
%% @doc get detail infomation of a schedule
%% <br>when Type is "range",the example of Data is:</br>
%% <br>[{id,'0E84F3C0B5B3CEB768E3A6906574C8475E4FA58D'},
%% {name,"aasa"},
%% {type,"range"},
%% {sunday,{"enabled","",""}},
%% {monday,{"enabled","",""}},
%% {tuesday,{"disabled","",""}},
%% {wednesday,{"enabled","",""}},
%% {thursday,{"enabled","10:10","17:30"}},
%% {friday,{"enabled","",""}},
%% {saturday,{"enabled","",""}}]
%%	</br>
%% <br>when Type is "absolute",the example of Data is:</br>
%% <br>[{id,'812A6F4716FB54E52D347829FEFDC3832B8BDF73'},
%% {name,"test1"},
%% {type,"absolute"},
%% {sunday,[]},
%% {monday,"12:00"},
%% {tuesday,[]},
%% {wednesday,[]},
%% {thursday,[]},
%% {friday,"14:00"},
%% {saturday,[]}]</br>
get_info(Id) ->
	dbcs_schedule:query_schedule(Id).	


%% @spec get_infos() ->[Data]
%% @doc get all of schedule
%% <br>the structure of Data see {@link get_info/1}</br>
get_infos() ->
	dbcs_schedule:list_schedules().
	
	
%% @spec get_schedulename(Id) -> ({ok,Name} | {error,Err})
%% where
%%	Name = string()
%%	Err = string()
%% @doc get name of schedule
get_schedulename(Id) ->
	case dbcs_schedule:get_name(Id) of		
		error ->
			{error, "0"};
		not_existed ->
			{error, "not_existed"};
		N ->
			{ok, N}
	end.

%% @spec get_info_by_name(Name) -> ([] | [Data])
%% where
%%	Name = string()
%%	Data = [{key,value}]
%% @doc get schedule by name
%%	<br></br>
%%	<dl>
%%	<dt>Data</dt><dd>See {@link get_info/1}</dd>
%%	</dl>
get_info_by_name(Name) ->
	dbcs_schedule:find("my.name=" ++ Name).
	
%% @spec name_existed(Name) -> (ok | not_existed | {error,Reason})
%% where
%%	Name = string()
%%	Reason = atom()
%% @doc Test the existence of the name
name_existed(Name)->
	dbcs_schedule:name_existed(Name).
	
%% @spec schedule_used(Id) -> (true | false)
%% where
%%	Id = string()
%% @doc check schedule is in used
schedule_used(Id)->
	case dbcs_monitor:get_monitor_match("my.schedule=" ++ Id ) of
		[S|_]->
			true;
		_->
			false
	end.