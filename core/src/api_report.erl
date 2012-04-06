%%
%% api_report
%%
-module(api_report).
-compile(export_all).
-define(PRECISION, precision).
-define(VMAX, vmax).
-define(SCHEDFILTER, schedFilter).
-define(STARTHOUR, startHour).
-define(FORMAT, format).
-define(MSCHEDULE, mschedule).
-define(HSCHEDULE, hschedule).
-extends(api_siteview).
-include("monitor.hrl").

%% @spec create(reportData)->({error,Reason} | {ok,Msg})
%% @doc create a new report
%%
create(ReportData)->
	Id=dbcs_base:uuid(),
%% 	Target = case lists:keysearch(target,1,ReportData) of
%% 				{value,{target,Val}}->
%% 					string:tokens(Val,"<>,");
%% 				_->
%% 					[]
%% 			end,
	dbcs_report:create_report([{id,Id}] ++ ReportData),
%% 	F = fun(X)->
%% 			case ?BASE_MODULE:find_object(X) of
%% 				[]->
%% 					error;
%% 				[Obj|_]->
%% 					case Obj:get_property(?CLASS) of
%% 						{ok,{?CLASS,group}}->
%% 							Obj:reload();
%% 						_->
%% 							Obj:reload(Obj)
%% 					end
%% 			end
%% 		end,
%% 	lists:foreach(F,Target),
	Id.
update(ReportData)->
	dbcs_report:update_report(ReportData),
	{ok,report_create_ok}.
get_mailtemplate() ->
 Ret= [{filename:basename(X),filename:basename(X)}||X<-filelib:wildcard("templates.history/*"),string:sub_string(filename:basename(X), 1,7)=:="History"],
 Ret.

getScalarValues(Prop,_)->
	case Prop of
		?PRECISION ->
			[{"automatic","default"},{"minute","60"},{"2 minutes","120"},{"5 minutes","300"},{"10 minutes","600"},{"15 minutes","900"},
			{"30 minutes","1800"},{"hour","3600"},{"2 hours","7200"},{"6 hours","21600"},{"12 hours","43200"},{"day","86400"}];
		?VMAX ->
			[{"automatic","default"},{"1","1"},{"5","5"},{"10","10"},{"20","20"},{"50","50"},{"100","100"},{"1000","1000"},{"5000","5000"},
			{"10000","10000"},{"20000","20000"},{"1000000","1000000"},{"10000000","10000000"}];
		?SCHEDFILTER->
			F = fun(X)->
				Id = proplists:get_value(id,X),
				Name = proplists:get_value(name,X),
				{Name,atom_to_list(Id)}
				end,
			[{"every day,all day","all"}] ++ lists:map(F,api_schedule:get_infos());
		?STARTHOUR ->
			[{"at the time the report is run","now"},{"at 00:00","0"},{"at 01:00","3600"},{"at 02:00","7200"}
			,{"at 03:00","10800"},{"at 04:00","14400"},{"at 05:00","18000"},{"at 06:00","21600"}
			 ,{"at 07:00","25200"},{"at 08:00","28800"},{"at 09:00","32400"},{"at 10:00","36000"}
			,{"at 11:00","39600"},{"at 12:00","43200"},{"at 13:00","46800"},{"at 14:00","50400"}
			,{"at 15:00","54000"},{"at 16:00","57600"},{"at 17:00","61200"},{"at 18:00","64800"}
			,{"at 19:00","68400"},{"at 20:00","72000"},{"at 21:00","75600"},{"at 22:00","79200"}
			,{"at 23:00","82800"}
			 
			];
		?FORMAT ->
			[{"color background (default)","bgcolor:#DDDDDD,border:1"},{"color background, no table borders","bgcolor:#DDDDDD,border:0"}
			,{"white background","bgcolor:#FFFFFF,border:1"}];
		?HSCHEDULE ->
			[{"00","0"},{"01","3600"},{"02","7200"},{"03","10800"},{"04","14400"},{"05","18000"}
			,{"06","21600"},{"07","25200"},{"08","28800"},{"09","32400"},{"10","36000"}
			,{"11","39600"},{"12","43200"},{"13","46800"},{"14","50400"},{"15","54000"}
			,{"16","57600"},{"17","61200"},{"18","64800"},{"19","68400"},{"20","72000"}
			,{"21","75600"},{"22","79200"},{"23","82800"}
			 ];
		?MSCHEDULE->
			[
			 {"00","0"},{"01","60"},{"02","120"},{"03","180"},{"04","240"},{"05","300"}
			,{"06","360"},{"07","420"},{"08","480"},{"09","540"},{"10","600"}
			,{"11","660"},{"12","720"},{"13","780"},{"14","840"},{"15","900"}
			,{"16","960"},{"17","1020"},{"18","1080"},{"19","1140"},{"20","1200"}
			,{"21","1260"},{"22","1320"},{"23","1380"},{"24","1440"},{"25","1500"}
			,{"26","1560"},{"27","1620"},{"28","1680"},{"29","1740"},{"30","1800"}
			,{"31","1860"},{"32","1920"},{"33","1980"},{"34","2040"},{"35","2100"}
			,{"36","2160"},{"37","2220"},{"38","2280"},{"39","2340"},{"40","2400"}
			,{"41","2460"},{"42","2520"},{"43","2580"},{"44","2640"},{"45","2700"},{"46","2760"}
			,{"47","2820"},{"48","2880"},{"49","2940"},{"50","3000"},{"51","3060"},{"52","3120"}
			,{"53","3180"},{"54","3240"},{"55","3300"},{"56","3360"},{"57","3420"}
			,{"58","3480"},{"59","3540"}
			 ];
		_->
			[]
	end.

%%
%% 
%%

%% @spec get_all()->({error,Reason} | reports)
%% @doc get all reports
%%
get_all()->
	dbcs_report:get_all().

%% @spec get(Id)->({error,Reason} | report)
%% @doc get a report
%%
get(Id)when is_atom(Id)->
	dbcs_report:get_report(Id);
get(Id)->
	{error,not_found_report}.
%%
%% filter one value report
%%
filter(Id,Filter) ->
  Report=api_report:get(Id),
  case Report of
	  {error,not_found_report} ->
		  Ret= false;
	  _ ->
		 {_,{_,Q}} =lists:keyfind(Filter, 1, Report),
		 Ret=Q
  end,
  Ret.

getids(Ids) ->
	dbcs_report:get_report_match("id in ["++Ids++"]").

%% @spec delete(Id)->({error,Reason} | {ok,Msg})
%% @doc delete a report
%%
delete(Id) when is_atom(Id)->
	case dbcs_report:remove_report(Id) of
		{ok,_}->
			{ok,deleted};
		{error,Err}->
			{error,Err}
	end.	

delete_all()->
	Rules = dbcs_report:get_all(),
	F = fun(X)->
			Id = proplists:get_value(id,X),
			delete(Id)
		end,
	lists:foreach(F,Rules),
	{ok,""}.	
