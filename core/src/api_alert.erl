%%
%% @doc api of alert 
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
%%
-module(api_alert).
-compile(export_all).
-extends(api_siteview).
-include("monitor.hrl").
-include("alert.hrl").

-export([create/1,get_all/0,get/1,delete/1,update/1,alert_test/2,get_scalar_property/3,disable_all/0,enable_all/0]).

-export([get_log/1,get_log/2,get_log/3,get_log/5,log_types/0,query_log/5,query_log/7]).

-export([get_template_file_list/1,read_template_file/2,write_template_file/3,remove_template_file/2]).

%% @spec create(AlertData)->({error,Reason} | {ok,Result})
%% where
%%	Reason = atom()
%%	AlertData = [{key,value}]
%%	Result = [{key,value}]
%%	
%% @doc create a new alert
%% <br></br>
%%	<dl>
%%	<dt>AlertData</dt> 
%%	<dd>[{name,string()},{class,rule},{target,Target},{action,ActionType},{action_param,Param},
%%					{category,Category},{condition,Condition},{enabled,Enabled},
%%					{name_match,string()},{status_match,string()},{type_match,string()}]
%%  </dd>
%%	<dt>Target</dt><dd>string(),example:"&lt;id1&gt;,&lt;id2&gt;"</dd>
%%	<dt>ActionType</dt><dd> mailto | sound | sms | script | post | database | snmptrap | syslog</dd>
%%	<dt>Param</dt> <dd>#mail_alert{} | #sms_alert{} | #script_alert{} | #snmptrap_alert{} | #sound_alert{} 
%%				| #syslog_alert{} | database_alert |  #disable_alert{} | #post_alert{}</dd>
%%	<dd>        action_param record is defined in file <a href="alert.hrl">alert.hrl</a></dd>
%%	<dt>Category </dt> <dd>error | warning | good</dd>
%%	<dt>Condition </dt><dd><li>{condition,{always,X}} -- alway trigger alert after X times</li></dd>
%%  <dd><li>            {condition,{once,X}}   -- trigger alert once at X times</li></dd>
%%  <dd><li>            {condition,{select,{X,Y}}} -- trigger alert every Y times after X     </li></dd>
%%  <dd><li>            {condition,{group,X}} -- trigger alert when group category reach X times </li></dd>
%%  <dd><li>            {condition,all}  -- trigger alert when all monitor in group at category</li></dd>
%%	<dt>Enabled</dt> 
%%			<dd><li>true -- enabled</li></dd>
%%          <dd><li>{false,permanently} -- permanently disabled</li></dd>
%%          <dd><li>{false,{next,NOW,X,TimeUnit}} -- disabled next X TimeUnit from NOW, TimeUnit = minutes | hours | days</li></dd>
%%          <dd><li>{false,{schedule,"16:13","8/21/09","17:13","8/21/09"}} -- disabled in a period time </li></dd>
%% </dl>
%%	
%% @end
create(AlertData) when is_list(AlertData)->
        %~ io:format("create:~p~n",[AlertData]),
	NewData = proplists:delete(id,AlertData),
	case check_data(NewData) of
		true->
			{error,data_error};
		_->
			Id=dbcs_base:uuid(),
			case dbcs_rule:create_rule([{id,Id}] ++ NewData) of
				{ok,Alert}->
					Rl = rule:new(),
					Rl:init(Rl,[{id,Id},{?APP,dbcs_base:get_app()}] ++ NewData),
					
					siteview:set_object(Id,rule,Rl),
					
					% create index
					%index_store:update(rule,[{id,Id},{?APP,dbcs_base:get_app()}] ++ NewData),
					
					{ok,Alert};
				Else->
					Else
			end
	end;
create(_)->{error,parameter_error}.

check_data(AlertData)->
        %~ io:format("check_data:~p~n",[AlertData]),   
	lists:any(fun(X)->
		case X of
			%~ {target,Target}->
				%~ case re:run(Target,"^(<all>|<[0-9]+(\.[0-9]+)+>)(,(<all>|<[0-9]+(\.[0-9]+)+>))*$") of
					%~ nomatch->
						%~ true;
					%~ _->
						%~ false
				%~ end;
			%~ {category,error}->
				%~ false;
			%~ {category,good}->
				%~ false;
			%~ {category,warning}->
				%~ false;
			%~ {category,_}->
				%~ true;
			{target,[]} -> true;
			{alert_level,0} -> false;	
			{alert_level,1} -> false;
			{alert_level,2} -> false;
			{alert_level,3} -> false;
			{alert_level,4} -> false;
			{alert_level,_} -> true;
			{action,mailto}->
				false;
			{action,sms}->	
				false;
			{action,sound}->	
				false;
			{action,script}->	
				false;
			{action,post}->	
				false;
			{action,database}->	
				false;
			{action,snmptrap}->	
				false;
			{action,syslog}->	
				false;
			{action,disable_enable_monitor}->	
				false;
			{action,_}->	
				true;
			{action_param,#mail_alert{}}->
				false;
			{action_param,#sms_alert{}}->
				false;
			{action_param,#script_alert{}}->
				false;
			{action_param,#snmptrap_alert{}}->
				false;
			{action_param,#sound_alert{}}->
				false;
			{action_param,#syslog_alert{}}->
				false;
			{action_param,#database_alert{}}->
				false;
			{action_param,#disable_alert{}}->
				false;
			{action_param,#post_alert{}}->
				false;
			{action_param,_}->
				true;
			{condition,{all,[]}}->
				false;
			{condition,all}->
				false;
			{condition,{always,N}} when is_integer(N)->
				false;
			{condition,{once,N}} when is_integer(N)->
				false;
			{condition,{group,N}} when is_integer(N)->
				false;
			{condition,{select,{M,N}}} when is_integer(N) and is_integer(M)->
				false;
			{condition,_}->
				true;
			_->
				false
		end
	end, AlertData).
			
%% @spec get_all()->({error,Reason} | Alerts)
%% where
%%	Reason = atom()
%%	Alerts = [AlertData]
%% @doc get all alerts
%% <br></br>
%% <dl><dt>AlertData</dt> <dd>See {@link create/1}</dd></dl>
%% @end
get_all()->
	dbcs_rule:get_all().

%% @spec get(Id)->({error,Reason} | Alert)
%% where
%%	Id = atom()
%%	Reason = atom()
%%	Alert = [{key,value}]
%% @doc get a alert
%% <br></br>
%% <dl>
%%	<dt>Alert</dt><dd>See AlertData in {@link create/1}</dd>
%% </dl>
get(Id)when is_atom(Id)->
	dbcs_rule:get_rule(Id);
get(_)->
	{error,parameter_error}.

%% @spec delete(Id)->({error,Reason} | {ok,Msg})
%% where
%%	Id = atom()
%% @doc delete a alert
%%
delete(Id) when is_atom(Id)->
	case dbcs_rule:remove_rule(Id) of
		{ok,_}->
			siteview:remove_object(Id),
			
			% remove index
			index_store:remove(rule,Id),
			
			{ok,deleted};
		{error,Err}->
			{error,Err}
	end;
delete(_)->{error,parameter_error}.

%% @spec update(AlertData)->({error,Reason} | {ok,Msg})
%% where
%%	AlertData = [{key,value}]
%% @doc update a new alert
%% <br></br>
%% <dl><dt>AlertData</dt><dd>See {@link create/1}</dd></dl>
%%
update(AlertData) when is_list(AlertData)->
	case check_data(AlertData) of
		true->
			{error,data_error};
		_->
			case dbcs_rule:update_rule(AlertData) of
				{ok,NewData}->
					% io:format("alert:~p~n",[NewData]),
					Id = proplists:get_value(id,NewData),
					case siteview:get_object(Id) of
						[]->
							{error,update_instant_error};
						[Al]->
							Al:init(Al,NewData),
							
							% create index
							%index_store:update(rule, NewData),
					
							{ok,update_ok};
						_->
							{error,update_instant_error}
					end;
				_->
					{error,update_fail}
			end
	end;
update(_)->{error,parameter_error}.

get_template(Key) when is_atom(Key)->
	M = Key:new(null,null),
	Ret = M:get_template_property(),
	% M:delete(),
	Ret;
get_template(_)->{error,parameter_error}.
	
	
%% @spec get_scalar_property(Key,Prop,Parms)-> [{Id,Name}]
%% where
%%	Key = atom()
%%	Prop = atom()
%%	Params = [{Key1,Value}]
%%	Key1 = atom()
%%	Value = term()
%%	Id = string()
%%	Name = string()
%% @doc get options of scalar property.
%% <br>when property's type is scalar, this function will be called
%% the function will return a list to display in a dropdown box</br>
get_scalar_property(Key,Prop,Parms)when is_atom(Key) andalso is_atom(Prop) andalso is_list(Parms)->
	M = Key:new(undefined,undefined),
	Ret = M:getScalarValues(Prop,Parms),
	% M:delete(),
	Ret;
get_scalar_property(_,_,_)->{error,parameter_error}.

%% @spec get_log(Date)->{ok,Log} | {error,Reason}
%% @doc get alert log by date
%%
get_log(Date) when is_tuple(Date) andalso size(Date)==3->
	{Y,M,D} = Date,
	DateStr = case date() of
			Date ->
				"";
			_->
				lists:flatten(lists:flatten(io_lib:format("~w-~w-~w",[Y,M,D])))
		end,
	case alert_logger:q(DateStr) of
		{error,Reason}->
			{error,Reason};
		{_,Ret}->
			{ok,Ret}
	end;
get_log(Date) when is_list(Date)->
	[Y,M,D] = [list_to_integer(X)||X<-string:tokens(Date,"-")],
	get_log({Y,M,D});
get_log(_)->{error,parameter_error}.

%% @spec get_log(Date,Id)->{ok,Log} | {error,Reason}
%% @doc get alert log by date and id
%%
get_log(Date,Id)when is_tuple(Date) andalso is_atom(Id) andalso size(Date)==3->
	{Y,M,D} = Date,
	DateStr = case date() of
			Date ->
				"";
			_->
				lists:flatten(io_lib:format("~w-~w-~w",[Y,M,D]))
		end,
	case alert_logger:q(DateStr,Id) of
		{error,Reason}->
			{error,Reason};
		{_,Ret}->
			{ok,Ret}
	end;
get_log(Date,Id)when is_list(Date) andalso is_atom(Id)->
	[Y,M,D] = [list_to_integer(X)||X<-string:tokens(Date,"-")],
	get_log({Y,M,D},Id);
get_log(_,_)->{error,parameter_error}.

%% @spec disable_all()->({ok,Result} | {error,Reason})
%% @doc disable all alerts
%% 
disable_all()->
	Rules = dbcs_rule:get_all(),
	F = fun(X)->
			Y = lists:keyreplace(enabled,1,X,{enabled,{false,permanently}}),
			case dbcs_rule:update_rule(Y) of
				{ok,_}->
					Id = proplists:get_value(id,X),
					case siteview:get_object(Id) of
						[]->
							pass;
						Rl->
							[Z:init(Z,Y)||Z<-Rl],
							{ok,""}
					end;
				Ret->
					Ret
			end
		end,
	lists:foreach(F,Rules),
	{ok,""}.

%% @spec enable_all()->({ok,Result} | {error,Reason})
%% @doc enable all alerts
%%
enable_all()->
	Rules = dbcs_rule:get_all(),
	F = fun(X)->
			Y = lists:keyreplace(enabled,1,X,{enabled,true}),
			dbcs_rule:update_rule(Y),
			Id = proplists:get_value(id,X),
			case siteview:get_object(Id) of
				[]->
					pass;
				Rl->
					[Z:init(Z,Y)||Z<-Rl]
			end
		end,
	lists:foreach(F,Rules),
	{ok,""}.	


%% @spec alert_test(Monitor,Id)->({ok,Result} | {error,Reason})
%% where
%%	
%% @doc test a alert with a monitor
%%
alert_test(Monitor,Id) when is_atom(Monitor) andalso is_atom(Id)->
	case siteview:get_object(Monitor) of
		[M|_]->
			case siteview:get_object(Id) of
				[Al|_]->
					Al:doAction(M);
				Err->
					{error,lists:flatten(io_lib:format("can not found alert instance,~p",[Err]))}
			end;
		Err2->
			{error,lists:flatten(io_lib:format("can not found monitor,~p",[Err2]))}
	end;
alert_test(_,_)->{error,parameter_error}.

%% @spec log_types()->[string()]
%%	
%% @doc return alert log's type
%%
log_types()->
	["Email alert sent","Sound alert sent","Snmp trap alert sent","database alert run",
	"Syslog alert sent","Post alert sent","SMS alert sent","Script alert run"].

%% @spec get_log(LogType,StartTime,EndTime)-> ({eof,Data} | {error,Reason})
%%	LogType = log_types()
%%	StartTime = {Year,Month,Day}
%%	EndTime = {Year,Month,Day}
%%	Data = [#alertlog{}]
%%	Reason = atom()
%%	
%% @doc get alert log by log type,Start time and end time
%% <br></br>
%% <dl>
%%	<dt>#alertlog{}</dt><dd>See <a href="alert.hrl">alert.hrl</a></dd>
%% </dl>
%%	
get_log(LogType,StartTime,EndTime) when is_tuple(StartTime) andalso is_tuple(EndTime)->
	alert_logger:q(StartTime,EndTime,LogType);
get_log(LogType,StartTime,EndTime) when is_list(StartTime) andalso is_list(EndTime)->
	[Y1,M1,D1] = [list_to_integer(X)||X<-string:tokens(StartTime,"-")],
	[Y2,M2,D2] = [list_to_integer(X)||X<-string:tokens(EndTime,"-")],
	alert_logger:q({Y1,M1,D1},{Y2,M2,D2},LogType);
get_log(_,_,_)->{error,parameter_error}.

%% @spec get_log(LogType,StartDate,StartTime,EndDate,EndTime)-> ({eof,Data} | {error,Reason})
%% where
%%	LogType = log_types()
%%	StartDate = {Year,Month,Day}
%% 	StartTime = {Hour,Minute,Seconds}
%%	EndData = {Year,Month,Day}
%% 	EndTime = {Hour,Minute,Seconds}
%%	Data = [#alertlog{}]
%%	Reason = atom()
%% @doc get alert log by log type,Start time and end time
%% <br></br>
%% <dl>
%%	<dt>#alertlog{}</dt><dd>See <a href="alert.hrl">alert.hrl</a></dd>
%% </dl>
%%	
get_log(LogType,StartDate,StartTime,EndDate,EndTime) when is_tuple(StartDate) andalso is_tuple(EndDate) andalso is_tuple(StartTime) andalso is_tuple(EndTime)->
	case get_log(LogType,StartDate,EndDate) of
		{eof,Data}->
			F=fun(X)->
				if
					X#alertlog.time > {StartDate,StartTime} andalso X#alertlog.time < {EndDate,EndTime} ->
						true;
					true->
						false
				end
			end,
			{eof,lists:filter(F,Data)};
		Else->
			Else
	end;
get_log(LogType,StartDate,StartTime,EndDate,EndTime) when is_list(StartDate) andalso is_list(EndDate) andalso is_list(StartTime) andalso is_list(EndTime)->
	case get_log(LogType,StartDate,EndDate) of
		{eof,Data}->
			[Y1,M1,D1] = [list_to_integer(X)||X<-string:tokens(StartDate,"-")],
			[Y2,M2,D2] = [list_to_integer(X)||X<-string:tokens(EndDate,"-")],
			[HH1,MM1,SS1] = [list_to_integer(X)||X<-string:tokens(StartTime,":")],
			[HH2,MM2,SS2] = [list_to_integer(X)||X<-string:tokens(EndTime,":")],
			F = fun(X)->
				if
					X#alertlog.time > {{Y1,M1,D1},{HH1,MM1,SS1}} andalso X#alertlog.time < {{Y2,M2,D2},{HH2,MM2,SS2}} ->
						true;
					true->
						false
				end
			end,
			{eof,lists:filter(F,Data)};
		Else->
			Else
	end;
get_log(_,_,_,_,_)->{error,parameter_error}.

%% @spec get_template_file_list(Key)-> [FileName]
%% where
%%	Key = (mailto | sms)
%%	FileName = string()
%% @doc get template files
get_template_file_list(Key)->
	M = Key:new(undefined,undefined),
	Ret = M:get_template_file_list(),
	% M:delete(),
	Ret.
	
%% @spec read_template_file(Key,Name)->( {ok,Content} | {error,Reason} )
%% where
%%	Key = (mailto | sms)
%%	Name = string()
%%	Content = string()
%%	Reason = atom()
%% @doc read content of template file
%%
%% <br>Typical error reasons:</br>
%% <dl>
%%	<dt>enoent</dt><dd>The file does not exist.</dd>
%%	<dt>eacces</dt><dd>Missing permission for reading the file, or for searching one of the parent directories.</dd>
%%	<dt>eisdir</dt><dd>The named file is a directory.</dd>
%%	<dt>enotdir</dt><dd>A component of the file name is not a directory. On some platforms, enoent is returned instead.</dd>
%%	<dt>enomem</dt><dd>There is not enough memory for the contents of the file.</dd>
%% </dl>
read_template_file(Key,Name)->
	M = Key:new(undefined,undefined),
	Ret = M:read_template_file(Name),
	% M:delete(),
	Ret.	

%% @spec write_template_file(Key,Name,Data)->( {ok,Result} | {error,Reason} )
%% where
%%	Key = (mailto | sms)
%%	Name = string()
%%	Result = string()
%%	Reason = atom()
%% @doc write content to template file
%%
%% <br>Typical error reasons:</br>
%% <dl>
%%	<dt>enoent</dt><dd>A component of the file name does not exist.</dd>
%%	<dt>eacces</dt><dd>Missing permission for writing the file or searching one of the parent directories.</dd>
%%	<dt>eisdir</dt><dd>The named file is a directory.</dd>
%%	<dt>enotdir</dt><dd>A component of the file name is not a directory. On some platforms, enoent is returned instead.</dd>
%%	<dt>enospc</dt><dd>There is a no space left on the device.</dd>
%% </dl>
write_template_file(Key,Name,Data)->
	M = Key:new(undefined,undefined),
	Ret = M:write_template_file(Name,Data),
	% M:delete(),
	Ret.
	
%% @spec remove_template_file(Key,Name)->( {ok,Content} | {error,Reason} )
%% where
%%	Key = (mailto | sms)
%%	Name = string()
%%	Content = string()
%%	Reason = atom()
%% @doc remove template file
%%
%% <br>Typical error reasons:</br>
%% <dl>
%%	<dt>enoent</dt><dd>The file does not exist.</dd>
%%	<dt>eacces</dt><dd>Missing permission for reading the file, or for searching one of the parent directories.</dd>
%%	<dt>eisdir</dt><dd>The named file is a directory.</dd>
%%	<dt>enotdir</dt><dd>A component of the file name is not a directory. On some platforms, enoent is returned instead.</dd>
%% </dl>
remove_template_file(_, Name) when ((Name=="Recover") or (Name=="Default")) ->
    {error, action_forbidden};
remove_template_file(Key,Name)->
	M = Key:new(undefined,undefined),
	Ret = M:remove_template_file(Name),
	% M:delete(),
	Ret.

%% @spec query_log(StartDate,StartTime,EndDate,EndTime,Params)-> ({eof,Data} | {error,Reason})
%% where
%%	StartDate = {Year,Month,Day}
%% 	StartTime = {Hour,Minute,Seconds}
%%	EndData = {Year,Month,Day}
%% 	EndTime = {Hour,Minute,Seconds}
%%	Params = [{Field,Op,Val}]
%%	Field = id | name | receiver | type
%%	Op = '='
%%	Data = [#alertlog{}]
%%	Reason = atom()
%% @doc query alert log by Start time , end time and other conditions
%% <br></br>
%% <dl>
%%	<dt>#alertlog{}</dt><dd>See <a href="alert.hrl">alert.hrl</a></dd>
%% </dl>
%%	
query_log(StartDate,StartTime,EndDate,EndTime,Params)  when is_tuple(StartDate) andalso is_tuple(EndDate) andalso is_tuple(StartTime) andalso is_tuple(EndTime)->
	alert_logger:q(StartDate,StartTime,EndDate,EndTime,Params);
query_log(_,_,_,_,_)->{error,parameter_error}.

%% @spec query_log(StartDate,StartTime,EndDate,EndTime,Params,From,Count)-> ({eof,Data} | {Left,Data} | {error,Reason})
%% where
%%	StartDate = {Year,Month,Day}
%% 	StartTime = {Hour,Minute,Seconds}
%%	EndData = {Year,Month,Day}
%% 	EndTime = {Hour,Minute,Seconds}
%%	Params = [{Field,Op,Val}]
%%	Field = id | name | receiver | type
%%	Op = '='
%%	Data = [#alertlog{}]
%%	Reason = atom()
%%	From = integer()
%%	Count = integer()
%%	Left = integer()
%% @doc query alert log by Start time , end time and other conditions
%% <br>return data limit by From and Count. </br>
%% <dl>
%%	<dt>#alertlog{}</dt><dd>See <a href="alert.hrl">alert.hrl</a></dd>
%% </dl>
%%	
query_log(StartDate,StartTime,EndDate,EndTime,Params,From,Count) when is_integer(From) andalso is_integer(Count) andalso From > 0 andalso Count >0->
	alert_logger:q(StartDate,StartTime,EndDate,EndTime,Params,From,Count);
query_log(_,_,_,_,_,_,_)->{error,parameter_error}.	