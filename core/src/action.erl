%%
%% @doc base class of action
%% @author shixianfang<xianfang.shi@dragonflow.com>
%% @copyright 2009 dragonflow.com
%% @version {1.0}
%%
-module(action,[Monitor,Rule]).
% -extends(siteview_object).
-compile(export_all).

-export([trigger/1,execute/0,createMessage/2,process_msg/2,logAlert/4,check_enabled/1,delete/0]).

-include("alert.hrl").
-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
	{action,undefined,undefined}.
	
new(Monitor,Rule)->
	{action,Monitor,Rule}.

	
%% @spec trigger(This)->({ok,Result}|{error,Reason})
%% where
%%	This = term()
%%	Result = string()
%%	Reason = string()
%% @doc trigger a action
%%
trigger(This)->
	case This:execute() of
		{ok,Result}->
			{ok,Result};
		{error,Reason}->
			{error,Reason};
		Err->
			{error,Err}
	end.


%% create_action_object(S)->
%%	THIS:create_action(S,false).

%% set_kill(Flag)->
%%	THIS:set_attribute(kill,Flag).


%% @spec execute()-> {ok,string()}
%% @doc base function 
%%
execute()->{ok,""}.

get_action_id()->undefined.

%%get_action_description()->"".

%%defaultsAreSet()->true.

%%showOptionalProperties()->false.

get_rule()->{ok,{rule,Rule}}.

get_monitor()->{ok,{monitor,Monitor}}.



%% @spec createMessage(Dir,Template)->string()
%% where
%%	Dir = string()
%%	Template = string()
%% @doc create message from template file,Dir is the directory contain template file,Tempalte is the file name of template
%%
createMessage(Dir,Template)->
	Path = 
		case Template of
			[] -> Dir ++ "/Default";
			_ -> Dir ++ "/" ++ Template
		end,
	{ok,{_,Monitor}} = THIS:get_monitor(),
	case file:read_file(Path) of
		{error,enoent}->
			"The file does not exist:" ++ Path;
		{error,eacces}->
			"Missing permission for reading the file, or for searching one of the parent directories:" ++ Path;
		{error,eisdir}->
			"A component of the file name is not a directory. On some platforms, enoent is returned instead.";
		{error,enomem}->
			"There is not enough memory for the contents of the file.";
		{ok,Bin}->
			Tmp = binary_to_list(Bin),
			THIS:process_msg(Tmp,Monitor)
	end.

alert_level(4) -> "critical";
alert_level(3) -> "severity";
alert_level(2) -> "important";
alert_level(1) -> "general";
alert_level(0) -> "notice";
alert_level(_) -> "notice".

%% @spec process_msg(Tmp,Monitor)->string()
%% where
%%	Temp = string()
%%	Monitor = term()
%% @doc process the template content read from file ,Tmp is data read from template file,Monitor is the alert target.
%%
process_msg([],_)->"";
process_msg("<name>" ++ T,Monitor)->
	{ok,{_,Name}} = Monitor:get_property(name),
	Name ++ process_msg(T,Monitor);
process_msg("<alert_level>" ++ T,Monitor)->
	case Rule:get_property(?ALERT_LEVEL) of	     
	      {ok,{_,Alert_level}} -> alert_level(Alert_level) ++ process_msg(T,Monitor);
	      undefined -> process_msg(T,Monitor)
	end;
process_msg("<alert_name>" ++ T,Monitor)->
	case Rule:get_property(name) of	      
	      {ok,{_,Alert_name}} -> Alert_name ++ process_msg(T,Monitor);
	      undefined -> process_msg(T,Monitor)
	end;	
process_msg("<alert_name>" ++ T,Monitor)->
	{ok,{_,Name}} = Monitor:get_property(name),
	Name ++ process_msg(T,Monitor);	
	
process_msg("<state>" ++ T,Monitor)->
	{ok,{_,State}} = Monitor:get_attribute(?STATE_STRING),
	util:br2n(State) ++ process_msg(T,Monitor);
process_msg("<sample>" ++ T,Monitor)->
	{ok,{_,Val}} = Monitor:get_attribute(?SAMPLE),
	integer_to_list(Val) ++ process_msg(T,Monitor);
process_msg("<groupdescription>" ++ T,Monitor)->
	case Monitor:get_parent() of
		{ok,{_,P}}->
			{ok,{_,N}}=P:get_property(?DESCRIPTION),
			N ++ process_msg(T,Monitor);
		_->
			process_msg(T,Monitor)
	end;
process_msg("<fullgroupid>" ++ T,Monitor)->
	case Monitor:get_parent() of
		{ok,{_,P}}->
			P:get_full_name() ++ process_msg(T,Monitor);
		_->
			process_msg(T,Monitor)
	end;
process_msg("<group>" ++ T,Monitor)->
	case Monitor:get_parent() of
		{ok,{_,P}}->
			{ok,{_,N}}=P:get_property(?NAME),
			N ++ process_msg(T,Monitor);
		_->
			process_msg(T,Monitor)
	end;
process_msg("<groupID>" ++ T,Monitor)->
	case Monitor:get_parent() of
		{ok,{_,P}}->
			{ok,{_,N}}=P:get_property(?NAME),
			N ++ process_msg(T,Monitor);
		_->
			process_msg(T,Monitor)
	end;
process_msg("<siteviewurl>" ++ T,Monitor)->
	siteview:get_serverurl() ++ process_msg(T,Monitor);
	%%"http://localhost" ++ process_msg(T,Monitor);
process_msg("<SiteViewURL>" ++ T,Monitor)->
	siteview:get_serverurl() ++ process_msg(T,Monitor);
	%%"http://localhost" ++ process_msg(T,Monitor);
process_msg("<currentTime>" ++ T,Monitor)->
	sv_datetime:now2str(sv_datetime:now()) ++ process_msg(T,Monitor);
process_msg("<time>" ++ T,Monitor)->
	Time = case Monitor:get_attribute(?LAST_UPDATE) of
			{ok,{_,0}}->
				"";
			{ok,{_,LastUpdate}}->
				sv_datetime:now2str(LastUpdate);
			_->
				""
		end,
	Time ++ process_msg(T,Monitor);
process_msg("<mainParameters>" ++ T,Monitor)->
	process_msg(T,Monitor);
process_msg("<mainStateProperties>" ++ T,Monitor)->
	get_state_properties(Monitor) ++ process_msg(T,Monitor);
process_msg("<secondaryStateProperties>" ++ T,Monitor)->
	process_msg(T,Monitor);
process_msg("<errorOnly>" ++ T,Monitor)->
	process_msg(T,Monitor);
process_msg("<secondaryParameters>" ++ T,Monitor)->
	process_msg(T,Monitor);
process_msg("<all>" ++ T,Monitor)->
	process_msg(T,Monitor);
process_msg("<remoteMachineName>" ++ T,Monitor)->
	process_msg(T,Monitor);
process_msg([C|T],Monitor)->
	[C] ++ process_msg(T,Monitor).

%% @spec get_state_properties(Monitor)->lists()
%% where
%%	Monitor = term()
%% @doc get state properties from monitor
%%
get_state_properties(Monitor)->
	Props = Monitor:getStateProperties(Monitor,""),
	get_state_properties(Props,Monitor).

get_state_properties([],_)->"";
get_state_properties([N|T],Monitor)->
	V = case Monitor:get_attribute(N#property.name) of
		{ok,{_,Tmp}}->
			Tmp;
		_->
			undefined
		end,
	lists:flatten(io_lib:format("~p:~p~n",[N#property.name,V])) ++ get_state_properties(T,Monitor).

%% @spec logAlert(Type,Title,Content,Result) ->(ok|error)
%% where
%%	Type = string()
%%	Title = string()
%%	Content = string()
%%	Result = string()
%% @doc record a alert  to log file,Type is alert type,Title is alert title,Content is the alert content,Result is "ok" or "fail"
logAlert(Type,Title,Content,Result)->
	{ok,{_,Monitor}} = THIS:get_monitor(),
	{ok,{_,MId}} = Monitor:get_property(id),
	{ok,{_,Rule}} = THIS:get_rule(),
	{ok,{_,Id}} = Rule:get_property(id),
	{ok,{_,Name}} = Rule:get_property(name),
	{ok,{_,Alert_level}} = Rule:get_property(?ALERT_LEVEL),
	NewResult = alert_level(Alert_level),
	{ok,{_,ParentId}} = Monitor:get_property(?PARENT),
	{ok,{_,Val}} = Monitor:get_attribute(?SAMPLE),
	
	OneAlert= #alertlog{id=Id,type=Type,name=Name,monitor=MId,title=Title,time=erlang:localtime(),content=Content,result=Result,alert_level=NewResult,
	groupid=ParentId,times=Val},
	%~ OneAlert= #alertlog{id=Id,type=Type,name=Name,monitor=MId,title=Title,time=erlang:localtime(),content=Content,result=Result},
	action_logAlertExtPoint_ErlangExtension(OneAlert),
	alert_logger:log(OneAlert).

%% @spec logAlert(Type,Title,Content,Result) ->(ok|error)
%% where
%%	Type = string()
%%	Title = string()
%%	Receiver = string()
%%	Content = string()
%%	Result = string()
%% @doc record a alert  to log file,Type is alert type,Title is alert title,Content is the alert content,Result is "ok" or "fail"
logAlert(Type,Receiver,Title,Content,Result)->
	{ok,{_,Monitor}} = THIS:get_monitor(),
	{ok,{_,MId}} = Monitor:get_property(id),
	{ok,{_,Rule}} = THIS:get_rule(),
	{ok,{_,Id}} = Rule:get_property(id),
	{ok,{_,Name}} = Rule:get_property(name),
	{ok,{_,Alert_level}} = Rule:get_property(?ALERT_LEVEL),
	NewResult = alert_level(Alert_level),
	{ok,{_,ParentId}} = Monitor:get_property(?PARENT),
	{ok,{_,Val}} = Monitor:get_attribute(?SAMPLE),
	
	OneAlert= #alertlog{id=Id,type=Type,name=Name,monitor=MId,receiver=Receiver,title=Title,time=erlang:localtime(),content=Content,result=Result,alert_level=NewResult,
	groupid=ParentId,times=Val},
	%~ OneAlert= #alertlog{id=Id,type=Type,name=Name,monitor=MId,receiver=Receiver,title=Title,time=erlang:localtime(),content=Content,result=Result},
	action_logAlertExtPoint_ErlangExtension(OneAlert),
	%~ io:format("OneAlert~p~n",[OneAlert]),
	alert_logger:log(OneAlert).


%% @spec check_enabled(Enabled)->(true | false)
%% where
%%	Enabled = (true | {false,permanently} | {false,{next,From,N,Unit}} |  {false,{schedule,Ft,Fd,Tt,Td}} )
%%	From = string()
%%	N = integer()
%%	Unit = string()
%%	Ft = string()
%%	Fd = string()
%%	Tt = string()
%%	Td = string()
%% @doc check a alert is enable or disabled,From is a time when this action is set,format is {{Y,M,D},{HH,MM,SS}},N is a counter for Unit,
%% when N is 1 and Unit is "minutes",it means 1 minute.Unit can be "minutes","Ft and Tt is time string as "12:30:59",Fd and Td is date string as "2009-11-12".
%%
check_enabled(Enabled)->
	case Enabled of
		true ->
			true;
		{false,permanently}->
			false;
		{false,{next,From,C,"minutes"}}->
			F = sv_datetime:time(From) + C*60*1000,
			Now = sv_datetime:now(),
			if
				Now > F ->
					true;
				true ->
					false
			end;
		{false,{next,From,C,"hours"}}->
			F = sv_datetime:time(From) + C*60*60*1000,
			Now = sv_datetime:now(),
			if
				Now > F ->
					true;
				true ->
					false
			end;
		{false,{next,From,C,"days"}}->
			F = sv_datetime:time(From) + C*24*60*60*1000,
			Now = sv_datetime:now(),
			if
				Now > F ->
					true;
				true ->
					false
			end;
		{false,{schedule,Ft,Fd,Tt,Td}}->
			io:format("=================================================wwwwwwwwww~n"),
			[HHF,MMF|_] = [list_to_integer(X)|| X<-string:tokens(Ft,":")],
			[YF,MF,DF|_] = [list_to_integer(X)|| X<-string:tokens(Fd,"/-")],
			[HHT,MMT|_] = [list_to_integer(X)|| X<-string:tokens(Tt,":")],
			[YT,MT,DT|_] = [list_to_integer(X)|| X<-string:tokens(Td,"/-")],
			From = sv_datetime:time({{YF,MF,DF},{HHF,MMF,0}}),
			To = sv_datetime:time({{YT,MT,DT},{HHT,MMT,0}}),
			Now = sv_datetime:now(),
			if
				Now < From orelse Now > To ->
					true;
				true ->
					false
			end;
		Any->
			io:format("=================================================4444444~p~n",[Any]),
			true
	end.

on_timeout()->
	ok.
	
delete()->ok.


%%Alarm extension points, you can write your own plug-ins for expanded details, please read \ plugin and \ plugin_node under the code and description
action_logAlertExtPoint_ErlangExtension(OneAlert)->
	extension:call_plugins(?MODULE, logAlertExtPoint, OneAlert).



  

