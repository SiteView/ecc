%% 
%% @doc api function of monitor template
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_monitor_template).
-extends(api_siteview).
-compile(export_all).
-include("monitor.hrl").

-export([get_templates/0,get_templates_beta/0,get_templates_release/0,get_template/1,get_template/2]).

-export([delete_template/1,get_template_name/1,get_scalar_property/3,get_classifier/2,get_monitor_counters/2]).

-export([get_template_state/2,get_template_state/3,get_servers/1,get_template_type/1]).

%% @spec get_templates()->[Temp]
%% where
%%	Temp = {Key,Name,Options}
%%	Key = atom()
%%	Name = string()
%%	Options =list()
%% @doc get list of monitor template
get_templates()->
	 case file:consult("conf/monitor_template.conf") of
		{ok,R}->
			F = fun(X,Y)->element(2,X)<element(2,Y) end,
			lists:sort(F,R);
		_->
			[]
	end.

%% @spec get_templates_beta()->[Temp]
%% where
%%	Temp = {Key,Name,Options}
%%	Key = atom()
%%	Name = string()
%%	Options =list()
%% @doc get list of beta monitor 
get_templates_beta()->
	 case file:consult("conf/monitor_template.conf") of
		{ok,R}->
			F = fun(X)->
				lists:member(beta,element(3,X))
			end,
			lists:filter(F,R);
		_->
			[]
	end.

%% @spec get_templates_release()->[Temp]
%% where
%%	Temp = {Key,Name,Options}
%%	Key = atom()
%%	Name = string()
%%	Options =list()
%% @doc get list of release monitor 
get_templates_release()->
	 case file:consult("conf/monitor_template.conf") of
		{ok,R}->
			F = fun(X)->
				not lists:member(beta,element(3,X) )
			end,
			lists:filter(F,R);
		_->
			[]
	end.

%% @spec get_template(Key)->([#property{}] | {error,Reason})
%% where
%%	Key = atom()
%%	Reason = atom()
%% @doc get detail information of monitor template by key
%%	<br>return list of #property{}</br>
%%	<br>#property{} is defined in <a href="monitor_template.hrl">monitor_template.hrl</a></br>
get_template(Key) when is_atom(Key)->
	M = Key:new(),
	try
	Ret = M:get_template_property(),
	Ret
	catch
		_:Err->
			{error,Err}
	after
		M:delete()
	end;
get_template(_)->{error,parameter_error}.

%% @spec get_template(Key,Params)->([#property{}] | {error,Reason})
%% where
%%	Key = atom()
%%	Params = [{Prop,Value}]
%%	Prop = atom()
%%	Value = term()
%%	Reason = atom()
%% @doc get detail information of monitor by key
%%	
%%	<br>return list of #property{}</br>
%%	<br>#property{} is defined in <a href="monitor_template.hrl">monitor_template.hrl</a></br>
get_template(Key,Params) when is_atom(Key) andalso is_list(Params) ->
	M = Key:new(),
	try
	M:set_property(?PAGE_PARAMS,Params),
	Ret = M:get_template_property(),
	Ret
	catch
		_:Err->
			{error,Err}
	after
		M:delete()
	end;
get_template(_,_)->{error,parameter_error}.
	
%% @spec delete_template(Key)-> ({ok,Result} | {error,Reason})
%% where
%%	Key = atom()
%%	Result = atom()
%%	Reason = atom()
%% @doc delete a template from template list
delete_template(Key) when is_atom(Key)->
	case file:consult("conf/monitor_template.conf") of
		{ok,R}->
			case lists:keymember(Key,1,R) of
				true->
					lists:keydelete(Key,1,R);
				false->
					{error,not_found_template}
			end;
		_->
			{error,template_file_error}
	end;
delete_template(_)->{error,parameter_error}.

%% @spec get_template_name(Key)-> ( string() |{error,Err})
%% where
%%	Key = atom()
%% @doc get a template name by key,if not found template,will return ""
%%
get_template_name(Key) when is_atom(Key)->
	MTs = get_templates(),
	case lists:keysearch(Key,1,MTs) of
		{value,M}->
			element(2,M);
		_->
			"NOT FOUND"
	end;
get_template_name(_)->{error,parameter_error}.

%% @spec get_template_type(Key)-> (Type |{error,Reason})
%% where
%%	Key = atom()
%%    Type = string()
%% @doc get a template type by key,if not found template,will return {error,"NOT FOUND"}
%%
get_template_type(Key) when is_atom(Key)->
	MTs = get_templates(),
	case lists:keysearch(Key,1,MTs) of
		{value,M}->
			Element = element(3,M),
            case lists:keysearch(type,1,Element) of
                {value,{type,Type}}->
                                Type;
				_->
					{error,"NOT FOUND"}
            end;
		_->
			{error,"NOT FOUND"}
	end;
get_template_type(_)->{error,"parameter_error"}.

%% @spec get_scalar_property(Key,Prop,Parms)-> [{Id,Name}]
%% where
%%	Key = atom()
%%	Prop = atom()
%%	Params = [{Key1,Value}]
%%	Key1 = atom()
%%	Value = term()
%%	Id = string()
%%	Name = string()
%% @doc get options of scalar property,when property's type is scalar this function will be called
%% the function will return a list which display in a dropdown box
get_scalar_property(Key,Prop,Parms) when is_atom(Key) andalso is_atom(Prop) andalso is_list(Parms)->
	M = Key:new(),
	try
		Ret = M:getScalarValues(Prop,Parms),
		%%io:format("get_scalar_property:~p,~p,~p~n",[Key,Prop,Ret]),
		M:delete(),
		Ret
	catch
		_:_->M:delete(),[]
	end;
get_scalar_property(_,_,_)->{error,parameter_error}.

%% @spec get_classifier(Key,Category)-> [Classifier]
%% where
%%	Key = atom()
%%	Category = (error | warning | good)
%%	Classifier = {property,Oper,Value}
%%	Oper = ('>' | '<' | '>=' | '<=' | '==' | '!=' | 'contains' | '!contains')
%% @doc get default threshold condition of monitor,Key is the monitor type
get_classifier(Key,Category) when is_atom(Key) and is_atom(Category)->
	M = Key:new(),
	io:format("............................................M.......>>~p~n",[M]),
	try
	Ret = M:get_classifier(Category),
	io:format("...................................................>>~p~n",[Ret]),
	Ret
	catch
		_:Err->
			{error,Err}
	after
		M:delete()
	end;
get_classifier(_,_)->{error,parameter_error}.

%% @spec get_monitor_counters(Key,Params)->({error,parameter_error} | [Counter])
%% where
%%	Key = atom()
%%  Counter = {string(),string()}
%% @doc get monitor's available counters
get_monitor_counters(Key,Params) when is_atom(Key) andalso is_list(Params)->
	M = Key:new(),
	try
		M:set_property(?PAGE_PARAMS,Params),
		Ret = M:getAvailableCounters(M),
		Ret
	catch
		_:Err->
			{error,Err}
	after
		M:delete()
	end;
get_monitor_counters(_,_)->{error,parameter_error}.

%% @spec get_template_state(Key,Params)->({error,parameter_error}|[#property{}])
%% where
%%	Key = atom()
%%	Params = [{key,term()}]
%% @doc get monitor's threshold property,,Params is the input from UI
get_template_state(Key,Params) when is_atom(Key)->
	M = Key:new(),
	try
		Ret = M:getStateProperties(M,Params),
		Ret
	catch
		_:Err->
			{error,Err}
	after
		M:delete()
	end;
get_template_state(_,_)->{error,parameter_error}.

%% @spec get_template_state(Id,Key,Params)->({error,parameter_error}|[#property{}])
%% where
%%	Key = atom()
%%	Id = atom()
%%	Params = [{key,term()}]
%% @doc get monitor's threshold property,,Params is the input from UI
%% Id is monitor'id
get_template_state(Id,Key,Params) when is_atom(Key) andalso is_atom(Id) ->
	case api_siteview:find_object(Id) of
		[] ->
			get_template_state(Key,Params);
		[M|_]->
			M:getStateProperties(M,Params)
	end;
get_template_state(_,_,_)->{error,parameter_error}.

%% @spec get_servers(Params)->[{Name,Addr}]
%% where
%%	Params = [{key,term()}]
%%	Name = string()
%%	Addr = string()
%% @doc get available servers of a monitor,Params is the input from UI
get_servers(Params) when is_list(Params)->
	% io:format("get_server:~p~n",[Params]),
	case proplists:get_value("_platform",Params) of
		undefined->
			case proplists:get_value("class",Params) of
				undefined->
					[];
				"log_monitor"->
					Mas = dbcs_machine:get_Unixmachine(),
					case platform:getOs() of
						1->
							[];
						_->
							[{"this server",""}]
					end ++
					[{X#machine.name,X#machine.host}||X<-Mas];
				Class->
					C = list_to_atom(Class),
					Obj = C:new(),
					case Obj:remoteCommandLineAllowed() of
						false->
							Obj:delete(),
							Ms = dbcs_machine:get_NTmachine(),
							case platform:getOs() of
								1->
									[{"this server",""}];
								_->
									[]
							end ++
							[{X#machine.name,X#machine.host}||X<-Ms];
						_->
							Obj:delete(),
							[{"this server",""}] ++
							[{X#machine.name,X#machine.host}|| X<-dbcs_machine:get_all()]
					end
			end;
		"windows"->
			Ms = dbcs_machine:get_NTmachine(),
			case platform:getOs() of
				1->
					[{"this server",""}];
				_->
					[]
			end ++
			[{X#machine.name,X#machine.host}||X<-Ms];
		"unix"->
			Mas = dbcs_machine:get_Unixmachine(),
			case platform:getOs() of
				1->
					[];
				_->
					[{"this server",""}]
			end ++
			[{X#machine.name,X#machine.host}||X<-Mas];
		_->
			[{"this server",""}] ++
			[{X#machine.name,X#machine.host}|| X<-dbcs_machine:get_all()]
	end;
get_servers(Params)->
	{error,parameter_error}.

%% @spec run_monitor_from_template(TempName,Values,Timeout)->({ok,Result} | {error,Reason})
%% where
%%	TempName = string()
%%	Values = [{Key,Value}]
%%	Key = atom()
%%	Value = term()
%%	Timeout = integer()
%%	Result = atom()
%%	Reason = atom()
%% @doc run monitor from template with input values
run_monitor_from_template(TempName,Values,Timout)->
	ok.

get_monitor_types()->
	Temps = get_templates(),
	F = fun(Key)->
		case lists:keysearch(Key,1,Temps) of
		{value,M}->
			element(2,M);
		_->
			"NOT FOUND"
		end
	end,
	
	case dbcs_monitor:get_all() of
		{error,_}->
			[];
		Monitors->
			lists:foldl(fun(X,R)->
				Class = proplists:get_value(class,X),
				case proplists:get_value(Class,R) of
					undefined->
						R ++ [{Class,F(Class)}];
					_->
						R
				end
			end,[],Monitors)
	end.
