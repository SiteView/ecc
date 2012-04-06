%% ---
%%monitor rest interface
%%
%%---
-module(rest_monitor).
%%-export([func_monitor_get/4,func_monitor_add/4,func_monitor_state/4,func_monitor_delete/4,func_monitor_update/4,func_monitor_copy/4]).

%%-export([func_monitortemplate_list/4,func_monitortemplate_get/4]).

-compile(export_all).

func_monitor_get(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(unknown);
		Parms->
			io:format("~p~n",[Parms]),
			case lists:keysearch(mid,1,Parms) of
				{value,{mid,Id}}->
					Result=api_monitor:info(Id),
					web_common:respond("<response><monitor>" ++ list2xml:process_list(Result) ++ "</monitor></response>");
				_->
					web_common:respond(unknown)
			end
	end.

func_monitor_add(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(params_error);
		Parms->
			if
				length(Parms) < 2 ->
					web_common:respond(params_error);
				true ->
					func_monitor_add(Host,_Req,Path,Raw_path,lists:keysearch(parentid,1,Parms),lists:keysearch(monitor,1,Parms))
			end
			
	end.
func_monitor_add(Host,_Req,Path,Raw_path,{value,{parentid,ParentId}},{value,{monitor,Data}})->
	Ret = api_monitor:create(ParentId,Data),
	web_common:respond(list2xml:to_result([Ret]));
func_monitor_add(Host,_Req,Path,Raw_path,_,_)->web_common:respond(params_error).


func_monitor_state(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(params_error);
		Parms->
			case lists:keysearch(mid,1,Parms) of
				{value,{mid,Id}}->
					Ret = api_monitor:get_run_info(Id),
					web_common:respond(list2xml:to_statexml(Ret));
				_->
					web_common:respond(params_error)
			end
	end.


func_monitor_delete(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(params_error);
		Parms->
			case lists:keysearch(mid,1,Parms) of
				{value,{mid,Id}}->
					Ret = api_monitor:delete(Id),
					web_common:respond(list2xml:to_result([Ret]));
				_->
					web_common:respond(params_error)
			end
	end.


func_monitor_update(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(unknown);
		Parms->
			case lists:keysearch(monitor,1,Parms) of
				{value,{monitor,Data}}->
					case xml2list:to_objlist(Data) of
						{error,_}->
							web_common:respond(params_error);
						 List->
							Ret = api_monitor:update(List),
							web_common:respond(list2xml:to_result([Ret]))
					end;
				_->
					web_common:respond(params_error)
			end
	end.

func_monitor_copy(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(params_error);
		Parms->
			if
				length(Parms) < 2 ->
					web_common:respond(params_error);
				true ->
					case {lists:keysearch(parentid,1,Parms),lists:keysearch(mid,1,Parms)} of
						{{value,{parentid,Parent}},{value,{mid,Monitor}}}->
							Ret = api_monitor:copy(Monitor,Parent),
							web_common:respond(list2xml:to_result([Ret]));
						_->
							web_common:respond(params_error)
					end
			end
			
	end.


func_monitortemplate_list(Host,_Req,Path,Raw_path) ->
	Ret = api_monitor_template:get_templates(),
	web_common:respond(list2xml:to_mtlist(Ret)).

func_monitortemplate_get(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(params_error);
		Parms->
			case lists:keysearch(key,1,Parms) of
				{value,{key,Key}}->
					Ret = api_monitor_template:get_template(Key),
					web_common:respond(list2xml:to_mt(Ret));
				_->
					web_common:respond(params_error)
			end
	end.