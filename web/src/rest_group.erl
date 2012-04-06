%% ---
%% rest_group
%%
%%---
-module(rest_group).
-compile(export_all).

func_group_childs(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(parms_error);
		Parms->
			case lists:keysearch(gid,1,Parms) of
				{value,{gid,Id}}->
					Ret = api_group:childs(Id),
					web_common:respond("<response>" ++ make_group_childs_response(Ret) ++ "</response>");
				_->
					web_common:respond(parms_error)
			end
	end.

make_group_childs_response([])->"";
make_group_childs_response([C|T])->
	case lists:keysearch(class,1,C) of
		{value,{class,group}}->
			"<group>" ++list2xml:process_list(C) ++"</group>" ++  make_group_childs_response(T);
		{value,{class,device}}->
			"<device>" ++list2xml:process_list(C) ++"</device>" ++  make_group_childs_response(T);
		_->
			"<monitor>" ++list2xml:process_list(C) ++"</monitor>" ++  make_group_childs_response(T)
	end;
make_group_childs_response(_)->"".


func_group_get(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(parms_error);
		Parms->
			case lists:keysearch(gid,1,Parms) of
				{value,{gid,Id}}->
					Ret = api_group:info(Id),
					web_common:respond("<response><group>" ++ list2xml:process_list(Ret) ++ "</group></response>" );
				_->
					web_common:respond(parms_error)
			end
	end.


func_group_state(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(unknown);
		Parms->
			case lists:keysearch(gid,1,Parms) of
				{value,{gid,Id}}->
					Ret = api_group:get_run_info(Id),
					web_common:respond(list2xml:to_statexml(Ret));
				_->
					web_common:respond(parms_error)
			end
	end.

func_group_add(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(parms_error);
		Parms->
			if
				length(Parms) < 2 ->
					web_common:respond(parms_error);
				true ->
					case {lists:keysearch(parentid,1,Parms),lists:keysearch(gname,1,Parms)} of
						{{value,{parentid,Parent}},{value,{gname,Name}}}->
							Desc = case lists:keysearch(desc,1,Parms) of
										{value,{desc,Val}}->
											Val;
										_->
											''
									end,	
							Ret = api_group:create(Parent,[{name,Name},{desc,Desc},{class,group}]),
							web_common:respond(list2xml:to_result([Ret]));
						_->
							web_common:respond(parms_error)
					end
			end
			
	end.

func_group_delete(Host,_Req,Path,Raw_path) ->
	case web_common:get_params_from_body(_Req) of
		[]->
			web_common:respond(unknown);
		Parms->
			case lists:keysearch(gid,1,Parms) of
				{value,{gid,Id}}->
					Ret = api_group:delete(Id),
					web_common:respond(list2xml:to_result(Ret));
				_->
					web_common:respond(parms_error)
			end
	end.