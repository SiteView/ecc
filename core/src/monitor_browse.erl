-module(monitor_browse).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

browse(Type,Filter)->
	Monitors=
	case Type of
		'all_type'->
			siteview:get_object_by_type(monitor);
		_->
			siteview:get_object_by_class(Type)
	end,
	case proplists:get_value("tag",Filter) of
		undefined->
			filter_monitors(Monitors,Filter);
		Tag->
			case tag_store:q([{tagid,'=',Tag}]) of
				{ok,TagList}->
					FIds = lists:foldl(fun(X,R)->
						R ++ [element(2,X)]
						end,[],TagList),
					TagMonitors = lists:foldl(fun(X,RR)->
									case X:get_property(id) of
										{ok,{_,TempId}}->
											case lists:member(TempId,FIds) of
												true->
													RR ++ [X];
												_->
													RR
											end;
										_->
											RR
									end
								end,[],Monitors),
					filter_monitors(TagMonitors,proplists:delete("tag",Filter));
				_->	
					filter_monitors(Monitors,proplists:delete("tag",Filter))
			end
	end.
	
filter_monitors([],_)->[];
filter_monitors([M|T],F)->
	filter_monitor(M,F) ++ filter_monitors(T,F).
	
filter_monitor(M,[])->
	 [case M:get_parent() of
		{ok,{_,G}}->
			[{'parent_name',case G:get_property(?NAME) of {ok,{_,V}}->V;_->"" end}];
		_->
			[]
	end ++
	M:get_properties() ++ M:get_run_info()
	];
filter_monitor(M,[{K,V}|T])->
	case K of
		"filter_category"->
			case V of
				"show_error_or_warning"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?WARNING_CATEGORY}}->
							filter_monitor(M,T);
						{ok,{_,?ERROR_CATEGORY}}->
							filter_monitor(M,T);
						_->
							[]
					end;
				"show_error"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?ERROR_CATEGORY}}->
							filter_monitor(M,T);
						_->
							[]
					end;
				"show_warning"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?WARNING_CATEGORY}}->
							filter_monitor(M,T);
						_->
							[]
					end;
				"show_ok"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?GOOD_CATEGORY}}->
							filter_monitor(M,T);
						_->
							[]
					end;
				"show_nodata"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?NO_DATA}}->
							filter_monitor(M,T);
						_->
							[]
					end;
				"show_disabled"->
					case M:get_property(?DISABLED) of
						{ok,{_,true}}->
							filter_monitor(M,T);
						_->
							[]
					end;
				"hide_error_or_warning"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?WARNING_CATEGORY}}->
							[];
						{ok,{_,?ERROR_CATEGORY}}->
							[];
						_->
							filter_monitor(M,T)
					end;
				"hide_error"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?ERROR_CATEGORY}}->
							[];
						_->
							filter_monitor(M,T)
					end;
				"hide_warning"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?WARNING_CATEGORY}}->
							[];
						_->
							filter_monitor(M,T)
					end;
				"hide_ok"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?GOOD_CATEGORY}}->
							[];
						_->
							filter_monitor(M,T)
					end;
				"hide_nodata"->
					case M:get_attribute(?CATEGORY) of
						{ok,{_,?NO_DATA}}->
							[];
						_->
							filter_monitor(M,T)
					end;
				"hide_disabled"->
					case M:get_property(?DISABLED) of
						{ok,{_,true}}->
							[];
						_->
							filter_monitor(M,T)
					end;
				_->
					filter_monitor(M,T)
			end;
		"match_status" when V=/=""->
			case M:get_attribute(?STATE_STRING) of
				{ok,{_,Status}}->
					case re:run(Status,V) of
						{match,_}->
							filter_monitor(M,T);
						_->
							[]
					end;
				_->
					[]
			end;
		"match_name" when V=/=""->
			case M:get_property(?NAME) of
				{ok,{_,Name}}->
					case re:run(Name,V) of
						{match,_}->
							filter_monitor(M,T);
						_->
							[]
					end;
				_->
					[]
			end;
		"match_machine" when V=/=""->
			case re:run(M:getHostname(),V) of
				{match,_}->
					filter_monitor(M,T);
				_->
					[]
			end;
        "match_id" when V=/=""->
			case M:get_property(?ID) of
				{ok,{_,ID}}->
					case re:run(atom_to_list(ID),V) of
						{match,_}->
							filter_monitor(M,T);
						_->
							[]
					end;
				_->
					[]
			end;
      "match_group_name" when V=/=""->
       case M:get_property(?ID) of
        {ok,{_,ID}}->
            case api_siteview:get_parent_name(ID) of
                []  ->    [];
				Parent_name->
                   New_Parent_name =  case erlang:is_atom(Parent_name) of 
                                                                true-> atom_to_list(Parent_name);
                                                                _->Parent_name
                                                            end,
					case re:run(New_Parent_name,V) of
						{match,_}->
							filter_monitor(M,T);
						_->
							[]
					end
               end;	
        _ ->
                []
	   end;
    "match_group_Id" when V=/=""->
       case M:get_property(?ID) of
        {ok,{_,ID}}->
            case api_siteview:get_parent_id(ID) of
                []  ->    [];
				Parent_ID->
                   New_Parent_ID =  case erlang:is_atom(Parent_ID) of 
                                                                true-> atom_to_list(Parent_ID);
                                                                _->Parent_ID
                                                            end,
					case re:run(New_Parent_ID,V) of
						{match,_}->
							filter_monitor(M,T);
						_->
							[]
					end
               end;	
        _ ->
                []
	end;
	_->
			filter_monitor(M,T)
	end.