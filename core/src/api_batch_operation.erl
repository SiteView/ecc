%% 
%% @doc api of batch operation
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(api_batch_operation).
-extends(api_siteview).

-include("monitor.hrl").

-export([delete/1,enable/1,disable/1,refresh/1]).

%% @spec delete(Ids)->({false,Reason} | {true,[]} )
%% where
%%	Ids = [Id]
%%	Id = atom()
%% 	Reason = [{Id,Err}]
%%	Err = not_found | delete_child_fail
%% @doc delete groups and monitors
delete(Ids) when is_list(Ids)->
	Ret = 
	lists:foldl(fun(X,R)->
		case api_siteview:find_object(X) of
			[]->
				R ++ [{X,not_found}];
			[M]->
				case M:get_property(?CLASS) of
					{ok,{_,group}}->
						case delete_group(X) of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{X,Err}]
						end;
					_->
						case api_monitor:delete(X) of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{X,Err}]
						end
				end
		end
	end, [], Ids),
	if
		length(Ret)>0->
			{false,Ret};
		true ->
			{true, Ret}
	end;
	
delete(_)->{error,parameter_error}.

delete_group(Id)->
	Childs = api_group:childs(Id),
	if
		length(Childs)>0->
			Ret = 
			lists:foldl(fun(X,R)->
				XId = proplists:get_value(id,X),
				case proplists:get_value(?CLASS,X) of
					group->
						case delete_group(XId) of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{XId,Err}]
						end;
					_->
						case api_monitor:delete(XId) of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{XId,Err}]
						end
				end
			end,[],Childs),
			if
				length(Ret)>0->
					{error,delete_child_fail};
				true->
					api_group:delete(Id)
			end;
		true->
			api_group:delete(Id)
	end.
	
%% @spec enable(Ids)->{false,Reason} | {true,[]}
%% where
%%	Ids = [Id]
%%	Id = atom()
%%	Reason = [{Id,Err}]
%%	Err = not_found | save_monitor_error | save_group_error
%% @doc enable groups and monitors
%%	
enable(Ids) when is_list(Ids)->
	Ret = 
	lists:foldl(fun(X,R)->
		case api_siteview:find_object(X) of
			[]->
				R ++ [{X,not_found}];
			[M]->
				case M:get_property(?CLASS) of
					{ok,{_,group}}->
						case api_group:enable_monitors(X) of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{X,Err}]
						end;
					_->
						case api_monitor:enable(X) of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{X,Err}]
						end
				end
		end
	end, [], Ids),
	if
		length(Ret)>0->
			{false,Ret};
		true->
			{true,Ret}
	end;
enable(_)->{error,parameter_error}.
	
%% @spec disable(Ids)->({false,Reason} | {true,[]})
%% where
%%	Ids = [Id]
%%	Id = atom()
%% 	Reason = [{Id,Err}]
%%	Err = not_found | save_monitor_error | save_group_error
%% @doc disable groups and monitors
disable(Ids) when is_list(Ids)->
	Ret = 
	lists:foldl(fun(X,R)->
		case api_siteview:find_object(X) of
			[]->
				R ++ [{X,not_found}];
			[M]->
				case M:get_property(?CLASS) of
					{ok,{_,group}}->
						case api_group:disable_monitors(X,"") of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{X,Err}]
						end;
					_->
						case api_monitor:disable(X,"") of
							{ok,_}->
								R;
							{error,Err}->
								R ++ [{X,Err}]
						end
				end
		end
	end, [], Ids),
	if
		length(Ret)>0->
			{false,Ret};
		true->
			{true,Ret}
	end;
disable(_)->{error,parameter_error}.

%% @spec refresh(Ids)->({false,ErrList} | {true,[]})
%% where
%%	Ids = [Id]
%%	Id = atom()
%% 	ErrList = [Id]
%% @doc refresh groups and monitors
refresh(Ids) when is_list(Ids)->
	Ret = 
	lists:foldl(fun(X,R)->
		case api_siteview:find_object(X) of
			[]->
				R ++ [{X,not_found}];
			[M]->
				case M:get_property(?CLASS) of
					{ok,{_,group}}->
						case api_group:refresh(X,true) of
							true->
								R;
							_->
								R ++ [X]
						end;
					_->
						case api_monitor:refresh(X) of
							true->
								R;
							_->
								R ++ [X]
						end
				end
		end
	end,[],Ids),
	if
		length(Ret)>0->
			{false,Ret};
		true ->
			{true,Ret}
	end;
refresh(_)->{error,parameter_error}.