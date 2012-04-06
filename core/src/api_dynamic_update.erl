%% 
%% @doc api of dynamic update
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
-module(api_dynamic_update).
-compile(export_all).

-export([get_all/0,get/1,update/1]).

get_all()->
	dbcs_dynamic_update:get_all().
	
get(Id) when is_atom(Id)->
	dbcs_dynamic_update:get_dynamic_update(Id);
get(_)->{error,parameter_error}.

update(Dyn)->
	dbcs_dynamic_update:update_dynamic_update(Dyn).
