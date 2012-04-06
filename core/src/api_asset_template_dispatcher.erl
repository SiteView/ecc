-module(api_asset_template_dispatcher).
-compile(export_all).
-import(asset_node, [get_node/0]).

create(Template) ->
    Node = get_node(),
    io:format("node :~p~n", [Node]),
    rpc:call(get_node(),api_asset_template,create,[Template]).

update(Template) ->
    rpc:call(get_node(),api_asset_template,update,[Template]).

delete(Id) ->
    rpc:call(get_node(),api_asset_template,delete,[Id]).

get_template(Id) ->
    rpc:call(get_node(),api_asset_template,get_template,[Id]).

get_template_by_alias(Alias) ->
    rpc:call(get_node(),api_asset_template,get_template_by_alias,[Alias]).

get_asset_template(Id) ->
    rpc:call(get_node(),api_asset_template,get_asset_template,[Id]).

get_child(Id) ->
    rpc:call(get_node(),api_asset_template,get_child,[Id]).

get_offspring(Id) ->
    rpc:call(get_node(),api_asset_template,get_offspring,[Id]).

get_parent(Id) ->
    rpc:call(get_node(),api_asset_template,get_parent,[Id]).

get_template_by_asset(Id) ->
    rpc:call(get_node(),api_asset_template,get_template_by_asset,[Id]).

get_all_templates(Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset_template,get_all_templates,[Index, Count, Sort, SortType]).

get_root_templates(Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset_template,get_root_templates,[Index, Count, Sort, SortType]).

import_template(Alias) ->
    rpc:call(get_node(),api_asset_template,import_template,[Alias]).

import_all() ->
    rpc:call(get_node(),api_asset_template,import_all,[]).

refresh_default_template() ->
    rpc:call(get_node(),api_asset_template,refresh_default_template,[]).

get_default_template_info(Alias) ->
    rpc:call(get_node(),api_asset_template,get_default_template_info,[Alias]).

get_imported_template() ->
    rpc:call(get_node(),api_asset_template,get_imported_template, []).

get_default_template() ->
    rpc:call(get_node(),api_asset_template,get_default_template,[]).
    
get_all_children(Id) ->
    rpc:call(get_node(),asset_template,get_all_children,[Id]).

