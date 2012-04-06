-module(api_asset_icon_dispatcher).
-compile(export_all).
-import(asset_node, [get_node/0]).

create(Icon) ->
    rpc:call(get_node(),api_asset_icon,create,[Icon]).

update(Icon) ->
    rpc:call(get_node(),api_asset_icon,update,[Icon]).

delete(Icon) ->
    rpc:call(get_node(),api_asset_icon,delete,[Icon]).

get_all() ->
    rpc:call(get_node(),api_asset_icon,get_all,[]).

get_all(Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset_icon,get_all,[Index, Count, Sort, SortType]).

import_all() ->
    rpc:call(get_node(),api_asset_icon,import_all,[]).

get_icon(Id) ->
    rpc:call(get_node(),api_asset_icon,get_icon,[Id]).

get_icon_by_alias(Alias) ->
    rpc:call(get_node(),api_asset_icon,get_icon_by_alias,[Alias]).