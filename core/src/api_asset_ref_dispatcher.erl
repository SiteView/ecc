-module(api_asset_ref_dispatcher).
-compile(export_all).
-import(asset_node, [get_node/0]).

create(Ref) ->
    rpc:call(get_node(),api_asset_icon,create,[Ref]).

update(Ref) ->
    rpc:call(get_node(),api_asset_icon,update,[Ref]).

delete(Ref) ->
    rpc:call(get_node(),api_asset_icon,delete,[Ref]).

get_all() ->
    rpc:call(get_node(),api_asset_icon,get_all,[]).

get_ref(Id) ->
    rpc:call(get_node(),api_asset_icon,get_ref,[Id]).

get_ref_by_source(AssetId) ->
    rpc:call(get_node(),api_asset_icon,get_ref_by_source,[AssetId]).

get_target_by_ref(RefId, Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset_icon,get_target_by_ref,[RefId, Index, Count, Sort, SortType]).

