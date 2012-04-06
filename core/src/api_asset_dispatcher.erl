-module(api_asset_dispatcher).
-compile(export_all).
-import(asset_node, [get_node/0]).

create(Asset) ->
    rpc:call(get_node(),api_asset,create,[Asset]).
    
update(Asset) ->
    rpc:call(get_node(),api_asset,update,[Asset]).
    
delete(Id) ->
    rpc:call(get_node(),api_asset,delete,[Id]).

remove_assets(Ids) ->
    rpc:call(get_node(),api_asset,remove_assets,[Ids]).

delete_asset_by_template(TempAlias) ->
    rpc:call(get_node(),api_asset,delete_asset_by_template,[TempAlias]).

get_asset(Id) ->
    rpc:call(get_node(),api_asset,get_asset,[Id]).

get_assets(Ids, Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset,get_assets,[Ids, Index, Count, Sort, SortType]).

get_all_assets() ->
    rpc:call(get_node(),api_asset,get_all_assets,[]).

get_all(Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset,get_all,[Index, Count, Sort, SortType]).

get_all_ids() ->
    rpc:call(get_node(),api_asset,get_all_ids,[]).

get_asset_by_template(TempAlias, Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset,get_asset_by_template,[TempAlias, Index, Count, Sort, SortType]).

get_offspring_by_template(TempAlias, Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset,get_offspring_by_template,[TempAlias, Index, Count, Sort, SortType]).

get_asset_by_label(Label, Index, Count, Sort, SortType) ->
    rpc:call(get_node(),api_asset,get_asset_by_label,[Label, Index, Count, Sort, SortType]).

get_source_asset(RefId) ->
    rpc:call(get_node(),api_asset,get_source_asset,[RefId]).

get_target_asset(RefId) ->
    rpc:call(get_node(),api_asset,get_target_asset,[RefId]).

get_log_by_asset(Id) ->
    rpc:call(get_node(),api_asset,get_log_by_asset,[Id]).
    
import_scan(IpAddress, User) ->
    rpc:call(get_node(),api_asset,import_scan,[IpAddress, User]).

