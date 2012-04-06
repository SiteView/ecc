-module(api_asset_label_dispatcher).
-compile(export_all).
-import(asset_node, [get_node/0]).

create_labeltreeNode(Parentid,Label)->
    rpc:call(get_node(),api_asset_lable,create_labeltreeNode,[Parentid,Label]).

update_labeltreeNode(Label)->
    rpc:call(get_node(),api_asset_lable,update_labeltreeNode,[Label]).

remove_labeltreeNode(Labelid)->
    rpc:call(get_node(),api_asset_lable,remove_labeltreeNode,[Labelid]).

get_user_label_root()->
    rpc:call(get_node(),api_asset_lable,get_user_label_root,[]).

get_user_sys_root()->
    rpc:call(get_node(),api_asset_lable,get_user_sys_root,[]).

get_label_children(Lid)->
    rpc:call(get_node(),api_asset_lable,get_label_children,[Lid]).

set_asset2label(Labelid,AssetIds)->
    rpc:call(get_node(),api_asset_lable,set_asset2label,[Labelid,AssetIds]).

remove_assetFromLabel(Labelid,Assetids)->
    rpc:call(get_node(),api_asset_lable,remove_assetFromLabel,[Labelid,Assetids]).

get_assetByLabel(Labelid,Index,Count,Sort,SortType)->
    rpc:call(get_node(),api_asset_lable,get_assetByLabel,[Labelid,Index,Count,Sort,SortType]).

get_labelByasset(Assetid,Index, Count, Sort, SortType)->
    rpc:call(get_node(),api_asset_lable,get_labelByasset,[Assetid,Index, Count, Sort, SortType]).

create_sysLabel(Template,Parentid)->
    rpc:call(get_node(),api_asset_lable,create_sysLabel,[Template,Parentid]).

get_label_byId(ID)->
    rpc:call(get_node(),api_asset_lable,get_label_byId,[ID]).

get_user_label()->
    rpc:call(get_node(),api_asset_lable,get_user_label,[]).
    
get_sys_label()->
    rpc:call(get_node(),api_asset_lable,get_sys_label,[]).
