-module(asset_node).
-compile(export_all).

get_node() ->
	case server_conf:getServerConf(assetNode) of
		undefined->
			{ok,Node}=inet:gethostname(),
			list_to_atom("eccAsset@"++Node);
		AssetNode->
			AssetNode
	end.
