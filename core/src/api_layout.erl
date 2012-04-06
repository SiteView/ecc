%% 
%% @doc layout operation
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author 

-module(api_layout).
-extends(api_siteview).

-compile(export_all).


get_proxy_node()->
	case server_conf:getLayoutNode() of
		undefined->
			{ok, Host} = inet:gethostname(),
			list_to_atom( "layout" ++ "@" ++ Host);
		Node->
			Node
	end.
	
	
do(DeviceList, EdgeList, LayoutType, NodeSize, Options) ->
      io:format("~p~n",[get_proxy_node()]),
      rpc:call(get_proxy_node(), layout, do, [DeviceList, EdgeList, LayoutType, NodeSize, Options]).