%% ---
%%子组
%%
%%---
-module(sub_group,[BASE,S]).
-extends(monitor).
-compile(export_all).

-include("monitor.hrl").

new(S)->
	Base = monitor:new(null),
	{?MODULE,Base,S}.

calculateIPAddresses()->[].


lookupGroup()->
	FullID = string:tokens(THIS:get_full_id(), ?ID_SEPARATOR),
	lookupGroup(FullID).
lookupGroup(FullID) when is_list(FullID)->
	[GId|_]=FullID,
	SV = siteview:get_current_siteview(),
	SV:get_group(GId);
lookupGroup(_)->
	{error,error_id}.
	