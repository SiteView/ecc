%% ---
%%portal_siteview
%%
%%---
-module(portal_siteview,[BASE]).
-compile(export_all).
-extends(monitor_group).

new()->
	{?MODULE,monitor_group:new(none)}.

