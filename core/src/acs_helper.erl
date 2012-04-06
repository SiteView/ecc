%% ---
%% acs_helper
%%
%%---
-module(acs_helper).
-compile(export_all).
-define(ACS_PG_NAME,asc_group).

get_acs_pid()->
	case pg:members(?ACS_PG_NAME) of
		[]->
			{error,not_found_acs};
		[P|_]->
			P
	end.
