%% ---
%% browsable_mdr_base
%%
%%---
-module(browsable_mdr_base,[BASE]).
-extends(browsable_base).
-compile(export_all).

-include("monitor_template.hrl").
-include("monitor.hrl").

-define(MAX_COUNTERS,30).

new()->
	Base = browsable_base:new(),
	Base:set_attribute(lastMeasurementTime,0),
	{?MODULE,Base}.


update()->

	ok.


isMultiThreshold()->true.



get_counter(I)->
	Count = THIS:getMaxCounter(),
	if
		I < Count, I =:= Count ->
			{ok,{_,V}} = THIS:get_property(list_to_atom(atom_to_list(browseNameid) ++ integer_to_list(I))),
			case V of
				''->
					[];
				_->
					[[list_to_integer(X) || X <- string:tokens(atom_to_list(V),".")]] ++ get_counter(I+1)
			end;
		true->
			[]
	end.


verify(Params)->
	BASE:verify(Params).


getMaxCounters()->
	?MAX_COUNTERS.

getScalarValues(Prop,Params)->
	case Prop of
		snmpversion->
			[{"V1","v1"},{"V2","v2"},{"V3","v3"}];
		snmpv3authtype->
			[{"MD5","MD5"},{"SHA","SHA"},{"NoAuthentication","None"}];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

getTargetNode()->
	null.