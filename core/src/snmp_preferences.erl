%% @doc log_preferences
%%
%%
-module(snmp_preferences).
-extends(preferences).
-compile(export_all).
-include("alert.hrl").

-define(SNMP_HOST,snmpHost).
-define(SNMP_OBJECTID,snmpObjectID).
-define(SNMP_OBJECTID_OTHER,snmpObjectIDOther).
-define(SNMP_COMMUNITY,snmpCommunity).
-define(SNMP_GENERIC,snmpGeneric).
-define(SNMP_SPECIFIC,snmpSpecific).
-define(SNMP_TRAP_VERSION,snmpTrapVersion).


verify(Prop,Params)->
	case Prop of
		?SNMP_OBJECTID_OTHER ->
			case proplists:get_value(?SNMP_OBJECTID,Params) of
				"0"->
					case proplists:get_value(?SNMP_OBJECTID_OTHER,Params) of
						""->
							{error,[{?SNMP_OBJECTID_OTHER,"Enter a value for the SNMP Object ID."}]};
						_->
							{ok,""}
					end;
				_->
					{ok,""}
				
			end;
		?SNMP_SPECIFIC ->
			case proplists:get_value(?SNMP_GENERIC,Params) of
				"6"->
					case proplists:get_value(?SNMP_SPECIFIC,Params) of
						""->
							{error,[{?SNMP_SPECIFIC,"Enter a value for the Trap ID."}]};
						_->
							{ok,""}
					end;
				_->
					{ok,""}
			end;
		_->
			{ok,""}
	end.
    
test({Msg,Sv}) ->
    Obj = snmptrap:new([],[]),
    Obj:test_additionPres(Sv, Msg, #snmptrap_alert{msgprefix = []}, 'snmptrap_test');
test(Params) ->
    {error, "parameter error"}.
    