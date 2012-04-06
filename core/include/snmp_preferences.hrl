-define(SNMP_HOST,snmpHost).
-define(SNMP_OBJECTID,snmpObjectID).
-define(SNMP_OBJECTID_OTHER,snmpObjectIDOther).
-define(SNMP_COMMUNITY,snmpCommunity).
-define(SNMP_GENERIC,snmpGeneric).
-define(SNMP_SPECIFIC,snmpSpecific).
-define(SNMP_TRAP_VERSION,snmpTrapVersion).

-define(SNMP_OIDS,[
        {"HP Open View Event", ".1.3.6.1.4.1.11.2.17.1"},
        {"System - MIB-II", ".1.3.6.1.2.1.1"},
		{"Microsoft - Vendor MIB",".1.3.6.1.4.1.311.1.1.3.1.2"},
		{"System - Host Resources MIB",".1.3.6.1.2.1.25.1"},
		{"other...","0"}
		]).
-define(TRAP_IDS,[
		{"cold start","0"},
		{"warm start","1"},
		{"link down","2"},
		{"link up","3"},
		{"enterprise specific","6"}
		]).

-define(SNMP_VERSIONS,[
		{"V1","V1"},
		{"V2c","V2"}
		]).

-define(SYSTEM_OID, "1.3.6.1.2.1.").