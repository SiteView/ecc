
-define(ENGINE, "ecc_snmp").
-define(USER,     "elecc_snmp_user").
-define(INIPWD, "siteview_ecc_snmp").
-define(SNMPENTITYETS, ecc_snmp_entitys_ets).

-record(trap_obj,{
        isgeneric = true,
		varbinds,			 %%Variable binding
		trap,			     %%trap Configuration
        session,             %%Call Interface
        msgprefix,           %%Prefix
        notifyname,
        contents,
        agentaddress        %%agent address
		}).
        
-define(INTERFACE_STATUS, [
        {1, "up"},
        {2, "down"},
        {3, "testing"},
        {4, "unknown"},
        {5, "notPresent"},
        {6, "lowerLayerDown"},
        {65536, "activing"},
        {65537, "block"},
        {65538, "config"},
        {65539, "localloopback"},
        {65540, "remoteloopback"},
        {65541, "payloadloopback"},
        {65542, "blockandlocalloopback"},
        {65543, "blockandremoteloopback"},
        {65544, "blockandpayloadloopback"},
        {65545, "linkrxblock"},
        {65546, "linktxblock"},
        {65547, "linkallblock"},
        {65548, "fault"},
        {65549, "unconfig"},
        {65550, "empty"}
]).

