-record
(
    snmpPara,
    {
        server,
        port      = "",
        community = "public",
        timeout   = 300,
        retry     = 2
    }
).

-record
(
    deviceInfo,
    {
        snmpFlag = 0,
        devType = 5,              %%pc
        sp,
        baseMac=[255,255,255,255,255,255],
        community_get="public",
        community_set="private",
        sysOid="000000",
        devFactory="",
        devModel="",
        devTypeName="",
        sysSvcs=0,
        sysName="",
        ipmskinf_list=[],
        mac_list=[],
        inf_list=[],
        aft_list=[],
        arp_list=[],
        route_list=[],
        logicCommunity=[]
    }
).

-record
(
    deviceType, 
    {
        sysOids, 
        devType, 
        devTypeName, 
        devModel, 
        devFac
    }
).


-record
(
    allInfo,
    {
        device_list,
        inf_list,
        aft_list,
        arp_list,
        route_list
    }
).


-record
(
    scanPro,
    {
        com_list        = {"public",[]},
        timeout         = 2000,
        retry           = 2,
        port            = 161,
        ping_len        = 512,
        dtd,
        seed_type       = "0",
        %% 0->not read,not write;1->read,not write;2->both.
        read_ip_list    = "0",
        %% 0->not get IP_mask from other device
        readimfromother = "0",
        %% 02->read 0,2 arp,012->read 012 arp,0->read all arp
        read_arp        = "02",
        read_router     = "0",
        read_ospf       = "0",
        read_vrrp       = "0",
        read_bgp        = "0"
    }
).


-record
(
    infInx,
    {
        ifindex,
        ifType,
        ifmac,
        ifport,
        ifdesc,
        ifspeed
    }
).

-record
(
    inf,
    {
        ifip,
        ifnum=0,
        infinx_list=[]
    }
).
















