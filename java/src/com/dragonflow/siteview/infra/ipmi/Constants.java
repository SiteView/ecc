package com.dragonflow.siteview.infra.ipmi;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class Constants
{

    public Constants()
    {
    }

    public static byte IPMI_SESSION_AUTHTYPE_NONE;
    public static byte IPMI_SESSION_AUTHTYPE_MD2;
    public static byte IPMI_SESSION_AUTHTYPE_MD5;
    public static byte IPMI_SESSION_AUTHTYPE_PASSWORD;
    public static byte IPMI_SESSION_AUTHTYPE_OEM;
    public static byte IPMI_SESSION_PRIV_CALLBACK;
    public static byte IPMI_SESSION_PRIV_USER;
    public static byte IPMI_SESSION_PRIV_OPERATOR;
    public static byte IPMI_SESSION_PRIV_ADMIN;
    public static byte IPMI_SESSION_PRIV_OEM;
    public static byte IPMI_LAN_CHANNEL_1 = 7;
    public static byte IPMI_LAN_CHANNEL_2 = 6;
    public static byte IPMI_LAN_CHANNEL_E = 14;
    public static ConcurrentMap privilegeLevelsStrings;
    public static ConcurrentMap sessionAuthenticationStrings;
    public static final byte IPMI_NETFN_CHASSIS = 0;
    public static final byte IPMI_NETFN_BRIDGE = 2;
    public static final byte IPMI_NETFN_SENSOR_EVENT = 4;
    public static final byte IPMI_NETFN_APP = 6;
    public static final byte IPMI_NETFN_FIRMWARE = 8;
    public static final byte IPMI_NETFN_STORAGE = 10;
    public static final byte IPMI_NETFN_TRANSPORT = 12;
    public static final byte IPMI_NETFN_SOL = 52;
    public static final byte IPMI_BMC_SLAVE_ADDR = 32;
    public static final byte IPMI_REMOTE_SWID = -127;
    public static final byte IPMI_APP_CMD_CHALLANGE_AUT_CAP = 56;
    public static final byte IPMI_APP_CMD_SESSION_CHALLANGE = 57;
    public static final byte IPMI_APP_CMD_ACTIVATE_SESSION = 58;
    public static final byte IPMI_APP_CMD_END_SESSION = 60;

    static 
    {
        IPMI_SESSION_AUTHTYPE_NONE = 0;
        IPMI_SESSION_AUTHTYPE_MD2 = 1;
        IPMI_SESSION_AUTHTYPE_MD5 = 2;
        IPMI_SESSION_AUTHTYPE_PASSWORD = 4;
        IPMI_SESSION_AUTHTYPE_OEM = 5;
        IPMI_SESSION_PRIV_CALLBACK = 1;
        IPMI_SESSION_PRIV_USER = 2;
        IPMI_SESSION_PRIV_OPERATOR = 3;
        IPMI_SESSION_PRIV_ADMIN = 4;
        IPMI_SESSION_PRIV_OEM = 5;
        privilegeLevelsStrings = new ConcurrentHashMap();
        privilegeLevelsStrings.put(new Byte(IPMI_SESSION_PRIV_CALLBACK), "CALLBACK");
        privilegeLevelsStrings.put(new Byte(IPMI_SESSION_PRIV_OPERATOR), "OPERATOR");
        privilegeLevelsStrings.put(new Byte(IPMI_SESSION_PRIV_ADMIN), "ADMINISTRATOR");
        privilegeLevelsStrings.put(new Byte(IPMI_SESSION_PRIV_OEM), "OEM");
        privilegeLevelsStrings.put(new Byte(IPMI_SESSION_PRIV_USER), "USER");
        privilegeLevelsStrings.put(new Byte((byte)15), "NO ACCESS");
        sessionAuthenticationStrings = new ConcurrentHashMap();
        sessionAuthenticationStrings.put(new Byte(IPMI_SESSION_AUTHTYPE_NONE), "NONE");
        sessionAuthenticationStrings.put(new Byte(IPMI_SESSION_AUTHTYPE_MD2), "MD2");
        sessionAuthenticationStrings.put(new Byte(IPMI_SESSION_AUTHTYPE_MD5), "MD5");
        sessionAuthenticationStrings.put(new Byte(IPMI_SESSION_AUTHTYPE_PASSWORD), "PASSWORD");
        sessionAuthenticationStrings.put(new Byte(IPMI_SESSION_AUTHTYPE_OEM), "OEM");
        sessionAuthenticationStrings.put(new Byte((byte)0), "NULL");
    }
}
