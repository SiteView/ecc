package com.dragonflow.erlangecc.monitor.weblogic;

import java.rmi.RemoteException;

import com.dragonflow.erlangecc.monitor.Weblogic6xMonitorImpl;


public interface WeblogicService
extends HeartBeatListener
{

public abstract WebLogicServer getServer(WeblogicConnectionProperties weblogicconnectionproperties)
    throws RemoteException;

public static final String URL_PROPERTY = "registryURL";
public static final String WEBLOGIC_MONITOR_PROPERTY = "weblogic.monitor";
public static final String DEBUG_PROPERTY = "weblogic.monitor.debug";
public static final String REMOTE_DEBUGGER_PROPERTY = "weblogic.monitor.remote.debugger";
public static final String HEART_BEAT_FREQUENCY = "heartBeatFrequency";
public static final String TIMEOUT = "serviceTimeout";
public static final String HOST = "host";
public static final String PORT = "port";
public static final String TOKEN_PROPERTY = "token";
public static final String WEBLOGIC_JAR_URL = "weblogicJarURL";
public static final String WLCIPHER_JAR_URL = "wlcipherJarURL";
}