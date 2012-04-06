package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.dragonflow.erlangecc.monitor.NonDeferringClassLoader;
import com.dragonflow.erlangecc.monitor.Weblogic6xMonitorImpl;



public class WeblogicServiceImpl extends UnicastRemoteObject
implements WeblogicService
{

public WeblogicServiceImpl(int timeout)
    throws RemoteException
{
    super(0, new TimeoutClientSocketFactory(timeout), new RegularServerSocketFactory());
    servers = Collections.synchronizedMap(new HashMap());
    url = null;
    debug = Boolean.getBoolean("weblogic.monitor.debug");
    url = System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.registryURL", "");
    try
    {
        token = Long.parseLong(System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.token"));
    }
    catch(NumberFormatException e)
    {
        token = -1L;
    }
}

public static void main(String args[])
{
    String weblogicJar = System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.weblogicJarURL");
    String wlcipherJar = System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.wlcipherJarURL");
    if(weblogicJar != null && weblogicJar.length() > 0)
    {
        URL jars[] = null;
        try
        {
            if(wlcipherJar != null && wlcipherJar.length() > 0)
                jars = (new URL[] {
                    new URL(weblogicJar), new URL(wlcipherJar)
                });
            else
                jars = (new URL[] {
                    new URL(weblogicJar)
                });
            specialWebLogicClassLoader = new NonDeferringClassLoader(jars);
        }
        catch(MalformedURLException e)
        {
            System.err.println("WebLogicServiceImpl encountered an exception while processing the weblogic.jar and wlcipher.jar URLs.  Exception was: " + e);
            System.err.println("WebLogicServiceImpl will attempt to continue, but may fail due to unresolvable class references.");
        }
    }
    int timeout;
    try
    {
        timeout = Integer.parseInt(System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.serviceTimeout", "20000"));
    }
    catch(NumberFormatException e)
    {
        timeout = 20000;
        System.err.println("WebLogicService process could not parse the serviceTimeoutproperty.  Using default of " + timeout + " milliseconds.");
    }
    if(System.getSecurityManager() == null)
    {
        System.out.println("WebLogicServiceImpl installing new SecurityManager... ");
        System.setSecurityManager(new NullSecurityManager());
    }
    WeblogicServiceImpl wlService = null;
    try
    {
        wlService = new WeblogicServiceImpl(timeout);
        System.out.println("Attempting to bind WebLogicService in rmiregistry as " + wlService.url);
        Naming.rebind(wlService.url, wlService);
        System.out.println("WebLogicService successfully bound in rmiregistry as " + wlService.url);
        System.out.flush();
        long heartBeatFreq;
        try
        {
            heartBeatFreq = Long.parseLong(System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.heartBeatFrequency", "3000"));
        }
        catch(NumberFormatException e)
        {
            heartBeatFreq = 3000L;
            System.err.println("WebLogicService process could not parse the heartBeatFrequencyproperty.  Using default of " + heartBeatFreq + " milliseconds.");
        }
        String registryHost = System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.host", "127.0.0.1");
        int registryPort;
        try
        {
            registryPort = Integer.parseInt(System.getProperty("com.dragonflow.erlangecc.monitor.weblogic.port", "1099"));
        }
        catch(NumberFormatException e)
        {
            registryPort = 1099;
            System.err.println("WebLogicService process could not parse the portproperty.  Using default port: " + registryPort + "");
        }
        (new RegistryHeartBeat(wlService.token, wlService.url, registryHost, registryPort, heartBeatFreq)).start();
    }
    catch(Exception e)
    {
        System.err.println("WebLogicServiceImpl.main() encountered an exception: " + e);
        e.printStackTrace();
    }
}

public WebLogicServer getServer(WeblogicConnectionProperties connProps)
    throws RemoteException
{
    if(debug)
        System.err.println("Entering WebLogicServiceImpl.getServer() with connProps=" + connProps);
    WebLogicServer server;
    synchronized(this)
    {
        server = (WebLogicServer)servers.get(connProps.getHashID());
        if(debug && server != null)
            System.err.println("WebLogicServiceImpl.getServer() got cached server=" + server + " with connProps.getHashID()=" + connProps.getHashID());
        if(server == null)
        {
            if(debug)
                System.err.println("WebLogicServiceImpl.getServer() is instantiating new server because none was found in the cache.");
            server = new WebLogicServerImpl(connProps,specialWebLogicClassLoader);
            servers.put(connProps.getHashID(), server);
        }
    }
    if(debug)
        System.err.println("Leaving WebLogicServiceImpl.getServer() with server=" + server);
    return server;
}

public long getToken()
    throws RemoteException
{
    return token;
}


protected Map servers;
protected static final String packagePrefix = "com.dragonflow.erlangecc.monitor.weblogic";
protected String url;
boolean debug;
private long token;
private static NonDeferringClassLoader specialWebLogicClassLoader;

}
