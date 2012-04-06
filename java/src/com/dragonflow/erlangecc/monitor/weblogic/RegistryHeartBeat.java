package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.PrintStream;
import java.rmi.Naming;


public class RegistryHeartBeat extends Thread
{

    public RegistryHeartBeat(long token, String url, String host, int port, long freq)
    {
        registryPort = 1099;
        registryHost = "127.0.0.1";
        frequency = 2000L;
        this.token = token;
        this.url = url;
        registryHost = host;
        registryPort = port;
        frequency = freq;
    }

    public void run()
    {
        do
        {
            HeartBeatListener hbListener = null;
            try
            {
                hbListener = (HeartBeatListener)Naming.lookup(url);
                if(hbListener.getToken() != token)
                    System.exit(0);
            }
            catch(Exception e)
            {
                e.printStackTrace();
                System.exit(0);
            }
            try
            {
                Thread.sleep(frequency);
            }
            catch(InterruptedException e)
            {
                System.err.println("Sleep interrupted in HeartBeat Thread in WebSphereService process.");
            }
        } while(true);
    }

    int registryPort;
    String registryHost;
    long frequency;
    String url;
    long token;
}