package com.dragonflow.erlangecc.monitor.weblogic;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface HeartBeatListener
    extends Remote
{

    public abstract long getToken()
        throws RemoteException;
}