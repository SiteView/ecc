package com.dragonflow.siteview.websphere.util;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface HeartBeatListener
    extends Remote
{

    public abstract long getToken()
        throws RemoteException;
}