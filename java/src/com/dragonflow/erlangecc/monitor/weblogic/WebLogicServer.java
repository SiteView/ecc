package com.dragonflow.erlangecc.monitor.weblogic;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;



public interface WebLogicServer
extends Remote
{

public abstract Object[] update(String as[], String as1[])
    throws  RemoteException;

public abstract Map<String,Object> getBrowseData()
    throws RemoteException;

public abstract String getServerName()
    throws RemoteException;
}