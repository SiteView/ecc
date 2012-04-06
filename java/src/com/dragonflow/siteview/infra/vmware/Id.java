package com.dragonflow.siteview.infra.vmware;

import com.vmware.vim.ManagedObjectReference;
import java.rmi.RemoteException;

public interface Id
{

    public abstract String stringId();

    public abstract ManagedObjectReference getObject(Connector connector)
        throws RemoteException;
}
