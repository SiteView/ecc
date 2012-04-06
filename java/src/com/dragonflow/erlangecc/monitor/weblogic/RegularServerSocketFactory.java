package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.IOException;
import java.io.Serializable;
import java.net.ServerSocket;
import java.rmi.server.RMIServerSocketFactory;

public class RegularServerSocketFactory
    implements RMIServerSocketFactory, Serializable
{

    public RegularServerSocketFactory()
    {
    }

    public ServerSocket createServerSocket(int port)
        throws IOException
    {
        ServerSocket serverSock = new ServerSocket(port);
        return serverSock;
    }

    public int hashCode()
    {
        return getClass().hashCode();
    }

    public boolean equals(Object obj)
    {
        return getClass() == obj.getClass();
    }
}
