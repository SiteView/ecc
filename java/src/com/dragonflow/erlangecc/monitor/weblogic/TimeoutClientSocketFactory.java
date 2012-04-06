package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.IOException;
import java.io.Serializable;
import java.net.Socket;
import java.rmi.server.RMIClientSocketFactory;

public class TimeoutClientSocketFactory
    implements RMIClientSocketFactory, Serializable
{

    public TimeoutClientSocketFactory(int timeout)
    {
        this.timeout = timeout;
    }

    public Socket createSocket(String host, int port)
        throws IOException
    {
        Socket sock = new Socket(host, port);
        sock.setSoTimeout(timeout);
        return sock;
    }

    public long getTimeout()
    {
        return (long)timeout;
    }

    public boolean equals(Object obj)
    {
        return getClass() == obj.getClass() && (long)timeout == ((TimeoutClientSocketFactory)obj).getTimeout();
    }

    public int hashCode()
    {
        return timeout;
    }

    private int timeout;
}
