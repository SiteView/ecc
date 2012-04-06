package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.Serializable;

public abstract class ConnectionProperties
    implements Serializable
{

    public ConnectionProperties(String serverName, String username, String password, int port, String uniqueID)
    {
        this.password = password;
        this.port = port;
        this.serverName = serverName;
        this.username = username;
        hashID = uniqueID;
    }

    public String getHashID()
    {
        return hashID;
    }

    public String getServerName()
    {
        return serverName;
    }

    public String getUsername()
    {
        return username;
    }

    public String getPassword()
    {
        return password;
    }

    public int getPort()
    {
        return port;
    }

    protected String serverName;
    protected String username;
    protected String password;
    protected int port;
    protected String hashID;
}