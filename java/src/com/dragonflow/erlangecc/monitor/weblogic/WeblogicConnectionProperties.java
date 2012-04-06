package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.Serializable;


public class WeblogicConnectionProperties extends ConnectionProperties
    implements Serializable
{

    public WeblogicConnectionProperties(String serverName, String username, String password, int timeout, boolean secure,String uniqueID)
    {
        super(serverName, username, password, 5050, uniqueID);
        secureConnection = secure;
        this.timeout = timeout;
    }

    public boolean isSecureConnection()
    {
        return secureConnection;
    }

    public int getTimeout()
    {
        return timeout;
    }
    


    private boolean secureConnection;
    private int timeout;
}