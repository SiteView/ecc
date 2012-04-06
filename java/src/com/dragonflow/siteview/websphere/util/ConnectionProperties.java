package com.dragonflow.siteview.websphere.util;

import java.io.Serializable;

public abstract class ConnectionProperties
  implements Serializable
{
  protected String serverName;
  protected String username;
  protected String password;
  protected int port;
  protected String hashID;

  public ConnectionProperties(String serverName, String username, String password, int port, String uniqueID)
  {
    this.password = password;
    this.port = port;
    this.serverName = serverName;
    this.username = username;
    this.hashID = uniqueID;
  }

  public String getHashID() {
    return this.hashID;
  }

  public String getServerName() {
    return this.serverName;
  }

  public String getUsername() {
    return this.username;
  }

  public String getPassword() {
    return this.password;
  }

  public int getPort() {
    return this.port;
  }
}
