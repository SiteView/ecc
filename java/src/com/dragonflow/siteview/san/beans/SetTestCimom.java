package com.dragonflow.siteview.san.beans;

import java.util.Calendar;

public class SetTestCimom
{
  private Integer id;
  private String providerName;
  private String ipAddress;
  private String username;
  private String password;
  private String cimomNamespace;
  private String cimomInteropNamespace;
  private String protocol;
  private Integer port;
  private Integer discoveryInterval;
  private Integer dataRetention;
  private Integer status;
  private Calendar timeOfCreation;

  public SetTestCimom()
  {
  }

  public SetTestCimom(String providerName, String protocol, String ipAddress, int port, String cimomNamespace, String cimomInteropNamespace, String username, String password, int discoveryInterval, int dataRetention)
  {
    this.providerName = providerName;
    this.protocol = protocol;
    this.ipAddress = ipAddress;
    this.port = Integer.valueOf(port);
    this.cimomNamespace = cimomNamespace;
    this.cimomInteropNamespace = cimomInteropNamespace;
    this.username = username;
    this.password = password;
    this.discoveryInterval = Integer.valueOf(discoveryInterval);
    this.dataRetention = Integer.valueOf(dataRetention);
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getIpAddress()
  {
    return this.ipAddress;
  }

  public void setIpAddress(String ipAddress)
  {
    this.ipAddress = ipAddress;
  }

  public String getUsername()
  {
    return this.username;
  }

  public void setUsername(String username)
  {
    this.username = username;
  }

  public String getPassword()
  {
    return this.password;
  }

  public void setPassword(String password)
  {
    this.password = password;
  }

  public Calendar getTimeOfCreation() {
    return this.timeOfCreation;
  }

  public void setTimeOfCreation(Calendar timeOfCreation) {
    this.timeOfCreation = timeOfCreation;
  }

  public Integer getDiscoveryInterval() {
    return this.discoveryInterval;
  }

  public void setDiscoveryInterval(Integer discoveryInterval) {
    this.discoveryInterval = discoveryInterval;
  }

  public Integer getStatus() {
    return this.status;
  }

  public void setStatus(Integer status) {
    this.status = status;
  }

  public Integer getDataRetention()
  {
    return this.dataRetention;
  }

  public void setDataRetention(Integer dataRetention)
  {
    this.dataRetention = dataRetention;
  }

  public Integer getPort()
  {
    return this.port;
  }

  public void setPort(Integer port)
  {
    this.port = port;
  }

  public String getProtocol()
  {
    return this.protocol;
  }

  public void setProtocol(String protocol)
  {
    this.protocol = protocol;
  }

  public String getProviderName()
  {
    return this.providerName;
  }

  public void setProviderName(String providerName)
  {
    this.providerName = providerName;
  }

  public String getCimomNamespace()
  {
    return this.cimomNamespace;
  }

  public void setCimomNamespace(String cimomNamespace)
  {
    this.cimomNamespace = cimomNamespace;
  }

  public String getCimomInteropNamespace() {
    return this.cimomInteropNamespace;
  }

  public void setCimomInteropNamespace(String cimomInteropNamespace) {
    this.cimomInteropNamespace = cimomInteropNamespace;
  }
}