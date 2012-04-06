package com.dragonflow.siteview.san.beans;

import java.util.Calendar;

public class WSMan
{
  private Integer id;
  private String wsManName;
  private String ipAddress;
  private String username;
  private String password;
  private String namespace;
  private String resourceURI;
  private String protocol;
  private Integer port;
  private Integer discoveryInterval;
  private Integer dataRetention;
  private Integer status;
  private Calendar timeOfCreation;

  public WSMan()
  {
  }

  public WSMan(Integer id)
  {
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

  public String getNamespace()
  {
    return this.namespace;
  }

  public void setNamespace(String namespace)
  {
    this.namespace = namespace;
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

  public String getResourceURI()
  {
    return this.resourceURI;
  }

  public void setResourceURI(String resourceURI)
  {
    this.resourceURI = resourceURI;
  }

  public String getWsManName()
  {
    return this.wsManName;
  }

  public void setWsManName(String wsManName)
  {
    this.wsManName = wsManName;
  }
}