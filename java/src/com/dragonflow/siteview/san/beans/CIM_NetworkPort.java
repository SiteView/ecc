package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_NetworkPort
{
  private Set<CIM_ProtocolEndpoint> cim_ProtocolEndpoint = new HashSet();
  private Integer id;
  private String systemCreationClassName;
  private String systemName;
  private String creationClassName;
  private String deviceID;
  private String linkTechnology;
  private String otherLinkTechnology;
  private String enabledState;
  private String requestedState;
  private String enabledDefault;
  private String name;
  private String status;
  private String caption;
  private String description;
  private String elementName;
  private String permanentAddress;

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
  }

  public String getDescription()
  {
    return this.description;
  }

  public void setDescription(String description)
  {
    this.description = description;
  }

  public String getDeviceID()
  {
    return this.deviceID;
  }

  public void setDeviceID(String deviceID)
  {
    this.deviceID = deviceID;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
  }

  public String getEnabledDefault()
  {
    return this.enabledDefault;
  }

  public void setEnabledDefault(String enabledDefault)
  {
    this.enabledDefault = enabledDefault;
  }

  public String getEnabledState()
  {
    return this.enabledState;
  }

  public void setEnabledState(String enabledState)
  {
    this.enabledState = enabledState;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getLinkTechnology()
  {
    return this.linkTechnology;
  }

  public void setLinkTechnology(String linkTechnology)
  {
    this.linkTechnology = linkTechnology;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getOtherLinkTechnology()
  {
    return this.otherLinkTechnology;
  }

  public void setOtherLinkTechnology(String otherLinkTechnology)
  {
    this.otherLinkTechnology = otherLinkTechnology;
  }

  public String getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(String requestedState)
  {
    this.requestedState = requestedState;
  }

  public String getStatus()
  {
    return this.status;
  }

  public void setStatus(String status)
  {
    this.status = status;
  }

  public String getSystemCreationClassName()
  {
    return this.systemCreationClassName;
  }

  public void setSystemCreationClassName(String systemCreationClassName)
  {
    this.systemCreationClassName = systemCreationClassName;
  }

  public String getSystemName()
  {
    return this.systemName;
  }

  public void setSystemName(String systemName)
  {
    this.systemName = systemName;
  }

  public Set<CIM_ProtocolEndpoint> getCim_ProtocolEndpoint()
  {
    return this.cim_ProtocolEndpoint;
  }

  public void setCim_ProtocolEndpoint(Set<CIM_ProtocolEndpoint> cim_ProtocolEndpoint)
  {
    this.cim_ProtocolEndpoint = cim_ProtocolEndpoint;
  }

  public String getPermanentAddress()
  {
    return this.permanentAddress;
  }

  public void setPermanentAddress(String permanentAddress)
  {
    this.permanentAddress = permanentAddress;
  }
}