package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_LogicalPort
{
  private Set<CIM_SCSIProtocolController> cim_SCSIProtocolController = new HashSet();
  private Set<CIM_ProtocolEndpoint> cim_ProtocolEndpoint = new HashSet();
  private Integer id;
  private String deviceId;
  private String creationClassName;
  private String systemName;
  private String systemCreationClassName;
  private int enabledDefault;
  private int enabledState;
  private int requestedState;

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
  }

  public String getDeviceId()
  {
    return this.deviceId;
  }

  public void setDeviceId(String deviceId)
  {
    this.deviceId = deviceId;
  }

  public int getEnabledDefault()
  {
    return this.enabledDefault;
  }

  public void setEnabledDefault(int enabledDefault)
  {
    this.enabledDefault = enabledDefault;
  }

  public int getEnabledState()
  {
    return this.enabledState;
  }

  public void setEnabledState(int enabledState)
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

  public int getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(int requestedState)
  {
    this.requestedState = requestedState;
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

  public Set<CIM_SCSIProtocolController> getCim_SCSIProtocolController()
  {
    return this.cim_SCSIProtocolController;
  }

  public void setCim_SCSIProtocolController(Set<CIM_SCSIProtocolController> cim_SCSIProtocolController)
  {
    this.cim_SCSIProtocolController = cim_SCSIProtocolController;
  }

  public Set<CIM_ProtocolEndpoint> getCim_ProtocolEndpoint()
  {
    return this.cim_ProtocolEndpoint;
  }

  public void setCim_ProtocolEndpoint(Set<CIM_ProtocolEndpoint> cim_ProtocolEndpoint)
  {
    this.cim_ProtocolEndpoint = cim_ProtocolEndpoint;
  }
}