package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_SCSIProtocolController
{
  private Set<CIM_ComputerSystemVM> cim_ComputerSystemVM = new HashSet();
  private Set<CIM_LogicalPort> cim_LogicalPort = new HashSet();
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

  public Set<CIM_ComputerSystemVM> getCim_ComputerSystemVM()
  {
    return this.cim_ComputerSystemVM;
  }

  public void setCim_ComputerSystemVM(Set<CIM_ComputerSystemVM> cim_ComputerSystemVM)
  {
    this.cim_ComputerSystemVM = cim_ComputerSystemVM;
  }

  public Set<CIM_LogicalPort> getCim_LogicalPort()
  {
    return this.cim_LogicalPort;
  }

  public void setCim_LogicalPort(Set<CIM_LogicalPort> cim_LogicalPort)
  {
    this.cim_LogicalPort = cim_LogicalPort;
  }
}
