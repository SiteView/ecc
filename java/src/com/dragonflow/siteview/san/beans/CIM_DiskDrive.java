package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_DiskDrive
{
  private Set<CIM_ComputerSystem> cim_ComputerSystem = new HashSet();
  private Set<CIM_PhysicalPackage> cim_PhysicalPackage = new HashSet();
  private Set<CIM_StorageExtent> cim_StorageExtent = new HashSet();
  private Integer id;
  private String deviceId;
  private String creationClassName;
  private String systemName;
  private String systemCreationClassName;
  private int enabledDefault;
  private Long defaultBlockSize;
  private Long maxMediaSize;
  private int requestedState;
  private int enabledState;
  private int healthState;
  private String operationalStatus;
  private String name;
  private String elementName;
  private int instancePropertySize;
  private Long instanceTimeMean;

  public CIM_DiskDrive(String systemCreationClassName, String systemName, String creationClassName, String deviceID)
  {
    this.systemCreationClassName = systemCreationClassName;
    this.systemName = systemName;
    this.creationClassName = creationClassName;
    this.deviceId = deviceID;
  }

  public CIM_DiskDrive()
  {
  }

  public Set getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set cim_ComputerSystem)
  {
    this.cim_ComputerSystem = cim_ComputerSystem;
  }

  public Set<CIM_PhysicalPackage> getCim_PhysicalPackage()
  {
    return this.cim_PhysicalPackage;
  }

  public void setCim_PhysicalPackage(Set<CIM_PhysicalPackage> cim_PhysicalPackage)
  {
    this.cim_PhysicalPackage = cim_PhysicalPackage;
  }

  public Set<CIM_StorageExtent> getCim_StorageExtent()
  {
    return this.cim_StorageExtent;
  }

  public void setCim_StorageExtent(Set<CIM_StorageExtent> cim_StorageExtent)
  {
    this.cim_StorageExtent = cim_StorageExtent;
  }

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
  }

  public Long getDefaultBlockSize()
  {
    return this.defaultBlockSize;
  }

  public void setDefaultBlockSize(Long defaultBlockSize)
  {
    this.defaultBlockSize = defaultBlockSize;
  }

  public String getDeviceId()
  {
    return this.deviceId;
  }

  public void setDeviceId(String deviceId)
  {
    this.deviceId = deviceId;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
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

  public int getHealthState()
  {
    return this.healthState;
  }

  public void setHealthState(int healthState)
  {
    this.healthState = healthState;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Long getMaxMediaSize()
  {
    return this.maxMediaSize;
  }

  public void setMaxMediaSize(Long maxMediaSize)
  {
    this.maxMediaSize = maxMediaSize;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
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

  public int getInstancePropertySize()
  {
    return this.instancePropertySize;
  }

  public void setInstancePropertySize(int instancePropertySize)
  {
    this.instancePropertySize = instancePropertySize;
  }

  public Long getInstanceTimeMean()
  {
    return this.instanceTimeMean;
  }

  public void setInstanceTimeMean(Long instanceTimeMean)
  {
    this.instanceTimeMean = instanceTimeMean;
  }
}