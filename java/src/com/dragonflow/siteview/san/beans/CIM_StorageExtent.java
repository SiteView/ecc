package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_StorageExtent
{
  private Set<CIM_DiskDrive> cim_DiskDrive = new HashSet();
  private Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData = new HashSet();
  private Integer id;
  private String deviceId;
  private String creationClassName;
  private String systemName;
  private String systemCreationClassName;
  private int enabledDefault;
  private String name;
  private boolean primordial;
  private String extentStatus;
  private Long consumableBlocks;
  private Long numberOfBlocks;
  private Long blockSize;
  private int requestedState;
  private int enabledState;
  private int healthState;
  private String operationalStatus;
  private String elementName;
  private Long instanceTimeMean;
  private int instancePropertySize;

  public Long getBlockSize()
  {
    return this.blockSize;
  }

  public void setBlockSize(Long blockSize)
  {
    this.blockSize = blockSize;
  }

  public Set<CIM_DiskDrive> getCim_DiskDrive()
  {
    return this.cim_DiskDrive;
  }

  public void setCim_DiskDrive(Set<CIM_DiskDrive> cim_DiskDrive)
  {
    this.cim_DiskDrive = cim_DiskDrive;
  }

  public Long getConsumableBlocks()
  {
    return this.consumableBlocks;
  }

  public void setConsumableBlocks(Long consumableBlocks)
  {
    this.consumableBlocks = consumableBlocks;
  }

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

  public String getExtentStatus()
  {
    return this.extentStatus;
  }

  public void setExtentStatus(String extentStatus)
  {
    this.extentStatus = extentStatus;
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

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public Long getNumberOfBlocks()
  {
    return this.numberOfBlocks;
  }

  public void setNumberOfBlocks(Long numberOfBlocks)
  {
    this.numberOfBlocks = numberOfBlocks;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public boolean isPrimordial()
  {
    return this.primordial;
  }

  public void setPrimordial(boolean primordial)
  {
    this.primordial = primordial;
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

  public Set<CIM_BlockStorageStatisticalData> getCim_BlockStorageStatisticalData()
  {
    return this.cim_BlockStorageStatisticalData;
  }

  public void setCim_BlockStorageStatisticalData(Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData)
  {
    this.cim_BlockStorageStatisticalData = cim_BlockStorageStatisticalData;
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