package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_StoragePool
{
  private Set<CIM_ComputerSystem> cim_ComputerSystem = new HashSet();
  private Set<CIM_StorageVolume> cim_StorageVolume = new HashSet();
  private Set<CIM_StorageCapabilities> cim_StorageCapabilities = new HashSet();
  private Set<CIM_StorageExtent> cim_StorageExtent = new HashSet();
  private Integer id;
  private Long remainingManagedSpace;
  private Long totalManagedSpace;
  private boolean primordial;
  private String poolId;
  private String operationalStatus;
  private String elementName;
  private String instanceId;
  private String addition1;
  private Long instanceTimeMean;
  private int instancePropertySize;
  private String allocatedFromStoragePool;

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Set<CIM_ComputerSystem> getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set<CIM_ComputerSystem> cim_ComputerSystem)
  {
    this.cim_ComputerSystem = cim_ComputerSystem;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public String getPoolId()
  {
    return this.poolId;
  }

  public void setPoolId(String poolId)
  {
    this.poolId = poolId;
  }

  public boolean isPrimordial()
  {
    return this.primordial;
  }

  public void setPrimordial(boolean primordial)
  {
    this.primordial = primordial;
  }

  public Long getRemainingManagedSpace()
  {
    return this.remainingManagedSpace;
  }

  public void setRemainingManagedSpace(Long remainingManagedSpace)
  {
    this.remainingManagedSpace = remainingManagedSpace;
  }

  public Long getTotalManagedSpace()
  {
    return this.totalManagedSpace;
  }

  public void setTotalManagedSpace(Long totalManagedSpace)
  {
    this.totalManagedSpace = totalManagedSpace;
  }

  public Set<CIM_StorageVolume> getCim_StorageVolume()
  {
    return this.cim_StorageVolume;
  }

  public void setCim_StorageVolume(Set<CIM_StorageVolume> cim_StorageVolume)
  {
    this.cim_StorageVolume = cim_StorageVolume;
  }

  public String getInstanceId()
  {
    return this.instanceId;
  }

  public void setInstanceId(String instanceId)
  {
    this.instanceId = instanceId;
  }

  public Set<CIM_StorageCapabilities> getCim_StorageCapabilities()
  {
    return this.cim_StorageCapabilities;
  }

  public void setCim_StorageCapabilities(Set<CIM_StorageCapabilities> cim_StorageCapabilities)
  {
    this.cim_StorageCapabilities = cim_StorageCapabilities;
  }

  public Set<CIM_StorageExtent> getCim_StorageExtent()
  {
    return this.cim_StorageExtent;
  }

  public void setCim_StorageExtent(Set<CIM_StorageExtent> cim_StorageExtent)
  {
    this.cim_StorageExtent = cim_StorageExtent;
  }

  public String getAddition1()
  {
    return this.addition1;
  }

  public void setAddition1(String addition1)
  {
    this.addition1 = addition1;
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

  public String getAllocatedFromStoragePool()
  {
    return this.allocatedFromStoragePool;
  }

  public void setAllocatedFromStoragePool(String allocatedFromStoragePool)
  {
    this.allocatedFromStoragePool = allocatedFromStoragePool;
  }
}