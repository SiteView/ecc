package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_StorageVolume
{
  private Set<CIM_StoragePool> cim_StoragePool = new HashSet();
  private Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData = new HashSet();
  private Integer id;
  private String deviceId;
  private int enabledDefault;
  private boolean sequentialAccess;
  private int nameNamespace;
  private int nameFormat;
  private String name;
  private boolean primordial;
  private int deltaReservation;
  private int packageRedundancy;
  private int dataRedundancy;
  private boolean noSinglePointOfFailure;
  private String extentStatus;
  private boolean isBasedOnUR;
  private Long consumableBlocks;
  private Long numberOfBlocks;
  private Long blockSize;
  private int access;
  private String purpose;
  private String identifyingDescriptions;
  private String otherIdentifyingInfo;
  private int statusInfo;
  private int requestedState;
  private int enabledState;
  private int healthState;
  private String operationalStatus;
  private String elementName;
  private Long instanceTimeMean;
  private int instancePropertySize;
  private String poolName;

  public String getPoolName() {
	return poolName;
}

public void setPoolName(String poolName) {
	this.poolName = poolName;
}

public void setBasedOnUR(boolean isBasedOnUR) {
	this.isBasedOnUR = isBasedOnUR;
}

public int getAccess()
  {
    return this.access;
  }

  public void setAccess(int access)
  {
    this.access = access;
  }

  public Long getBlockSize()
  {
    return this.blockSize;
  }

  public void setBlockSize(Long blockSize)
  {
    this.blockSize = blockSize;
  }

  public Long getConsumableBlocks()
  {
    return this.consumableBlocks;
  }

  public void setConsumableBlocks(Long consumableBlocks)
  {
    this.consumableBlocks = consumableBlocks;
  }

  public int getDataRedundancy()
  {
    return this.dataRedundancy;
  }

  public void setDataRedundancy(int dataRedundancy)
  {
    this.dataRedundancy = dataRedundancy;
  }

  public int getDeltaReservation()
  {
    return this.deltaReservation;
  }

  public void setDeltaReservation(int deltaReservation)
  {
    this.deltaReservation = deltaReservation;
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

  public String getIdentifyingDescriptions()
  {
    return this.identifyingDescriptions;
  }

  public void setIdentifyingDescriptions(String identifyingDescriptions)
  {
    this.identifyingDescriptions = identifyingDescriptions;
  }

  public boolean getIsBasedOnUR()
  {
    return this.isBasedOnUR;
  }

  public void setIsBasedOnUR(boolean isBasedOnUR)
  {
    this.isBasedOnUR = isBasedOnUR;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public int getNameFormat()
  {
    return this.nameFormat;
  }

  public void setNameFormat(int nameFormat)
  {
    this.nameFormat = nameFormat;
  }

  public int getNameNamespace()
  {
    return this.nameNamespace;
  }

  public void setNameNamespace(int nameNamespace)
  {
    this.nameNamespace = nameNamespace;
  }

  public boolean isNoSinglePointOfFailure()
  {
    return this.noSinglePointOfFailure;
  }

  public void setNoSinglePointOfFailure(boolean noSinglePointOfFailure)
  {
    this.noSinglePointOfFailure = noSinglePointOfFailure;
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

  public String getOtherIdentifyingInfo()
  {
    return this.otherIdentifyingInfo;
  }

  public void setOtherIdentifyingInfo(String otherIdentifyingInfo)
  {
    this.otherIdentifyingInfo = otherIdentifyingInfo;
  }

  public int getPackageRedundancy()
  {
    return this.packageRedundancy;
  }

  public void setPackageRedundancy(int packageRedundancy)
  {
    this.packageRedundancy = packageRedundancy;
  }

  public boolean isPrimordial()
  {
    return this.primordial;
  }

  public void setPrimordial(boolean primordial)
  {
    this.primordial = primordial;
  }

  public String getPurpose()
  {
    return this.purpose;
  }

  public void setPurpose(String purpose)
  {
    this.purpose = purpose;
  }

  public int getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(int requestedState)
  {
    this.requestedState = requestedState;
  }

  public boolean getSequentialAccess()
  {
    return this.sequentialAccess;
  }

  public void setSequentialAccess(boolean sequentialAccess)
  {
    this.sequentialAccess = sequentialAccess;
  }

  public int getStatusInfo()
  {
    return this.statusInfo;
  }

  public void setStatusInfo(int statusInfo)
  {
    this.statusInfo = statusInfo;
  }

  public Set<CIM_StoragePool> getCim_StoragePool()
  {
    return this.cim_StoragePool;
  }

  public void setCim_StoragePool(Set<CIM_StoragePool> cim_StoragePool)
  {
    this.cim_StoragePool = cim_StoragePool;
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

  public Set<CIM_BlockStorageStatisticalData> getCim_BlockStorageStatisticalData()
  {
    return this.cim_BlockStorageStatisticalData;
  }

  public void setCim_BlockStorageStatisticalData(Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData)
  {
    this.cim_BlockStorageStatisticalData = cim_BlockStorageStatisticalData;
  }
}