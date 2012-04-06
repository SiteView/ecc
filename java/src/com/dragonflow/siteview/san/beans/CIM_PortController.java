package com.dragonflow.siteview.san.beans;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_PortController
{
  private Set<CIM_ComputerSystem> cim_ComputerSystem = new HashSet();
  private Set<CIM_PhysicalPackage> cim_PhysicalPackage = new HashSet();
  private Set<CIM_SoftwareIdentity> cim_SoftwareIdentity = new HashSet();
  private Set<CIM_FCPort> cim_FCPort = new HashSet();
  private Integer id;
  private String systemCreationClassName;
  private String systemName;
  private String creationClassName;
  private String deviceId;
  private String caption;
  private String description;
  private String elementName;
  private Calendar installDate;
  private String name;
  private String operationalStatus;
  private String statusDescriptions;
  private String status;
  private int healthState;
  private int enabledState;
  private String otherEnabledState;
  private int requestedState;
  private int enabledDefault;
  private Calendar timeOfLastStateChange;
  private boolean powerManagementSupported;
  private String powerManagementCapabilities;
  private int availability;
  private int statusInfo;
  private int lastErrorCode;
  private String errorDescription;
  private boolean errorCleared;
  private String otherIdentifyingInfo;
  private Long powerOnHours;
  private Long totalPowerOnHours;
  private String identifyingDescriptions;
  private String additionalAvailability;
  private Long maxQuiesceTime;
  private Calendar timeOfLastReset;
  private int protocolSupported;
  private int maxNumberControlled;
  private String protocolDescription;
  private int controllerType;
  private String otherControllerType;
  private int controllerVersion;

  public String getAdditionalAvailability()
  {
    return this.additionalAvailability;
  }

  public void setAdditionalAvailability(String additionalAvailability)
  {
    this.additionalAvailability = additionalAvailability;
  }

  public int getAvailability()
  {
    return this.availability;
  }

  public void setAvailability(int availability)
  {
    this.availability = availability;
  }

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public int getControllerType()
  {
    return this.controllerType;
  }

  public void setControllerType(int controllerType)
  {
    this.controllerType = controllerType;
  }

  public int getControllerVersion()
  {
    return this.controllerVersion;
  }

  public void setControllerVersion(int controllerVersion)
  {
    this.controllerVersion = controllerVersion;
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

  public boolean isErrorCleared()
  {
    return this.errorCleared;
  }

  public void setErrorCleared(boolean errorCleared)
  {
    this.errorCleared = errorCleared;
  }

  public String getErrorDescription()
  {
    return this.errorDescription;
  }

  public void setErrorDescription(String errorDescription)
  {
    this.errorDescription = errorDescription;
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

  public Calendar getInstallDate()
  {
    return this.installDate;
  }

  public void setInstallDate(Calendar installDate)
  {
    this.installDate = installDate;
  }

  public int getLastErrorCode()
  {
    return this.lastErrorCode;
  }

  public void setLastErrorCode(int lastErrorCode)
  {
    this.lastErrorCode = lastErrorCode;
  }

  public int getMaxNumberControlled()
  {
    return this.maxNumberControlled;
  }

  public void setMaxNumberControlled(int maxNumberControlled)
  {
    this.maxNumberControlled = maxNumberControlled;
  }

  public Long getMaxQuiesceTime()
  {
    return this.maxQuiesceTime;
  }

  public void setMaxQuiesceTime(Long maxQuiesceTime)
  {
    this.maxQuiesceTime = maxQuiesceTime;
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

  public String getOtherControllerType()
  {
    return this.otherControllerType;
  }

  public void setOtherControllerType(String otherControllerType)
  {
    this.otherControllerType = otherControllerType;
  }

  public String getOtherEnabledState()
  {
    return this.otherEnabledState;
  }

  public void setOtherEnabledState(String otherEnabledState)
  {
    this.otherEnabledState = otherEnabledState;
  }

  public String getOtherIdentifyingInfo()
  {
    return this.otherIdentifyingInfo;
  }

  public void setOtherIdentifyingInfo(String otherIdentifyingInfo)
  {
    this.otherIdentifyingInfo = otherIdentifyingInfo;
  }

  public String getPowerManagementCapabilities()
  {
    return this.powerManagementCapabilities;
  }

  public void setPowerManagementCapabilities(String powerManagementCapabilities)
  {
    this.powerManagementCapabilities = powerManagementCapabilities;
  }

  public boolean isPowerManagementSupported()
  {
    return this.powerManagementSupported;
  }

  public void setPowerManagementSupported(boolean powerManagementSupported)
  {
    this.powerManagementSupported = powerManagementSupported;
  }

  public Long getPowerOnHours()
  {
    return this.powerOnHours;
  }

  public void setPowerOnHours(Long powerOnHours)
  {
    this.powerOnHours = powerOnHours;
  }

  public String getProtocolDescription()
  {
    return this.protocolDescription;
  }

  public void setProtocolDescription(String protocolDescription)
  {
    this.protocolDescription = protocolDescription;
  }

  public int getProtocolSupported()
  {
    return this.protocolSupported;
  }

  public void setProtocolSupported(int protocolSupported)
  {
    this.protocolSupported = protocolSupported;
  }

  public int getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(int requestedState)
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

  public String getStatusDescriptions()
  {
    return this.statusDescriptions;
  }

  public void setStatusDescriptions(String statusDescriptions)
  {
    this.statusDescriptions = statusDescriptions;
  }

  public int getStatusInfo()
  {
    return this.statusInfo;
  }

  public void setStatusInfo(int statusInfo)
  {
    this.statusInfo = statusInfo;
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

  public Calendar getTimeOfLastReset()
  {
    return this.timeOfLastReset;
  }

  public void setTimeOfLastReset(Calendar timeOfLastReset)
  {
    this.timeOfLastReset = timeOfLastReset;
  }

  public Calendar getTimeOfLastStateChange()
  {
    return this.timeOfLastStateChange;
  }

  public void setTimeOfLastStateChange(Calendar timeOfLastStateChange)
  {
    this.timeOfLastStateChange = timeOfLastStateChange;
  }

  public Long getTotalPowerOnHours()
  {
    return this.totalPowerOnHours;
  }

  public void setTotalPowerOnHours(Long totalPowerOnHours)
  {
    this.totalPowerOnHours = totalPowerOnHours;
  }

  public String getDeviceId()
  {
    return this.deviceId;
  }

  public void setDeviceId(String deviceId)
  {
    this.deviceId = deviceId;
  }

  public Set<CIM_ComputerSystem> getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set<CIM_ComputerSystem> cim_ComputerSystem)
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

  public Set<CIM_SoftwareIdentity> getCim_SoftwareIdentity()
  {
    return this.cim_SoftwareIdentity;
  }

  public void setCim_SoftwareIdentity(Set<CIM_SoftwareIdentity> cim_SoftwareIdentity)
  {
    this.cim_SoftwareIdentity = cim_SoftwareIdentity;
  }

  public Set<CIM_FCPort> getCim_FCPort()
  {
    return this.cim_FCPort;
  }

  public void setCim_FCPort(Set<CIM_FCPort> cim_FCPort)
  {
    this.cim_FCPort = cim_FCPort;
  }
}
