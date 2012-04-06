package com.dragonflow.siteview.san.beans;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_ProtocolEndpoint
{
  private Set<CIM_FCPort> cim_FCPort = new HashSet();
  private Set<CIM_ProtocolEndpointEndpoint> cim_ProtocolEndpointEndpoint = new HashSet();
  private Integer id;
  private String name;
  private String systemCreationClassName;
  private String systemName;
  private String creationClassName;
  private String ipV4Address;
  private String subnetMask;
  private String description;
  private String operationalStatus;
  private int enabledState;
  private Calendar timeOfLastCreation;
  private String nameFormat;
  private int protocolType;
  private int protocolIFType;
  private String otherTypeDescription;
  private boolean broadcastResetSupported;
  private String otherEnabledState;
  private int requestedState;
  private int enabledDefault;
  private Calendar installDate;
  private String statusDescriptions;
  private String status;
  private int healthState;
  private String caption;
  private String elementName;

  public boolean isBroadcastResetSupported()
  {
    return this.broadcastResetSupported;
  }

  public void setBroadcastResetSupported(boolean broadcastResetSupported)
  {
    this.broadcastResetSupported = broadcastResetSupported;
  }

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public Set<CIM_FCPort> getCim_FCPort()
  {
    return this.cim_FCPort;
  }

  public void setCim_FCPort(Set<CIM_FCPort> cim_FCPort)
  {
    this.cim_FCPort = cim_FCPort;
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

  public int getHealthState()
  {
    return this.healthState;
  }

  public void setHealthState(int healthState)
  {
    this.healthState = healthState;
  }

  public Calendar getInstallDate()
  {
    return this.installDate;
  }

  public void setInstallDate(Calendar installDate)
  {
    this.installDate = installDate;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getNameFormat()
  {
    return this.nameFormat;
  }

  public void setNameFormat(String nameFormat)
  {
    this.nameFormat = nameFormat;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public String getOtherEnabledState()
  {
    return this.otherEnabledState;
  }

  public void setOtherEnabledState(String otherEnabledState)
  {
    this.otherEnabledState = otherEnabledState;
  }

  public String getOtherTypeDescription()
  {
    return this.otherTypeDescription;
  }

  public void setOtherTypeDescription(String otherTypeDescription)
  {
    this.otherTypeDescription = otherTypeDescription;
  }

  public int getProtocolIFType()
  {
    return this.protocolIFType;
  }

  public void setProtocolIFType(int protocolIFType)
  {
    this.protocolIFType = protocolIFType;
  }

  public int getProtocolType()
  {
    return this.protocolType;
  }

  public void setProtocolType(int protocolType)
  {
    this.protocolType = protocolType;
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

  public Calendar getTimeOfLastCreation()
  {
    return this.timeOfLastCreation;
  }

  public void setTimeOfLastCreation(Calendar timeOfLastCreation)
  {
    this.timeOfLastCreation = timeOfLastCreation;
  }

  public Set<CIM_ProtocolEndpointEndpoint> getCim_ProtocolEndpointEndpoint()
  {
    return this.cim_ProtocolEndpointEndpoint;
  }

  public void setCim_ProtocolEndpointEndpoint(Set<CIM_ProtocolEndpointEndpoint> cim_ProtocolEndpointEndpoint)
  {
    this.cim_ProtocolEndpointEndpoint = cim_ProtocolEndpointEndpoint;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getIpV4Address()
  {
    return this.ipV4Address;
  }

  public void setIpV4Address(String ipV4Address)
  {
    this.ipV4Address = ipV4Address;
  }

  public String getSubnetMask()
  {
    return this.subnetMask;
  }

  public void setSubnetMask(String subnetMask)
  {
    this.subnetMask = subnetMask;
  }
}