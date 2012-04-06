package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_ComputerSystemController
{
  private Set<CIM_FCPort> cim_FCPort = new HashSet();

  private Set cim_ComputerSystem = new HashSet();

  private Set cim_PhysicalPackage = new HashSet();
  private Set<CIM_SCSIProtocolEndpoint> cim_SCSIProtocolEndpoint = new HashSet();
  private Set<CIM_RemoteServiceAccessPoint> cim_RemoteServiceAccessPoint = new HashSet();
  private Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData = new HashSet();
  private Integer id;
  private String name;
  private String creationClassName;
  private int memorySize;
  private int numPorts;
  private String protocol;
  private String roles;
  private int slotNumber;
  private int enabledDefault;
  private String dedicated;
  private String nameFormat;
  private String requestedState;
  private int enabledState;
  private String statusDescriptions;
  private String operationalStatus;
  private String elementName;
  private String description;
  private String caption;

  public String getName()
  {
    return this.name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Set getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set cim_ComputerSystem)
  {
    this.cim_ComputerSystem = cim_ComputerSystem;
  }

  public Set getCim_PhysicalPackage()
  {
    return this.cim_PhysicalPackage;
  }

  public void setCim_PhysicalPackage(Set cim_PhysicalPackage)
  {
    this.cim_PhysicalPackage = cim_PhysicalPackage;
  }

  public Integer getId() {
    return this.id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public Set<CIM_FCPort> getCim_FCPort() {
    return this.cim_FCPort;
  }

  public void setCim_FCPort(Set<CIM_FCPort> cim_FCPort) {
    this.cim_FCPort = cim_FCPort;
  }

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

  public String getDedicated()
  {
    return this.dedicated;
  }

  public void setDedicated(String dedicated)
  {
    this.dedicated = dedicated;
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

  public int getMemorySize()
  {
    return this.memorySize;
  }

  public void setMemorySize(int memorySize)
  {
    this.memorySize = memorySize;
  }

  public String getNameFormat()
  {
    return this.nameFormat;
  }

  public void setNameFormat(String nameFormat)
  {
    this.nameFormat = nameFormat;
  }

  public int getNumPorts()
  {
    return this.numPorts;
  }

  public void setNumPorts(int numPorts)
  {
    this.numPorts = numPorts;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public String getProtocol()
  {
    return this.protocol;
  }

  public void setProtocol(String protocol)
  {
    this.protocol = protocol;
  }

  public String getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(String requestedState)
  {
    this.requestedState = requestedState;
  }

  public String getRoles()
  {
    return this.roles;
  }

  public void setRoles(String roles)
  {
    this.roles = roles;
  }

  public int getSlotNumber()
  {
    return this.slotNumber;
  }

  public void setSlotNumber(int slotNumber)
  {
    this.slotNumber = slotNumber;
  }

  public String getStatusDescriptions()
  {
    return this.statusDescriptions;
  }

  public void setStatusDescriptions(String statusDescriptions)
  {
    this.statusDescriptions = statusDescriptions;
  }

  public Set<CIM_SCSIProtocolEndpoint> getCim_SCSIProtocolEndpoint()
  {
    return this.cim_SCSIProtocolEndpoint;
  }

  public void setCim_SCSIProtocolEndpoint(Set<CIM_SCSIProtocolEndpoint> cim_SCSIProtocolEndpoint)
  {
    this.cim_SCSIProtocolEndpoint = cim_SCSIProtocolEndpoint;
  }

  public Set<CIM_RemoteServiceAccessPoint> getCim_RemoteServiceAccessPoint()
  {
    return this.cim_RemoteServiceAccessPoint;
  }

  public void setCim_RemoteServiceAccessPoint(Set<CIM_RemoteServiceAccessPoint> cim_RemoteServiceAccessPoint)
  {
    this.cim_RemoteServiceAccessPoint = cim_RemoteServiceAccessPoint;
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