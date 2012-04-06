package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_ZoneCapabilities
{
  private Set<CIM_ComputerSystemController> cim_ComputerSystemController = new HashSet();
  private Set<CIM_ComputerSystem> cim_ComputerSystem = new HashSet();

  private Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData = new HashSet();
  private Set<CIM_PortController> cim_PortController = new HashSet();
  private Set<CIM_FCPortStatistics> cim_FCPortStatistics = new HashSet();
  private Set<CIM_LogicalPortGroup> cim_LogicalPortGroup = new HashSet();
  private Set<CIM_ProtocolEndpoint> cim_ProtocolEndpoint = new HashSet();
  private Integer id;
  private String systemCreationClassName;
  private String systemName;
  private String creationClassName;
  private String deviceId;
  private String elementName;
  private String usageRestriction;
  private String operationalStatus;
  private Long speed;
  private Long maxSpeed;
  private String portType;
  private String linkTechnology;
  private Long supportedMaximumTransmissionUnit;
  private String permanentAddress;
  private int portNumber;

  public String getPermanentAddress()
  {
    return this.permanentAddress;
  }

  public void setPermanentAddress(String permanentAddress) {
    this.permanentAddress = permanentAddress;
  }

  public Set getCim_ComputerSystemController()
  {
    return this.cim_ComputerSystemController;
  }

  public void setCim_ComputerSystemController(Set cim_ComputerSystemController)
  {
    this.cim_ComputerSystemController = cim_ComputerSystemController;
  }

  public Integer getId() {
    return this.id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public Set getCim_BlockStorageStatisticalData()
  {
    return this.cim_BlockStorageStatisticalData;
  }

  public void setCim_BlockStorageStatisticalData(Set cim_BlockStorageStatisticalData)
  {
    this.cim_BlockStorageStatisticalData = cim_BlockStorageStatisticalData;
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

  public String getLinkTechnology()
  {
    return this.linkTechnology;
  }

  public void setLinkTechnology(String linkTechnology)
  {
    this.linkTechnology = linkTechnology;
  }

  public Long getMaxSpeed()
  {
    return this.maxSpeed;
  }

  public void setMaxSpeed(Long maxSpeed)
  {
    this.maxSpeed = maxSpeed;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public Long getSpeed()
  {
    return this.speed;
  }

  public void setSpeed(Long speed)
  {
    this.speed = speed;
  }

  public Long getSupportedMaximumTransmissionUnit()
  {
    return this.supportedMaximumTransmissionUnit;
  }

  public void setSupportedMaximumTransmissionUnit(Long supportedMaximumTransmissionUnit)
  {
    this.supportedMaximumTransmissionUnit = supportedMaximumTransmissionUnit;
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

  public String getPortType()
  {
    return this.portType;
  }

  public void setPortType(String portType)
  {
    this.portType = portType;
  }

  public String getUsageRestriction()
  {
    return this.usageRestriction;
  }

  public void setUsageRestriction(String usageRestriction)
  {
    this.usageRestriction = usageRestriction;
  }

  public int getPortNumber()
  {
    return this.portNumber;
  }

  public void setPortNumber(int portNumber)
  {
    this.portNumber = portNumber;
  }

  public Set getCim_PortController()
  {
    return this.cim_PortController;
  }

  public void setCim_PortController(Set cim_PortController)
  {
    this.cim_PortController = cim_PortController;
  }

  public Set getCim_FCPortStatistics()
  {
    return this.cim_FCPortStatistics;
  }

  public void setCim_FCPortStatistics(Set cim_FCPortStatistics)
  {
    this.cim_FCPortStatistics = cim_FCPortStatistics;
  }

  public Set getCim_LogicalPortGroup()
  {
    return this.cim_LogicalPortGroup;
  }

  public void setCim_LogicalPortGroup(Set cim_LogicalPortGroup)
  {
    this.cim_LogicalPortGroup = cim_LogicalPortGroup;
  }

  public Set getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set cim_ComputerSystem)
  {
    this.cim_ComputerSystem = cim_ComputerSystem;
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