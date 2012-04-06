package com.dragonflow.siteview.san.beans;


import java.util.HashSet;
import java.util.Set;

public class CIM_MediaAccessDevice
{
  private Set<CIM_SCSIProtocolController> cim_SCSIProtocolController = new HashSet();
  private Set<CIM_SoftwareIdentity> cim_SoftwareIdentity = new HashSet();
  private Set<CIM_PhysicalPackage> cim_PhysicalPackage = new HashSet();
  private Set<CIM_StorageMediaLocation> cim_StorageMediaLocation = new HashSet();
  private Integer id;
  private String elementName;
  private String deviceID;

  public String getDeviceID()
  {
    return this.deviceID;
  }

  public void setDeviceID(String deviceID)
  {
    this.deviceID = deviceID;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Set<CIM_SCSIProtocolController> getCim_SCSIProtocolController()
  {
    return this.cim_SCSIProtocolController;
  }

  public void setCim_SCSIProtocolController(Set<CIM_SCSIProtocolController> cim_SCSIProtocolController)
  {
    this.cim_SCSIProtocolController = cim_SCSIProtocolController;
  }

  public Set<CIM_SoftwareIdentity> getCim_SoftwareIdentity()
  {
    return this.cim_SoftwareIdentity;
  }

  public void setCim_SoftwareIdentity(Set<CIM_SoftwareIdentity> cim_SoftwareIdentity)
  {
    this.cim_SoftwareIdentity = cim_SoftwareIdentity;
  }

  public Set<CIM_PhysicalPackage> getCim_PhysicalPackage()
  {
    return this.cim_PhysicalPackage;
  }

  public void setCim_PhysicalPackage(Set<CIM_PhysicalPackage> cim_PhysicalPackage)
  {
    this.cim_PhysicalPackage = cim_PhysicalPackage;
  }

  public Set<CIM_StorageMediaLocation> getCim_StorageMediaLocation()
  {
    return this.cim_StorageMediaLocation;
  }

  public void setCim_StorageMediaLocation(Set<CIM_StorageMediaLocation> cim_StorageMediaLocation)
  {
    this.cim_StorageMediaLocation = cim_StorageMediaLocation;
  }
}