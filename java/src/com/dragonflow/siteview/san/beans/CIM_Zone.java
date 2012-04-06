package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_Zone
{
  private Set<CIM_ComputerSystem> cim_ComputerSystem = new HashSet();
  private Set<CIM_ZoneSet> cim_ZoneSet = new HashSet();
  private Set<CIM_NamedAddressCollection> cim_NamedAddressCollection = new HashSet();
  private Set<CIM_ZoneMembershipSettingData> cim_ZoneMembershipSettingData = new HashSet();
  private Integer id;
  private String instanceId;
  private String elementName;
  private boolean active;
  private String zoneType;
  private String zoneSubType;
  private String otherZoneTypeDescription;
  private String otherZoneSubTypeDescription;
  private String connectivityStatus;
  private String caption;
  private String description;

  public boolean isActive()
  {
    return this.active;
  }

  public void setActive(boolean active)
  {
    this.active = active;
  }

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public Set<CIM_ComputerSystem> getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set<CIM_ComputerSystem> cim_ComputerSystem)
  {
    this.cim_ComputerSystem = cim_ComputerSystem;
  }

  public Set<CIM_ZoneSet> getCim_ZoneSet()
  {
    return this.cim_ZoneSet;
  }

  public void setCim_ZoneSet(Set<CIM_ZoneSet> cim_ZoneSet)
  {
    this.cim_ZoneSet = cim_ZoneSet;
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

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getOtherZoneSubTypeDescription()
  {
    return this.otherZoneSubTypeDescription;
  }

  public void setOtherZoneSubTypeDescription(String otherZoneSubTypeDescription)
  {
    this.otherZoneSubTypeDescription = otherZoneSubTypeDescription;
  }

  public String getOtherZoneTypeDescription()
  {
    return this.otherZoneTypeDescription;
  }

  public void setOtherZoneTypeDescription(String otherZoneTypeDescription)
  {
    this.otherZoneTypeDescription = otherZoneTypeDescription;
  }

  public String getZoneType()
  {
    return this.zoneType;
  }

  public void setZoneType(String zoneType)
  {
    this.zoneType = zoneType;
  }

  public String getInstanceId()
  {
    return this.instanceId;
  }

  public void setInstanceId(String instanceId)
  {
    this.instanceId = instanceId;
  }

  public String getConnectivityStatus()
  {
    return this.connectivityStatus;
  }

  public void setConnectivityStatus(String connectivityStatus)
  {
    this.connectivityStatus = connectivityStatus;
  }

  public String getZoneSubType()
  {
    return this.zoneSubType;
  }

  public void setZoneSubType(String zoneSubType)
  {
    this.zoneSubType = zoneSubType;
  }

  public Set<CIM_NamedAddressCollection> getCim_NamedAddressCollection()
  {
    return this.cim_NamedAddressCollection;
  }

  public void setCim_NamedAddressCollection(Set<CIM_NamedAddressCollection> cim_NamedAddressCollection)
  {
    this.cim_NamedAddressCollection = cim_NamedAddressCollection;
  }

  public Set<CIM_ZoneMembershipSettingData> getCim_ZoneMembershipSettingData()
  {
    return this.cim_ZoneMembershipSettingData;
  }

  public void setCim_ZoneMembershipSettingData(Set<CIM_ZoneMembershipSettingData> cim_ZoneMembershipSettingData)
  {
    this.cim_ZoneMembershipSettingData = cim_ZoneMembershipSettingData;
  }
}