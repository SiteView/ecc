package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_NamedAddressCollection
{
  private Set<CIM_Zone> cim_Zone = new HashSet();
  private Set<CIM_ZoneMembershipSettingData> cim_ZoneMembershipSettingData = new HashSet();
  private Integer id;
  private String collectionAlias;

  public Set<CIM_Zone> getCim_Zone()
  {
    return this.cim_Zone;
  }

  public void setCim_Zone(Set<CIM_Zone> cim_Zone)
  {
    this.cim_Zone = cim_Zone;
  }

  public String getCollectionAlias()
  {
    return this.collectionAlias;
  }

  public void setCollectionAlias(String collectionAlias)
  {
    this.collectionAlias = collectionAlias;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
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