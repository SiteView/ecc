package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_ZoneMembershipSettingData
{
  private Set<CIM_Zone> cim_Zone = new HashSet();
  private Set<CIM_NamedAddressCollection> cim_NamedAddressCollection = new HashSet();
  private Integer id;
  private String connectivityMemberId;
  private String elementName;

  public String getConnectivityMemberId()
  {
    return this.connectivityMemberId;
  }

  public void setConnectivityMemberId(String connectivityMemberId)
  {
    this.connectivityMemberId = connectivityMemberId;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Set<CIM_Zone> getCim_Zone()
  {
    return this.cim_Zone;
  }

  public void setCim_Zone(Set<CIM_Zone> cim_Zone)
  {
    this.cim_Zone = cim_Zone;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
  }

  public Set<CIM_NamedAddressCollection> getCim_NamedAddressCollection()
  {
    return this.cim_NamedAddressCollection;
  }

  public void setCim_NamedAddressCollection(Set<CIM_NamedAddressCollection> cim_NamedAddressCollection)
  {
    this.cim_NamedAddressCollection = cim_NamedAddressCollection;
  }
}