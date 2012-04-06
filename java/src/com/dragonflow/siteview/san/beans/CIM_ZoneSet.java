package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_ZoneSet
{
  private Set<CIM_AdminDomain> cim_AdminDomain = new HashSet();
  private Set<CIM_Zone> cim_Zone = new HashSet();
  private Integer id;
  private String instanceId;
  private String elementName;
  private boolean active;
  private String connectivityStatus;
  private String caption;
  private String description;

  public boolean getActive()
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

  public Set<CIM_AdminDomain> getCim_AdminDomain()
  {
    return this.cim_AdminDomain;
  }

  public void setCim_AdminDomain(Set<CIM_AdminDomain> cim_AdminDomain)
  {
    this.cim_AdminDomain = cim_AdminDomain;
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

  public Set<CIM_Zone> getCim_Zone()
  {
    return this.cim_Zone;
  }

  public void setCim_Zone(Set<CIM_Zone> cim_Zone)
  {
    this.cim_Zone = cim_Zone;
  }
}