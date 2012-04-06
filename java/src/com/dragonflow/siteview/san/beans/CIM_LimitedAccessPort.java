package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_LimitedAccessPort
{
  private Set<CIM_StorageMediaLocation> cim_StorageMediaLocation = new HashSet();
  private Integer id;
  private String elementName;
  private boolean extended;

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

  public boolean isExtended()
  {
    return this.extended;
  }

  public void setExtended(boolean extended)
  {
    this.extended = extended;
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