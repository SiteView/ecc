package com.dragonflow.siteview.san.beans;


import java.util.HashSet;
import java.util.Set;

public class CIM_StorageMediaLocation
{
  private Set<CIM_PhysicalTape> cim_PhysicalTape = new HashSet();
  private Integer id;
  private int count;
  private String mediaTypeSupported;
  private String locationCoordinates;

  public int getCount()
  {
    return this.count;
  }

  public void setCount(int count)
  {
    this.count = count;
  }

  public String getMediaTypeSupported()
  {
    return this.mediaTypeSupported;
  }

  public void setMediaTypeSupported(String mediaTypeSupported)
  {
    this.mediaTypeSupported = mediaTypeSupported;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getLocationCoordinates()
  {
    return this.locationCoordinates;
  }

  public void setLocationCoordinates(String locationCoordinates)
  {
    this.locationCoordinates = locationCoordinates;
  }

  public Set<CIM_PhysicalTape> getCim_PhysicalTape()
  {
    return this.cim_PhysicalTape;
  }

  public void setCim_PhysicalTape(Set<CIM_PhysicalTape> cim_PhysicalTape)
  {
    this.cim_PhysicalTape = cim_PhysicalTape;
  }
}