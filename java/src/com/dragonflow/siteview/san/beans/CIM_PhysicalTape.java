package com.dragonflow.siteview.san.beans;


public class CIM_PhysicalTape
{
  private Integer id;
  private String physicalLabels;
  private String mediaType;
  private Long capacity;
  private String tag;
  private boolean dualSided;
  private boolean cleanerMedia;

  public Long getCapacity()
  {
    return this.capacity;
  }

  public void setCapacity(Long capacity)
  {
    this.capacity = capacity;
  }

  public String getMediaType()
  {
    return this.mediaType;
  }

  public void setMediaType(String mediaType)
  {
    this.mediaType = mediaType;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getPhysicalLabels()
  {
    return this.physicalLabels;
  }

  public void setPhysicalLabels(String physicalLabels)
  {
    this.physicalLabels = physicalLabels;
  }

  public String getTag()
  {
    return this.tag;
  }

  public void setTag(String tag)
  {
    this.tag = tag;
  }

  public boolean isDualSided()
  {
    return this.dualSided;
  }

  public void setDualSided(boolean dualSided)
  {
    this.dualSided = dualSided;
  }

  public boolean isCleanerMedia()
  {
    return this.cleanerMedia;
  }

  public void setCleanerMedia(boolean cleanerMedia)
  {
    this.cleanerMedia = cleanerMedia;
  }
}