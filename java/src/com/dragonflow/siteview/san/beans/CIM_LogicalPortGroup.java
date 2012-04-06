package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_LogicalPortGroup
{
  private Set<CIM_FCPort> cim_FCPort = new HashSet();
  private Integer id;
  private String instanceId;
  private String caption;
  private String description;
  private String elementName;
  private String name;
  private String nameFormat;
  private String otherNameFormat;

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public Set<CIM_FCPort> getCim_FCPort()
  {
    return this.cim_FCPort;
  }

  public void setCim_FCPort(Set<CIM_FCPort> cim_FCPort)
  {
    this.cim_FCPort = cim_FCPort;
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

  public String getInstanceId()
  {
    return this.instanceId;
  }

  public void setInstanceId(String instanceId)
  {
    this.instanceId = instanceId;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getNameFormat()
  {
    return this.nameFormat;
  }

  public void setNameFormat(String nameFormat)
  {
    this.nameFormat = nameFormat;
  }

  public String getOtherNameFormat()
  {
    return this.otherNameFormat;
  }

  public void setOtherNameFormat(String otherNameFormat)
  {
    this.otherNameFormat = otherNameFormat;
  }
}