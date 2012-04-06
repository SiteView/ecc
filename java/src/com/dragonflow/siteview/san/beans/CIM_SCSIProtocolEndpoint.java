package com.dragonflow.siteview.san.beans;

public class CIM_SCSIProtocolEndpoint
{
  private Integer id;
  private String name;
  private String creationClassName;
  private String systemName;
  private String systemCreationClassName;
  private int enabledDefault;
  private int role;
  private int connectionType;
  private String otherTypeDescription;
  private int protocolIFType;
  private int enabledState;
  private int requestedState;

  public int getConnectionType()
  {
    return this.connectionType;
  }

  public void setConnectionType(int connectionType)
  {
    this.connectionType = connectionType;
  }

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
  }

  public int getEnabledDefault()
  {
    return this.enabledDefault;
  }

  public void setEnabledDefault(int enabledDefault)
  {
    this.enabledDefault = enabledDefault;
  }

  public int getEnabledState()
  {
    return this.enabledState;
  }

  public void setEnabledState(int enabledState)
  {
    this.enabledState = enabledState;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getOtherTypeDescription()
  {
    return this.otherTypeDescription;
  }

  public void setOtherTypeDescription(String otherTypeDescription)
  {
    this.otherTypeDescription = otherTypeDescription;
  }

  public int getProtocolIFType()
  {
    return this.protocolIFType;
  }

  public void setProtocolIFType(int protocolIFType)
  {
    this.protocolIFType = protocolIFType;
  }

  public int getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(int requestedState)
  {
    this.requestedState = requestedState;
  }

  public int getRole()
  {
    return this.role;
  }

  public void setRole(int role)
  {
    this.role = role;
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
}
