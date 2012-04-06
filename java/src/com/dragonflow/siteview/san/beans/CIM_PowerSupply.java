package com.dragonflow.siteview.san.beans;


public class CIM_PowerSupply
{
  private Integer id;
  private Integer wsmanId;
  private String creationClassName;
  private String systemName;
  private String deviceId;
  private String elementName;
  private String enabledState;
  private String healthState;
  private String operationalStatus;
  private String requestedState;
  private String systemCreationClassName;
  private String totalOutputPower;
  private String typeOfRangeSwitching;

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
  }

  public String getDeviceId()
  {
    return this.deviceId;
  }

  public void setDeviceId(String deviceId)
  {
    this.deviceId = deviceId;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
  }

  public String getEnabledState()
  {
    return this.enabledState;
  }

  public void setEnabledState(String enabledState)
  {
    this.enabledState = enabledState;
  }

  public String getHealthState()
  {
    return this.healthState;
  }

  public void setHealthState(String healthState)
  {
    this.healthState = healthState;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public String getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(String requestedState)
  {
    this.requestedState = requestedState;
  }

  public String getSystemCreationClassName()
  {
    return this.systemCreationClassName;
  }

  public void setSystemCreationClassName(String systemCreationClassName)
  {
    this.systemCreationClassName = systemCreationClassName;
  }

  public Integer getWsmanId()
  {
    return this.wsmanId;
  }

  public void setWsmanId(Integer wsmanId)
  {
    this.wsmanId = wsmanId;
  }

  public String getTotalOutputPower()
  {
    return this.totalOutputPower;
  }

  public void setTotalOutputPower(String totalOutputPower)
  {
    this.totalOutputPower = totalOutputPower;
  }

  public String getTypeOfRangeSwitching()
  {
    return this.typeOfRangeSwitching;
  }

  public void setTypeOfRangeSwitching(String typeOfRangeSwitching)
  {
    this.typeOfRangeSwitching = typeOfRangeSwitching;
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