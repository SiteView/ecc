package com.dragonflow.siteview.san.beans;

public class CIM_SoftwareIdentity
{
  private Integer id;
  private String instanceId;
  private String manufacturer;
  private String versionString;
  private String firmware;
  private String fcCodeBios;
  private String driver;
  private String classification;
  private Integer wsmanId;
  private String isEntity;
  private String majorVersion;
  private String minorVersion;

  public String getInstanceId()
  {
    return this.instanceId;
  }

  public void setInstanceId(String instanceId)
  {
    this.instanceId = instanceId;
  }

  public String getManufacturer()
  {
    return this.manufacturer;
  }

  public void setManufacturer(String manufacturer)
  {
    this.manufacturer = manufacturer;
  }

  public String getVersionString()
  {
    return this.versionString;
  }

  public void setVersionString(String versionString)
  {
    this.versionString = versionString;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getDriver()
  {
    return this.driver;
  }

  public void setDriver(String driver)
  {
    this.driver = driver;
  }

  public String getFcCodeBios()
  {
    return this.fcCodeBios;
  }

  public void setFcCodeBios(String fcCodeBios)
  {
    this.fcCodeBios = fcCodeBios;
  }

  public String getFirmware()
  {
    return this.firmware;
  }

  public void setFirmware(String firmware)
  {
    this.firmware = firmware;
  }

  public String getClassification()
  {
    return this.classification;
  }

  public void setClassification(String classification)
  {
    this.classification = classification;
  }

  public String getIsEntity()
  {
    return this.isEntity;
  }

  public void setIsEntity(String isEntity)
  {
    this.isEntity = isEntity;
  }

  public String getMajorVersion()
  {
    return this.majorVersion;
  }

  public void setMajorVersion(String majorVersion)
  {
    this.majorVersion = majorVersion;
  }

  public String getMinorVersion()
  {
    return this.minorVersion;
  }

  public void setMinorVersion(String minorVersion)
  {
    this.minorVersion = minorVersion;
  }

  public Integer getWsmanId()
  {
    return this.wsmanId;
  }

  public void setWsmanId(Integer wsmanId)
  {
    this.wsmanId = wsmanId;
  }
}