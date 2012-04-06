package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_StorageCapabilities
{
  private Set<CIM_StoragePool> cim_StoragePool = new HashSet();
  private Integer id;
  private String instanceId;
  private int instanceType;
  private int extentStripeLengthDefault;
  private int deltaReservationDefault;
  private int deltaReservationMin;
  private int deltaReservationMax;
  private int packageRedundancyDefault;
  private int packageRedundancyMin;
  private int packageRedundancyMax;
  private int dataRedundancyDefault;
  private int dataRedundancyMin;
  private int dataRedundancyMax;
  private boolean noSinglePointOfFailureDefault;
  private boolean noSinglePointOfFailure;
  private int elementType;
  private String elementName;
  private String caption;
  private String description;

  public Set<CIM_StoragePool> getCim_StoragePool()
  {
    return this.cim_StoragePool;
  }

  public void setCim_StoragePool(Set<CIM_StoragePool> cim_StoragePool)
  {
    this.cim_StoragePool = cim_StoragePool;
  }

  public int getDataRedundancyDefault()
  {
    return this.dataRedundancyDefault;
  }

  public void setDataRedundancyDefault(int dataRedundancyDefault)
  {
    this.dataRedundancyDefault = dataRedundancyDefault;
  }

  public int getDataRedundancyMax()
  {
    return this.dataRedundancyMax;
  }

  public void setDataRedundancyMax(int dataRedundancyMax)
  {
    this.dataRedundancyMax = dataRedundancyMax;
  }

  public int getDataRedundancyMin()
  {
    return this.dataRedundancyMin;
  }

  public void setDataRedundancyMin(int dataRedundancyMin)
  {
    this.dataRedundancyMin = dataRedundancyMin;
  }

  public int getDeltaReservationDefault()
  {
    return this.deltaReservationDefault;
  }

  public void setDeltaReservationDefault(int deltaReservationDefault)
  {
    this.deltaReservationDefault = deltaReservationDefault;
  }

  public int getDeltaReservationMax()
  {
    return this.deltaReservationMax;
  }

  public void setDeltaReservationMax(int deltaReservationMax)
  {
    this.deltaReservationMax = deltaReservationMax;
  }

  public int getDeltaReservationMin()
  {
    return this.deltaReservationMin;
  }

  public void setDeltaReservationMin(int deltaReservationMin)
  {
    this.deltaReservationMin = deltaReservationMin;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
  }

  public int getElementType()
  {
    return this.elementType;
  }

  public void setElementType(int elementType)
  {
    this.elementType = elementType;
  }

  public int getExtentStripeLengthDefault()
  {
    return this.extentStripeLengthDefault;
  }

  public void setExtentStripeLengthDefault(int extentStripeLengthDefault)
  {
    this.extentStripeLengthDefault = extentStripeLengthDefault;
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

  public int getInstanceType()
  {
    return this.instanceType;
  }

  public void setInstanceType(int instanceType)
  {
    this.instanceType = instanceType;
  }

  public boolean isNoSinglePointOfFailure()
  {
    return this.noSinglePointOfFailure;
  }

  public void setNoSinglePointOfFailure(boolean noSinglePointOfFailure)
  {
    this.noSinglePointOfFailure = noSinglePointOfFailure;
  }

  public boolean isNoSinglePointOfFailureDefault()
  {
    return this.noSinglePointOfFailureDefault;
  }

  public void setNoSinglePointOfFailureDefault(boolean noSinglePointOfFailureDefault)
  {
    this.noSinglePointOfFailureDefault = noSinglePointOfFailureDefault;
  }

  public int getPackageRedundancyDefault()
  {
    return this.packageRedundancyDefault;
  }

  public void setPackageRedundancyDefault(int packageRedundancyDefault)
  {
    this.packageRedundancyDefault = packageRedundancyDefault;
  }

  public int getPackageRedundancyMax()
  {
    return this.packageRedundancyMax;
  }

  public void setPackageRedundancyMax(int packageRedundancyMax)
  {
    this.packageRedundancyMax = packageRedundancyMax;
  }

  public int getPackageRedundancyMin()
  {
    return this.packageRedundancyMin;
  }

  public void setPackageRedundancyMin(int packageRedundancyMin)
  {
    this.packageRedundancyMin = packageRedundancyMin;
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
}
