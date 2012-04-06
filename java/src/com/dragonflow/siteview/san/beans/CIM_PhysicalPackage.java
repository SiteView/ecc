package com.dragonflow.siteview.san.beans;


import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_PhysicalPackage
{
  private Set<CIM_ComputerSystem> cim_ComputerSystem = new HashSet();
  private Set<CIM_DiskDrive> cim_DiskDrive = new HashSet();
  private Set<CIM_PortController> cim_PortController = new HashSet();
  private Set<CIM_StorageMediaLocation> cim_StorageMediaLocation = new HashSet();
  private Set<CIM_PhysicalTape> cim_PhysicalTape = new HashSet();
  private Integer id;
  private Integer wsmanId;
  private String canBeFRUed;
  private String hostingBoard;
  private String Tag;
  private String creationClassName;
  private String caption;
  private Calendar installDate;
  private String name;
  private String operationalStatus;
  private String statusDescriptions;
  private String status;
  private int healthState;
  private String description;
  private String elementName;
  private String manufacturer;
  private String sku;
  private String serialNumber;
  private String version;
  private String partNumber;
  private String otherIdentifyingInfo;
  private boolean poweredOn;
  private Calendar manufacturerDate;
  private String vendorEquipmentType;
  private String userTracking;
  private String removalConditions;
  private boolean removable;
  private boolean replacable;
  private boolean hotSwappable;
  private int packageType;
  private String otherPackageType;
  private String vendorCompatibilityStrings;
  private String model;

  public String getCanBeFRUed()
  {
    return this.canBeFRUed;
  }

  public void setCanBeFRUed(String canBeFRUed)
  {
    this.canBeFRUed = canBeFRUed;
  }

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public Set<CIM_ComputerSystem> getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set<CIM_ComputerSystem> cim_ComputerSystem)
  {
    this.cim_ComputerSystem = cim_ComputerSystem;
  }

  public Set<CIM_DiskDrive> getCim_DiskDrive()
  {
    return this.cim_DiskDrive;
  }

  public void setCim_DiskDrive(Set<CIM_DiskDrive> cim_DiskDrive)
  {
    this.cim_DiskDrive = cim_DiskDrive;
  }

  public Set<CIM_PhysicalTape> getCim_PhysicalTape()
  {
    return this.cim_PhysicalTape;
  }

  public void setCim_PhysicalTape(Set<CIM_PhysicalTape> cim_PhysicalTape)
  {
    this.cim_PhysicalTape = cim_PhysicalTape;
  }

  public Set<CIM_PortController> getCim_PortController()
  {
    return this.cim_PortController;
  }

  public void setCim_PortController(Set<CIM_PortController> cim_PortController)
  {
    this.cim_PortController = cim_PortController;
  }

  public Set<CIM_StorageMediaLocation> getCim_StorageMediaLocation()
  {
    return this.cim_StorageMediaLocation;
  }

  public void setCim_StorageMediaLocation(Set<CIM_StorageMediaLocation> cim_StorageMediaLocation)
  {
    this.cim_StorageMediaLocation = cim_StorageMediaLocation;
  }

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
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

  public int getHealthState()
  {
    return this.healthState;
  }

  public void setHealthState(int healthState)
  {
    this.healthState = healthState;
  }

  public String getHostingBoard()
  {
    return this.hostingBoard;
  }

  public void setHostingBoard(String hostingBoard)
  {
    this.hostingBoard = hostingBoard;
  }

  public boolean isHotSwappable()
  {
    return this.hotSwappable;
  }

  public void setHotSwappable(boolean hotSwappable)
  {
    this.hotSwappable = hotSwappable;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Calendar getInstallDate()
  {
    return this.installDate;
  }

  public void setInstallDate(Calendar installDate)
  {
    this.installDate = installDate;
  }

  public String getManufacturer()
  {
    return this.manufacturer;
  }

  public void setManufacturer(String manufacturer)
  {
    this.manufacturer = manufacturer;
  }

  public Calendar getManufacturerDate()
  {
    return this.manufacturerDate;
  }

  public void setManufacturerDate(Calendar manufacturerDate)
  {
    this.manufacturerDate = manufacturerDate;
  }

  public String getModel()
  {
    return this.model;
  }

  public void setModel(String model)
  {
    this.model = model;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public String getOtherIdentifyingInfo()
  {
    return this.otherIdentifyingInfo;
  }

  public void setOtherIdentifyingInfo(String otherIdentifyingInfo)
  {
    this.otherIdentifyingInfo = otherIdentifyingInfo;
  }

  public String getOtherPackageType()
  {
    return this.otherPackageType;
  }

  public void setOtherPackageType(String otherPackageType)
  {
    this.otherPackageType = otherPackageType;
  }

  public int getPackageType()
  {
    return this.packageType;
  }

  public void setPackageType(int packageType)
  {
    this.packageType = packageType;
  }

  public String getPartNumber()
  {
    return this.partNumber;
  }

  public void setPartNumber(String partNumber)
  {
    this.partNumber = partNumber;
  }

  public boolean isPoweredOn()
  {
    return this.poweredOn;
  }

  public void setPoweredOn(boolean poweredOn)
  {
    this.poweredOn = poweredOn;
  }

  public boolean isRemovable()
  {
    return this.removable;
  }

  public void setRemovable(boolean removable)
  {
    this.removable = removable;
  }

  public String getRemovalConditions()
  {
    return this.removalConditions;
  }

  public void setRemovalConditions(String removalConditions)
  {
    this.removalConditions = removalConditions;
  }

  public boolean isReplacable()
  {
    return this.replacable;
  }

  public void setReplacable(boolean replacable)
  {
    this.replacable = replacable;
  }

  public String getSerialNumber()
  {
    return this.serialNumber;
  }

  public void setSerialNumber(String serialNumber)
  {
    this.serialNumber = serialNumber;
  }

  public String getSku()
  {
    return this.sku;
  }

  public void setSku(String sku)
  {
    this.sku = sku;
  }

  public String getStatus()
  {
    return this.status;
  }

  public void setStatus(String status)
  {
    this.status = status;
  }

  public String getStatusDescriptions()
  {
    return this.statusDescriptions;
  }

  public void setStatusDescriptions(String statusDescriptions)
  {
    this.statusDescriptions = statusDescriptions;
  }

  public String getTag()
  {
    return this.Tag;
  }

  public void setTag(String tag)
  {
    this.Tag = tag;
  }

  public String getUserTracking()
  {
    return this.userTracking;
  }

  public void setUserTracking(String userTracking)
  {
    this.userTracking = userTracking;
  }

  public String getVendorCompatibilityStrings()
  {
    return this.vendorCompatibilityStrings;
  }

  public void setVendorCompatibilityStrings(String vendorCompatibilityStrings)
  {
    this.vendorCompatibilityStrings = vendorCompatibilityStrings;
  }

  public String getVendorEquipmentType()
  {
    return this.vendorEquipmentType;
  }

  public void setVendorEquipmentType(String vendorEquipmentType)
  {
    this.vendorEquipmentType = vendorEquipmentType;
  }

  public String getVersion()
  {
    return this.version;
  }

  public void setVersion(String version)
  {
    this.version = version;
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