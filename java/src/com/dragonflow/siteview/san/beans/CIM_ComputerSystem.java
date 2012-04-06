package com.dragonflow.siteview.san.beans;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_ComputerSystem
{
  private Set<CIM_DiskDrive> cim_DiskDrive = new HashSet();
  private Set<CIM_StorageHardwareID> cim_StorageHardwareID = new HashSet();
  private Set<CIM_ComputerSystemController> cim_ComputerSystemController = new HashSet();
  private Set<CIM_FCPort> cim_FCPort = new HashSet();
  private Set<CIM_StoragePool> cim_StoragePool = new HashSet();
  private Set<CIM_PhysicalPackage> cim_PhysicalPackage = new HashSet();
  private Set<CIM_ComputerSystemVM> cim_ComputerSystemVM = new HashSet();
  private Set<CIM_SoftwareIdentity> cim_SoftwareIdentity = new HashSet();
  private Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData = new HashSet();
  private Set<CIM_PortController> cim_PortController = new HashSet();
  private Set<CIM_AdminDomain> cim_AdminDomain = new HashSet();
  private Set<CIM_ZoneSet> cim_ZoneSet = new HashSet();
  private Set<CIM_StorageExtent> cim_StorageExtent = new HashSet();
  private Set<CIM_StorageVolume> cim_StorageVolume = new HashSet();
  private Set<CIM_RemoteServiceAccessPoint> cim_RemoteServiceAccessPoint = new HashSet();
  private Set<CIM_MediaAccessDevice> cim_MediaAccessDevice = new HashSet();
  private Set<CIM_ChangerDevice> cim_ChangerDevice = new HashSet();
  private Set<CIM_LimitedAccessPort> cim_LimitedAccessPort = new HashSet();
  private Set<CIM_OperatingSystem> cim_OperatingSystem = new HashSet();
  private Set<CIM_FileSystem> cim_FileSystem = new HashSet();
  private Set<CIM_NetworkPort> cim_NetworkPort = new HashSet();
  private Integer id;
  private Integer cimomID;
  private Integer wsManID;
  private String name;
  private String creationClassName;
  private String dedicated;
  private String caption;
  private String description;
  private String elementName;
  private String installDate;
  private String operationalStatus;
  private String statusDescriptions;
  private String status;
  private String otherEnabledState;
  private int requestedState;
  private int healthState;
  private int enabledState;
  private int enabledDefault;
  private String timeOfLastStateChange;
  private String primaryOwnerName;
  private String primaryOwnerContact;
  private String roles;
  private String nameFormat;
  private Calendar timeOfCreation;
  private String timeOfCreationString;
  private Long instanceTimeMean;
  private int instancePropertySize;
  private Long totalDiscoveryTime;

  public CIM_ComputerSystem(String name, String creationClassName, String caption, String description, String elementName, String operationalStatus, String statusDescriptions, int enabledState)
  {
    this.name = name;
    this.creationClassName = creationClassName;
    this.caption = caption;
    this.description = description;
    this.elementName = elementName;

    this.operationalStatus = operationalStatus;
    this.statusDescriptions = statusDescriptions;

    this.enabledState = enabledState;
  }

  public CIM_ComputerSystem(String name, String creationClassName) {
    this.name = name;
    this.creationClassName = creationClassName;
  }

  public CIM_ComputerSystem() {
  }

  public String getNameFormat() {
    return this.nameFormat;
  }

  public void setNameFormat(String nameFormat) {
    this.nameFormat = nameFormat;
  }

  public String getRoles() {
    return this.roles;
  }

  public void setRoles(String roles) {
    this.roles = roles;
  }

  public String getDedicated() {
    return this.dedicated;
  }

  public void setDedicated(String dedicated) {
    this.dedicated = dedicated;
  }

  public String getPrimaryOwnerContact() {
    return this.primaryOwnerContact;
  }

  public void setPrimaryOwnerContact(String primaryOwnerContact) {
    this.primaryOwnerContact = primaryOwnerContact;
  }

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
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

  public int getEnabledState()
  {
    return this.enabledState;
  }

  public void setEnabledState(int enabledState)
  {
    this.enabledState = enabledState;
  }

  public int getHealthState()
  {
    return this.healthState;
  }

  public void setHealthState(int healthState)
  {
    this.healthState = healthState;
  }

  public String getInstallDate()
  {
    return this.installDate;
  }

  public void setInstallDate(String installDate)
  {
    this.installDate = installDate;
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

  public String getOtherEnabledState() {
    return this.otherEnabledState;
  }

  public void setOtherEnabledState(String otherEnabledState) {
    this.otherEnabledState = otherEnabledState;
  }

  public int getRequestedState() {
    return this.requestedState;
  }

  public void setRequestedState(int requestedState) {
    this.requestedState = requestedState;
  }

  public int getEnabledDefault() {
    return this.enabledDefault;
  }

  public void setEnabledDefault(int enabledDefault) {
    this.enabledDefault = enabledDefault;
  }

  public String getTimeOfLastStateChange() {
    return this.timeOfLastStateChange;
  }

  public void setTimeOfLastStateChange(String timeOfLastStateChange) {
    this.timeOfLastStateChange = timeOfLastStateChange;
  }

  public String getPrimaryOwnerName() {
    return this.primaryOwnerName;
  }

  public void setPrimaryOwnerName(String primaryOwnerName) {
    this.primaryOwnerName = primaryOwnerName;
  }

  public Set<CIM_DiskDrive> getCim_DiskDrive() {
    return this.cim_DiskDrive;
  }

  public void setCim_DiskDrive(Set<CIM_DiskDrive> diskDrives) {
    this.cim_DiskDrive = diskDrives;
  }

  public Set<CIM_StorageHardwareID> getCim_StorageHardwareID() {
    return this.cim_StorageHardwareID;
  }

  public void setCim_StorageHardwareID(Set<CIM_StorageHardwareID> storageHardwareIDs) {
    this.cim_StorageHardwareID = storageHardwareIDs;
  }

  public Set<CIM_ComputerSystemController> getCim_ComputerSystemController() {
    return this.cim_ComputerSystemController;
  }

  public void setCim_ComputerSystemController(Set<CIM_ComputerSystemController> computerSystemControllers) {
    this.cim_ComputerSystemController = computerSystemControllers;
  }

  public Set<CIM_SoftwareIdentity> getCim_SoftwareIdentity()
  {
    return this.cim_SoftwareIdentity;
  }

  public void setCim_SoftwareIdentity(Set<CIM_SoftwareIdentity> cim_SoftwareIdentity)
  {
    this.cim_SoftwareIdentity = cim_SoftwareIdentity;
  }

  public Set<CIM_BlockStorageStatisticalData> getCim_BlockStorageStatisticalData()
  {
    return this.cim_BlockStorageStatisticalData;
  }

  public void setCim_BlockStorageStatisticalData(Set<CIM_BlockStorageStatisticalData> cim_BlockStorageStatisticalData)
  {
    this.cim_BlockStorageStatisticalData = cim_BlockStorageStatisticalData;
  }

  public Calendar getTimeOfCreation() {
    return this.timeOfCreation;
  }

  public void setTimeOfCreation(Calendar timeOfCreation) {
    this.timeOfCreation = timeOfCreation;
  }

  public Integer getCimomID() {
    return this.cimomID;
  }

  public void setCimomID(Integer cimomID) {
    this.cimomID = cimomID;
  }

  public Set<CIM_FCPort> getCim_FCPort() {
    return this.cim_FCPort;
  }

  public void setCim_FCPort(Set<CIM_FCPort> cim_FCPort) {
    this.cim_FCPort = cim_FCPort;
  }

  public Set<CIM_StoragePool> getCim_StoragePool()
  {
    return this.cim_StoragePool;
  }

  public void setCim_StoragePool(Set<CIM_StoragePool> cim_StoragePool)
  {
    this.cim_StoragePool = cim_StoragePool;
  }

  public Set<CIM_PhysicalPackage> getCim_PhysicalPackage()
  {
    return this.cim_PhysicalPackage;
  }

  public void setCim_PhysicalPackage(Set<CIM_PhysicalPackage> cim_PhysicalPackage)
  {
    this.cim_PhysicalPackage = cim_PhysicalPackage;
  }

  public Set<CIM_ComputerSystemVM> getCim_ComputerSystemVM()
  {
    return this.cim_ComputerSystemVM;
  }

  public void setCim_ComputerSystemVM(Set<CIM_ComputerSystemVM> cim_ComputerSystemVM)
  {
    this.cim_ComputerSystemVM = cim_ComputerSystemVM;
  }

  public Set<CIM_PortController> getCim_PortController()
  {
    return this.cim_PortController;
  }

  public void setCim_PortController(Set<CIM_PortController> cim_PortController)
  {
    this.cim_PortController = cim_PortController;
  }

  public String getTimeOfCreationString()
  {
    return this.timeOfCreationString;
  }

  public void setTimeOfCreationString(String timeOfCreationString)
  {
    this.timeOfCreationString = timeOfCreationString;
  }

  public Set<CIM_AdminDomain> getCim_AdminDomain()
  {
    return this.cim_AdminDomain;
  }

  public void setCim_AdminDomain(Set<CIM_AdminDomain> cim_AdminDomain)
  {
    this.cim_AdminDomain = cim_AdminDomain;
  }

  public Set<CIM_ZoneSet> getCim_ZoneSet()
  {
    return this.cim_ZoneSet;
  }

  public void setCim_ZoneSet(Set<CIM_ZoneSet> cim_ZoneSet)
  {
    this.cim_ZoneSet = cim_ZoneSet;
  }

  public Set<CIM_StorageExtent> getCim_StorageExtent()
  {
    return this.cim_StorageExtent;
  }

  public void setCim_StorageExtent(Set<CIM_StorageExtent> cim_StorageExtent)
  {
    this.cim_StorageExtent = cim_StorageExtent;
  }

  public Set<CIM_StorageVolume> getCim_StorageVolume()
  {
    return this.cim_StorageVolume;
  }

  public void setCim_StorageVolume(Set<CIM_StorageVolume> cim_StorageVolume)
  {
    this.cim_StorageVolume = cim_StorageVolume;
  }

  public Set<CIM_RemoteServiceAccessPoint> getCim_RemoteServiceAccessPoint()
  {
    return this.cim_RemoteServiceAccessPoint;
  }

  public void setCim_RemoteServiceAccessPoint(Set<CIM_RemoteServiceAccessPoint> cim_RemoteServiceAccessPoint)
  {
    this.cim_RemoteServiceAccessPoint = cim_RemoteServiceAccessPoint;
  }

  public Set<CIM_MediaAccessDevice> getCim_MediaAccessDevice()
  {
    return this.cim_MediaAccessDevice;
  }

  public void setCim_MediaAccessDevice(Set<CIM_MediaAccessDevice> cim_MediaAccessDevice)
  {
    this.cim_MediaAccessDevice = cim_MediaAccessDevice;
  }

  public Set<CIM_ChangerDevice> getCim_ChangerDevice()
  {
    return this.cim_ChangerDevice;
  }

  public void setCim_ChangerDevice(Set<CIM_ChangerDevice> cim_ChangerDevice)
  {
    this.cim_ChangerDevice = cim_ChangerDevice;
  }

  public Set<CIM_LimitedAccessPort> getCim_LimitedAccessPort()
  {
    return this.cim_LimitedAccessPort;
  }

  public void setCim_LimitedAccessPort(Set<CIM_LimitedAccessPort> cim_LimitedAccessPort)
  {
    this.cim_LimitedAccessPort = cim_LimitedAccessPort;
  }

  public Set<CIM_OperatingSystem> getCim_OperatingSystem()
  {
    return this.cim_OperatingSystem;
  }

  public void setCim_OperatingSystem(Set<CIM_OperatingSystem> cim_OperatingSystem)
  {
    this.cim_OperatingSystem = cim_OperatingSystem;
  }

  public Set<CIM_FileSystem> getCim_FileSystem()
  {
    return this.cim_FileSystem;
  }

  public void setCim_FileSystem(Set<CIM_FileSystem> cim_FileSystem)
  {
    this.cim_FileSystem = cim_FileSystem;
  }

  public Set<CIM_NetworkPort> getCim_NetworkPort()
  {
    return this.cim_NetworkPort;
  }

  public void setCim_NetworkPort(Set<CIM_NetworkPort> cim_NetworkPort)
  {
    this.cim_NetworkPort = cim_NetworkPort;
  }

  public Integer getWsManID()
  {
    return this.wsManID;
  }

  public void setWsManID(Integer wsManID)
  {
    this.wsManID = wsManID;
  }

  public Long getInstanceTimeMean()
  {
    return this.instanceTimeMean;
  }

  public void setInstanceTimeMean(Long instanceTimeMean)
  {
    this.instanceTimeMean = instanceTimeMean;
  }

  public int getInstancePropertySize()
  {
    return this.instancePropertySize;
  }

  public void setInstancePropertySize(int instancePropertySize)
  {
    this.instancePropertySize = instancePropertySize;
  }

  public Long getTotalDiscoveryTime()
  {
    return this.totalDiscoveryTime;
  }

  public void setTotalDiscoveryTime(Long totalDiscoveryTime)
  {
    this.totalDiscoveryTime = totalDiscoveryTime;
  }
}