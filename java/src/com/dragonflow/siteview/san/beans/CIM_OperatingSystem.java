package com.dragonflow.siteview.san.beans;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_OperatingSystem
{
  private Set<CIM_DiskDrive> cim_DiskDrive = new HashSet();
  private Integer id;
  private String name;
  private String creationClassName;
  private String csName;
  private String csCreationClassName;
  private String caption;
  private String description;
  private String elementName;
  private String installDate;
  private String operationalStatus;
  private String statusDescriptions;
  private String status;
  private int healthState;
  private int enabledState;
  private String otherEnabledState;
  private int requestedState;
  private int enabledDefault;
  private String timeOfLastStateChange;
  private String languageEdition;
  private String codeSet;
  private int defaultPageSize;
  private int pctTotalCPUTime;
  private int osType;
  private String otherTypeDescription;
  private String version;
  private Calendar lastBootUpTime;
  private Calendar localDateTime;
  private int currentTimeZone;
  private int numberOfLicensedUsers;
  private int numberOfUsers;
  private int numberOfProcesses;
  private int maxNumberOfProcesses;
  private int maxProcessesPerUser;
  private boolean distributed;
  private Long totalSwapSpaceSize;
  private Long totalVirtualMemorySize;
  private Long freeVirtualMemory;
  private Long freePhysicalMemory;
  private Long totalVisibleMemorySize;
  private Long sizeStoredInPagingFiles;
  private Long freeSpaceInPagingFiles;
  private Long maxProcessMemorySize;

  public CIM_OperatingSystem(String name, String creationClassName)
  {
    this.name = name;
    this.creationClassName = creationClassName;
  }

  public CIM_OperatingSystem()
  {
  }

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public Set<CIM_DiskDrive> getCim_DiskDrive()
  {
    return this.cim_DiskDrive;
  }

  public void setCim_DiskDrive(Set<CIM_DiskDrive> cim_DiskDrive)
  {
    this.cim_DiskDrive = cim_DiskDrive;
  }

  public String getCodeSet()
  {
    return this.codeSet;
  }

  public void setCodeSet(String codeSet)
  {
    this.codeSet = codeSet;
  }

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
  }

  public String getCsCreationClassName()
  {
    return this.csCreationClassName;
  }

  public void setCsCreationClassName(String csCreationClassName)
  {
    this.csCreationClassName = csCreationClassName;
  }

  public String getCsName()
  {
    return this.csName;
  }

  public void setCsName(String csName)
  {
    this.csName = csName;
  }

  public int getCurrentTimeZone()
  {
    return this.currentTimeZone;
  }

  public void setCurrentTimeZone(int currentTimeZone)
  {
    this.currentTimeZone = currentTimeZone;
  }

  public int getDefaultPageSize()
  {
    return this.defaultPageSize;
  }

  public void setDefaultPageSize(int defaultPageSize)
  {
    this.defaultPageSize = defaultPageSize;
  }

  public String getDescription()
  {
    return this.description;
  }

  public void setDescription(String description)
  {
    this.description = description;
  }

  public boolean isDistributed()
  {
    return this.distributed;
  }

  public void setDistributed(boolean distributed)
  {
    this.distributed = distributed;
  }

  public String getElementName()
  {
    return this.elementName;
  }

  public void setElementName(String elementName)
  {
    this.elementName = elementName;
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

  public Long getFreePhysicalMemory()
  {
    return this.freePhysicalMemory;
  }

  public void setFreePhysicalMemory(Long freePhysicalMemory)
  {
    this.freePhysicalMemory = freePhysicalMemory;
  }

  public Long getFreeSpaceInPagingFiles()
  {
    return this.freeSpaceInPagingFiles;
  }

  public void setFreeSpaceInPagingFiles(Long freeSpaceInPagingFiles)
  {
    this.freeSpaceInPagingFiles = freeSpaceInPagingFiles;
  }

  public Long getFreeVirtualMemory()
  {
    return this.freeVirtualMemory;
  }

  public void setFreeVirtualMemory(Long freeVirtualMemory)
  {
    this.freeVirtualMemory = freeVirtualMemory;
  }

  public int getHealthState()
  {
    return this.healthState;
  }

  public void setHealthState(int healthState)
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

  public String getInstallDate()
  {
    return this.installDate;
  }

  public void setInstallDate(String installDate)
  {
    this.installDate = installDate;
  }

  public String getLanguageEdition()
  {
    return this.languageEdition;
  }

  public void setLanguageEdition(String languageEdition)
  {
    this.languageEdition = languageEdition;
  }

  public Calendar getLastBootUpTime()
  {
    return this.lastBootUpTime;
  }

  public void setLastBootUpTime(Calendar lastBootUpTime)
  {
    this.lastBootUpTime = lastBootUpTime;
  }

  public Calendar getLocalDateTime()
  {
    return this.localDateTime;
  }

  public void setLocalDateTime(Calendar localDateTime)
  {
    this.localDateTime = localDateTime;
  }

  public int getMaxNumberOfProcesses()
  {
    return this.maxNumberOfProcesses;
  }

  public void setMaxNumberOfProcesses(int maxNumberOfProcesses)
  {
    this.maxNumberOfProcesses = maxNumberOfProcesses;
  }

  public int getMaxProcessesPerUser()
  {
    return this.maxProcessesPerUser;
  }

  public void setMaxProcessesPerUser(int maxProcessesPerUser)
  {
    this.maxProcessesPerUser = maxProcessesPerUser;
  }

  public Long getMaxProcessMemorySize()
  {
    return this.maxProcessMemorySize;
  }

  public void setMaxProcessMemorySize(Long maxProcessMemorySize)
  {
    this.maxProcessMemorySize = maxProcessMemorySize;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public int getNumberOfLicensedUsers()
  {
    return this.numberOfLicensedUsers;
  }

  public void setNumberOfLicensedUsers(int numberOfLicensedUsers)
  {
    this.numberOfLicensedUsers = numberOfLicensedUsers;
  }

  public int getNumberOfProcesses()
  {
    return this.numberOfProcesses;
  }

  public void setNumberOfProcesses(int numberOfProcesses)
  {
    this.numberOfProcesses = numberOfProcesses;
  }

  public int getNumberOfUsers()
  {
    return this.numberOfUsers;
  }

  public void setNumberOfUsers(int numberOfUsers)
  {
    this.numberOfUsers = numberOfUsers;
  }

  public String getOperationalStatus()
  {
    return this.operationalStatus;
  }

  public void setOperationalStatus(String operationalStatus)
  {
    this.operationalStatus = operationalStatus;
  }

  public int getOsType()
  {
    return this.osType;
  }

  public void setOsType(int osType)
  {
    this.osType = osType;
  }

  public String getOtherEnabledState()
  {
    return this.otherEnabledState;
  }

  public void setOtherEnabledState(String otherEnabledState)
  {
    this.otherEnabledState = otherEnabledState;
  }

  public String getOtherTypeDescription()
  {
    return this.otherTypeDescription;
  }

  public void setOtherTypeDescription(String otherTypeDescription)
  {
    this.otherTypeDescription = otherTypeDescription;
  }

  public int getPctTotalCPUTime()
  {
    return this.pctTotalCPUTime;
  }

  public void setPctTotalCPUTime(int pctTotalCPUTime)
  {
    this.pctTotalCPUTime = pctTotalCPUTime;
  }

  public int getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(int requestedState)
  {
    this.requestedState = requestedState;
  }

  public Long getSizeStoredInPagingFiles()
  {
    return this.sizeStoredInPagingFiles;
  }

  public void setSizeStoredInPagingFiles(Long sizeStoredInPagingFiles)
  {
    this.sizeStoredInPagingFiles = sizeStoredInPagingFiles;
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

  public String getTimeOfLastStateChange()
  {
    return this.timeOfLastStateChange;
  }

  public void setTimeOfLastStateChange(String timeOfLastStateChange)
  {
    this.timeOfLastStateChange = timeOfLastStateChange;
  }

  public Long getTotalSwapSpaceSize()
  {
    return this.totalSwapSpaceSize;
  }

  public void setTotalSwapSpaceSize(Long totalSwapSpaceSize)
  {
    this.totalSwapSpaceSize = totalSwapSpaceSize;
  }

  public Long getTotalVirtualMemorySize()
  {
    return this.totalVirtualMemorySize;
  }

  public void setTotalVirtualMemorySize(Long totalVirtualMemorySize)
  {
    this.totalVirtualMemorySize = totalVirtualMemorySize;
  }

  public Long getTotalVisibleMemorySize()
  {
    return this.totalVisibleMemorySize;
  }

  public void setTotalVisibleMemorySize(Long totalVisibleMemorySize)
  {
    this.totalVisibleMemorySize = totalVisibleMemorySize;
  }

  public String getVersion()
  {
    return this.version;
  }

  public void setVersion(String version)
  {
    this.version = version;
  }
}
