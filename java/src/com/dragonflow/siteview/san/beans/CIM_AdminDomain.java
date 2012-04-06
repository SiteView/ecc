package com.dragonflow.siteview.san.beans;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_AdminDomain
{
  private Set cim_ComputerSystem = new HashSet();
  private Set<CIM_ZoneSet> cim_ZoneSet = new HashSet();
  private Integer id;
  private String creationClassName;
  private String name;
  private String nameFormat;
  private String primaryOwnerName;
  private String primaryOwnerContact;
  private String roles;
  private String enabledState;
  private String otherEnabledState;
  private String requestedState;
  private int enabledDefault;
  private Calendar timeOfLastStateChange;
  private Calendar installDate;
  private String operationalStatus;
  private String statusDescriptions;
  private String status;
  private String healthState;
  private String caption;
  private String description;
  private String elementName;

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }

  public Set<?> getCim_ComputerSystem()
  {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set<?> cim_ComputerSystem)
  {
    this.cim_ComputerSystem = cim_ComputerSystem;
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

  public int getEnabledDefault()
  {
    return this.enabledDefault;
  }

  public void setEnabledDefault(int enabledDefault)
  {
    this.enabledDefault = enabledDefault;
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

  public Calendar getInstallDate()
  {
    return this.installDate;
  }

  public void setInstallDate(Calendar installDate)
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

  public String getOtherEnabledState()
  {
    return this.otherEnabledState;
  }

  public void setOtherEnabledState(String otherEnabledState)
  {
    this.otherEnabledState = otherEnabledState;
  }

  public String getPrimaryOwnerContact()
  {
    return this.primaryOwnerContact;
  }

  public void setPrimaryOwnerContact(String primaryOwnerContact)
  {
    this.primaryOwnerContact = primaryOwnerContact;
  }

  public String getPrimaryOwnerName()
  {
    return this.primaryOwnerName;
  }

  public void setPrimaryOwnerName(String primaryOwnerName)
  {
    this.primaryOwnerName = primaryOwnerName;
  }

  public String getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(String requestedState)
  {
    this.requestedState = requestedState;
  }

  public String getRoles()
  {
    return this.roles;
  }

  public void setRoles(String roles)
  {
    this.roles = roles;
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

  public Calendar getTimeOfLastStateChange()
  {
    return this.timeOfLastStateChange;
  }

  public void setTimeOfLastStateChange(Calendar timeOfLastStateChange)
  {
    this.timeOfLastStateChange = timeOfLastStateChange;
  }

  public String getNameFormat()
  {
    return this.nameFormat;
  }

  public void setNameFormat(String nameFormat)
  {
    this.nameFormat = nameFormat;
  }

  public Set<CIM_ZoneSet> getCim_ZoneSet()
  {
    return this.cim_ZoneSet;
  }

  public void setCim_ZoneSet(Set<CIM_ZoneSet> cim_ZoneSet)
  {
    this.cim_ZoneSet = cim_ZoneSet;
  }
}
