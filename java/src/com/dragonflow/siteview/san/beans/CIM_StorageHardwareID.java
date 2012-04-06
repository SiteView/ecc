package com.dragonflow.siteview.san.beans;

import java.util.HashSet;
import java.util.Set;

public class CIM_StorageHardwareID
{
  private Set<CIM_ComputerSystem> cim_ComputerSystem = new HashSet();
  private Integer id;
  private String storageID;

  public String getStorageID()
  {
    return this.storageID;
  }

  public void setStorageID(String storageID) {
    this.storageID = storageID;
  }

  public Set<CIM_ComputerSystem> getCim_ComputerSystem() {
    return this.cim_ComputerSystem;
  }

  public void setCim_ComputerSystem(Set<CIM_ComputerSystem> cim_ComputerSystem) {
    this.cim_ComputerSystem = cim_ComputerSystem;
  }

  public Integer getId() {
    return this.id;
  }

  public void setId(Integer id) {
    this.id = id;
  }
}