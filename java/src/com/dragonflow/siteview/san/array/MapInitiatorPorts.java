package com.dragonflow.siteview.san.array;

import java.util.Set;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
//import org.hibernate.Session;

import com.dragonflow.siteview.san.beans.*;
import com.dragonflow.siteview.san.util.*;

public class MapInitiatorPorts
{
 // private Logger logger;
  private String CN = "MapInitiatorPorts";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
 // private Session sessionMapInitiatorPorts;
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  //private Integer computerSystemID;
  private CIM_ComputerSystem ComputerSystem;

  public MapInitiatorPorts( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem)
  {
   // this.logger = Logger.getLogger(this.CN);
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem = ComputerSystem;
   // this.logger.debug("Session1 = " + session1);
   // this.logger.debug("cc1 = " + cc1);
   // this.logger.debug("instaceCOP1 = " + instanceCOP1);
   // this.logger.debug("ComputerSystemID1 = " + computerSystemID1);
   // this.logger.debug("");
    mapInitiatorPortData();
  }

  public boolean mapInitiatorPortData()
  {
    try {
      CloseableIterator cim_SCSIProtocolControllerEnum = this.cc.associators(this.instanceCOP, 
        "CIM_SystemDevice", "CIM_SCSIProtocolController", 
        "GroupComponent", "PartComponent", false, false, 
        null);

      while (cim_SCSIProtocolControllerEnum.hasNext()) {
      //  this.logger.debug("enumerated scsiprotocolcontroller and has more elements");
        CIMInstance cim_SCSIProtocolControllerCI = (CIMInstance)cim_SCSIProtocolControllerEnum.next();
        CIMObjectPath cim_SCSIProtocolControllerInstanceCOP = cim_SCSIProtocolControllerCI.getObjectPath();
        CIM_StorageHardwareID cshid = new CIM_StorageHardwareID();
        if (cim_SCSIProtocolControllerEnum == null)
          continue;
        try {
          CloseableIterator cim_AuthorizedPrivilegeEnum = this.cc.associators(cim_SCSIProtocolControllerInstanceCOP, "CIM_AuthorizedTarget", "CIM_AuthorizedPrivilege", "TargetElement", "Privilege", false, false, null);
          while (cim_AuthorizedPrivilegeEnum.hasNext()) {
        //    this.logger.debug("enumerated authorizedprivilege and has more elements");
            CIMInstance cim_AuthorizedPrivilegeCI = (CIMInstance)cim_AuthorizedPrivilegeEnum.next();
            CIMObjectPath cim_AuthorizedPrivilegeCOP = cim_AuthorizedPrivilegeCI.getObjectPath();
            if (cim_AuthorizedPrivilegeEnum == null)
              continue;
            CloseableIterator cim_StorageHardwareIDEnum = this.cc
              .associators(cim_AuthorizedPrivilegeCOP, 
              "CIM_AuthorizedSubject", 
              "CIM_StorageHardwareID", "Privilege", 
              "PrivilegedElement", false, false, null);
            while (cim_StorageHardwareIDEnum.hasNext()) {
          //    this.logger.debug("enumerated storagehardwareid and has more elements");
              CIMInstance cim_StorageHardwareIDCI = (CIMInstance)cim_StorageHardwareIDEnum.next();

              String initiatorPort = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageHardwareIDCI, "StorageID");
           //   this.logger.debug(this.CN + " initiatorPort = " + initiatorPort);
              cshid.setStorageID(initiatorPort);
              //----------------------
              
              this.ComputerSystem.getCim_StorageHardwareID().add(cshid);

              /*
              this.sessionMapInitiatorPorts.save(cshid);
              String storageHardwareID = this.sessionMapInitiatorPorts.getIdentifier(cshid).toString();
              Integer storageHardwareIDp = Integer.valueOf(storageHardwareID);
            //  this.logger.debug("storageHardwareIDp = " + storageHardwareIDp);
            //  this.logger.debug("computerSystemID = " + this.computerSystemID);
              CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapInitiatorPorts.load(CIM_ComputerSystem.class, this.computerSystemID);
              CIM_StorageHardwareID aStorageHardwareID = (CIM_StorageHardwareID)this.sessionMapInitiatorPorts.load(CIM_StorageHardwareID.class, storageHardwareIDp);
              aComputerSystem.getCim_StorageHardwareID().add(aStorageHardwareID);
              this.sessionMapInitiatorPorts.save(aComputerSystem);
              */
            }

          }

        }
        catch (Exception e)
        {
         // this.logger.error(this.CN, e);
        }
      }
    }
    catch (WBEMException ce) {
      //this.logger.error(this.CN, ce);
      return false;
    }
    return true;
  }
}