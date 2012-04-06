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

public class MapPhysicalPackages
{
 // private Logger logger;
  private String CN = "MapPhysicalPackage";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  //private Integer computerSystemID;
 // private Session sessionMapPhysicalPackage;
  
  private CIM_ComputerSystem ComputerSystem;

  public MapPhysicalPackages( WBEMClient cc1, CIMObjectPath instanceCOP1,CIM_ComputerSystem ComputerSystem)
  {
    //this.logger = Logger.getLogger(this.CN);
//    this.sessionMapPhysicalPackage = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
//    this.computerSystemID = computerSystemID1;
   // this.logger.debug("Session1 = " + session1);
   // this.logger.debug("cc1 = " + cc1);
   // this.logger.debug("instaceCOP1 = " + instanceCOP1);
   // this.logger.debug("ComputerSystemID1 = " + computerSystemID1);
    mapPhysicalPackageData();
  }

  public boolean mapPhysicalPackageData()
  {
    try {
      CloseableIterator cim_PhysicalPackageEnum = this.cc.associators(this.instanceCOP, 
        "CIM_SystemPackaging", "CIM_PhysicalPackage", 
        "Dependent", "Antecedent", false, false, 
        null);

      while (cim_PhysicalPackageEnum.hasNext())
      {
       // this.logger.debug(this.CN + " Enumerated PhysicalPackage and has more elements");
        CIMInstance cim_PhysicalPackageCI = (CIMInstance)cim_PhysicalPackageEnum.next();

        CIM_PhysicalPackage cpp = new CIM_PhysicalPackage();
        if (cim_PhysicalPackageEnum != null) {
          String serialNumber = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "SerialNumber");
          String model = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Model");
          String manufacturer = "Not Available";
          try {
            manufacturer = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Manufacturer");
          } catch (Exception e) {
          //  this.logger.error(this.CN, e);
          }
          String version;
          try {
            version = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Version");
          } catch (NullPointerException npe) {
            version = null;
          }

          cpp.setSerialNumber(serialNumber);
          cpp.setModel(model);
          cpp.setManufacturer(manufacturer);
          cpp.setVersion(version);
          //-------------
          this.ComputerSystem.getCim_PhysicalPackage().add(cpp);

          /*
          this.sessionMapPhysicalPackage.save(cpp);
          String physicalPackageID = this.sessionMapPhysicalPackage.getIdentifier(cpp).toString();
          Integer physicalPackageIDp = Integer.valueOf(physicalPackageID);
         // this.logger.debug("physicalPackageIDp = " + physicalPackageID);
          CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapPhysicalPackage.load(CIM_ComputerSystem.class, this.computerSystemID);
          CIM_PhysicalPackage aPhysicalPackage = (CIM_PhysicalPackage)this.sessionMapPhysicalPackage.load(CIM_PhysicalPackage.class, physicalPackageIDp);
          aComputerSystem.getCim_PhysicalPackage().add(aPhysicalPackage);
          this.sessionMapPhysicalPackage.save(aComputerSystem);
          */
        }
      }
    } catch (WBEMException ce) {
     // this.logger.error(this.CN, ce);
      return false;
    }
    return true;
  }
}