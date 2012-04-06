package com.dragonflow.siteview.san.array;

import java.util.Set;
import java.util.Vector;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.UnsignedInteger16;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
//import org.hibernate.Session;

import com.dragonflow.siteview.san.beans.*;
import com.dragonflow.siteview.san.util.*;

public class MapSoftwareIdentity
{
  //private Logger logger;
  private String CN = "MapSoftwareIdentity";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  //private Integer computerSystemID;
  //private Session sessionMapSoftwareIdentity;
  CIM_ComputerSystem ComputerSystem;

  public MapSoftwareIdentity( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem)
  {
    //this.logger = Logger.getLogger(this.CN);
//    this.sessionMapSoftwareIdentity = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
//    this.computerSystemID = computerSystemID1;
    //this.logger.debug("Session1 = " + session1);
    //this.logger.debug("cc1 = " + cc1);
    //this.logger.debug("instaceCOP1 = " + instanceCOP1);
    //this.logger.debug("ComputerSystemID1 = " + computerSystemID1);
    mapSoftwareIdentity();
  }

  public boolean mapSoftwareIdentity()
  {
    try {
      CloseableIterator cim_SoftwareIdentityEnum = this.cc.associators(this.instanceCOP, "CIM_InstalledSoftwareIdentity", "CIM_SoftwareIdentity", "System", "InstalledSoftware", false, false, null);
      while (cim_SoftwareIdentityEnum.hasNext())
      {
       // this.logger.debug(this.CN + "enumerated SoftwareIdentity and has more elements");
        CIMInstance cim_SoftwareIdentityCI = (CIMInstance)cim_SoftwareIdentityEnum.next();

        CIM_SoftwareIdentity csi = new CIM_SoftwareIdentity();
        if (cim_SoftwareIdentityEnum == null) continue;
        String instanceId;
        try {
          instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_SoftwareIdentityCI, "InstanceID");
        } catch (Exception e1) {
          instanceId = "Unknown";
        }
        String manufacturer;
        try {
          manufacturer = this.cim_DT.getCIMInstancePropertyValueString(cim_SoftwareIdentityCI, "Manufacturer");
        } catch (Exception e1) {
          manufacturer = "Unknown";
        }
        String versionString;
        try {
          versionString = this.cim_DT.getCIMInstancePropertyValueString(cim_SoftwareIdentityCI, "VersionString");
        } catch (Exception e1) {
          versionString = "Unknown";
        }

        try
        {
          UnsignedInteger16[] classifications = this.cim_DT.getUint16ArrayPropertyValue(cim_SoftwareIdentityCI, "Classifications");
          int classificationsSize = 0;
          if (classifications != null) {
            classificationsSize = classifications.length;
          }
          CIM_Qualifiers cim_Q = new CIM_Qualifiers();
         // this.logger.debug("classificationsSize = " + classificationsSize);
          Vector classificationsString = new Vector();
          for (int x = 0; x < classificationsSize; ++x)
          {
            UnsignedInteger16 classificationsUnsignedInt = classifications[x];
            int classificationsInt = Integer.parseInt(classificationsUnsignedInt.toString());

            String classificationsValue = cim_Q.classification(classificationsInt);

            classificationsString.add(classificationsValue);
          }

          String classificationsFinal = cim_Q.buildStringFromVector(classificationsString, ",");
          csi.setClassification(classificationsFinal);
        } catch (Exception e) {
          csi.setClassification("Unknown");
        }

        csi.setInstanceId(instanceId);
        csi.setManufacturer(manufacturer);
        csi.setVersionString(versionString);
        
        this.ComputerSystem.getCim_SoftwareIdentity().add(csi);
        
        /*

        this.sessionMapSoftwareIdentity.save(csi);
        String softwareIdentityID = this.sessionMapSoftwareIdentity.getIdentifier(csi).toString();
        Integer softwareIdentityIDp = Integer.valueOf(softwareIdentityID);
       // this.logger.debug("softwareIdentityIDp = " + softwareIdentityID);
        CIM_ComputerSystem aComputerSystemForSoftwareIdentity = (CIM_ComputerSystem)this.sessionMapSoftwareIdentity.load(CIM_ComputerSystem.class, this.computerSystemID);
        CIM_SoftwareIdentity aSoftwareIdentity = (CIM_SoftwareIdentity)this.sessionMapSoftwareIdentity.load(CIM_SoftwareIdentity.class, softwareIdentityIDp);
        aComputerSystemForSoftwareIdentity.getCim_SoftwareIdentity().add(aSoftwareIdentity);
        this.sessionMapSoftwareIdentity.save(aComputerSystemForSoftwareIdentity);
        */
      }

    }
    catch (WBEMException ce)
    {
     // this.logger.error(this.CN, ce);
      return false;
    }
    return true;
  }
}