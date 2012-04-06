package com.dragonflow.siteview.san.array;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import javax.cim.CIMDateTime;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.UnsignedInteger16;
import javax.cim.UnsignedInteger32;
import javax.cim.UnsignedInteger64;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
//import org.hibernate.Session;

import com.dragonflow.siteview.san.beans.*;
import com.dragonflow.siteview.san.util.*;

public class MMapFCPorts
{
  //private Logger logger;
  private String CN = "MapFCPorts";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  private String ftag;
 // private Integer computerSystemID;
  public CIM_ComputerSystem  ComputerSystem;
  
  CIM_ComputerSystemController tempComputerSystemController;
  public List<CIM_FCPort> FCPortlist=new ArrayList<CIM_FCPort>();
  CIM_FCPort tempFCPort;

  public MMapFCPorts(WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem  ComputerSystem,String ftag)
  {
    //this.logger = Logger.getLogger(this.CN);
   // this.sessionMapFCPorts = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem = ComputerSystem;
  //  this.logger.debug("Session1 = " + session1);
  //  this.logger.debug("cc1 = " + cc1);
   // this.logger.debug("instaceCOP1 = " + instanceCOP1);
   // this.logger.debug("ComputerSystemID1 = " + computerSystemID1);
   // this.logger.debug("");
    if (ftag.equals("v7000"))
    {
    	v7000mapFCPortData();
    }else
    {	
     mapFCPortData();
    }
  }

  public boolean v7000mapFCPortData()
  {

	    try {
	      CloseableIterator cim_ComputerSystemEnum = this.cc.associators(this.instanceCOP, 
	        "CIM_ComponentCS", "CIM_ComputerSystem", 
	        "GroupComponent", "PartComponent", false, false, 
	        null);

	      while (cim_ComputerSystemEnum.hasNext()) {
	       // this.logger.debug("enumerated computersystem for controller and has more elements");
	        CIMInstance cim_ComputerSystemCI = (CIMInstance)cim_ComputerSystemEnum.next();
	        CIMObjectPath cim_ComputerSystemInstanceCOP = cim_ComputerSystemCI.getObjectPath();
	        if (cim_ComputerSystemEnum != null) {
	          CloseableIterator cim_FCPortEnum = this.cc
	            .associators(cim_ComputerSystemInstanceCOP, 
	            "CIM_SystemDevice", 
	            "CIM_FCPort", "GroupComponent", 
	            "PartComponent", false, false, null);

	          while (cim_FCPortEnum.hasNext()) {
	           // this.logger.debug("enumerated FCPort and has more elements");
	            CIMInstance cim_FCPortCI = (CIMInstance)cim_FCPortEnum.next();
	            CIMObjectPath cim_FCPortCOP = cim_FCPortCI.getObjectPath();
	            tempFCPort = new CIM_FCPort();
	            if (cim_FCPortEnum == null) continue;
	            String permanentAddress;
	            try {
	              permanentAddress = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "PermanentAddress");
	            } catch (Exception e2) {
	              permanentAddress = null;
	            }
	            String systemCreationClassNamePort;
	            try {
	              systemCreationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemCreationClassName");
	            } catch (Exception e2) {
	              systemCreationClassNamePort = null;
	            }
	            String systemNamePort;
	            try {
	              systemNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemName");
	            } catch (Exception e2) {
	              systemNamePort = null;
	            }
	            String creationClassNamePort;
	            try {
	              creationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "CreationClassName");
	            } catch (Exception e2) {
	              creationClassNamePort = null;
	            }
	            String deviceIdPort;
	            try {
	              deviceIdPort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "DeviceID");
	            } catch (Exception e2) {
	              deviceIdPort = null;
	            }
	            String elementNamePort;
	            try {
	              elementNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "ElementName");
	            } catch (Exception e2) {
	              elementNamePort = null;
	            }

	            int usageRestrictionPort;
	            try
	            {
	              usageRestrictionPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "UsageRestriction").intValue();
	            } catch (RuntimeException e1) {
	              usageRestrictionPort = 0;
	            }
	            String usageRestrictionConversion = this.cim_Q.usageRestriction(usageRestrictionPort);
	            try
	            {
	              UnsignedInteger16[] operationalStatusPort = this.cim_DT.getUint16ArrayPropertyValue(cim_FCPortCI, "OperationalStatus");
	              int operationalStatusSize = 0;
	              if (operationalStatusPort != null) {
	                operationalStatusSize = operationalStatusPort.length;
	              }
	             // this.logger.debug("operationalStatusSize = " + operationalStatusSize);
	              Vector portOperationalStatusString = new Vector();
	              for (int x = 0; x < operationalStatusSize; ++x)
	              {
	                UnsignedInteger16 opsInt = operationalStatusPort[x];
	                int operationalStatusInt = Integer.parseInt(opsInt.toString());

	                String operationalStatusValue = this.cim_Q.portOperationalStatus(operationalStatusInt);

	                portOperationalStatusString.add(operationalStatusValue);
	              }

	              String portOperationalStatusFinal = this.cim_Q.buildStringFromVector(portOperationalStatusString, ",");
	              tempFCPort.setOperationalStatus(portOperationalStatusFinal);
	            } catch (Exception e) {
	            	tempFCPort.setOperationalStatus("Unknown");
	            }

	            Long speedPort = null;
	            try {
	              speedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "Speed").longValue());
	            } catch (Exception localException4) {
	            }
	            Long maxSpeedPort = null;
	            try {
	              maxSpeedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "MaxSpeed").longValue());
	            } catch (Exception localException5) {
	            }
	            int portTypePort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "PortType").intValue();
	            String portTypeConversion = this.cim_Q.portType(portTypePort);
	            int linkTechnologyPort;
	            try {
	              linkTechnologyPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "LinkTechnology").intValue();
	            } catch (Exception e) {
	              linkTechnologyPort = 555;
	            }
	            Long supportedMaximumTransmissionUnit = null;
	            try {
	              supportedMaximumTransmissionUnit = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "SupportedMaximumTransmissionUnit").longValue());
	            } catch (Exception localException6) {
	            }
	           // this.logger.debug(this.CN + " FCPort WWN = " + permanentAddress);

	            tempFCPort.setPermanentAddress(permanentAddress);
	            tempFCPort.setSystemCreationClassName(systemCreationClassNamePort);
	            tempFCPort.setSystemName(systemNamePort);
	            tempFCPort.setCreationClassName(creationClassNamePort);
	            tempFCPort.setDeviceId(deviceIdPort);
	            tempFCPort.setElementName(elementNamePort);
	            tempFCPort.setUsageRestriction(usageRestrictionConversion);
	            tempFCPort.setSpeed(speedPort);
	            tempFCPort.setMaxSpeed(maxSpeedPort);
	            tempFCPort.setPortType(portTypeConversion);

	            String linkTechnologyConversion = this.cim_Q.linkTechnology(linkTechnologyPort);
	            if (linkTechnologyPort != 555) {
	            	tempFCPort.setLinkTechnology(linkTechnologyConversion);
	            }
	            tempFCPort.setSupportedMaximumTransmissionUnit(supportedMaximumTransmissionUnit);

	           this.FCPortlist.add(tempFCPort);
	          }
	        }
	      }
	    }
	    catch (WBEMException ce) {
	     // this.logger.error(this.CN, ce);
	      return false;
	    }
	    return true;
	    
  }
  public boolean mapFCPortData()
  {
    try {
      CloseableIterator cim_ComputerSystemEnum = this.cc.associators(this.instanceCOP, 
        "CIM_ComponentCS", "CIM_ComputerSystem", 
        "GroupComponent", "PartComponent", false, false, 
        null);

      while (cim_ComputerSystemEnum.hasNext()) {
       // this.logger.debug("enumerated computersystem for controller and has more elements");
        CIMInstance cim_ComputerSystemCI = (CIMInstance)cim_ComputerSystemEnum.next();
        CIMObjectPath cim_ComputerSystemInstanceCOP = cim_ComputerSystemCI.getObjectPath();
        if (cim_ComputerSystemEnum != null) {
          this.tempComputerSystemController = new CIM_ComputerSystemController();
          String name = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Name");
         // this.logger.debug(this.CN + " Controller Name = " + name);
          String creationClassName = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "CreationClassName");

          int memorySize = 0;
          try {
            memorySize = this.cim_DT.getCIMInstancePropertyUnsignedInt32Value(cim_ComputerSystemCI, "MemorySize").intValue();
          } catch (Exception e) {
          //  this.logger.info("No MemorySize");
          }
          int numPorts = 0;
          try {
            numPorts = this.cim_DT.getCIMInstancePropertyUnsignedInt32Value(cim_ComputerSystemCI, "NumPorts").intValue();
          } catch (Exception e) {
           // this.logger.info("No NumPorts");
          }
          String protocol = null;
          try {
            protocol = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Protocol");
          } catch (Exception e) {
           //this.logger.info("no Protocol");
          }
          String[] roles = (String[])null;
          try {
            roles = this.cim_DT.getStringArrayPropertyValue(cim_ComputerSystemCI, "Roles");

            int rolesSize = 0;
            if (roles != null) {
              rolesSize = roles.length;
            }
            //this.logger.debug("rolesSize = " + rolesSize);
            Vector rolesString = new Vector();
            for (int x = 0; x < rolesSize; ++x) {
              String rolesfinal = roles[x];
              rolesString.add(rolesfinal);
            }

            String rolesfinalf = this.cim_Q.buildStringFromVector(rolesString, ",");
            tempComputerSystemController.setRoles(rolesfinalf);
          } catch (Exception e) {
           // this.logger.debug("No Roles");
          }
          int slotNumber = 0;
          try {
            slotNumber = this.cim_DT.getCIMInstancePropertyUnsignedInt32Value(cim_ComputerSystemCI, "SlotNumber").intValue();
          } catch (Exception e) {
           // this.logger.debug("No SlotNumber");
          }
          int enabledDefault = 0;
          try {
            enabledDefault = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ComputerSystemCI, "EnabledDefault").intValue();
          } catch (Exception e) {
            //this.logger.debug("No EnabledDefault");
          }

          String dedicated = null;
          try
          {
            UnsignedInteger16[] dedicatedArray = this.cim_DT.getUint16ArrayPropertyValue(cim_ComputerSystemCI, "Dedicated");
            int dedicatedSize = 0;
            if (dedicatedArray != null) {
              dedicatedSize = dedicatedArray.length;
            }
            //this.logger.debug("dedicatedSize = " + dedicatedSize);
            String dedicated1;
            if (dedicatedSize == 1) {
              dedicated1 = dedicatedArray[0].toString();
              dedicated = dedicated1; 
            }
            if (dedicatedSize == 2) {
              dedicated1 = dedicatedArray[0].toString();

              String dedicated2 = dedicatedArray[1].toString();

              dedicated = dedicated1 + "," + dedicated2;
             // this.logger.debug("dedicated1 = " + dedicated1 + " dedicated2 = " + dedicated2);
            }
          } catch (Exception e) {
            //this.logger.debug("No Dedicated");
          }
          String nameFormat = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "NameFormat");

          int requestedState = 0;
          try {
            requestedState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ComputerSystemCI, "RequestedState").intValue();
          } catch (Exception e) {
            //this.logger.debug("No RequestedState");
          }
          String requestedStateConversion = this.cim_Q.requestedState(requestedState);

          int enabledState = 0;
          try {
            enabledState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ComputerSystemCI, "EnabledState").intValue();
          } catch (Exception e) {
            //this.logger.debug("No EnabledDefault");
          }

          String statusDescriptions = null;
          try
          {
            String[] statusDescriptionsArray = this.cim_DT.getStringArrayPropertyValue(cim_ComputerSystemCI, "StatusDescriptions");
            int statusDescriptionsSize = 0;
            if (statusDescriptionsArray != null) {
              statusDescriptionsSize = statusDescriptionsArray.length;
            }
            //this.logger.debug("statusDescriptionsSize = " + statusDescriptionsSize);
            String statusDescriptions1;
            if (statusDescriptionsSize == 1) {
              statusDescriptions1 = statusDescriptionsArray[0].toString();
              dedicated = statusDescriptions1; 
            }
            if (statusDescriptionsSize == 2) {
              statusDescriptions1 = statusDescriptionsArray[0].toString();
              String statusDescriptions2 = statusDescriptionsArray[1].toString();
              statusDescriptions = statusDescriptions1 + "," + statusDescriptions2;
             // this.logger.debug("statusDescriptions1 = " + statusDescriptions1 + " statusDescriptions2 = " + statusDescriptions2);
            }
          } catch (Exception e) {
           // this.logger.error("ERROR", e);
          }

          try
          {
             UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(cim_ComputerSystemCI, "OperationalStatus");
            int operationalStatusSize = 0;
            if (operationalStatusArray != null) {
              operationalStatusSize = operationalStatusArray.length;
            }
            //this.logger.debug("opearationalStatusSize = " + operationalStatusSize);
            Vector operationalStatusString = new Vector();
            for (int x = 0; x < operationalStatusSize; ++x)
            {
              UnsignedInteger16 opstsint = operationalStatusArray[x];

              int operationalStatusInt = Integer.parseInt(opstsint.toString());
              String operationalStatusValue = this.cim_Q.diskOperationalStatus(operationalStatusInt);

              operationalStatusString.add(operationalStatusValue);
            }

            String operationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
            tempComputerSystemController.setOperationalStatus(operationalStatusFinal);
          } catch (Exception operationalStatusArray) {
        	  tempComputerSystemController.setOperationalStatus("Unknown");
            //this.logger.error("Operational Status", e);
          }

          String elementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "ElementName");
          String description = null;
          try {
            description = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Description");
          } catch (Exception e) {
            //this.logger.debug("No Description");
          }
          String caption = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Caption");

          tempComputerSystemController.setName(name);
          tempComputerSystemController.setCreationClassName(creationClassName);
          tempComputerSystemController.setMemorySize(memorySize);
          tempComputerSystemController.setNumPorts(numPorts);
          tempComputerSystemController.setProtocol(protocol);
          tempComputerSystemController.setSlotNumber(slotNumber);
          tempComputerSystemController.setEnabledDefault(enabledDefault);
          tempComputerSystemController.setDedicated(dedicated);
          tempComputerSystemController.setNameFormat(nameFormat);
          tempComputerSystemController.setRequestedState(requestedStateConversion);
          tempComputerSystemController.setEnabledState(enabledState);
          tempComputerSystemController.setStatusDescriptions(statusDescriptions);
          tempComputerSystemController.setElementName(elementName);
          tempComputerSystemController.setDescription(description);
          tempComputerSystemController.setCaption(caption);

          //-------
          this.ComputerSystem.getCim_ComputerSystemController().add(tempComputerSystemController);
          
          /*
          this.sessionMapFCPorts.save(ccsc);
          String computerSystemControllerID = this.sessionMapFCPorts.getIdentifier(ccsc).toString();
          Integer computerSystemControllerIDp = Integer.valueOf(computerSystemControllerID);
          //this.logger.debug("computerSystemControllerIDp = " + computerSystemControllerID);
         // CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapFCPorts.load(CIM_ComputerSystem.class, this.computerSystemID);
          CIM_ComputerSystem aComputerSystem =this.ComputerSystem;
          CIM_ComputerSystemController aComputerSystemController = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
          aComputerSystem.getCim_ComputerSystemController().add(aComputerSystemController);
          this.sessionMapFCPorts.save(aComputerSystem);
          */
          try
          {
            CloseableIterator cim_RemoteServiceAccessPointEnum = this.cc.associators(cim_ComputerSystemInstanceCOP, "CIM_HostedAccessPoint", "CIM_RemoteServiceAccessPoint", "Antecedent", "Dependent", false, false, null);
            while (cim_RemoteServiceAccessPointEnum.hasNext())
            {
             // this.logger.debug(this.CN + "enumerated SoftwareIdentity and has more elements");
              CIMInstance cim_RemoteServiceAccessPointCI = (CIMInstance)cim_RemoteServiceAccessPointEnum.next();

              CIM_RemoteServiceAccessPoint crsap = new CIM_RemoteServiceAccessPoint();
              if (cim_RemoteServiceAccessPointEnum != null) {
                String nameAP = this.cim_DT.getCIMInstancePropertyValueString(cim_RemoteServiceAccessPointCI, "Name");
                String creationClassNameAP = this.cim_DT.getCIMInstancePropertyValueString(cim_RemoteServiceAccessPointCI, "CreationClassName");
                String systemNameAP = this.cim_DT.getCIMInstancePropertyValueString(cim_RemoteServiceAccessPointCI, "SystemName");
                String systemCreationClassNameAP = this.cim_DT.getCIMInstancePropertyValueString(cim_RemoteServiceAccessPointCI, "SystemCreationClassName");
                int infoFormatAP = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_RemoteServiceAccessPointCI, "InfoFormat").intValue();
                String accessInfoAP = this.cim_DT.getCIMInstancePropertyValueString(cim_RemoteServiceAccessPointCI, "AccessInfo");
                int enabledDefaultAP = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_RemoteServiceAccessPointCI, "EnabledDefault").intValue();
                int requestedStateAP = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_RemoteServiceAccessPointCI, "RequestedState").intValue();
                int enabledStateAP = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_RemoteServiceAccessPointCI, "EnabledState").intValue();
                String elementNameAP = this.cim_DT.getCIMInstancePropertyValueString(cim_RemoteServiceAccessPointCI, "ElementName");

                crsap.setName(nameAP);
                crsap.setCreationClassName(creationClassNameAP);
                crsap.setSystemName(systemNameAP);
                crsap.setSystemCreationClassName(systemCreationClassNameAP);
                crsap.setInfoFormat(infoFormatAP);
                crsap.setAccessInfo(accessInfoAP);
                crsap.setEnabledDefault(enabledDefaultAP);
                crsap.setRequestedState(requestedStateAP);
                crsap.setEnabledState(enabledStateAP);
                crsap.setElementName(elementNameAP);
                
                this.tempComputerSystemController.getCim_RemoteServiceAccessPoint().add(crsap);

                /*
                this.sessionMapFCPorts.save(crsap);
                String accessPointID = this.sessionMapFCPorts.getIdentifier(crsap).toString();
                Integer accessPointIDp = Integer.valueOf(accessPointID);
               // this.logger.debug("accessPointIDp = " + accessPointID);
                CIM_ComputerSystemController aComputerSystemControllerForAccessPoint = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
                CIM_RemoteServiceAccessPoint aAccessPoint = (CIM_RemoteServiceAccessPoint)this.sessionMapFCPorts.load(CIM_RemoteServiceAccessPoint.class, accessPointIDp);
                aComputerSystemControllerForAccessPoint.getCim_RemoteServiceAccessPoint().add(aAccessPoint);
                this.sessionMapFCPorts.save(aComputerSystemControllerForAccessPoint);
                */
              }

            }

          }
          catch (Exception localException1)
          {
          }

          CloseableIterator cim_SCSIProtocolEndpointEnum = this.cc.associators(cim_ComputerSystemInstanceCOP, "CIM_HostedAccessPoint", "CIM_SCSIProtocolEndpoint", "Antecedent", "Dependent", false, false, null);
          while (cim_SCSIProtocolEndpointEnum.hasNext())
          {
            //this.logger.debug(this.CN + "enumerated SCSIProtocolEndpoint and has more elements");
            CIMInstance cim_SCSIProtocolEndpointCI = (CIMInstance)cim_SCSIProtocolEndpointEnum.next();

            CIM_SCSIProtocolEndpoint cspe = new CIM_SCSIProtocolEndpoint();
            if (cim_SCSIProtocolEndpointEnum != null) {
              String nameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "Name");
              String creationClassNameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "CreationClassName");
              String systemNameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "SystemName");
              String systemCreationClassNameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "SystemCreationClassName");

              int enabledDefaultSPE = 0;
              try {
                enabledDefaultSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "EnabledDefault").intValue();
              } catch (Exception e) {
               // this.logger.debug("No EnabledDefaultSPE");
              }

              int roleSPE = 0;
              try {
                roleSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "Role").intValue();
              } catch (Exception e) {
               // this.logger.debug("No RoleSPE");
              }

              int connectionTypeSPE = 0;
              try {
                connectionTypeSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "ConnectionType").intValue();
              } catch (RuntimeException e) {
               // this.logger.debug("No ConnectionType");
              }

              String otherTypeDescriptionSPE = null;
              try {
                otherTypeDescriptionSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "OtherTypeDescription");
              }
              catch (Exception localException2)
              {
              }
              int protocolIFTypeSPE = 0;
              try {
                protocolIFTypeSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "ProtocolIFType").intValue();
              } catch (Exception e) {
                protocolIFTypeSPE = 0;
              }

              int enabledStateSPE = 555;
              boolean enabledStateSPEDiscovery = true;
              try {
                enabledStateSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "EnabledState").intValue();
              } catch (Exception e) {
                enabledStateSPEDiscovery = false;
              }

              int requestedStateSPE = 0;
              try {
                requestedStateSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "RequestedState").intValue();
              } catch (Exception e) {
                //this.logger.debug("No RequestedStateSPE");
              }

              cspe.setName(nameSPE);
              cspe.setCreationClassName(creationClassNameSPE);
              cspe.setSystemName(systemNameSPE);
              cspe.setSystemCreationClassName(systemCreationClassNameSPE);
              cspe.setEnabledDefault(enabledDefaultSPE);
              cspe.setRole(roleSPE);
              cspe.setConnectionType(connectionTypeSPE);
              cspe.setOtherTypeDescription(otherTypeDescriptionSPE);
              cspe.setProtocolIFType(protocolIFTypeSPE);
              if (enabledStateSPEDiscovery) {
                cspe.setEnabledState(enabledStateSPE);
              }
              cspe.setRequestedState(requestedStateSPE);
              
              
              this.tempComputerSystemController.getCim_SCSIProtocolEndpoint().add(cspe);

              /*
              this.sessionMapFCPorts.save(cspe);
              String scsiProtocolEndpointID = this.sessionMapFCPorts.getIdentifier(cspe).toString();
              Integer scsiProtocolEndpointIDp = Integer.valueOf(scsiProtocolEndpointID);
              //this.logger.debug("endPointIDp = " + scsiProtocolEndpointID);
              CIM_ComputerSystemController aComputerSystemControllerForEndPoint = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
              CIM_SCSIProtocolEndpoint aEndPoint = (CIM_SCSIProtocolEndpoint)this.sessionMapFCPorts.load(CIM_SCSIProtocolEndpoint.class, scsiProtocolEndpointIDp);
              aComputerSystemControllerForEndPoint.getCim_SCSIProtocolEndpoint().add(aEndPoint);
              this.sessionMapFCPorts.save(aComputerSystemControllerForEndPoint);
              */
            }

          }

          try
          {
            CloseableIterator cim_BlockStorageStatisticalDataControllerEnum = this.cc.associators(cim_ComputerSystemInstanceCOP, "CIM_ElementStatisticalData", "CIM_BlockStorageStatisticalData", "ManagedElement", "Stats", false, false, null);
            while (cim_BlockStorageStatisticalDataControllerEnum.hasNext())
            {
              //this.logger.debug(this.CN + "enumerated BlockStorageStatisticalData and has more elements");
              CIMInstance cim_BlockStorageStatisticalDataControllerCI = (CIMInstance)cim_BlockStorageStatisticalDataControllerEnum.next();
              CIM_BlockStorageStatisticalData cbssdController = new CIM_BlockStorageStatisticalData();
              if (cim_BlockStorageStatisticalDataControllerEnum != null) {
                int elementTypeController = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_BlockStorageStatisticalDataControllerCI, "ElementType").intValue();
                cbssdController.setElementType(elementTypeController);
                Long kBytesTransferredController;
                try {
                  kBytesTransferredController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "KBytesTransferred").longValue());
                  cbssdController.setKBytesTransferred(kBytesTransferredController);
                } catch (NullPointerException npe) {
                  kBytesTransferredController = null;
                }
                Long writeIOsController;
                try
                {
                  writeIOsController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "WriteIOs").longValue());
                  cbssdController.setWriteIos(writeIOsController);
                } catch (NullPointerException npe) {
                  writeIOsController = null;
                }
                Long readIOsController;
                try
                {
                  readIOsController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "ReadIOs").longValue());
                  cbssdController.setReadIos(readIOsController);
                } catch (NullPointerException npe) {
                  readIOsController = null;
                }
                Long totalIOsController;
                try
                {
                  totalIOsController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "TotalIOs").longValue());
                  cbssdController.setTotalIos(totalIOsController);
                } catch (NullPointerException npe) {
                  totalIOsController = null;
                }

                try
                {
                  String instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_BlockStorageStatisticalDataControllerCI, "InstanceID");
                  cbssdController.setInstanceId(instanceId);
                }
                catch (NullPointerException localNullPointerException1) {
                }
                try {
                  CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_BlockStorageStatisticalDataControllerCI, "StatisticTime");
                  Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
                  cbssdController.setStatisticTime(cal);
                } catch (Exception e) {
                 // this.logger.error("CS BSP Error", e);
                }
                
                this.tempComputerSystemController.getCim_BlockStorageStatisticalData().add(cbssdController);
                
                /*
                this.sessionMapFCPorts.save(cbssdController);

                String blockStorageStatisticalDataID = this.sessionMapFCPorts.getIdentifier(cbssdController).toString();
                Integer blockStorageStatisticalDataIDp = Integer.valueOf(blockStorageStatisticalDataID);
                //this.logger.debug("blockStorageStatisticalDataIDp = " + blockStorageStatisticalDataIDp);
                //this.logger.debug("blockStorageStatisticalDataID = " + blockStorageStatisticalDataID);
                CIM_ComputerSystemController aControllerForStats = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
                CIM_BlockStorageStatisticalData aBlockStorageStatisticalData = (CIM_BlockStorageStatisticalData)this.sessionMapFCPorts.load(CIM_BlockStorageStatisticalData.class, blockStorageStatisticalDataIDp);
                aControllerForStats.getCim_BlockStorageStatisticalData().add(aBlockStorageStatisticalData);
                this.sessionMapFCPorts.save(aControllerForStats);
                */
              }

            }

          }
          catch (Exception localException3)
          {
          }

          CloseableIterator cim_FCPortEnum = this.cc
            .associators(cim_ComputerSystemInstanceCOP, 
            "CIM_SystemDevice", 
            "CIM_FCPort", "GroupComponent", 
            "PartComponent", false, false, null);

          while (cim_FCPortEnum.hasNext()) {
           // this.logger.debug("enumerated FCPort and has more elements");
            CIMInstance cim_FCPortCI = (CIMInstance)cim_FCPortEnum.next();
            CIMObjectPath cim_FCPortCOP = cim_FCPortCI.getObjectPath();
            tempFCPort = new CIM_FCPort();
            if (cim_FCPortEnum == null) continue;
            String permanentAddress;
            try {
              permanentAddress = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "PermanentAddress");
            } catch (Exception e2) {
              permanentAddress = null;
            }
            String systemCreationClassNamePort;
            try {
              systemCreationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemCreationClassName");
            } catch (Exception e2) {
              systemCreationClassNamePort = null;
            }
            String systemNamePort;
            try {
              systemNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemName");
            } catch (Exception e2) {
              systemNamePort = null;
            }
            String creationClassNamePort;
            try {
              creationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "CreationClassName");
            } catch (Exception e2) {
              creationClassNamePort = null;
            }
            String deviceIdPort;
            try {
              deviceIdPort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "DeviceID");
            } catch (Exception e2) {
              deviceIdPort = null;
            }
            String elementNamePort;
            try {
              elementNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "ElementName");
            } catch (Exception e2) {
              elementNamePort = null;
            }

            int usageRestrictionPort;
            try
            {
              usageRestrictionPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "UsageRestriction").intValue();
            } catch (RuntimeException e1) {
              usageRestrictionPort = 0;
            }
            String usageRestrictionConversion = this.cim_Q.usageRestriction(usageRestrictionPort);
            try
            {
              UnsignedInteger16[] operationalStatusPort = this.cim_DT.getUint16ArrayPropertyValue(cim_FCPortCI, "OperationalStatus");
              int operationalStatusSize = 0;
              if (operationalStatusPort != null) {
                operationalStatusSize = operationalStatusPort.length;
              }
             // this.logger.debug("operationalStatusSize = " + operationalStatusSize);
              Vector portOperationalStatusString = new Vector();
              for (int x = 0; x < operationalStatusSize; ++x)
              {
                UnsignedInteger16 opsInt = operationalStatusPort[x];
                int operationalStatusInt = Integer.parseInt(opsInt.toString());

                String operationalStatusValue = this.cim_Q.portOperationalStatus(operationalStatusInt);

                portOperationalStatusString.add(operationalStatusValue);
              }

              String portOperationalStatusFinal = this.cim_Q.buildStringFromVector(portOperationalStatusString, ",");
              tempFCPort.setOperationalStatus(portOperationalStatusFinal);
            } catch (Exception e) {
            	tempFCPort.setOperationalStatus("Unknown");
            }

            Long speedPort = null;
            try {
              speedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "Speed").longValue());
            } catch (Exception localException4) {
            }
            Long maxSpeedPort = null;
            try {
              maxSpeedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "MaxSpeed").longValue());
            } catch (Exception localException5) {
            }
            int portTypePort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "PortType").intValue();
            String portTypeConversion = this.cim_Q.portType(portTypePort);
            int linkTechnologyPort;
            try {
              linkTechnologyPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "LinkTechnology").intValue();
            } catch (Exception e) {
              linkTechnologyPort = 555;
            }
            Long supportedMaximumTransmissionUnit = null;
            try {
              supportedMaximumTransmissionUnit = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "SupportedMaximumTransmissionUnit").longValue());
            } catch (Exception localException6) {
            }
           // this.logger.debug(this.CN + " FCPort WWN = " + permanentAddress);

            tempFCPort.setPermanentAddress(permanentAddress);
            tempFCPort.setSystemCreationClassName(systemCreationClassNamePort);
            tempFCPort.setSystemName(systemNamePort);
            tempFCPort.setCreationClassName(creationClassNamePort);
            tempFCPort.setDeviceId(deviceIdPort);
            tempFCPort.setElementName(elementNamePort);
            tempFCPort.setUsageRestriction(usageRestrictionConversion);
            tempFCPort.setSpeed(speedPort);
            tempFCPort.setMaxSpeed(maxSpeedPort);
            tempFCPort.setPortType(portTypeConversion);

            String linkTechnologyConversion = this.cim_Q.linkTechnology(linkTechnologyPort);
            if (linkTechnologyPort != 555) {
            	tempFCPort.setLinkTechnology(linkTechnologyConversion);
            }
            tempFCPort.setSupportedMaximumTransmissionUnit(supportedMaximumTransmissionUnit);

           this.tempComputerSystemController.getCim_FCPort().add(tempFCPort);
            /*
            this.sessionMapFCPorts.save(cfcp);
            String portID = this.sessionMapFCPorts.getIdentifier(cfcp).toString();
            Integer fCPortIDp = Integer.valueOf(portID);
            //this.logger.debug("fCPortIDp = " + fCPortIDp);
            //this.logger.debug("computerSystemControllerID = " + this.computerSystemID);

            CIM_ComputerSystemController aComputerSystemControllerForPort = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
            CIM_FCPort aFCPort = (CIM_FCPort)this.sessionMapFCPorts.load(CIM_FCPort.class, fCPortIDp);
            aComputerSystemController.getCim_FCPort().add(aFCPort);
            this.sessionMapFCPorts.save(aComputerSystemControllerForPort);
            */
            try
            {
              CloseableIterator cim_BlockStorageStatisticalData = this.cc
                .associators(cim_FCPortCOP, 
                "CIM_ElementStatisticalData", 
                "CIM_BlockStorageStatisticalData", "ManagedElement", 
                "Stats", false, false, null);

              while (cim_BlockStorageStatisticalData.hasNext()) {
                //this.logger.debug("enumerated BlockStorageStatisticalData and has more elements");
                CIMInstance cim_BlockStorageStatisticalDataCI = (CIMInstance)cim_BlockStorageStatisticalData.next();

                CIM_BlockStorageStatisticalData cbssd = new CIM_BlockStorageStatisticalData();
                if (cim_BlockStorageStatisticalData != null) {
                  int elementType = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_BlockStorageStatisticalDataCI, "ElementType").intValue();
                  cbssd.setElementType(elementType);
                  Long kBytesTransferred;
                  try {
                    kBytesTransferred = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "KBytesTransferred").longValue());
                  } catch (NullPointerException npe) {
                    kBytesTransferred = null;
                  }
                  cbssd.setKBytesTransferred(kBytesTransferred);
                  Long totalIOs;
                  try {
                    totalIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "TotalIOs").longValue());
                  } catch (NullPointerException npe) {
                    totalIOs = null;
                  }
                  cbssd.setTotalIos(totalIOs);
                  try
                  {
                    String instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_BlockStorageStatisticalDataCI, "InstanceID");
                    cbssd.setInstanceId(instanceId);
                  }
                  catch (NullPointerException localNullPointerException2) {
                  }
                  try {
                    CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_BlockStorageStatisticalDataCI, "StatisticTime");
                    Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
                    cbssd.setStatisticTime(cal);
                  } catch (Exception e) {
                  //  this.logger.error("CS BSP Error", e);
                  }

                  
                  /*
                  this.sessionMapFCPorts.save(cbssd);

                  String blockStorageStatisticalDataID = this.sessionMapFCPorts.getIdentifier(cbssd).toString();
                  Integer blockStorageStatisticalDataIDp = Integer.valueOf(blockStorageStatisticalDataID);
                  //this.logger.debug("blockStorageStatisticalDataIDp = " + blockStorageStatisticalDataIDp);
                 // this.logger.debug("blockStorageStatisticalDataID = " + blockStorageStatisticalDataID);

                  CIM_FCPort aFCPortForStats = (CIM_FCPort)this.sessionMapFCPorts.load(CIM_FCPort.class, fCPortIDp);
                  CIM_BlockStorageStatisticalData aBlockStorageStatisticalData = (CIM_BlockStorageStatisticalData)this.sessionMapFCPorts.load(CIM_BlockStorageStatisticalData.class, blockStorageStatisticalDataIDp);
                  aFCPort.getCim_BlockStorageStatisticalData().add(aBlockStorageStatisticalData);
                  this.sessionMapFCPorts.save(aFCPortForStats);
                  */

                 // CCalculationsBSP calculatePortDelta = new CCalculationsBSP();
                 // calculatePortDelta.calculatePortMetric(this.computerSystemID.intValue(), computerSystemControllerIDp.intValue(), fCPortIDp.intValue(), cfcp, cbssd);
                }
              }
            }
            catch (Exception localException7) {
            }
          }
        }
      }
    }
    catch (WBEMException ce) {
     // this.logger.error(this.CN, ce);
      return false;
    }
    return true;
  }
}