package com.dragonflow.siteview.san.array;

import java.util.Calendar;
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

public class MapFCPortsFujitsuE6000
{
//  private Logger logger;
  private String CN = "MapFCPortsFujitsuE6000";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  
  CIM_ComputerSystem ComputerSystem;
  CIM_ComputerSystemController tempComputerSystemController;
  CIM_FCPort tempFCPort;
 // private Integer computerSystemID;
 // private Session sessionMapFCPorts;

  public MapFCPortsFujitsuE6000( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem)
  {
  //  this.logger = Logger.getLogger(this.CN);
   // this.sessionMapFCPorts = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
   // this.computerSystemID = computerSystemID1;

    mapFCPortData();
  }

  public boolean mapFCPortData()
  {
    try
    {
      CloseableIterator cim_ComputerSystemEnum = this.cc.associators(this.instanceCOP, 
        "CIM_ComponentCS", "CIM_ComputerSystem", 
        "GroupComponent", "PartComponent", false, false, 
        null);

      while (cim_ComputerSystemEnum.hasNext()) {
    //    this.logger.info("enumerated computersystem for controller and has more elements");
        CIMInstance cim_ComputerSystemCI = (CIMInstance)cim_ComputerSystemEnum.next();
        CIMObjectPath cim_ComputerSystemInstanceCOP = cim_ComputerSystemCI.getObjectPath();
        if (cim_ComputerSystemEnum != null) {
        	tempComputerSystemController = new CIM_ComputerSystemController();
          String name = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Name");
      //    this.logger.info(this.CN + " Controller Name = " + name);
          String creationClassName = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "CreationClassName");

          int memorySize = 0;
          try {
            memorySize = this.cim_DT.getCIMInstancePropertyUnsignedInt32Value(cim_ComputerSystemCI, "MemorySize").intValue();
          } catch (Exception e) {
        //    this.logger.info("No MemorySize");
          }
          int numPorts = 0;
          try {
            numPorts = this.cim_DT.getCIMInstancePropertyUnsignedInt32Value(cim_ComputerSystemCI, "NumPorts").intValue();
          } catch (Exception e) {
          //  this.logger.info("No NumPorts");
          }
          String protocol = null;
          try {
            protocol = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Protocol");
          } catch (Exception e) {
          //  this.logger.info("no Protocol");
          }
          String roles = null;
          try {
            roles = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "roles");
          } catch (Exception e) {
           // this.logger.info("No Roles");
          }
          int slotNumber = 0;
          try {
            slotNumber = this.cim_DT.getCIMInstancePropertyUnsignedInt32Value(cim_ComputerSystemCI, "SlotNumber").intValue();
          } catch (Exception e) {
          //  this.logger.info("No SlotNumber");
          }
          int enabledDefault = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ComputerSystemCI, "EnabledDefault").intValue();

          String dedicated = null;
          try
          {
            UnsignedInteger16[] dedicatedArray = this.cim_DT.getUint16ArrayPropertyValue(cim_ComputerSystemCI, "Dedicated");
            int dedicatedSize = 0;
            if (dedicatedArray != null) {
              dedicatedSize = dedicatedArray.length;
            }
           // this.logger.debug("dedicatedSize = " + dedicatedSize);
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
          int requestedState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ComputerSystemCI, "RequestedState").intValue();
          String requestedStateConversion = this.cim_Q.requestedState(requestedState);
          int enabledState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ComputerSystemCI, "EnabledState").intValue();

          String statusDescriptions = null;
          Vector statusDescriptionsVector = this.cim_DT.getCIMInstancePropertyValueVector(cim_ComputerSystemCI, "StatusDescriptions");
          int statusDescriptionsSize = 0;
          if (statusDescriptionsVector != null) {
            statusDescriptionsSize = statusDescriptionsVector.size();
          }
          //this.logger.debug("statusDescriptionsSize = " + statusDescriptionsSize);
          String statusDescriptions1;
          if (statusDescriptionsSize == 1) {
            statusDescriptions1 = statusDescriptionsVector.get(0).toString();
            dedicated = statusDescriptions1;
          }
          else if (statusDescriptionsSize == 2) {
            statusDescriptions1 = statusDescriptionsVector.get(0).toString();
            String statusDescriptions2 = statusDescriptionsVector.get(1).toString();
            statusDescriptions = statusDescriptions1 + "," + statusDescriptions2;
           // this.logger.debug("statusDescriptions1 = " + statusDescriptions1 + " statusDescriptions2 = " + statusDescriptions2);
          }

          try
          {
            Vector operationalStatusPort = this.cim_DT.getCIMInstancePropertyValueVector(cim_ComputerSystemCI, "OperationalStatus");
            int operationalStatusSize = 0;
            if (operationalStatusPort != null) {
              operationalStatusSize = operationalStatusPort.size();
            }
           // this.logger.debug("operationalStatusSize = " + operationalStatusSize);
            Vector portOperationalStatusString = new Vector();
            for (int x = 0; x < operationalStatusSize; ++x)
            {
              int operationalStatusInt = Integer.parseInt(operationalStatusPort.get(x).toString());

              String operationalStatusValue = this.cim_Q.portOperationalStatus(operationalStatusInt);

              portOperationalStatusString.add(operationalStatusValue);
            }

            String portOperationalStatusFinal = this.cim_Q.buildStringFromVector(portOperationalStatusString, ",");
            tempComputerSystemController.setOperationalStatus(portOperationalStatusFinal);
          } catch (Exception e) {
        	  tempComputerSystemController.setOperationalStatus("Unknown");
          }

          String elementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "ElementName");
          String description = null;
          try {
            description = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Description");
          } catch (Exception e) {
          //  this.logger.debug("No Description");
          }
          String caption = this.cim_DT.getCIMInstancePropertyValueString(cim_ComputerSystemCI, "Caption");

          tempComputerSystemController.setName(name);
          tempComputerSystemController.setCreationClassName(creationClassName);
          tempComputerSystemController.setMemorySize(memorySize);
          tempComputerSystemController.setNumPorts(numPorts);
          tempComputerSystemController.setProtocol(protocol);
          tempComputerSystemController.setRoles(roles);
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
          
          //----------------------
          this.ComputerSystem.getCim_ComputerSystemController().add(tempComputerSystemController);

          /*
          this.sessionMapFCPorts.save(ccsc);
          String computerSystemControllerID = this.sessionMapFCPorts.getIdentifier(ccsc).toString();
          Integer computerSystemControllerIDp = Integer.valueOf(computerSystemControllerID);
         // this.logger.debug("computerSystemControllerIDp = " + computerSystemControllerID);
          CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapFCPorts.load(CIM_ComputerSystem.class, this.computerSystemID);
          CIM_ComputerSystemController aComputerSystemController = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
          aComputerSystem.getCim_ComputerSystemController().add(aComputerSystemController);
          this.sessionMapFCPorts.save(aComputerSystem);
          */

          CloseableIterator cim_RemoteServiceAccessPointEnum = this.cc.associators(cim_ComputerSystemInstanceCOP, "CIM_HostedAccessPoint", "CIM_RemoteServiceAccessPoint", "Antecedent", "Dependent", false, false, null);
          while (cim_RemoteServiceAccessPointEnum.hasNext())
          {
        //    this.logger.debug(this.CN + "enumerated SoftwareIdentity and has more elements");
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
              //-----------
              
              tempComputerSystemController.getCim_RemoteServiceAccessPoint().add(crsap);

              /*
              this.sessionMapFCPorts.save(crsap);
              String accessPointID = this.sessionMapFCPorts.getIdentifier(crsap).toString();
              Integer accessPointIDp = Integer.valueOf(accessPointID);
         //     this.logger.debug("accessPointIDp = " + accessPointID);
              CIM_ComputerSystemController aComputerSystemControllerForAccessPoint = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
              CIM_RemoteServiceAccessPoint aAccessPoint = (CIM_RemoteServiceAccessPoint)this.sessionMapFCPorts.load(CIM_RemoteServiceAccessPoint.class, accessPointIDp);
              aComputerSystemControllerForAccessPoint.getCim_RemoteServiceAccessPoint().add(aAccessPoint);
              this.sessionMapFCPorts.save(aComputerSystemControllerForAccessPoint);
              */
            }

          }

          CloseableIterator cim_SCSIProtocolEndpointEnum = this.cc.associators(cim_ComputerSystemInstanceCOP, "CIM_HostedAccessPoint", "CIM_SCSIProtocolEndpoint", "Antecedent", "Dependent", false, false, null);
          while (cim_SCSIProtocolEndpointEnum.hasNext())
          {
       //     this.logger.debug(this.CN + "enumerated SCSIProtocolEndpoint and has more elements");
            CIMInstance cim_SCSIProtocolEndpointCI = (CIMInstance)cim_SCSIProtocolEndpointEnum.next();

            CIM_SCSIProtocolEndpoint cspe = new CIM_SCSIProtocolEndpoint();
            if (cim_SCSIProtocolEndpointEnum != null) {
              String nameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "Name");
              String creationClassNameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "CreationClassName");
              String systemNameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "SystemName");
              String systemCreationClassNameSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "SystemCreationClassName");
              int enabledDefaultSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "EnabledDefault").intValue();
              int roleSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "Role").intValue();
              int connectionTypeSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "ConnectionType").intValue();
              String otherTypeDescriptionSPE = this.cim_DT.getCIMInstancePropertyValueString(cim_SCSIProtocolEndpointCI, "OtherTypeDescription");
              int protocolIFTypeSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "ProtocolIFType").intValue();
              int enabledStateSPE = 555;
              boolean enabledStateSPEDiscovery = true;
              try {
                enabledStateSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "EnabledState").intValue();
              } catch (NullPointerException npe) {
                enabledStateSPEDiscovery = false;
              }
              int requestedStateSPE = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_SCSIProtocolEndpointCI, "RequestedState").intValue();

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
              
              //---------------------
              
               tempComputerSystemController.getCim_SCSIProtocolEndpoint().add(cspe);

              /*
               * 
               * 
              this.sessionMapFCPorts.save(cspe);
              String scsiProtocolEndpointID = this.sessionMapFCPorts.getIdentifier(cspe).toString();
              Integer scsiProtocolEndpointIDp = Integer.valueOf(scsiProtocolEndpointID);
         //     this.logger.debug("endPointIDp = " + scsiProtocolEndpointID);
              CIM_ComputerSystemController aComputerSystemControllerForEndPoint = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
              CIM_SCSIProtocolEndpoint aEndPoint = (CIM_SCSIProtocolEndpoint)this.sessionMapFCPorts.load(CIM_SCSIProtocolEndpoint.class, scsiProtocolEndpointIDp);
              aComputerSystemControllerForEndPoint.getCim_SCSIProtocolEndpoint().add(aEndPoint);
              this.sessionMapFCPorts.save(aComputerSystemControllerForEndPoint);
              *
              *
              */
            }

          }

          CloseableIterator cim_BlockStorageStatisticalDataControllerEnum = this.cc.associators(cim_ComputerSystemInstanceCOP, "CIM_ElementStatisticalData", "CIM_BlockStorageStatisticalData", "ManagedElement", "Stats", false, false, null);
          while (cim_BlockStorageStatisticalDataControllerEnum.hasNext())
          {
        //    this.logger.debug(this.CN + "enumerated BlockStorageStatisticalData and has more elements");
            CIMInstance cim_BlockStorageStatisticalDataControllerCI = (CIMInstance)cim_BlockStorageStatisticalDataControllerEnum.next();
            CIM_BlockStorageStatisticalData cbssdController = new CIM_BlockStorageStatisticalData();
            if (cim_BlockStorageStatisticalDataControllerEnum != null) {
              int elementTypeController = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_BlockStorageStatisticalDataControllerCI, "ElementType").intValue();
              cbssdController.setElementType(elementTypeController);
              Long kBytesTransferredController;
              try {
                kBytesTransferredController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "KBytesTransferred").longValue());
              } catch (NullPointerException npe) {
                kBytesTransferredController = null;
              }
              cbssdController.setKBytesTransferred(kBytesTransferredController);
              Long writeIOsController;
              try {
                writeIOsController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "WriteIOs").longValue());
              } catch (NullPointerException npe) {
                writeIOsController = null;
              }
              cbssdController.setWriteIos(writeIOsController);
              Long readIOsController;
              try {
                readIOsController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "ReadIOs").longValue());
              } catch (NullPointerException npe) {
                readIOsController = null;
              }
              cbssdController.setReadIos(readIOsController);
              Long totalIOsController;
              try {
                totalIOsController = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataControllerCI, "TotalIOs").longValue());
              } catch (NullPointerException npe) {
                totalIOsController = null;
              }
              cbssdController.setTotalIos(totalIOsController);
              try
              {
                CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_BlockStorageStatisticalDataControllerCI, "StatisticTime");
                Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
                cbssdController.setStatisticTime(cal);
              } catch (Exception e) {
              //  this.logger.error("CS BSP Error", e);
              }
              
              //----------------
              
               tempComputerSystemController.getCim_BlockStorageStatisticalData().add(cbssdController);
              
           /*
              String blockStorageStatisticalDataID = this.sessionMapFCPorts.getIdentifier(cbssdController).toString();
              Integer blockStorageStatisticalDataIDp = Integer.valueOf(blockStorageStatisticalDataID);
             // this.logger.debug("blockStorageStatisticalDataIDp = " + blockStorageStatisticalDataIDp);
              //this.logger.debug("blockStorageStatisticalDataID = " + blockStorageStatisticalDataID);
              CIM_ComputerSystemController aControllerForStats = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
              CIM_BlockStorageStatisticalData aBlockStorageStatisticalData = (CIM_BlockStorageStatisticalData)this.sessionMapFCPorts.load(CIM_BlockStorageStatisticalData.class, blockStorageStatisticalDataIDp);
              aControllerForStats.getCim_BlockStorageStatisticalData().add(aBlockStorageStatisticalData);
              this.sessionMapFCPorts.save(aControllerForStats);
              */
            }

          }

          CloseableIterator cim_FCPortEnum = this.cc.associators(this.instanceCOP, "CIM_SystemDevice", "CIM_FCPort", "GroupComponent", "PartComponent", false, false, null);

          while (cim_FCPortEnum.hasNext()) {
            //this.logger.info("enumerated FCPort and has more elements");
            CIMInstance cim_FCPortCI = (CIMInstance)cim_FCPortEnum.next();
            CIMObjectPath cim_FCPortCOP = cim_FCPortCI.getObjectPath();
            tempFCPort = new CIM_FCPort();
            if (cim_FCPortEnum != null) {
              String permanentAddress = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "PermanentAddress");
              String systemCreationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemCreationClassName");
              String systemNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemName");
              String creationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "CreationClassName");
              String deviceIdPort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "DeviceID");
              String elementNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "ElementName");
              int usageRestrictionPort;
              try {
                usageRestrictionPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "UsageRestriction").intValue();
              } catch (Exception e) {
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
              //  this.logger.debug("operationalStatusSize = " + operationalStatusSize);
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

              try
              {
                Long speedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "Speed").longValue());
                tempFCPort.setSpeed(speedPort);
              } catch (NullPointerException localNullPointerException1) {
              }
              Long maxSpeedPort = null;
              try {
                maxSpeedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "MaxSpeed").longValue());
              } catch (NullPointerException localNullPointerException2) {
              }
              String portTypeConversion = null;
              try {
                int portTypePort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "PortType").intValue();
                portTypeConversion = this.cim_Q.portType(portTypePort);
              } catch (NullPointerException localNullPointerException3) {
              }
              int linkTechnologyPort;
              try {
                linkTechnologyPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "LinkTechnology").intValue();
              } catch (Exception e) {
                linkTechnologyPort = 555;
              }
              Long supportedMaximumTransmissionUnit = null;
              try {
                supportedMaximumTransmissionUnit = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "SupportedMaximumTransmissionUnit").longValue());
              } catch (Exception localException1) {
              }
             // this.logger.debug(this.CN + " FCPort WWN = " + permanentAddress);

              tempFCPort.setPermanentAddress(permanentAddress);
              tempFCPort.setSystemCreationClassName(systemCreationClassNamePort);
              tempFCPort.setSystemName(systemNamePort);
              tempFCPort.setCreationClassName(creationClassNamePort);
              tempFCPort.setDeviceId(deviceIdPort);
              tempFCPort.setElementName(elementNamePort);
              tempFCPort.setUsageRestriction(usageRestrictionConversion);

              tempFCPort.setMaxSpeed(maxSpeedPort);
              tempFCPort.setPortType(portTypeConversion);

              String linkTechnologyConversion = this.cim_Q.linkTechnology(linkTechnologyPort);
              if (linkTechnologyPort != 555) {
            	  tempFCPort.setLinkTechnology(linkTechnologyConversion);
              }
              tempFCPort.setSupportedMaximumTransmissionUnit(supportedMaximumTransmissionUnit);

             
              //--------------
              tempComputerSystemController.getCim_FCPort().add(tempFCPort);
              
              
              /*
              this.sessionMapFCPorts.save(cfcp);
              String portID = this.sessionMapFCPorts.getIdentifier(cfcp).toString();
              Integer fCPortIDp = Integer.valueOf(portID);
             // this.logger.debug("fCPortIDp = " + fCPortIDp);
             // this.logger.debug("computerSystemControllerID = " + this.computerSystemID);

              CIM_ComputerSystemController aComputerSystemControllerForPort = (CIM_ComputerSystemController)this.sessionMapFCPorts.load(CIM_ComputerSystemController.class, computerSystemControllerIDp);
              CIM_FCPort aFCPort = (CIM_FCPort)this.sessionMapFCPorts.load(CIM_FCPort.class, fCPortIDp);
              aComputerSystemController.getCim_FCPort().add(aFCPort);
              this.sessionMapFCPorts.save(aComputerSystemControllerForPort);
              */

              CloseableIterator cim_BlockStorageStatisticalData = this.cc
                .associators(cim_FCPortCOP, 
                "CIM_ElementStatisticalData", 
                "CIM_BlockStorageStatisticalData", "ManagedElement", 
                "Stats", false, false, null);

              while (cim_BlockStorageStatisticalData.hasNext()) {
             //   this.logger.debug("enumerated BlockStorageStatisticalData and has more elements");
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
                  catch (NullPointerException localNullPointerException4) {
                  }
                  try {
                    CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_BlockStorageStatisticalDataCI, "StatisticTime");
                    Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
                    cbssd.setStatisticTime(cal);
                  } catch (Exception e) {
               //     this.logger.error("CS BSP Error", e);
                  }
                  //-------------
                  
                   tempFCPort.getCim_BlockStorageStatisticalData().add(cbssd);

                  /*
                  this.sessionMapFCPorts.save(cbssd);

                  String blockStorageStatisticalDataID = this.sessionMapFCPorts.getIdentifier(cbssd).toString();
                  Integer blockStorageStatisticalDataIDp = Integer.valueOf(blockStorageStatisticalDataID);
               //   this.logger.debug("blockStorageStatisticalDataIDp = " + blockStorageStatisticalDataIDp);
               //   this.logger.debug("blockStorageStatisticalDataID = " + blockStorageStatisticalDataID);

                  CIM_FCPort aFCPortForStats = (CIM_FCPort)this.sessionMapFCPorts.load(CIM_FCPort.class, fCPortIDp);
                  CIM_BlockStorageStatisticalData aBlockStorageStatisticalData = (CIM_BlockStorageStatisticalData)this.sessionMapFCPorts.load(CIM_BlockStorageStatisticalData.class, blockStorageStatisticalDataIDp);
                  aFCPort.getCim_BlockStorageStatisticalData().add(aBlockStorageStatisticalData);
                  this.sessionMapFCPorts.save(aFCPortForStats);
                  */

                //  CalculationsBSP calculatePortDelta = new CalculationsBSP();
                //  calculatePortDelta.calculatePortMetric(this.computerSystemID.intValue(), computerSystemControllerIDp.intValue(), fCPortIDp.intValue(), cfcp, cbssd);
                }
              }
            }
          }
        }
      }
    } catch (WBEMException ce) {
      //this.logger.error("MapFCPortsFujitsu", ce);
      return false;
    }
    return true;
  }
}