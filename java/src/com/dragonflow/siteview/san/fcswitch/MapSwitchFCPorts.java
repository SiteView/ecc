package com.dragonflow.siteview.san.fcswitch;

import java.util.Calendar;
import java.util.Set;
import java.util.Vector;
import javax.cim.CIMDateTime;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.UnsignedInteger16;
import javax.cim.UnsignedInteger64;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;

//import org.hibernate.Session;

import com.dragonflow.siteview.san.util.*;
import com.dragonflow.siteview.san.beans.*;

public class MapSwitchFCPorts
{
 // private Logger logger;
  private String CN = "MapSwitchFCPorts";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  
  private CIM_ComputerSystem ComputerSystem;
  //private Integer computerSystemID;
  //private Session sessionMapFCPorts;
  
  CIM_FCPort tempFCPort;

  public MapSwitchFCPorts( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem)
  {
  //  this.logger = Logger.getLogger(this.CN);
    //this.sessionMapFCPorts = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
    // this.computerSystemID = computerSystemID1;
    mapFCPortData();
  }

  public boolean mapFCPortData()
  {
    try {
      CloseableIterator cim_FCPortEnum = this.cc.associators(this.instanceCOP, "CIM_SystemDevice", "CIM_FCPort", "GroupComponent", "PartComponent", false, false, null);

      while (cim_FCPortEnum.hasNext()) {
   //     this.logger.info("STARTED SWITCHPORT MAPPING");
        CIMInstance cim_FCPortCI = (CIMInstance)cim_FCPortEnum.next();
        CIMObjectPath cim_FCPortCOP = cim_FCPortCI.getObjectPath();
        tempFCPort= new CIM_FCPort();
        if (cim_FCPortEnum == null) continue;
        try {
          String permanentAddress = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "PermanentAddress");
          tempFCPort.setPermanentAddress(permanentAddress);
        }
        catch (NullPointerException localNullPointerException1) {
        }
        try {
          String systemCreationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemCreationClassName");
          tempFCPort.setSystemCreationClassName(systemCreationClassNamePort);
        }
        catch (NullPointerException localNullPointerException2) {
        }
        try {
          String systemNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemName");
          tempFCPort.setSystemName(systemNamePort);
        }
        catch (NullPointerException localNullPointerException3) {
        }
        try {
          String creationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "CreationClassName");
          tempFCPort.setCreationClassName(creationClassNamePort);
        }
        catch (NullPointerException localNullPointerException4) {
        }
        try {
          String deviceIdPort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "DeviceID");
          tempFCPort.setDeviceId(deviceIdPort);
        }
        catch (NullPointerException localNullPointerException5) {
        }
        try {
          String elementNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "ElementName");
          tempFCPort.setElementName(elementNamePort);
        }
        catch (NullPointerException localNullPointerException6) {
        }
        try {
          int usageRestrictionPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "UsageRestriction").intValue();
          String usageRestrictionConversion = this.cim_Q.usageRestriction(usageRestrictionPort);
          tempFCPort.setUsageRestriction(usageRestrictionConversion);
        }
        catch (NullPointerException localNullPointerException7)
        {
        }
        try {
          UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(cim_FCPortCI, "OperationalStatus");
          int operationalStatusSize = 0;
          if (operationalStatusArray != null) {
            operationalStatusSize = operationalStatusArray.length;
          }
       //   this.logger.debug("opearationalStatusSize = " + operationalStatusSize);
          Vector operationalStatusString = new Vector();
          for (int x = 0; x < operationalStatusSize; ++x)
          {
            UnsignedInteger16 opstsint = operationalStatusArray[x];

            int operationalStatusInt = Integer.parseInt(opstsint.toString());
            String operationalStatusValue = this.cim_Q.diskOperationalStatus(operationalStatusInt);

            operationalStatusString.add(operationalStatusValue);
          }

          String operationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
          tempFCPort.setOperationalStatus(operationalStatusFinal);
        } catch (Exception e) {
        	tempFCPort.setOperationalStatus("Unknown");
        //  this.logger.error("Operational Status", e);
        }
        try
        {
          Long speedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "Speed").longValue());
          tempFCPort.setSpeed(speedPort);
        }
        catch (NullPointerException localNullPointerException8) {
        }
        try {
          Long maxSpeedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "MaxSpeed").longValue());
          tempFCPort.setMaxSpeed(maxSpeedPort);
        }
        catch (NullPointerException localNullPointerException9) {
        }
        try {
          int portTypePort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "PortType").intValue();
          String portTypeConversion = this.cim_Q.portType(portTypePort);
          tempFCPort.setPortType(portTypeConversion);
        }
        catch (NullPointerException localNullPointerException10) {
        }
        try {
          int linkTechnologyPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "LinkTechnology").intValue();
          String linkTechnologyConversion = this.cim_Q.linkTechnology(linkTechnologyPort);
          tempFCPort.setLinkTechnology(linkTechnologyConversion);
        }
        catch (NullPointerException localNullPointerException11) {
        }
        try {
          Long supportedMaximumTransmissionUnit = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "SupportedMaximumTransmissionUnit").longValue());
          tempFCPort.setSupportedMaximumTransmissionUnit(supportedMaximumTransmissionUnit);
        }
        catch (NullPointerException localNullPointerException12)
        {
        }

        //--------------------------
        this.ComputerSystem.getCim_FCPort().add(tempFCPort);
        
        /*
        this.sessionMapFCPorts.save(cfcp);
        String portID = this.sessionMapFCPorts.getIdentifier(cfcp).toString();
        Integer fCPortIDp = Integer.valueOf(portID);
       // this.logger.debug("fCPortIDp = " + fCPortIDp);
       // this.logger.debug("computerSystemControllerID = " + this.computerSystemID);

        CIM_ComputerSystem aComputerSystemForPort = (CIM_ComputerSystem)this.sessionMapFCPorts.load(CIM_ComputerSystem.class, this.computerSystemID);
        CIM_FCPort aFCPort = (CIM_FCPort)this.sessionMapFCPorts.load(CIM_FCPort.class, fCPortIDp);
        aComputerSystemForPort.getCim_FCPort().add(aFCPort);
        this.sessionMapFCPorts.save(aComputerSystemForPort);
        */

        CloseableIterator cim_ProtocolEndpointEnum = this.cc.associators(cim_FCPortCOP, "CIM_DeviceSAPImplementation", "CIM_ProtocolEndpoint", "Antecedent", "Dependent", false, false, null);
        while (cim_ProtocolEndpointEnum.hasNext()) {
       //   this.logger.info("STARTED SWITCHPORT MAPPING: PROTOCOLENDPOINT");

          CIMInstance cim_ProtocolEndpointCI = (CIMInstance)cim_ProtocolEndpointEnum.next();
          CIMObjectPath cim_ProtocolEndpointCOP = cim_ProtocolEndpointCI.getObjectPath();
          CIM_ProtocolEndpoint cpe = new CIM_ProtocolEndpoint();
          if (cim_ProtocolEndpointEnum == null) continue;
          try {
            String systemCreationClassNameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointCI, "SystemCreationClassName");
            cpe.setSystemCreationClassName(systemCreationClassNameProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException13) {
          }
          try {
            String systemNameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointCI, "SystemName");
            cpe.setSystemName(systemNameProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException14) {
          }
          try {
            String creationClassNameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointCI, "CreationClassName");
            cpe.setCreationClassName(creationClassNameProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException15) {
          }
          try {
            String nameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointCI, "Name");
            cpe.setCreationClassName(nameProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException16) {
          }
          try {
            String nameFormatProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointCI, "NameFormat");
            cpe.setCreationClassName(nameFormatProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException17) {
          }
          try {
            int protocolIFTypeProtocolEndpoint = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ProtocolEndpointCI, "ProtocolIFType").intValue();
            cpe.setProtocolIFType(protocolIFTypeProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException18) {
          }
          try {
            int requestedStateProtocolEndpoint = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ProtocolEndpointCI, "RequestedState").intValue();
            cpe.setRequestedState(requestedStateProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException19) {
          }
          try {
            int enabledDefaultProtocolEndpoint = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ProtocolEndpointCI, "EnabledDefault").intValue();
            cpe.setEnabledDefault(enabledDefaultProtocolEndpoint);
          }
          catch (NullPointerException localNullPointerException20)
          {
          }
          //--------------------------
          tempFCPort.getCim_ProtocolEndpoint().add(cpe);
          
          /*
          this.sessionMapFCPorts.save(cpe);
          String protocolEndpointID = this.sessionMapFCPorts.getIdentifier(cpe).toString();
          Integer protocolEndpointIDp = Integer.valueOf(protocolEndpointID);
         // this.logger.debug("protocolEndpointIDp = " + protocolEndpointID);
          CIM_FCPort aFCPortForProtocolEndpoint = (CIM_FCPort)this.sessionMapFCPorts.load(CIM_FCPort.class, fCPortIDp);
          CIM_ProtocolEndpoint aProtocolEndpoint = (CIM_ProtocolEndpoint)this.sessionMapFCPorts.load(CIM_ProtocolEndpoint.class, protocolEndpointIDp);
          aFCPortForProtocolEndpoint.getCim_ProtocolEndpoint().add(aProtocolEndpoint);
          this.sessionMapFCPorts.save(aFCPortForProtocolEndpoint);
          */

          CloseableIterator cim_ProtocolEndpointEndpointEnum = this.cc.associators(cim_ProtocolEndpointCOP, "CIM_ActiveConnection", "CIM_ProtocolEndpoint", "Antecedent", "Dependent", false, false, null);
          while (cim_ProtocolEndpointEndpointEnum.hasNext()) {
          //  this.logger.info("STARTED SWITCHPORT MAPPING: ACTIVECONNECTION");

            CIMInstance cim_ProtocolEndpointEndpointCI = (CIMInstance)cim_ProtocolEndpointEndpointEnum.next();

            CIM_ProtocolEndpointEndpoint cpee = new CIM_ProtocolEndpointEndpoint();
            if (cim_ProtocolEndpointEnum == null) continue;
            try {
              String systemCreationClassNameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointEndpointCI, "SystemCreationClassName");
              cpee.setSystemCreationClassName(systemCreationClassNameProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException21) {
            }
            try {
              String systemNameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointEndpointCI, "SystemName");
              cpee.setSystemName(systemNameProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException22) {
            }
            try {
              String creationClassNameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointEndpointCI, "CreationClassName");
              cpee.setCreationClassName(creationClassNameProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException23) {
            }
            try {
              String nameProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointEndpointCI, "Name");
              cpee.setCreationClassName(nameProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException24) {
            }
            try {
              String nameFormatProtocolEndpoint = this.cim_DT.getCIMInstancePropertyValueString(cim_ProtocolEndpointEndpointCI, "NameFormat");
              cpee.setNameFormat(nameFormatProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException25) {
            }
            try {
              int protocolIFTypeProtocolEndpoint = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ProtocolEndpointEndpointCI, "ProtocolIFType").intValue();
              cpee.setProtocolIFType(protocolIFTypeProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException26) {
            }
            try {
              int requestedStateProtocolEndpoint = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ProtocolEndpointEndpointCI, "RequestedState").intValue();
              cpee.setRequestedState(requestedStateProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException27) {
            }
            try {
              int enabledDefaultProtocolEndpoint = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ProtocolEndpointEndpointCI, "EnabledDefault").intValue();
              cpee.setEnabledDefault(enabledDefaultProtocolEndpoint);
            }
            catch (NullPointerException localNullPointerException28)
            {
            }
            //-----------------
            cpe.getCim_ProtocolEndpointEndpoint().add(cpee);
            
            /*
            this.sessionMapFCPorts.save(cpee);
            String protocolEndpointEndpointID = this.sessionMapFCPorts.getIdentifier(cpee).toString();
            Integer protocolEndpointEndpointIDp = Integer.valueOf(protocolEndpointEndpointID);
          //  this.logger.debug("protocolEndpointEndpointIDp = " + protocolEndpointEndpointID);
            CIM_ProtocolEndpoint aProtocoleEnpointForProtocolEndpointEndpoint = (CIM_ProtocolEndpoint)this.sessionMapFCPorts.load(CIM_ProtocolEndpoint.class, protocolEndpointIDp);
            CIM_ProtocolEndpointEndpoint aProtocolEndpointEndpoint = (CIM_ProtocolEndpointEndpoint)this.sessionMapFCPorts.load(CIM_ProtocolEndpointEndpoint.class, protocolEndpointEndpointIDp);
            aProtocoleEnpointForProtocolEndpointEndpoint.getCim_ProtocolEndpointEndpoint().add(aProtocolEndpointEndpoint);
            this.sessionMapFCPorts.save(aProtocoleEnpointForProtocolEndpointEndpoint);
            */
          //  this.logger.info("FINISHED SWITCH PORT MAPPING");
          }

        }

        CloseableIterator cim_FCPortStatistics = this.cc.associators(cim_FCPortCOP, "CIM_ElementStatisticalData", "CIM_FCPortStatistics", "ManagedElement", "Stats", false, false, null);
        while (cim_FCPortStatistics.hasNext()) {
        //  this.logger.info("STARTED SWITCHPORT MAPPING: DEVICESAPIMPL");

        //  this.logger.debug(this.CN + "enumerated FCPortStatistics and has more elements");
          CIMInstance cim_FCPortStatisticsCI = (CIMInstance)cim_FCPortStatistics.next();
          CIM_FCPortStatistics fcPortStatistics = new CIM_FCPortStatistics();
          if (cim_FCPortStatistics == null) continue;
          String instanceId;
          try {
            instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortStatisticsCI, "InstanceID");
            fcPortStatistics.setInstanceId(instanceId);
          } catch (Exception e1) {
            instanceId = null;
          }
          String elementName;
          try
          {
            elementName = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortStatisticsCI, "ElementName");
            fcPortStatistics.setElementName(elementName);
          } catch (Exception e1) {
            elementName = null;
          }
          Long bytesTransmitted;
          try {
            bytesTransmitted = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "BytesTransmitted").longValue());
            fcPortStatistics.setBytesTransmitted(bytesTransmitted);
          } catch (NullPointerException npe) {
            bytesTransmitted = null;
          }
          Long bytesReceived;
          try
          {
            bytesReceived = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "BytesReceived").longValue());
            fcPortStatistics.setBytesReceived(bytesReceived);
          } catch (NullPointerException npe) {
            bytesReceived = null;
          }
          Long packetsTransmitted;
          try
          {
            packetsTransmitted = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "PacketsTransmitted").longValue());
            fcPortStatistics.setPacketsTransmitted(packetsTransmitted);
          } catch (NullPointerException npe) {
            packetsTransmitted = null;
          }
          Long packetsReceived;
          try
          {
            packetsReceived = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "PacketsReceived").longValue());
            fcPortStatistics.setPacketsReceived(packetsReceived);
          } catch (NullPointerException npe) {
            packetsReceived = null;
          }
          Long linkFailures;
          try
          {
            linkFailures = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "LinkFailures").longValue());
            fcPortStatistics.setLinkFailures(linkFailures);
          } catch (NullPointerException npe) {
            linkFailures = null;
          }
          try
          {
            CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_FCPortStatisticsCI, "StatisticTime");
            Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
            fcPortStatistics.setStatisticTime(cal);
          } catch (Exception e) {
           // this.logger.error("CS FCPortStatistics StatisticTime Error", e);
          }
          
          //-----------------
          tempFCPort.getCim_FCPortStatisticsSwitch().add(fcPortStatistics);
          
          /*
          this.sessionMapFCPorts.save(fcPortStatistics);

          String fcPortStatisticsID = this.sessionMapFCPorts.getIdentifier(fcPortStatistics).toString();
          Integer fcPortStatisticsIDp = Integer.valueOf(fcPortStatisticsID);
         // this.logger.info("fcPortStatisticsIDp = " + fcPortStatisticsIDp);
        //  this.logger.info("fcPortStatisticsID = " + fcPortStatisticsID);
          CIM_FCPort aPortForStats = (CIM_FCPort)this.sessionMapFCPorts.load(CIM_FCPort.class, fCPortIDp);
          CIM_FCPortStatistics aFCPortStatistics = (CIM_FCPortStatistics)this.sessionMapFCPorts.load(CIM_FCPortStatistics.class, fcPortStatisticsIDp);
          aPortForStats.getCim_FCPortStatisticsSwitch().add(aFCPortStatistics);
          this.sessionMapFCPorts.save(aPortForStats);
          */
        }
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