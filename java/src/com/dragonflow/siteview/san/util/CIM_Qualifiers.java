package com.dragonflow.siteview.san.util;

import java.util.Vector;

public class CIM_Qualifiers
{
  public String buildStringFromVector(Vector<?> v, String separator)
  {
    StringBuffer result = new StringBuffer();
    if (v.size() > 0) {
      result.append(v.get(0));
      for (int i = 1; i < v.size(); ++i) {
        result.append(separator);
        result.append(v.get(i));
      }
    }
    return result.toString();
  }

  public String operationalStatus(int status)
  {
    String operationalStatusConversion = null;
    switch (status)
    {
    case 0:
      operationalStatusConversion = "Unknown"; break;
    case 1:
      operationalStatusConversion = "Other"; break;
    case 2:
      operationalStatusConversion = "OK"; break;
    case 3:
      operationalStatusConversion = "Degraded"; break;
    case 4:
      operationalStatusConversion = "Stressed"; break;
    case 5:
      operationalStatusConversion = "Predictive Failure"; break;
    case 6:
      operationalStatusConversion = "Error"; break;
    case 7:
      operationalStatusConversion = "Non-Recoverable Error"; break;
    case 8:
      operationalStatusConversion = "Starting"; break;
    case 9:
      operationalStatusConversion = "Stopping"; break;
    case 10:
      operationalStatusConversion = "Stopped"; break;
    case 11:
      operationalStatusConversion = "In Service"; break;
    case 12:
      operationalStatusConversion = "No Contact"; break;
    case 13:
      operationalStatusConversion = "Lost Communication"; break;
    case 14:
      operationalStatusConversion = "Aborted"; break;
    case 15:
      operationalStatusConversion = "Dormant"; break;
    case 16:
      operationalStatusConversion = "Supporting Entity In Error"; break;
    case 17:
      operationalStatusConversion = "Completed"; break;
    case 18:
      operationalStatusConversion = "Power Mode"; break;
    case 19:
      operationalStatusConversion = "DMTF Reserved"; break;
    case 20:
      operationalStatusConversion = "Vendor Reserved"; break;
    case 32768:
      operationalStatusConversion = "Removed"; break;
    case 32769:
      operationalStatusConversion = "Online"; break;
    case 32770:
      operationalStatusConversion = "Offline"; break;
    case 32771:
      operationalStatusConversion = "Rebooting"; break;
    case 32772:
      operationalStatusConversion = "Success"; break;
    case 32773:
      operationalStatusConversion = "Failure"; break;
    case 32774:
      operationalStatusConversion = "Write Disabled"; break;
    case 32775:
      operationalStatusConversion = "Write Protected"; break;
    case 32776:
      operationalStatusConversion = "Not Ready"; break;
    case 32777:
      operationalStatusConversion = "Vendor Reserved"; break;
    default:
      operationalStatusConversion = "Not Available";
    }
    return operationalStatusConversion;
  }

  public String diskOperationalStatus(int status)
  {
    String operationalStatusConversion = null;
    switch (status)
    {
    case 0:
      operationalStatusConversion = "Unknown"; break;
    case 1:
      operationalStatusConversion = "Other"; break;
    case 2:
      operationalStatusConversion = "OK"; break;
    case 3:
      operationalStatusConversion = "Degraded"; break;
    case 4:
      operationalStatusConversion = "Stressed"; break;
    case 5:
      operationalStatusConversion = "Predictive Failure"; break;
    case 6:
      operationalStatusConversion = "Error"; break;
    case 7:
      operationalStatusConversion = "Non-Recoverable Error"; break;
    case 8:
      operationalStatusConversion = "Starting"; break;
    case 9:
      operationalStatusConversion = "Stopping"; break;
    case 10:
      operationalStatusConversion = "Stopped"; break;
    case 11:
      operationalStatusConversion = "In Service"; break;
    case 12:
      operationalStatusConversion = "No Contact"; break;
    case 13:
      operationalStatusConversion = "Lost Communication"; break;
    case 14:
      operationalStatusConversion = "Aborted"; break;
    case 15:
      operationalStatusConversion = "Dormant"; break;
    case 16:
      operationalStatusConversion = "Supporting Entity In Error"; break;
    case 17:
      operationalStatusConversion = "Completed"; break;
    case 18:
      operationalStatusConversion = "Power Mode"; break;
    case 19:
      operationalStatusConversion = "DMTF Reserved"; break;
    case 20:
      operationalStatusConversion = "Vendor Reserved"; break;
    case 32768:
      operationalStatusConversion = "Removed"; break;
    case 32769:
      operationalStatusConversion = "Online"; break;
    case 32770:
      operationalStatusConversion = "Offline"; break;
    case 32771:
      operationalStatusConversion = "Rebooting"; break;
    case 32772:
      operationalStatusConversion = "Success"; break;
    case 32773:
      operationalStatusConversion = "Failure"; break;
    case 32774:
      operationalStatusConversion = "Write Disabled"; break;
    case 32775:
      operationalStatusConversion = "Write Protected"; break;
    case 32776:
      operationalStatusConversion = "Not Ready"; break;
    case 32777:
      operationalStatusConversion = "Power Saving Mode"; break;
    case 32778:
      operationalStatusConversion = "Vendor Reserved"; break;
    default:
      operationalStatusConversion = "Not Available";
    }
    return operationalStatusConversion;
  }

  public String lunOperationalStatus(int status)
  {
    String operationalStatusConversion = null;
    switch (status)
    {
    case 0:
      operationalStatusConversion = "Unknown"; break;
    case 1:
      operationalStatusConversion = "Other"; break;
    case 2:
      operationalStatusConversion = "OK"; break;
    case 3:
      operationalStatusConversion = "Degraded or Predicted Failure"; break;
    case 4:
      operationalStatusConversion = "Stressed"; break;
    case 5:
      operationalStatusConversion = "Predictive Failure"; break;
    case 6:
      operationalStatusConversion = "Error"; break;
    case 7:
      operationalStatusConversion = "Non-Recoverable Error"; break;
    case 8:
      operationalStatusConversion = "Starting"; break;
    case 9:
      operationalStatusConversion = "Stopping"; break;
    case 10:
      operationalStatusConversion = "Stopped"; break;
    case 11:
      operationalStatusConversion = "In Service"; break;
    case 12:
      operationalStatusConversion = "No Contact"; break;
    case 13:
      operationalStatusConversion = "Lost Communication"; break;
    case 14:
      operationalStatusConversion = "Aborted"; break;
    case 15:
      operationalStatusConversion = "Dormant"; break;
    case 16:
      operationalStatusConversion = "Supporting Entity In Error"; break;
    case 17:
      operationalStatusConversion = "Completed"; break;
    case 18:
      operationalStatusConversion = "Power Mode"; break;
    case 19:
      operationalStatusConversion = "DMTF Reserved"; break;
    case 20:
      operationalStatusConversion = "Vendor Reserved"; break;
    case 32768:
      operationalStatusConversion = "Removed"; break;
    case 32769:
      operationalStatusConversion = "Online"; break;
    case 32770:
      operationalStatusConversion = "Offline"; break;
    case 32771:
      operationalStatusConversion = "Rebooting"; break;
    case 32772:
      operationalStatusConversion = "Success"; break;
    case 32773:
      operationalStatusConversion = "Failure"; break;
    case 32774:
      operationalStatusConversion = "Write Disabled"; break;
    case 32775:
      operationalStatusConversion = "Write Protected"; break;
    case 32776:
      operationalStatusConversion = "Not Ready"; break;
    case 32777:
      operationalStatusConversion = "Vendor Reserved"; break;
    default:
      operationalStatusConversion = "Not Available";
    }
    return operationalStatusConversion;
  }

  public String portOperationalStatus(int status)
  {
    String operationalStatusConversion = null;
    switch (status)
    {
    case 0:
      operationalStatusConversion = "Unknown"; break;
    case 1:
      operationalStatusConversion = "Other"; break;
    case 2:
      operationalStatusConversion = "OK"; break;
    case 3:
      operationalStatusConversion = "Degraded or Predicted Failure"; break;
    case 4:
      operationalStatusConversion = "Stressed"; break;
    case 5:
      operationalStatusConversion = "Predictive Failure"; break;
    case 6:
      operationalStatusConversion = "Error"; break;
    case 7:
      operationalStatusConversion = "Non-Recoverable Error"; break;
    case 8:
      operationalStatusConversion = "Starting"; break;
    case 9:
      operationalStatusConversion = "Stopping"; break;
    case 10:
      operationalStatusConversion = "Stopped"; break;
    case 11:
      operationalStatusConversion = "In Service"; break;
    case 12:
      operationalStatusConversion = "No Contact"; break;
    case 13:
      operationalStatusConversion = "Lost Communication"; break;
    case 14:
      operationalStatusConversion = "Aborted"; break;
    case 15:
      operationalStatusConversion = "Dormant"; break;
    case 16:
      operationalStatusConversion = "Supporting Entity in Error"; break;
    case 17:
      operationalStatusConversion = "Completed"; break;
    case 18:
      operationalStatusConversion = "Power Mode"; break;
    case 19:
      operationalStatusConversion = "DMTF Reserved"; break;
    case 20:
      operationalStatusConversion = "Vendor Reserved"; break;
    case 32768:
      operationalStatusConversion = "Removed"; break;
    case 32769:
      operationalStatusConversion = "Online"; break;
    case 32770:
      operationalStatusConversion = "Offline"; break;
    case 32771:
      operationalStatusConversion = "Rebooting"; break;
    case 32772:
      operationalStatusConversion = "Success"; break;
    case 32773:
      operationalStatusConversion = "Failure"; break;
    case 32774:
      operationalStatusConversion = "Write Disabled"; break;
    case 32775:
      operationalStatusConversion = "Write Protected"; break;
    case 32776:
      operationalStatusConversion = "Not Ready"; break;
    case 32777:
      operationalStatusConversion = "Vendor Reserved"; break;
    default:
      operationalStatusConversion = "Not Available";
    }
    return operationalStatusConversion;
  }

  public String diskHealthState(int status)
  {
    String healthStateConversion = null;
    switch (status)
    {
    case 5:
      healthStateConversion = "OK"; break;
    case 10:
      healthStateConversion = "Good"; break;
    case 15:
      healthStateConversion = "OK"; break;
    case 20:
      healthStateConversion = "Major Failure"; break;
    case 30:
      healthStateConversion = "Non-Recoverable Error"; break;
    default:
      healthStateConversion = "Not Available";
    }
    return healthStateConversion;
  }

  public String zoneConnectivityStatus(int status)
  {
    String connectivityStatusConversion = null;
    switch (status)
    {
    case 0:
      connectivityStatusConversion = "Unknown"; break;
    case 2:
      connectivityStatusConversion = "Connectivity/Up"; break;
    case 3:
      connectivityStatusConversion = "No Connectivity/down"; break;
    case 4:
      connectivityStatusConversion = "Partitioned"; break;
    case 1:
    default:
      connectivityStatusConversion = "Not Available";
    }
    return connectivityStatusConversion;
  }

  public String zoneType(int type)
  {
    String zoneTypeConversion = null;
    switch (type)
    {
    case 0:
      zoneTypeConversion = "Unknown"; break;
    case 1:
      zoneTypeConversion = "Other"; break;
    case 2:
      zoneTypeConversion = "Default"; break;
    case 3:
      zoneTypeConversion = "Protocol"; break;
    case 4:
      zoneTypeConversion = "DMTF Reserved"; break;
    case 5:
      zoneTypeConversion = "Vendor Reserved"; break;
    default:
      zoneTypeConversion = "Not Available";
    }
    return zoneTypeConversion;
  }

  public String zoneSubType(int type)
  {
    String zoneTypeConversion = null;
    switch (type)
    {
    case 0:
      zoneTypeConversion = "Unknown"; break;
    case 1:
      zoneTypeConversion = "Other"; break;
    case 2:
      zoneTypeConversion = "SCSI"; break;
    case 3:
      zoneTypeConversion = "VI"; break;
    case 4:
      zoneTypeConversion = "IP"; break;
    case 5:
      zoneTypeConversion = "DMTF Reserved"; break;
    case 6:
      zoneTypeConversion = "Vendor Reserved"; break;
    default:
      zoneTypeConversion = "Not Available";
    }
    return zoneTypeConversion;
  }

  public String linkTechnology(int linkTechnology)
  {
    String linkTechnologyConversion = null;
    switch (linkTechnology)
    {
    case 0:
      linkTechnologyConversion = "Unknown"; break;
    case 1:
      linkTechnologyConversion = "Other"; break;
    case 2:
      linkTechnologyConversion = "Ethernet"; break;
    case 3:
      linkTechnologyConversion = "IB"; break;
    case 4:
      linkTechnologyConversion = "FC"; break;
    case 5:
      linkTechnologyConversion = "FDDI"; break;
    case 6:
      linkTechnologyConversion = "ATM"; break;
    case 7:
      linkTechnologyConversion = "Token Ring"; break;
    case 8:
      linkTechnologyConversion = "Frame Relay"; break;
    case 9:
      linkTechnologyConversion = "Infrared"; break;
    case 10:
      linkTechnologyConversion = "BlueTooth"; break;
    case 11:
      linkTechnologyConversion = "Wireless LAN"; break;
    default:
      linkTechnologyConversion = "Not Available";
    }
    return linkTechnologyConversion;
  }

  public String usageRestriction(int usageRestriction)
  {
    String usageRestrictionConversion = null;
    switch (usageRestriction)
    {
    case 0:
      usageRestrictionConversion = "Unknown"; break;
    case 1:
      usageRestrictionConversion = "Front-End Only"; break;
    case 2:
      usageRestrictionConversion = "Back-End Only"; break;
    case 3:
      usageRestrictionConversion = "Not Restricted"; break;
    default:
      usageRestrictionConversion = "Not Available";
    }
    return usageRestrictionConversion;
  }

  public String requestedState(int requestedState)
  {
    String requestedStateConversion = null;
    switch (requestedState)
    {
    case 2:
      requestedStateConversion = "Enabled"; break;
    case 3:
      requestedStateConversion = "Disabled"; break;
    case 4:
      requestedStateConversion = "Shut Down"; break;
    case 5:
      requestedStateConversion = "No Change"; break;
    case 6:
      requestedStateConversion = "Test"; break;
    case 7:
      requestedStateConversion = "Deferred"; break;
    case 8:
      requestedStateConversion = "Quiesce"; break;
    case 9:
      requestedStateConversion = "Reboot"; break;
    case 10:
      requestedStateConversion = "Reset"; break;
    case 11:
      requestedStateConversion = "Not Applicable"; break;
    case 12:
      requestedStateConversion = "DMTF Reserved"; break;
    case 13:
      requestedStateConversion = "Vendor Reserved"; break;
    default:
      requestedStateConversion = "Not Available";
    }
    return requestedStateConversion;
  }

  public String portType(int portType)
  {
    String portTypeConversion = null;
    switch (portType)
    {
    case 0:
      portTypeConversion = "Unknown"; break;
    case 1:
      portTypeConversion = "Other"; break;
    case 10:
      portTypeConversion = "N"; break;
    case 11:
      portTypeConversion = "NL"; break;
    case 12:
      portTypeConversion = "F/NL"; break;
    case 13:
      portTypeConversion = "Nx"; break;
    case 14:
      portTypeConversion = "E"; break;
    case 15:
      portTypeConversion = "F"; break;
    case 16:
      portTypeConversion = "FL"; break;
    case 17:
      portTypeConversion = "B"; break;
    case 18:
      portTypeConversion = "G"; break;
    case 16000:
      portTypeConversion = "Vendor Reserved"; break;
    default:
      portTypeConversion = "Not Available";
    }
    return portTypeConversion;
  }

  public String softwareClassifications(int classification)
  {
    String classificationConversion = null;
    switch (classification)
    {
    case 0:
      classificationConversion = "Unknown"; break;
    case 1:
      classificationConversion = "Other"; break;
    case 2:
      classificationConversion = "Driver"; break;
    case 3:
      classificationConversion = "Configuration Sofware"; break;
    case 4:
      classificationConversion = "Application Software"; break;
    case 5:
      classificationConversion = "Instrumentation"; break;
    case 6:
      classificationConversion = "Firmware/BIOS"; break;
    case 7:
      classificationConversion = "Diagnostic Software"; break;
    case 8:
      classificationConversion = "Operating System"; break;
    case 9:
      classificationConversion = "Middleware"; break;
    case 10:
      classificationConversion = "Firmware"; break;
    case 11:
      classificationConversion = "Bios/FCCode"; break;
    case 12:
      classificationConversion = "Support/ServicePack"; break;
    case 13:
      classificationConversion = "DMTF Reserved"; break;
    case 14:
      classificationConversion = "Vendor Reserved"; break;
    default:
      classificationConversion = "Not Available";
    }
    return classificationConversion;
  }

  public String mediaTypesSupported(int mediaTypesSupported)
  {
    String classificationConversion = null;
    switch (mediaTypesSupported)
    {
    case 0:
      classificationConversion = "Unknown"; break;
    case 1:
      classificationConversion = "Other"; break;
    case 2:
      classificationConversion = "Tape Cartridge"; break;
    case 3:
      classificationConversion = "QIC Cartridge"; break;
    case 4:
      classificationConversion = "AIT Cartridge"; break;
    case 5:
      classificationConversion = "DTF Cartridge"; break;
    case 6:
      classificationConversion = "DAT Cartridge"; break;
    case 7:
      classificationConversion = "8mm Tape Cartridge"; break;
    case 8:
      classificationConversion = "19mm Tape Cartridge"; break;
    case 9:
      classificationConversion = "DLT Cartridge"; break;
    case 10:
      classificationConversion = "Half-Inch Magnetic Tape Cartridge"; break;
    case 11:
      classificationConversion = "Cartridge Disk"; break;
    case 12:
      classificationConversion = "JAZ Disk"; break;
    case 13:
      classificationConversion = "ZIP Disk"; break;
    case 14:
      classificationConversion = "SyQuest Disk"; break;
    case 15:
      classificationConversion = "Winchester Removable Disk"; break;
    case 16:
      classificationConversion = "CD-ROM"; break;
    case 17:
      classificationConversion = "CD-ROM/XA"; break;
    case 18:
      classificationConversion = "CD-I"; break;
    case 19:
      classificationConversion = "CD Recordable"; break;
    case 20:
      classificationConversion = "WORM"; break;
    case 21:
      classificationConversion = "Magneto-Optical"; break;
    case 22:
      classificationConversion = "DVD"; break;
    case 23:
      classificationConversion = "DVD-RW+"; break;
    case 24:
      classificationConversion = "DVD-RAM"; break;
    case 25:
      classificationConversion = "DVD-ROM"; break;
    case 26:
      classificationConversion = "DVD-Video"; break;
    case 27:
      classificationConversion = "Divx"; break;
    case 28:
      classificationConversion = "Floppy/Diskette"; break;
    case 29:
      classificationConversion = "Hard Disk"; break;
    case 30:
      classificationConversion = "Memory Card"; break;
    case 31:
      classificationConversion = "Hard Copy"; break;
    case 32:
      classificationConversion = "Clik Disk"; break;
    case 33:
      classificationConversion = "CD-RW"; break;
    case 34:
      classificationConversion = "CD-DA"; break;
    case 35:
      classificationConversion = "CD+"; break;
    case 36:
      classificationConversion = "DVD Recordable"; break;
    case 37:
      classificationConversion = "DVD-RW"; break;
    case 38:
      classificationConversion = "DVD-Audio"; break;
    case 39:
      classificationConversion = "DVD-5"; break;
    case 40:
      classificationConversion = "DVD-9"; break;
    case 41:
      classificationConversion = "DVD-10"; break;
    case 42:
      classificationConversion = "DVD-18"; break;
    case 43:
      classificationConversion = "Magneto-Optical Rewriteable"; break;
    case 44:
      classificationConversion = "Magneto-Optical Write Once"; break;
    case 45:
      classificationConversion = "Magneto-Optical Rewriteable (LIMDOW)"; break;
    case 46:
      classificationConversion = "Phase Change Write Once"; break;
    case 47:
      classificationConversion = "Phase Change Rewritable"; break;
    case 48:
      classificationConversion = "Phase Change Dual Rewriteable"; break;
    case 49:
      classificationConversion = "Ablative Write Once"; break;
    case 50:
      classificationConversion = "Near Field Recording"; break;
    case 51:
      classificationConversion = "MiniQic"; break;
    case 52:
      classificationConversion = "Travan"; break;
    case 53:
      classificationConversion = "8mm Metal Particle"; break;
    case 54:
      classificationConversion = "8mm Advanced Metal Evaporate"; break;
    case 55:
      classificationConversion = "NCTP"; break;
    case 56:
      classificationConversion = "LTO Ultrium"; break;
    case 57:
      classificationConversion = "LTO Accelis"; break;
    case 58:
      classificationConversion = "9 Track Tape"; break;
    case 59:
      classificationConversion = "18 Track Tape"; break;
    case 60:
      classificationConversion = "36 Track Tape"; break;
    case 61:
      classificationConversion = "Magstar 3590"; break;
    case 62:
      classificationConversion = "Magstar MP"; break;
    case 63:
      classificationConversion = "D2 Tape"; break;
    case 64:
      classificationConversion = "Tape-DST Small"; break;
    case 65:
      classificationConversion = "Tape-DST Medium"; break;
    case 66:
      classificationConversion = "Tape-DST Large"; break;
    default:
      classificationConversion = "Not Available";
    }
    return classificationConversion;
  }

  public String classification(int classification)
  {
    String classificationConversion = null;
    switch (classification)
    {
    case 0:
      classificationConversion = "Unknown"; break;
    case 1:
      classificationConversion = "Other"; break;
    case 2:
      classificationConversion = "Driver"; break;
    case 3:
      classificationConversion = "Configuration Software"; break;
    case 4:
      classificationConversion = "Application Software"; break;
    case 5:
      classificationConversion = "Instrumentation"; break;
    case 6:
      classificationConversion = "Firmware/BIOS"; break;
    case 7:
      classificationConversion = "Diagnostic Software"; break;
    case 8:
      classificationConversion = "Operating System"; break;
    case 9:
      classificationConversion = "Middleware"; break;
    case 10:
      classificationConversion = "Firmware"; break;
    case 11:
      classificationConversion = "BIOS/FCode"; break;
    case 12:
      classificationConversion = "Support/Service Pack"; break;
    default:
      classificationConversion = "Not Available";
    }
    return classificationConversion;
  }

  public String extentStatus(int extentStatus)
  {
    String classificationConversion = null;
    switch (extentStatus)
    {
    case 0:
      classificationConversion = "Other"; break;
    case 1:
      classificationConversion = "Unknown"; break;
    case 2:
      classificationConversion = "None/Not Applicable"; break;
    case 3:
      classificationConversion = "Broken"; break;
    case 4:
      classificationConversion = "Data Lost"; break;
    case 5:
      classificationConversion = "Dynamic Reconfig"; break;
    case 6:
      classificationConversion = "Exposed"; break;
    case 7:
      classificationConversion = "Fractionally Exposed"; break;
    case 8:
      classificationConversion = "Partially Exposed"; break;
    case 9:
      classificationConversion = "Protection Disabled"; break;
    case 10:
      classificationConversion = "Readying"; break;
    case 11:
      classificationConversion = "Rebuild"; break;
    case 12:
      classificationConversion = "Recalculate"; break;
    case 13:
      classificationConversion = "Spare In Use"; break;
    case 14:
      classificationConversion = "Verify In Progress"; break;
    case 15:
      classificationConversion = "In-Band Access Granted"; break;
    case 16:
      classificationConversion = "Imported"; break;
    case 17:
      classificationConversion = "Exported"; break;
    default:
      classificationConversion = "Not Available";
    }
    return classificationConversion;
  }
}