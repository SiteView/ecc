package com.dragonflow.siteview.san.fcswitch;

import java.util.Set;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.UnsignedInteger16;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
//import org.hibernate.Session;


import com.dragonflow.siteview.san.util.*;
import com.dragonflow.siteview.san.beans.*;

public class MapSwitchFabric
{
 // private Logger logger;
  private String CN = "MapSwitchFabric";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  
  private CIM_ComputerSystem ComputerSystem;
  
  CIM_AdminDomain tempAdminDomain;
  CIM_ZoneSet tempZoneSet;
  CIM_Zone tempZone;
  CIM_NamedAddressCollection tempNamedAddressCollection;
  //private Integer computerSystemID;
  //private Session sessionMapFabrics;

  public MapSwitchFabric( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem)
  {
  //  this.logger = Logger.getLogger(this.CN);
   // this.sessionMapFabrics = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
   // this.computerSystemID = computerSystemID1;
    mapActiveZoneSetData();
    mapInactiveZoneSetData();
  }

  public boolean mapActiveZoneSetData()
  {
    try {
      CloseableIterator cim_AdminDomainEnum = this.cc.associators(this.instanceCOP, "CIM_Component", "CIM_AdminDomain", "PartComponent", "GroupComponent", false, false, null);

      while (cim_AdminDomainEnum.hasNext()) {
     //   this.logger.debug("STARTED FABRIC MAPPING");
        CIMInstance cim_AdminDomainCI = (CIMInstance)cim_AdminDomainEnum.next();
        CIMObjectPath cim_AdminDomainCOP = cim_AdminDomainCI.getObjectPath();
        tempAdminDomain = new CIM_AdminDomain();
        if (cim_AdminDomainEnum == null) continue;
        try {
          String adminDomainName = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "Name");
          tempAdminDomain.setName(adminDomainName);
        }
        catch (NullPointerException localNullPointerException) {
        }
        try {
          String adminDomainElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "ElementName");
          tempAdminDomain.setElementName(adminDomainElementName);
        }
        catch (NullPointerException localNullPointerException1) {
        }
        try {
          String adminDomainCreationClassName = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "CreationClassName");
          tempAdminDomain.setCreationClassName(adminDomainCreationClassName);
        }
        catch (NullPointerException localNullPointerException2) {
        }
        try {
          String adminDomainNameFormat = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "NameFormat");
          tempAdminDomain.setNameFormat(adminDomainNameFormat);
        }
        catch (NullPointerException localNullPointerException3)
        {
        }
        
        //--------------
        this.ComputerSystem.getCim_AdminDomain().add(tempAdminDomain);
        
        /*
        this.sessionMapFabrics.save(cad);
        String adminDomainID = this.sessionMapFabrics.getIdentifier(cad).toString();
        Integer adminDomainIDp = Integer.valueOf(adminDomainID);
      //  this.logger.debug("adminDomainIDp = " + adminDomainIDp);
      //  this.logger.debug("computerSystemID = " + this.computerSystemID);
        CIM_ComputerSystem aComputerSystemForAdminDomain = (CIM_ComputerSystem)this.sessionMapFabrics.load(CIM_ComputerSystem.class, this.computerSystemID);
        CIM_AdminDomain aAdminDomain = (CIM_AdminDomain)this.sessionMapFabrics.load(CIM_AdminDomain.class, adminDomainIDp);
        aComputerSystemForAdminDomain.getCim_AdminDomain().add(aAdminDomain);
        this.sessionMapFabrics.save(aComputerSystemForAdminDomain);
        */

        CloseableIterator cim_ZoneSetEnum = this.cc.associators(cim_AdminDomainCOP, "CIM_HostedCollection", "CIM_ZoneSet", "Antecedent", "Dependent", false, false, null);

        while (cim_ZoneSetEnum.hasNext())
        {
          CIMInstance cim_ZoneSetCI = (CIMInstance)cim_ZoneSetEnum.next();
          CIMObjectPath cim_ZoneSetCOP = cim_ZoneSetCI.getObjectPath();
          tempZoneSet = new CIM_ZoneSet();
          if (cim_ZoneSetEnum == null) continue;
          try {
            String zoneSetInstanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "InstanceID");
            tempZoneSet.setInstanceId(zoneSetInstanceId);
          }
          catch (NullPointerException localNullPointerException4) {
          }
          try {
            String zoneSetElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "ElementName");
            tempZoneSet.setElementName(zoneSetElementName);
          }
          catch (NullPointerException localNullPointerException5) {
          }
          try {
            boolean zoneSetActive = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_ZoneSetCI, "Active").booleanValue();
            tempZoneSet.setActive(zoneSetActive);
          }
          catch (NullPointerException localNullPointerException6) {
          }
          try {
            int zoneSetConnectivityStatus = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneSetCI, "ConnectivityStatus").intValue();
            String zoneSetConnectivityStatusConversion = this.cim_Q.zoneConnectivityStatus(zoneSetConnectivityStatus);
            tempZoneSet.setConnectivityStatus(zoneSetConnectivityStatusConversion);
          }
          catch (NullPointerException localNullPointerException7) {
          }
          try {
            String zoneSetCaption = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "Caption");
            tempZoneSet.setCaption(zoneSetCaption);
          }
          catch (NullPointerException localNullPointerException8) {
          }
          try {
            String zoneSetDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "Description");
            tempZoneSet.setDescription(zoneSetDescription);
          }
          catch (NullPointerException localNullPointerException9)
          {
          }
          //------------------
          
          tempAdminDomain.getCim_ZoneSet().add(tempZoneSet);
          
          /*
          this.sessionMapFabrics.save(czs);
          String zoneSetID = this.sessionMapFabrics.getIdentifier(czs).toString();
          Integer zoneSetIDp = Integer.valueOf(zoneSetID);
       //   this.logger.debug("zoneSetIDp = " + zoneSetIDp);
       //   this.logger.debug("adminDomainID = " + adminDomainID);
          CIM_AdminDomain aAdminDomainForZoneSet = (CIM_AdminDomain)this.sessionMapFabrics.load(CIM_AdminDomain.class, adminDomainIDp);
          CIM_ZoneSet aZoneSet = (CIM_ZoneSet)this.sessionMapFabrics.load(CIM_ZoneSet.class, zoneSetIDp);
          aAdminDomainForZoneSet.getCim_ZoneSet().add(aZoneSet);
          this.sessionMapFabrics.save(aAdminDomainForZoneSet);
          */

          CloseableIterator cim_ZoneEnum = this.cc.associators(cim_ZoneSetCOP, "CIM_MemberOfCollection", "CIM_Zone", "Collection", "Member", false, false, null);
          while (cim_ZoneEnum.hasNext())
          {
            CIMInstance cim_ZoneCI = (CIMInstance)cim_ZoneEnum.next();
            CIMObjectPath cim_ZoneCOP = cim_ZoneCI.getObjectPath();
            tempZone = new CIM_Zone();
            if (cim_ZoneEnum == null) continue;
            try {
              String zoneInstanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "InstanceID");
              tempZone.setInstanceId(zoneInstanceId);
            }
            catch (NullPointerException localNullPointerException10) {
            }
            try {
              String zoneElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "ElementName");
              tempZone.setElementName(zoneElementName);
            }
            catch (NullPointerException localNullPointerException11) {
            }
            try {
              boolean zoneActive = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_ZoneCI, "Active").booleanValue();
              tempZone.setActive(zoneActive);
            }
            catch (NullPointerException localNullPointerException12) {
            }
            try {
              int zoneZoneType = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneCI, "ZoneType").intValue();
              String zoneTypeConversion = this.cim_Q.zoneType(zoneZoneType);
              tempZone.setZoneType(zoneTypeConversion);
            }
            catch (NullPointerException localNullPointerException13) {
            }
            try {
              String zoneOtherZoneTypeDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "OtherZoneTypeDescription");
              tempZone.setOtherZoneTypeDescription(zoneOtherZoneTypeDescription);
            }
            catch (NullPointerException localNullPointerException14) {
            }
            try {
              int zoneZoneSubType = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneCI, "ZoneSubType").intValue();
              String zoneSubTypeConversion = this.cim_Q.zoneSubType(zoneZoneSubType);
              tempZone.setZoneSubType(zoneSubTypeConversion);
            }
            catch (NullPointerException localNullPointerException15) {
            }
            try {
              String zoneOtherZoneSubTypeDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "OtherZoneSubTypeDescription").toString();
              tempZone.setOtherZoneSubTypeDescription(zoneOtherZoneSubTypeDescription);
            }
            catch (NullPointerException localNullPointerException16) {
            }
            try {
              int zoneConectivityStatus = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneCI, "ConnectivityStatus").intValue();
              String conectivityStatusConversion = this.cim_Q.zoneConnectivityStatus(zoneConectivityStatus);
              tempZone.setConnectivityStatus(conectivityStatusConversion);
            }
            catch (NullPointerException localNullPointerException17) {
            }
            try {
              String zoneCaption = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "Caption").toString();
              tempZone.setCaption(zoneCaption);
            }
            catch (NullPointerException localNullPointerException18) {
            }
            try {
              String zoneDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "Description").toString();
              tempZone.setDescription(zoneDescription);
            }
            catch (NullPointerException localNullPointerException19)
            {
            }
            //----------------
            
            tempZoneSet.getCim_Zone().add(tempZone);
            
            /*
            this.sessionMapFabrics.save(cz);
            String zoneID = this.sessionMapFabrics.getIdentifier(cz).toString();
            Integer zoneIDp = Integer.valueOf(zoneID);
          //  this.logger.debug("zoneIDp = " + zoneID);
            CIM_ZoneSet aZoneSetForZone = (CIM_ZoneSet)this.sessionMapFabrics.load(CIM_ZoneSet.class, zoneSetIDp);
            CIM_Zone aZone = (CIM_Zone)this.sessionMapFabrics.load(CIM_Zone.class, zoneIDp);
            aZoneSetForZone.getCim_Zone().add(aZone);
            this.sessionMapFabrics.save(aZoneSetForZone);
            */

            CloseableIterator cim_ZoneMembershipSettingDataEnum = this.cc.associators(cim_ZoneCOP, "CIM_ElementSettingData", "CIM_ZoneMembershipSettingData", "ManagedElement", "SettingData", false, false, null);
            while (cim_ZoneMembershipSettingDataEnum.hasNext())
            {
              CIMInstance cim_ZoneMembershipSettingDataCI = (CIMInstance)cim_ZoneMembershipSettingDataEnum.next();

              CIM_ZoneMembershipSettingData czmsd = new CIM_ZoneMembershipSettingData();
              if (cim_ZoneMembershipSettingDataEnum == null) continue;
              try {
                String zoneMembershipSettingDataConnectivityMemberId = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneMembershipSettingDataCI, "ConnectivityMemberID");
                czmsd.setConnectivityMemberId(zoneMembershipSettingDataConnectivityMemberId);
              }
              catch (NullPointerException localNullPointerException20)
              {
              }
              
              //--------------
              
              tempZone.getCim_ZoneMembershipSettingData().add(czmsd);
              /*
              this.sessionMapFabrics.save(czmsd);
              String zoneMembershipSettingDataID = this.sessionMapFabrics.getIdentifier(czmsd).toString();
              Integer zoneMembershipSettingDataIDp = Integer.valueOf(zoneMembershipSettingDataID);
          //    this.logger.debug("zoneMembershipSettingDataIDp = " + zoneMembershipSettingDataIDp);
              CIM_Zone aZoneForZoneMembershipSettingData = (CIM_Zone)this.sessionMapFabrics.load(CIM_Zone.class, zoneIDp);
              CIM_ZoneMembershipSettingData aZoneMembershipSettingData = (CIM_ZoneMembershipSettingData)this.sessionMapFabrics.load(CIM_ZoneMembershipSettingData.class, zoneMembershipSettingDataIDp);
              aZoneForZoneMembershipSettingData.getCim_ZoneMembershipSettingData().add(aZoneMembershipSettingData);
              this.sessionMapFabrics.save(aZoneForZoneMembershipSettingData);
              */
            }

            CloseableIterator cim_NamedAddressCollectionEnum = this.cc.associators(cim_ZoneCOP, "CIM_MemberOfCollection", "CIM_NamedAddressCollection", "Collection", "Member", false, false, null);
            while (cim_NamedAddressCollectionEnum.hasNext())
            {
              CIMInstance cim_NamedAddressCollectionCI = (CIMInstance)cim_NamedAddressCollectionEnum.next();
              CIMObjectPath cim_NamedAddressCollectionCOP = cim_NamedAddressCollectionCI.getObjectPath();
              tempNamedAddressCollection = new CIM_NamedAddressCollection();
              if (cim_NamedAddressCollectionEnum == null) continue;
              try {
                String namedAddressCollectionCollectionAlias = this.cim_DT.getCIMInstancePropertyValueString(cim_NamedAddressCollectionCI, "CollectionAlias");
                tempNamedAddressCollection.setCollectionAlias(namedAddressCollectionCollectionAlias);
              }
              catch (NullPointerException localNullPointerException21)
              {
              }
              
              
              /*
              this.sessionMapFabrics.save(cnac);
              String namedAddressCollectionID = this.sessionMapFabrics.getIdentifier(cnac).toString();
              Integer namedAddressCollectionIDp = Integer.valueOf(namedAddressCollectionID);
         //     this.logger.debug("namedAddressCollectionIDp = " + namedAddressCollectionID);
              CIM_Zone aZoneForNamedAddressCollection = (CIM_Zone)this.sessionMapFabrics.load(CIM_Zone.class, zoneIDp);
              CIM_NamedAddressCollection aNamedAddressCollection = (CIM_NamedAddressCollection)this.sessionMapFabrics.load(CIM_NamedAddressCollection.class, namedAddressCollectionIDp);
              aZoneForNamedAddressCollection.getCim_NamedAddressCollection().add(aNamedAddressCollection);
              this.sessionMapFabrics.save(aZoneForNamedAddressCollection);
              
              */
              
              //--------------
              tempZone.getCim_NamedAddressCollection().add(tempNamedAddressCollection);
              

              CloseableIterator cim_NamedAddressZoneMembershipSettingDataEnum = this.cc.associators(cim_NamedAddressCollectionCOP, "CIM_MemberOfCollection", "CIM_ZoneMembershipSettingData", "Collection", "Member", false, false, null);
              while (cim_NamedAddressZoneMembershipSettingDataEnum.hasNext())
              {
                CIMInstance cim_ZoneMemSetDataNamedAddressCollectionCI = (CIMInstance)cim_NamedAddressCollectionEnum.next();

                CIM_ZoneMembershipSettingData czmsd1 = new CIM_ZoneMembershipSettingData();
                if (cim_NamedAddressZoneMembershipSettingDataEnum == null) continue;
                try {
                  String nac_zmsd_ElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneMemSetDataNamedAddressCollectionCI, "ElementName");
                  czmsd1.setElementName(nac_zmsd_ElementName);
                }
                catch (NullPointerException localNullPointerException22)
                {
                }
                //-------
                tempNamedAddressCollection.getCim_ZoneMembershipSettingData().add(czmsd1);
                /*
                this.sessionMapFabrics.save(czmsd1);
                String zoneMembershipSettingID1 = this.sessionMapFabrics.getIdentifier(czmsd1).toString();
                Integer zoneMembershipSettingID1p = Integer.valueOf(zoneMembershipSettingID1);
         //       this.logger.debug("zoneMembershipSettingID1p = " + zoneMembershipSettingID1p);
                CIM_NamedAddressCollection aNamedAddressCollectionForZoneMembershipSettingData = (CIM_NamedAddressCollection)this.sessionMapFabrics.load(CIM_NamedAddressCollection.class, namedAddressCollectionIDp);
                CIM_ZoneMembershipSettingData aZoneMembershipSettingData = (CIM_ZoneMembershipSettingData)this.sessionMapFabrics.load(CIM_ZoneMembershipSettingData.class, zoneMembershipSettingID1p);
                aNamedAddressCollectionForZoneMembershipSettingData.getCim_ZoneMembershipSettingData().add(aZoneMembershipSettingData);
                this.sessionMapFabrics.save(aNamedAddressCollectionForZoneMembershipSettingData);
                */
              }
            }
          }

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

  public boolean mapInactiveZoneSetData()
  {
    try {
      CloseableIterator cim_AdminDomainEnum = this.cc.associators(this.instanceCOP, "CIM_Component", "CIM_AdminDomain", "PartComponent", "GroupComponent", false, false, null);

      while (cim_AdminDomainEnum.hasNext()) {
      //  this.logger.debug("STARTED FABRIC MAPPING");
        CIMInstance cim_AdminDomainCI = (CIMInstance)cim_AdminDomainEnum.next();
        CIMObjectPath cim_AdminDomainCOP = cim_AdminDomainCI.getObjectPath();
        tempAdminDomain = new CIM_AdminDomain();
        if (cim_AdminDomainEnum == null) continue;
        try {
          String adminDomainName = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "Name");
          tempAdminDomain.setName(adminDomainName);
        }
        catch (NullPointerException localNullPointerException) {
        }
        try {
          String adminDomainElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "ElementName");
          tempAdminDomain.setElementName(adminDomainElementName);
        }
        catch (NullPointerException localNullPointerException1) {
        }
        try {
          String adminDomainCreationClassName = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "CreationClassName");
          tempAdminDomain.setCreationClassName(adminDomainCreationClassName);
        }
        catch (NullPointerException localNullPointerException2) {
        }
        try {
          String adminDomainNameFormat = this.cim_DT.getCIMInstancePropertyValueString(cim_AdminDomainCI, "NameFormat");
          tempAdminDomain.setNameFormat(adminDomainNameFormat);
        }
        catch (NullPointerException localNullPointerException3)
        {
        }
        //------------
         this.ComputerSystem.getCim_AdminDomain().add(tempAdminDomain);
        
        /*
        this.sessionMapFabrics.save(cad);
        String adminDomainID = this.sessionMapFabrics.getIdentifier(cad).toString();
        Integer adminDomainIDp = Integer.valueOf(adminDomainID);
     //   this.logger.debug("adminDomainIDp = " + adminDomainIDp);
     //   this.logger.debug("computerSystemID = " + this.computerSystemID);
        CIM_ComputerSystem aComputerSystemForAdminDomain = (CIM_ComputerSystem)this.sessionMapFabrics.load(CIM_ComputerSystem.class, this.computerSystemID);
        CIM_AdminDomain aAdminDomain = (CIM_AdminDomain)this.sessionMapFabrics.load(CIM_AdminDomain.class, adminDomainIDp);
        aComputerSystemForAdminDomain.getCim_AdminDomain().add(aAdminDomain);
        this.sessionMapFabrics.save(aComputerSystemForAdminDomain);
        */

        CloseableIterator cim_ZoneSetEnum = this.cc.associators(cim_AdminDomainCOP, "CIM_HostedCollection", "CIM_ZoneSet", "Antecedent", "Dependent", false, false, null);

        while (cim_ZoneSetEnum.hasNext())
        {
          CIMInstance cim_ZoneSetCI = (CIMInstance)cim_ZoneSetEnum.next();
          CIMObjectPath cim_ZoneSetCOP = cim_ZoneSetCI.getObjectPath();
          tempZoneSet = new CIM_ZoneSet();
          if (cim_ZoneSetEnum == null) continue;
          try {
            String zoneSetInstanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "InstanceID");
            tempZoneSet.setInstanceId(zoneSetInstanceId);
          }
          catch (NullPointerException localNullPointerException4) {
          }
          try {
            String zoneSetElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "ElementName");
            tempZoneSet.setElementName(zoneSetElementName);
          }
          catch (NullPointerException localNullPointerException5) {
          }
          try {
            boolean zoneSetActive = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_ZoneSetCI, "Active").booleanValue();
            tempZoneSet.setActive(zoneSetActive);
          }
          catch (NullPointerException localNullPointerException6) {
          }
          try {
            int zoneSetConnectivityStatus = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneSetCI, "ConnectivityStatus").intValue();
            String zoneSetConnectivityStatusConversion = this.cim_Q.zoneConnectivityStatus(zoneSetConnectivityStatus);
            tempZoneSet.setConnectivityStatus(zoneSetConnectivityStatusConversion);
          }
          catch (NullPointerException localNullPointerException7) {
          }
          try {
            String zoneSetCaption = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "Caption");
            tempZoneSet.setCaption(zoneSetCaption);
          }
          catch (NullPointerException localNullPointerException8) {
          }
          try {
            String zoneSetDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneSetCI, "Description");
            tempZoneSet.setDescription(zoneSetDescription);
          }
          catch (NullPointerException localNullPointerException9)
          {
          }
          //------
          
          tempAdminDomain.getCim_ZoneSet().add(tempZoneSet);
          /*
          this.sessionMapFabrics.save(czs);
          String zoneSetID = this.sessionMapFabrics.getIdentifier(czs).toString();
          Integer zoneSetIDp = Integer.valueOf(zoneSetID);
       //   this.logger.debug("zoneSetIDp = " + zoneSetIDp);
       //   this.logger.debug("adminDomainID = " + adminDomainID);
          CIM_AdminDomain aAdminDomainForZoneSet = (CIM_AdminDomain)this.sessionMapFabrics.load(CIM_AdminDomain.class, adminDomainIDp);
          CIM_ZoneSet aZoneSet = (CIM_ZoneSet)this.sessionMapFabrics.load(CIM_ZoneSet.class, zoneSetIDp);
          aAdminDomainForZoneSet.getCim_ZoneSet().add(aZoneSet);
          this.sessionMapFabrics.save(aAdminDomainForZoneSet);
          */

          CloseableIterator cim_ZoneEnum = this.cc.associators(cim_ZoneSetCOP, "CIM_MemberOfCollection", "CIM_Zone", "Collection", "Member", false, false, null);
          while (cim_ZoneEnum.hasNext())
          {
            CIMInstance cim_ZoneCI = (CIMInstance)cim_ZoneEnum.next();
            CIMObjectPath cim_ZoneCOP = cim_ZoneCI.getObjectPath();
            tempZone = new CIM_Zone();
            if (cim_ZoneEnum == null) continue;
            try {
              String zoneInstanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "InstanceID");
              tempZone.setInstanceId(zoneInstanceId);
            }
            catch (NullPointerException localNullPointerException10) {
            }
            try {
              String zoneElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "ElementName");
              tempZone.setElementName(zoneElementName);
            }
            catch (NullPointerException localNullPointerException11) {
            }
            try {
              boolean zoneActive = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_ZoneCI, "Active").booleanValue();
              tempZone.setActive(zoneActive);
            }
            catch (NullPointerException localNullPointerException12) {
            }
            try {
              int zoneZoneType = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneCI, "ZoneType").intValue();
              String zoneTypeConversion = this.cim_Q.zoneType(zoneZoneType);
              tempZone.setZoneType(zoneTypeConversion);
            }
            catch (NullPointerException localNullPointerException13) {
            }
            try {
              String zoneOtherZoneTypeDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "OtherZoneTypeDescription");
              tempZone.setOtherZoneTypeDescription(zoneOtherZoneTypeDescription);
            }
            catch (NullPointerException localNullPointerException14) {
            }
            try {
              int zoneZoneSubType = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneCI, "ZoneSubType").intValue();
              String zoneSubTypeConversion = this.cim_Q.zoneSubType(zoneZoneSubType);
              tempZone.setZoneSubType(zoneSubTypeConversion);
            }
            catch (NullPointerException localNullPointerException15) {
            }
            try {
              String zoneOtherZoneSubTypeDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "OtherZoneSubTypeDescription").toString();
              tempZone.setOtherZoneSubTypeDescription(zoneOtherZoneSubTypeDescription);
            }
            catch (NullPointerException localNullPointerException16) {
            }
            try {
              int zoneConectivityStatus = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_ZoneCI, "ConnectivityStatus").intValue();
              String conectivityStatusConversion = this.cim_Q.zoneConnectivityStatus(zoneConectivityStatus);
              tempZone.setConnectivityStatus(conectivityStatusConversion);
            }
            catch (NullPointerException localNullPointerException17) {
            }
            try {
              String zoneCaption = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "Caption").toString();
              tempZone.setCaption(zoneCaption);
            }
            catch (NullPointerException localNullPointerException18) {
            }
            try {
              String zoneDescription = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneCI, "Description").toString();
              tempZone.setDescription(zoneDescription);
            }
            catch (NullPointerException localNullPointerException19)
            {
            }
            //---------------
            this.tempZoneSet.getCim_Zone().add(tempZone);
            
            /*
            this.sessionMapFabrics.save(cz);
            String zoneID = this.sessionMapFabrics.getIdentifier(cz).toString();
            Integer zoneIDp = Integer.valueOf(zoneID);
        //    this.logger.debug("zoneIDp = " + zoneID);
            CIM_ZoneSet aZoneSetForZone = (CIM_ZoneSet)this.sessionMapFabrics.load(CIM_ZoneSet.class, zoneSetIDp);
            CIM_Zone aZone = (CIM_Zone)this.sessionMapFabrics.load(CIM_Zone.class, zoneIDp);
            aZoneSetForZone.getCim_Zone().add(aZone);
            this.sessionMapFabrics.save(aZoneSetForZone);
            */

            CloseableIterator cim_ZoneMembershipSettingDataEnum = this.cc.associators(cim_ZoneCOP, "CIM_ElementSettingData", "CIM_ZoneMembershipSettingData", "ManagedElement", "SettingData", false, false, null);
            while (cim_ZoneMembershipSettingDataEnum.hasNext())
            {
              CIMInstance cim_ZoneMembershipSettingDataCI = (CIMInstance)cim_ZoneMembershipSettingDataEnum.next();

              CIM_ZoneMembershipSettingData czmsd = new CIM_ZoneMembershipSettingData();
              if (cim_ZoneMembershipSettingDataEnum == null) continue;
              try {
                String zoneMembershipSettingDataConnectivityMemberId = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneMembershipSettingDataCI, "ConnectivityMemberID");
                czmsd.setConnectivityMemberId(zoneMembershipSettingDataConnectivityMemberId);
              }
              catch (NullPointerException localNullPointerException20)
              {
              }
              //-----------------
              
              this.tempZone.getCim_ZoneMembershipSettingData().add(czmsd);
              
              /*
              this.sessionMapFabrics.save(czmsd);
              String zoneMembershipSettingDataID = this.sessionMapFabrics.getIdentifier(czmsd).toString();
              Integer zoneMembershipSettingDataIDp = Integer.valueOf(zoneMembershipSettingDataID);
         //     this.logger.debug("zoneMembershipSettingDataIDp = " + zoneMembershipSettingDataIDp);
              CIM_Zone aZoneForZoneMembershipSettingData = (CIM_Zone)this.sessionMapFabrics.load(CIM_Zone.class, zoneIDp);
              CIM_ZoneMembershipSettingData aZoneMembershipSettingData = (CIM_ZoneMembershipSettingData)this.sessionMapFabrics.load(CIM_ZoneMembershipSettingData.class, zoneMembershipSettingDataIDp);
              aZoneForZoneMembershipSettingData.getCim_ZoneMembershipSettingData().add(aZoneMembershipSettingData);
              this.sessionMapFabrics.save(aZoneForZoneMembershipSettingData);
              */
            }

            CloseableIterator cim_NamedAddressCollectionEnum = this.cc.associators(cim_ZoneCOP, "CIM_MemberOfCollection", "CIM_NamedAddressCollection", "Collection", "Member", false, false, null);
            while (cim_NamedAddressCollectionEnum.hasNext())
            {
              CIMInstance cim_NamedAddressCollectionCI = (CIMInstance)cim_NamedAddressCollectionEnum.next();
              CIMObjectPath cim_NamedAddressCollectionCOP = cim_NamedAddressCollectionCI.getObjectPath();
              tempNamedAddressCollection = new CIM_NamedAddressCollection();
              if (cim_NamedAddressCollectionEnum == null) continue;
              try {
                String namedAddressCollectionCollectionAlias = this.cim_DT.getCIMInstancePropertyValueString(cim_NamedAddressCollectionCI, "CollectionAlias");
                tempNamedAddressCollection.setCollectionAlias(namedAddressCollectionCollectionAlias);
              }
              catch (NullPointerException localNullPointerException21)
              {
              }
              //----------------------
              
              tempZone.getCim_NamedAddressCollection().add(tempNamedAddressCollection);
              /*
              this.sessionMapFabrics.save(cnac);
              String namedAddressCollectionID = this.sessionMapFabrics.getIdentifier(cnac).toString();
              Integer namedAddressCollectionIDp = Integer.valueOf(namedAddressCollectionID);
         //     this.logger.debug("namedAddressCollectionIDp = " + namedAddressCollectionID);
              CIM_Zone aZoneForNamedAddressCollection = (CIM_Zone)this.sessionMapFabrics.load(CIM_Zone.class, zoneIDp);
              CIM_NamedAddressCollection aNamedAddressCollection = (CIM_NamedAddressCollection)this.sessionMapFabrics.load(CIM_NamedAddressCollection.class, namedAddressCollectionIDp);
              aZoneForNamedAddressCollection.getCim_NamedAddressCollection().add(aNamedAddressCollection);
              this.sessionMapFabrics.save(aZoneForNamedAddressCollection);
              */

              CloseableIterator cim_NamedAddressZoneMembershipSettingDataEnum = this.cc.associators(cim_NamedAddressCollectionCOP, "CIM_MemberOfCollection", "CIM_ZoneMembershipSettingData", "Collection", "Member", false, false, null);
              while (cim_NamedAddressZoneMembershipSettingDataEnum.hasNext())
              {
                CIMInstance cim_ZoneMemSetDataNamedAddressCollectionCI = (CIMInstance)cim_NamedAddressCollectionEnum.next();

                CIM_ZoneMembershipSettingData czmsd1 = new CIM_ZoneMembershipSettingData();
                if (cim_NamedAddressZoneMembershipSettingDataEnum == null) continue;
                try {
                  String nac_zmsd_ElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_ZoneMemSetDataNamedAddressCollectionCI, "ElementName");
                  czmsd1.setElementName(nac_zmsd_ElementName);
                }
                catch (NullPointerException localNullPointerException22)
                {
                }
                //-----------------------
                tempNamedAddressCollection.getCim_ZoneMembershipSettingData().add(czmsd1);
                
                /*
                this.sessionMapFabrics.save(czmsd1);
                String zoneMembershipSettingID1 = this.sessionMapFabrics.getIdentifier(czmsd1).toString();
                Integer zoneMembershipSettingID1p = Integer.valueOf(zoneMembershipSettingID1);
          //      this.logger.debug("zoneMembershipSettingID1p = " + zoneMembershipSettingID1p);
                CIM_NamedAddressCollection aNamedAddressCollectionForZoneMembershipSettingData = (CIM_NamedAddressCollection)this.sessionMapFabrics.load(CIM_NamedAddressCollection.class, namedAddressCollectionIDp);
                CIM_ZoneMembershipSettingData aZoneMembershipSettingData = (CIM_ZoneMembershipSettingData)this.sessionMapFabrics.load(CIM_ZoneMembershipSettingData.class, zoneMembershipSettingID1p);
                aNamedAddressCollectionForZoneMembershipSettingData.getCim_ZoneMembershipSettingData().add(aZoneMembershipSettingData);
                this.sessionMapFabrics.save(aNamedAddressCollectionForZoneMembershipSettingData);
                */
              }
            }
          }

        }

      }

    }
    catch (WBEMException ce)
    {
    //  this.logger.error(this.CN, ce);
      return false;
    }
    return true;
  }
}