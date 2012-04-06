package com.dragonflow.siteview.san.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Vector;
import javax.cim.CIMDateTime;
import javax.cim.CIMDateTimeAbsolute;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.CIMProperty;
import javax.cim.UnsignedInteger16;
import javax.cim.UnsignedInteger32;
import javax.cim.UnsignedInteger64;
import javax.cim.UnsignedInteger8;
import javax.wbem.CloseableIterator;

public class CIM_DataTypes
{
  private String CN = "CIM_DataTypes";
  Calendar realCal = null;

  public CIM_DataTypes()
  {
    //this.logger = Logger.getLogger(this.CN);
  }

  public CIMDateTime getCIMInstancePropertyDateTime(CIMInstance cimInstance, String propertyName)
  {
    CIMDateTime propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);

    if (property.getValue() != null)
    {
      if (property.getValue() instanceof CIMDateTime)
      {
        propertyValue = (CIMDateTime)property.getValue();
      }
      else
      {
        try
        {
          String strValue = (String)property.getValue();

          propertyValue = new CIMDateTimeAbsolute(strValue);
        }
        catch (Exception e)
        {
          //this.logger.error("ERROR", e);
        }

      }

    }
    else {
      //this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public Boolean getCIMInstancePropertyBooleanValue(CIMInstance cimInstance, String propertyName)
  {
    Boolean propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);
    if (property.getValue() != null)
    {
      if (property.getValue() instanceof Boolean)
      {
        propertyValue = (Boolean)property.getValue();
      }
      else
      {
        try
        {
          String strValue = (String)property.getValue();

          propertyValue = new Boolean(strValue);
        }
        catch (Exception e)
        {
         // this.logger.error("ERROR", e);
        }

      }

    }
    else {
      //this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public UnsignedInteger64 getCIMInstancePropertyUnsignedInt64Value(CIMInstance cimInstance, String propertyName)
  {
    UnsignedInteger64 propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);

    if (property.getValue() != null)
    {
      if (property.getValue() instanceof UnsignedInteger64)
      {
        propertyValue = (UnsignedInteger64)property.getValue();
      }
      else
      {
        try
        {
          String strValue = (String)property.getValue();

          propertyValue = new UnsignedInteger64(strValue);
        }
        catch (Exception e)
        {
          //this.logger.info("ERROR", e);
        }

      }

    }
    else {
     // this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public UnsignedInteger32 getCIMInstancePropertyUnsignedInt32Value(CIMInstance cimInstance, String propertyName)
  {
    UnsignedInteger32 propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);
    if (property.getValue() != null)
    {
      if (property.getValue() instanceof UnsignedInteger32)
      {
        propertyValue = (UnsignedInteger32)property.getValue();
      }
      else
      {
        try
        {
          String strValue = (String)property.getValue();

          propertyValue = new UnsignedInteger32(strValue);
        }
        catch (Exception e)
        {
         // this.logger.error("ERROR", e);
        }

      }

    }
    else {
     // this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public UnsignedInteger16 getCIMInstancePropertyUnsignedInt16Value(CIMInstance cimInstance, String propertyName)
  {
    UnsignedInteger16 propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);
    if (property.getValue() != null)
    {
      if (property.getValue() instanceof UnsignedInteger16)
      {
        propertyValue = (UnsignedInteger16)property.getValue();
      }
      else
      {
        try
        {
          String strValue = (String)property.getValue();

          propertyValue = new UnsignedInteger16(strValue);
        }
        catch (Exception e)
        {
          //this.logger.error("Error", e);
        }

      }

    }
    else {
      //this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public UnsignedInteger8 getCIMInstancePropertyUnsignedInt8Value(CIMInstance cimInstance, String propertyName)
  {
    UnsignedInteger8 propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);
    if (property.getValue() != null)
    {
      if (property.getValue() instanceof UnsignedInteger8)
      {
        propertyValue = (UnsignedInteger8)property.getValue();
      }
      else
      {
        try
        {
          String strValue = (String)property.getValue();

          propertyValue = new UnsignedInteger8(strValue);
        }
        catch (Exception e)
        {
          //this.logger.error("ERROR", e);
        }

      }

    }
    else {
     // this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public CIMObjectPath getCIMInstancePropertyObjectPath(CIMInstance cimInstance, String propertyName)
  {
    CIMObjectPath propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);
    if (property.getValue() != null)
    {
      try
      {
        propertyValue = (CIMObjectPath)property.getValue();
      }
      catch (Exception e)
      {
        //this.logger.info("Error", e);
      }

    }
    else
    {
      //this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public String getCIMInstancePropertyValueString(CIMInstance cimInstance, String propertyName)
  {
    String propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);
    if (property.getValue() != null)
    {
      try
      {
        propertyValue = (String)property.getValue();
      }
      catch (Exception e)
      {
        //this.logger.error("ERROR", e);
      }

    }
    else
    {
     // this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public Vector getCIMInstancePropertyValueVector(CIMInstance cimInstance, String propertyName)
  {
    Vector propertyValue = null;

    CIMProperty property = cimInstance.getProperty(propertyName);
    if (property.getValue() != null)
    {
      try
      {
        propertyValue = (Vector)property.getValue();
      }
      catch (Exception e)
      {
        ///this.logger.info("Error", e);
      }

    }
    else
    {
     // this.logger.info(propertyName + " does not exist");
    }
    return propertyValue;
  }

  public ArrayList<CIMInstance> iteratorToInstanceArrayList(CloseableIterator<CIMInstance> pIter)
  {
    ArrayList instList = new ArrayList();

    while (pIter.hasNext())
    {
      instList.add((CIMInstance)pIter.next());
    }
    return instList;
  }

  public UnsignedInteger16[] getUint16ArrayPropertyValue(CIMInstance pInst, String pPropName)
    throws Exception
  {
    UnsignedInteger16[] propertyValue = (UnsignedInteger16[])null;

    CIMProperty property = pInst.getProperty(pPropName);
    if (property.getValue() != null)
    {
      if (property.getValue() instanceof UnsignedInteger16[])
      {
        propertyValue = (UnsignedInteger16[])property.getValue();
      }
      else
      {
        try
        {
          String[] strArray = (String[])property.getValue();

          propertyValue = stringArrayToUInt16Array(strArray);
        }
        catch (Exception e)
        {
          //this.logger.error("ERROR", e);
        }

      }

    }
    else {
     // this.logger.info(pPropName + " does not exist");
    }
    return propertyValue;
  }

  public UnsignedInteger16[] stringArrayToUInt16Array(String[] pStringArray)
    throws NumberFormatException
  {
    UnsignedInteger16[] retArray = (UnsignedInteger16[])null;
    if (pStringArray != null) {
      retArray = new UnsignedInteger16[pStringArray.length];

      for (int i = 0; i < retArray.length; ++i)
      {
        retArray[i] = new UnsignedInteger16(pStringArray[i]);
      }
    }
    return retArray;
  }

  public String[] getStringArrayPropertyValue(CIMInstance pInst, String pPropName)
    throws Exception
  {
    String[] propertyValue = (String[])null;

    CIMProperty property = pInst.getProperty(pPropName);
    if (property.getValue() != null)
    {
      if (property.getValue() instanceof String[])
      {
        propertyValue = (String[])property.getValue();
      }
      else
      {
        try
        {
          String[] strArray = (String[])property.getValue();

          propertyValue = strArray;
        }
        catch (Exception e)
        {
         // this.logger.error("ERROR", e);
        }

      }

    }
    else {
      //this.logger.info(pPropName + " does not exist");
    }
    return propertyValue;
  }

  public Calendar getCalendarFromCIMDateTime(CIMDateTime cdt)
    throws NumberFormatException
  {
    Calendar cal = Calendar.getInstance();
    if (cdt != null) {
      String statisticTimeString = cdt.toString();
      CIMDateTimeAbsolute cdtAbs = new CIMDateTimeAbsolute(statisticTimeString);

      cal.set(1, cdtAbs.getYear());
      cal.set(2, cdtAbs.getMonth() - 1);
      cal.set(5, cdtAbs.getDay());
      cal.set(11, cdtAbs.getHour());
      cal.set(12, cdtAbs.getMinute());
      cal.set(13, cdtAbs.getSecond());
      //this.logger.info("*******************************************************************");
      ///this.logger.info("Calendar.YEAR: " + cdtAbs.getYear());
      //this.logger.info("Calendar.MONTH: " + cdtAbs.getMonth());
      //this.logger.info("Calendar.DAY_OF_MONTH: " + cdtAbs.getDay());
      //this.logger.info("Calendar.HOUR_OF_DAY: " + cdtAbs.getHour());
      //this.logger.info("Calendar.MINUTE: " + cdtAbs.getMinute());
      //this.logger.info("Calendar.SECOND: " + cdtAbs.getSecond());
      //this.logger.info("*******************************************************************");
    }
    else
    {
      //this.logger.info(cdt + " does not exist");
    }
    return cal;
  }
}