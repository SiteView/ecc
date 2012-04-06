package com.dragonflow.siteview.infra.ipmi.commands;

import java.util.Locale;
import com.dragonflow.siteview.infra.ipmi.util.*;

public class FullSensorInfoRecord extends SensorInfoRecord
{

    public FullSensorInfoRecord(byte sensorData[], SensorInfoRecord.HeaderInfo headerInfo)
    {
        super(sensorData, headerInfo);
    }

    protected int getIDStringLengthCodeOffset()
    {
        return 42;
    }

    public byte getSensorType()
    {
        return sensorData[7];
    }

    public boolean isAnalog()
    {
        return sensorData[15] >>> 6 != 3;
    }

    public String getUnitsString(Locale locale)
    {
        int unitModifierCode = sensorData[15] >>> 1 & 3;
        byte base = sensorData[16];
        byte modifier = sensorData[17];
        IPMITranslator ipmiTranslator = TranslatorFactory.getTranslator(locale);
        String baseStr = ipmiTranslator.getUnitDescrition(base);
        String modifierStr = ipmiTranslator.getUnitDescrition(modifier);
        String unitString;
        if(unitModifierCode == 1)
            unitString = (new StringBuilder()).append(baseStr).append("/").append(modifierStr).toString();
        else
        if(unitModifierCode == 2)
            unitString = (new StringBuilder()).append(baseStr).append(" * ").append(modifierStr).toString();
        else
            unitString = baseStr;
        return unitString;
    }

    private boolean isLinearized()
    {
        return true;
    }

    private int getMFactor()
    {
        int m = IPMIUtils.get2MSBitsSigned(sensorData[20]);
        m <<= 8;
        int lsBits = sensorData[19] & 255;
        m |= lsBits;
        return m;
    }

    private int getToleranceFactor()
    {
        return IPMIUtils.get6LSBitsUnsigned(sensorData[20]);
    }

    private int getBFactor()
    {
        int b = IPMIUtils.get2MSBitsSigned(sensorData[22]);
        b <<= 8;
        int lsBits = sensorData[21] & 255;
        b |= lsBits;
        return b;
    }

    private int getAccuracyFactor()
    {
        int accuracy = IPMIUtils.get4MSBitsUnsigned(sensorData[23]);
        accuracy <<= 6;
        int lsBits = sensorData[22] & 63;
        accuracy |= lsBits;
        return accuracy;
    }

    private int getAccuracyExpFactor()
    {
        return (byte)((sensorData[23] & 12) >>> 2);
    }

    private int getRExpFactor()
    {
        return IPMIUtils.get4MSBitsSigned(sensorData[24]);
    }

    private int getBExpFactor()
    {
        return IPMIUtils.get4LSBitsSigned(sensorData[24]);
    }

    public float getLinearizedValue(byte value)
    {
        int iVal = value;
        iVal &= 255;
        int m = getMFactor();
        int b = getBFactor();
        int bExp = getBExpFactor();
        int rExp = getRExpFactor();
        float result = (float)(((double)(m * iVal) + (double)b * pow10(bExp)) * pow10(rExp));
        return result;
    }

    private static double pow10(int exp)
    {
        double res = 1.0D;
        int absExp = Math.abs(exp);
        for(int i = 0; i < absExp; i++)
            res *= 10D;

        if(exp < 0)
            res = 1.0D / res;
        return res;
    }

    private static final int OFFSET_STRING_TYPE_LENGTH_CODE = 42;
    private static final int OFFSET_SENSOR_TYPE = 7;
    private static final int OFFSET_SENSOR_UNITS = 15;
    private static final int OFFSET_SENSOR_BASE_UNITS = 16;
    private static final int OFFSET_SENSOR_MODIFIER_UNITS = 17;
    private static final int OFFSET_LINEARIZATION = 18;
    private static final int OFFSET_MLS_FACTOR = 19;
    private static final int OFFSET_MMS_TOLERANCE_FACTOR = 20;
    private static final int OFFSET_BLS_FACTOR = 21;
    private static final int OFFSET_BMS_ACCURACYLS_FACTOR = 22;
    private static final int OFFSET_ACCURACYMS_ACCURACYEXP_FACTOR = 23;
    private static final int OFFSET_REXP_BEXP_FACTOR = 24;
}
