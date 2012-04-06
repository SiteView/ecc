package com.dragonflow.siteview.infra.ipmi.commands;

import java.util.HashSet;
import java.util.Set;



public class DiscreteSensorReadingRecord extends SensorReadingRecord
{

    public DiscreteSensorReadingRecord(SensorInfoRecord sensorInfoRecord, byte recordData[], byte ccode)
    {
        super(sensorInfoRecord, recordData, ccode);
    }

    public Set getStateAsserted()
    {
        Set s = new HashSet();
        if(!isValidRead())
            return s;
        if((recordData[2] & 1) != 0)
            s.add(BYTE0);
        if((recordData[2] & 2) != 0)
            s.add(BYTE1);
        if((recordData[2] & 4) != 0)
            s.add(BYTE2);
        if((recordData[2] & 8) != 0)
            s.add(BYTE3);
        if((recordData[2] & 16) != 0)
            s.add(BYTE4);
        if((recordData[2] & 32) != 0)
            s.add(BYTE5);
        if((recordData[2] & 64) != 0)
            s.add(BYTE6);
        if((recordData[2] & 128) != 0)
            s.add(BYTE7);
        if(recordData.length >= 4)
        {
            if((recordData[3] & 1) != 0)
                s.add(BYTE8);
            if((recordData[3] & 2) != 0)
                s.add(BYTE9);
            if((recordData[3] & 4) != 0)
                s.add(BYTE10);
            if((recordData[3] & 8) != 0)
                s.add(BYTE11);
            if((recordData[3] & 16) != 0)
                s.add(BYTE12);
            if((recordData[3] & 32) != 0)
                s.add(BYTE13);
            if((recordData[3] & 64) != 0)
                s.add(BYTE14);
        }
        return s;
    }

    public int getStatus()
    {
        Set s = getStateAsserted();
        byte eventTypeCode = getSensorInfoRecord().getEventReadingTypeCodeByte();
        if(eventTypeCode == 2)
            return 0;
        if(eventTypeCode == 3 || eventTypeCode == 4 || eventTypeCode == 5)
            return !s.contains(BYTE1) ? 0 : 2;
        if(eventTypeCode == 7)
        {
            if(s.contains(BYTE2) || s.contains(BYTE3) || s.contains(BYTE5) || s.contains(BYTE6))
                return 2;
            return !s.contains(BYTE1) && !s.contains(BYTE4) ? 0 : 1;
        }
        if(eventTypeCode == 8)
            return !s.contains(BYTE0) ? 0 : 2;
        if(eventTypeCode == 9)
            return !s.contains(BYTE0) ? 0 : 2;
        if(eventTypeCode == 10)
            return 0;
        return eventTypeCode != 11 ? 0 : 0;
    }

    protected static final Byte BYTE0 = new Byte((byte)0);
    protected static final Byte BYTE1 = new Byte((byte)1);
    protected static final Byte BYTE2 = new Byte((byte)2);
    protected static final Byte BYTE3 = new Byte((byte)3);
    protected static final Byte BYTE4 = new Byte((byte)4);
    protected static final Byte BYTE5 = new Byte((byte)5);
    protected static final Byte BYTE6 = new Byte((byte)6);
    protected static final Byte BYTE7 = new Byte((byte)7);
    protected static final Byte BYTE8 = new Byte((byte)8);
    protected static final Byte BYTE9 = new Byte((byte)9);
    protected static final Byte BYTE10 = new Byte((byte)16);
    protected static final Byte BYTE11 = new Byte((byte)17);
    protected static final Byte BYTE12 = new Byte((byte)18);
    protected static final Byte BYTE13 = new Byte((byte)19);
    protected static final Byte BYTE14 = new Byte((byte)20);

}
