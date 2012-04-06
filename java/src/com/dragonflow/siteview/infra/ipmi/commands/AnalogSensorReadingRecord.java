package com.dragonflow.siteview.infra.ipmi.commands;

public class AnalogSensorReadingRecord extends 
	DiscreteSensorReadingRecord
{

    public AnalogSensorReadingRecord(SensorInfoRecord sensorInfoRecord, byte recordData[], byte ccode)
    {
        super(sensorInfoRecord, recordData, ccode);
    }

    public int getStatus()
    {
        byte eventTypeCode = getSensorInfoRecord().getEventReadingTypeCodeByte();
        if(eventTypeCode != 1)
            return super.getStatus();
        if((recordData[2] & 36) != 0)
            return 2;
        if((recordData[2] & 18) != 0)
            return 2;
        return (recordData[2] & 9) == 0 ? 0 : 1;
    }

    public float getValue()
    {
        return ((FullSensorInfoRecord)getSensorInfoRecord()).getLinearizedValue(recordData[0]);
    }
}

