package com.dragonflow.siteview.infra.ipmi.commands;

public class SensorErrorReadingRecord extends SensorReadingRecord
{

    public SensorErrorReadingRecord(SensorInfoRecord sensorInfoRecord, byte recordData[], byte ccode)
    {
        super(sensorInfoRecord, recordData, ccode);
    }

    public int getStatus()
    {
        return 2;
    }

    public final boolean isError()
    {
        return true;
    }
}
