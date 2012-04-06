package com.dragonflow.siteview.infra.ipmi.commands;

public final class CompactSensorInfoRecord extends SensorInfoRecord
{

    public CompactSensorInfoRecord(byte sensorData[], SensorInfoRecord.HeaderInfo headerInfo)
    {
        super(sensorData, headerInfo);
    }

    protected final int getIDStringLengthCodeOffset()
    {
        return 26;
    }

    public final byte getSensorType()
    {
        return sensorData[7];
    }

    public final boolean isAnalog()
    {
        return false;
    }
}
