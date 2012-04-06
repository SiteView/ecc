package com.dragonflow.siteview.infra.ipmi.commands;

public final class UnsupportedRecordInfo extends SensorInfoRecord
{

    public UnsupportedRecordInfo(byte sensorData[], SensorInfoRecord.HeaderInfo headerInfo)
    {
        super(sensorData, headerInfo);
    }

    protected final int getIDStringLengthCodeOffset()
    {
        return -1;
    }

    public final byte getSensorType()
    {
        return 0;
    }

    public final boolean isAnalog()
    {
        return false;
    }

    public final boolean isSupported()
    {
        return false;
    }
}
