package com.dragonflow.siteview.infra.ipmi.commands;

public abstract class SensorReadingRecord
{

    public SensorReadingRecord(SensorInfoRecord sensorInfoRecord, byte recordData[], byte ccode)
    {
        isValidRead = false;
        this.recordData = recordData;
        this.sensorInfoRecord = sensorInfoRecord;
        this.ccode = ccode;
        if((this.recordData[1] & 32) != 0)
            isValidRead = false;
        else
            isValidRead = true;
    }

    public SensorInfoRecord getSensorInfoRecord()
    {
        return sensorInfoRecord;
    }

    public boolean isValidRead()
    {
        return isValidRead;
    }

    public float getValue()
    {
        return (float)recordData[0];
    }

    public byte getCcode()
    {
        return ccode;
    }

    public boolean isError()
    {
        return false;
    }

    public abstract int getStatus();

    public static final int SENSOR_STATUS_OK = 0;
    public static final int SENSOR_STATUS_WARNING = 1;
    public static final int SENSOR_STATUS_ERROR = 2;
    public static final int SENSOR_STATUS_UNKNOWN = 3;
    protected byte recordData[];
    protected SensorInfoRecord sensorInfoRecord;
    private boolean isValidRead;
    private byte ccode;
}
