package com.dragonflow.siteview.infra.ipmi.commands;

import com.dragonflow.siteview.infra.ipmi.*;
import com.dragonflow.siteview.infra.ipmi.interfaces.IPMIInterface;


public class Sensor
{

    public Sensor(IPMIInterface ipmiInterface)
    {
        this.ipmiInterface = ipmiInterface;
    }

    public SensorReadingRecord readSensor(SensorInfoRecord sensorInfoRecord)
        throws IPMIException
    {
        byte commandData[] = {
            sensorInfoRecord.getSensorNumber()
        };
        Command cmd = new CommandBase((byte)4, (byte)45, commandData);
        IPMIMessage responseMessage = null;
        try
        {
            responseMessage = ipmiInterface.executeCommand(cmd, true);
        }
        catch(IPMIException e)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to read sensor. ID:").append(sensorInfoRecord.getSensorNumber()).append(" Command: ").append(cmd.toString()).toString(), e);
        }
        SensorReadingRecord sensorReadingRecord = null;
        if(responseMessage.getCcode() != 0)
            return new SensorErrorReadingRecord(sensorInfoRecord, new byte[] {
                0, 32
            }, responseMessage.getCcode());
        byte responseData[] = responseMessage.getCmd().getCommandData();
        if(sensorInfoRecord.isAnalog())
            sensorReadingRecord = new AnalogSensorReadingRecord(sensorInfoRecord, responseData, responseMessage.getCcode());
        else
            sensorReadingRecord = new DiscreteSensorReadingRecord(sensorInfoRecord, responseData, responseMessage.getCcode());
        return sensorReadingRecord;
    }

    private byte[] getSensorReadingFactors(byte sensorNumber, byte readingData[])
        throws IPMIException
    {
        byte commandData[] = {
            sensorNumber, readingData[0]
        };
        Command cmd = new CommandBase((byte)4, (byte)35, commandData);
        IPMIMessage responseMessage = null;
        try
        {
            responseMessage = ipmiInterface.executeCommand(cmd, true);
        }
        catch(IPMIException e)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to read sensor. Sensor ID:").append(sensorNumber).append(" Command: ").append(cmd.toString()).toString(), e);
        }
        if(responseMessage.getCcode() != 0)
            return readingData;
        else
            return responseMessage.getCmd().getCommandData();
    }

    public static final byte SDR_GET_SENSOR_READING = 45;
    public static final byte SDR_GET_SENSOR_READING_FACTORS = 35;
    IPMIInterface ipmiInterface;
}
