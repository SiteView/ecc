package com.dragonflow.siteview.infra.ipmi.commands;

import com.dragonflow.siteview.infra.ipmi.*;
import com.dragonflow.siteview.infra.ipmi.interfaces.IPMIInterface;
import com.dragonflow.siteview.infra.ipmi.util.IPMIUtils;
import java.util.Vector;

public class SDR
{
    public static class SDRInfo
    {

        public final short getFreeSpace()
        {
            return freeSpace;
        }

        public final short getRecordCount()
        {
            return recordCount;
        }

        public final byte getVersion()
        {
            return version;
        }

        private byte version;
        private short recordCount;
        private short freeSpace;
        private int lastAddStamp;
        private int lastDelStamp;
        private byte op_support;

        public SDRInfo(byte version, short recordCount, short freeSpace, int lastAddStamp, int lastDelStamp, byte op_support)
        {
            this.version = version;
            this.recordCount = recordCount;
            this.freeSpace = freeSpace;
            this.lastAddStamp = lastAddStamp;
            this.lastDelStamp = lastDelStamp;
            this.op_support = op_support;
        }
    }


    public SDR(IPMIInterface ipmiInterface)
        throws IPMIException
    {
        reservationID = 0;
        this.ipmiInterface = null;
        sdrInfo = null;
        maxSizeOfData = SDR_MAX_READ;
        this.ipmiInterface = ipmiInterface;
        initSDR();
        sdrInfo = getSDRInfo();
    }

    public Vector getRecordHeaders()
        throws IPMIException
    {
        Vector headers = new Vector();
        for(SensorInfoRecord.HeaderInfo sdrRecordHeaderInfo = getRecordHeader((short)0); sdrRecordHeaderInfo != null && sdrRecordHeaderInfo.getNextRecordID() != -1;)
        {
            headers.add(sdrRecordHeaderInfo);
            short nextRecordId = sdrRecordHeaderInfo.getNextRecordID();
            sdrRecordHeaderInfo = getRecordHeader(nextRecordId);
            if(sdrRecordHeaderInfo == null)
                throw new IPMIException((new StringBuilder()).append("Failed to get header for record id:").append(nextRecordId).toString());
        }

        return headers;
    }

    public Vector getFullRecords()
        throws IPMIException
    {
        Vector records = new Vector();
        Vector headers = getRecordHeaders();
        if(headers == null)
            throw new IPMIException("Failed to get full record since could not retrieve headers.");
        for(int i = 0; i < headers.size(); i++)
        {
            SensorInfoRecord.HeaderInfo sdrRecordHeaderInfo = (SensorInfoRecord.HeaderInfo)headers.get(i);
            SensorInfoRecord sensorRecord = getRecordInfo(sdrRecordHeaderInfo);
            if(sensorRecord == null)
                throw new IPMIException((new StringBuilder()).append("Failed to get full record info for record:").append(sdrRecordHeaderInfo.getRecordID()).toString());
            if(sensorRecord.isSupported())
                records.add(sensorRecord);
        }

        return records;
    }

    private void initSDR()
        throws IPMIException
    {
        Command cmd = new CommandBase((byte)10, (byte)34, null);
        IPMIMessage responseMessage = null;
        responseMessage = ipmiInterface.executeCommand(cmd, true);
        if(responseMessage == null || responseMessage.getCcode() != 0)
        {
            throw new IPMIException("Failed to retrieve SDR info");
        } else
        {
            byte responseData[] = responseMessage.getCmd().getCommandData();
            reservationID = IPMIUtils.bytesToShort(responseData, 0);
            return;
        }
    }

    private SensorInfoRecord.HeaderInfo getRecordHeader(short recordId)
        throws IPMIException
    {
        byte commandData[] = new byte[6];
        IPMIUtils.shortToBytes(commandData, 0, reservationID);
        IPMIUtils.shortToBytes(commandData, 2, recordId);
        commandData[4] = 0;
        commandData[5] = 5;
        Command cmd = new CommandBase((byte)10, (byte)35, commandData);
        IPMIMessage responseMessage = null;
        responseMessage = ipmiInterface.executeCommand(cmd, true);
        if(responseMessage == null || responseMessage.getCcode() != 0)
            throw new IPMIException("Failed to retrieve record header");
        byte responseData[] = responseMessage.getCmd().getCommandData();
        short nextRecordId = IPMIUtils.bytesToShort(responseData, 0);
        short retRecordId = IPMIUtils.bytesToShort(responseData, 2);
        if(recordId != 0 && retRecordId != recordId)
            throw new IPMIException((new StringBuilder()).append("not match record id. Expected:").append(recordId).append(", returned:").append(retRecordId).toString());
        else
            return new SensorInfoRecord.HeaderInfo(nextRecordId, retRecordId, responseData[4], responseData[5], responseData[6]);
    }

    private SensorInfoRecord getRecordInfo(SensorInfoRecord.HeaderInfo sdrRecordHeaderInfo)
        throws IPMIException
    {
        byte commandData[] = new byte[6];
        IPMIUtils.shortToBytes(commandData, 0, reservationID);
        IPMIUtils.shortToBytes(commandData, 2, sdrRecordHeaderInfo.getRecordID());
        Command cmd = new CommandBase((byte)10, (byte)35, commandData);
        short index = 0;
        short totalRecordLength = sdrRecordHeaderInfo.getLength();
        IPMIMessage responseMessage = null;
        byte recordData[] = new byte[totalRecordLength + 1];
        while(index < totalRecordLength) 
        {
            short requestedLength;
            if(totalRecordLength - index < maxSizeOfData)
                requestedLength = (byte)(totalRecordLength - index);
            else
                requestedLength = (byte)maxSizeOfData;
            commandData[4] = (byte)(index + 5);
            commandData[5] = (byte)requestedLength;
            try
            {
                responseMessage = ipmiInterface.executeCommand(cmd, true);
            }
            catch(IPMIException e)
            {
                throw new IPMIException((new StringBuilder()).append("Failed to retrieve record info, ID=").append(sdrRecordHeaderInfo.getRecordID()).append(" Commad: ").append(cmd.toString()).toString(), e);
            }
            if(responseMessage.getCcode() != 0)
            {
                if((responseMessage.getCcode() & 255) == 202)
                    maxSizeOfData = (byte)((maxSizeOfData >> 1) - 1);
                else
                if((responseMessage.getCcode() & 255) == 197)
                {
                    try
                    {
                        Thread.sleep(3000L);
                    }
                    catch(InterruptedException e)
                    {
                        throw new IPMIException("Failed to wait", e);
                    }
                    initSDR();
                    IPMIUtils.shortToBytes(commandData, 0, reservationID);
                } else
                {
                    throw new IPMIException((new StringBuilder()).append("Failed to retrieve record info, ID=").append(sdrRecordHeaderInfo.getRecordID()).toString());
                }
            } else
            {
                byte responseData[] = responseMessage.getCmd().getCommandData();
                System.arraycopy(responseData, 2, recordData, index, requestedLength);
                index += requestedLength;
            }
        }
        SensorInfoRecord sensorRecord = null;
        if(sdrRecordHeaderInfo.getType() == 1)
            sensorRecord = new FullSensorInfoRecord(recordData, sdrRecordHeaderInfo);
        else
        if(sdrRecordHeaderInfo.getType() == 2)
            sensorRecord = new CompactSensorInfoRecord(recordData, sdrRecordHeaderInfo);
        else
            sensorRecord = new UnsupportedRecordInfo(recordData, sdrRecordHeaderInfo);
        return sensorRecord;
    }

    public SDRInfo getSDRInfo()
        throws IPMIException
    {
        Command cmd = new CommandBase((byte)10, (byte)32, null);
        IPMIMessage responseMessage = null;
        responseMessage = ipmiInterface.executeCommand(cmd, true);
        if(responseMessage == null || responseMessage.getCcode() != 0)
            throw new IPMIException("Failed to retrieve SDR info");
        byte responseData[] = responseMessage.getCmd().getCommandData();
        if(responseData[0] != 81)
        {
            throw new IPMIException((new StringBuilder()).append("Unexpected SDR version, expected 51h, got:").append(IPMIUtils.byteToHex(responseData[0])).toString());
        } else
        {
            short numberOfRecords = IPMIUtils.bytesToShort(responseData, 1);
            short freeSpace = IPMIUtils.bytesToShort(responseData, 3);
            int addTimeStamp = IPMIUtils.bytesToInt(responseData, 5, 4);
            int delTimeStamp = IPMIUtils.bytesToInt(responseData, 9, 4);
            SDRInfo sdrInfo = new SDRInfo(responseData[0], numberOfRecords, freeSpace, addTimeStamp, delTimeStamp, responseData[13]);
            return sdrInfo;
        }
    }

    public static final byte SDR_RECORD_TYPE_FULL = 1;
    public static final byte SDR_RECORD_TYPE_COMPAC = 2;
    public static final byte SDR_RECORD_TYPE_EVENT_ONLY = 3;
    public static final byte SDR_RECORD_TYPE_ENTITY_ASSOC = 8;
    public static final byte SDR_RECORD_TYPE_DEVICE_RELATIVE_ENTITY_ASSOC = 9;
    public static final byte SDR_RECORD_TYPE_GENERIC_DEVICE_LOCATOR = 16;
    public static final byte SDR_RECORD_TYPE_FRU_DEVICE_LOCATOR = 17;
    public static final byte SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR = 18;
    public static final byte SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION = 19;
    public static final byte SDR_RECORD_TYPE_BMC_MESSAGE_CHANNEL_INFO = 20;
    public static final byte SDR_GET_REPOSITORY_INFO = 32;
    public static final byte SDR_GET_RESERVATION = 34;
    public static final byte SDR_GET_RECORD = 35;
    public static short SDR_MAX_READ = 255;
    private short reservationID;
    private IPMIInterface ipmiInterface;
    SDRInfo sdrInfo;
    short maxSizeOfData;

}

