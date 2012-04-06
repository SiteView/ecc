package com.dragonflow.siteview.infra.ipmi.commands;

import com.dragonflow.siteview.infra.ipmi.commands.SensorInfoRecord.HeaderInfo;

public abstract class SensorInfoRecord
{
    public static class HeaderInfo
    {

        public byte getLength()
        {
            return length;
        }

        public short getNextRecordID()
        {
            return nextRecordID;
        }

        public short getRecordID()
        {
            return recordID;
        }

        public byte getSdrVersion()
        {
            return sdrVersion;
        }

        public byte getType()
        {
            return type;
        }

        private short nextRecordID;
        private short recordID;
        private byte sdrVersion;
        private byte type;
        private byte length;

        public HeaderInfo(short nextRecordID, short recordID, byte sdrVersion, byte type, byte length)
        {
            this.nextRecordID = nextRecordID;
            this.recordID = recordID;
            this.sdrVersion = sdrVersion;
            this.type = type;
            this.length = length;
        }
    }


    public SensorInfoRecord(byte sensorData[], HeaderInfo headerInfo)
    {
        IDStringBytes = null;
        this.sensorData = sensorData;
        this.headerInfo = headerInfo;
        if(isSupported())
        {
            sensorUniqueId = (new StringBuilder()).append(String.valueOf(getEntityId())).append(".").append(String.valueOf(getEntityInstanceNumber())).append(".").append(String.valueOf(getSensorNumber())).toString();
        }
    }

    private SensorInfoRecord()
    {
        IDStringBytes = null;
    }

    public byte getEventReadingTypeCodeByte()
    {
        return sensorData[8];
    }

    public byte getEventReadingTypeCodeCategory()
    {
        byte eventReadingType = sensorData[8];
        if(eventReadingType == 0)
            return 0;
        if(eventReadingType == 1)
            return 1;
        if(eventReadingType >= 2 && eventReadingType <= 12 || eventReadingType == 111)
            return 2;
        return ((byte)(eventReadingType < 112 || eventReadingType > 127 ? 0 : 3));
    }

    public HeaderInfo getHeaderInfo()
    {
        return headerInfo;
    }

    public byte[] getIDStringBytes()
    {
        byte size = getIDStringLengthCode();
        if(size == 0)
            return new byte[0];
        int offset = getIDStringLengthCodeOffset() + 1;
        if(IDStringBytes == null)
        {
            IDStringBytes = new byte[size];
            System.arraycopy(sensorData, offset, IDStringBytes, 0, size);
        }
        return IDStringBytes;
    }

    public String getSensorUniqueIdAsString()
    {
        return sensorUniqueId;
    }

    public byte getSensorNumber()
    {
        return sensorData[2];
    }

    public final byte getEntityId()
    {
        return sensorData[3];
    }

    public final boolean isPhysicalEntityInstance()
    {
        return (sensorData[4] & 128) == 0;
    }

    public final byte getEntityInstanceNumber()
    {
        return (byte)(sensorData[4] & 127);
    }

    public final boolean isIgnoreIfEntityIsAbsentOrDisabled()
    {
        return (sensorData[6] & 128) != 0;
    }

    public boolean isSupported()
    {
        return true;
    }

    public abstract byte getSensorType();

    public abstract boolean isAnalog();

    protected byte getIDStringLengthCode()
    {
        int offset = getIDStringLengthCodeOffset();
        return (byte)(sensorData[offset] & 31);
    }

    protected abstract int getIDStringLengthCodeOffset();

    public static final int EVENT_READING_TYPE_CATEGORY_UNSPECIFY = 0;
    public static final int EVENT_READING_TYPE_CATEGORY_THRESHOLD = 1;
    public static final int EVENT_READING_TYPE_CATEGORY_DISCRATE = 2;
    public static final int EVENT_READING_TYPE_CATEGORY_OEM = 3;
    protected static final int OFFSET_HEADER = 5;
    protected static final int OFFSET_SENSOR_NUMBER = 2;
    protected static final int OFFSET_ENTITY_ID = 3;
    protected static final int OFFSET_ENTITY_INSTANCE = 4;
    protected static final int OFFSET_SENSOR_CAPABILITY = 6;
    protected static final int OFFSET_READING_TYPE_CODE = 8;
    protected byte sensorData[];
    protected HeaderInfo headerInfo;
    private byte IDStringBytes[];
    private String sensorUniqueId;

}
