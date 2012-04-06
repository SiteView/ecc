package com.dragonflow.siteview.infra.ipmi;

public class IPMIMessage {
    public IPMIMessage()
    {
        ccode = 0;
    }

    public byte getCcode()
    {
        return ccode;
    }

    public byte getMsglen()
    {
        return msglen;
    }

    public byte getRq_addr()
    {
        return rq_addr;
    }

    public byte getRq_lun()
    {
        return rq_lun;
    }

    public byte getRs_addr()
    {
        return rs_addr;
    }

    public byte getRq_seq()
    {
        return rq_seq;
    }

    public byte getRs_lun()
    {
        return rs_lun;
    }

    public Command getCmd()
    {
        return cmd;
    }

    public int getSize()
    {
        return 8 + cmd.getCommandData().length;
    }

    private byte twosComplementChecksum()
    {
        byte csum = (byte)(rq_addr + rq_seq + cmd.getCommand());
        int sizeOfData = cmd.getCommandData().length;
        for(int i = 0; i < sizeOfData; i++)
            csum += cmd.getCommandData()[i];

        return (byte)(-csum);
    }

    public int parseDataToMessage(byte data[], int offset)
        throws IPMIException
    {
        msglen = data[offset++];
        rq_addr = data[offset++];
        byte netfn = (byte)(data[offset] >>> 2);
        rq_lun = (byte)(data[offset++] & 3);
        offset++;
        rs_addr = data[offset++];
        rq_seq = (byte)(data[offset] >>> 2);
        rs_lun = (byte)(data[offset++] & 3);
        byte cmd = data[offset++];
        ccode = data[offset++];
        byte cmdData[] = null;
        if(msglen > 8)
        {
            cmdData = new byte[msglen - 8];
            System.arraycopy(data, offset, cmdData, 0, msglen - 8);
        }
        this.cmd = new CommandBase(netfn, cmd, cmdData);
        return msglen;
    }

    public void setCommand(Command cmd)
    {
        this.cmd = cmd;
    }

    public int buildDataToSend(byte data[], int offset)
        throws IPMIException
    {
        int orgOffset = offset;
        if(cmd == null)
            throw new IPMIException("Failed to build message since command was not set");
        rq_seq = (byte)(reqCounter++);
        if(reqCounter >= 64)
            reqCounter = 0;
        rq_seq = (byte)(rq_seq << 2);
        rq_addr = -127;
        rs_addr = 32;
        data[offset] = (byte)(getSize() - 1);
        data[offset + 1] = rs_addr;
        byte netFn = (byte)(cmd.getNetFn() << 2);
        data[offset + 2] = (byte)(netFn | 0);
        byte checksum = (byte)(256 - (32 + netFn));
        data[offset + 3] = checksum;
        data[offset + 4] = rq_addr;
        data[offset + 5] = rq_seq;
        data[offset + 6] = cmd.getCommand();
        int datasize = cmd.getCommandData().length;
        System.arraycopy(cmd.getCommandData(), 0, data, offset + 7, datasize);
        data[offset + 7 + datasize] = twosComplementChecksum();
        return getSize();
    }

    public int getMessageBodyIndex()
    {
        return 1;
    }

    public boolean isGenericCcode()
    {
        int code = ccode & 255;
        return code == 0 || code >= 192 && code <= 255;
    }

    private static final int MESSAGE_HEADER_SIZE = 8;
    private static int reqCounter = 0;
    private byte ccode;
    private byte msglen;
    private byte rq_addr;
    private byte rq_lun;
    private byte rs_addr;
    private byte rq_seq;
    private byte rs_lun;
    private Command cmd;

}
