package com.dragonflow.siteview.infra.ipmi.interfaces.lan;

import com.dragonflow.siteview.infra.ipmi.IPMIException;
import com.dragonflow.siteview.infra.ipmi.IPMISession;
import com.dragonflow.siteview.infra.ipmi.IPMISessionedMessage;
import com.dragonflow.siteview.infra.ipmi.interfaces.lan.RMCP;

public class LanIPMIMessage 
	extends IPMISessionedMessage
{

    public LanIPMIMessage(IPMISession ipmiSession)
    {
        super(ipmiSession);
    }

    public int buildDataToSend(byte data[], int offset)
        throws IPMIException
    {
        int sizeofHeader = rmcpHeader.getMessageData(data, offset);
        offset += sizeofHeader;
        int sizefMessage = super.buildDataToSend(data, offset);
        offset += sizefMessage;
        return sizeofHeader + sizefMessage;
    }

    public int parseDataToMessage(byte responseData[], int offset)
        throws IPMIException
    {
        offset += 4;
        int size = super.parseDataToMessage(responseData, offset);
        return size + 4;
    }

    static RMCP.RMCPHeader rmcpHeader = new RMCP.RMCPHeader((byte)7, (byte)-1);

}
