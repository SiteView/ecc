package com.dragonflow.siteview.infra.ipmi.interfaces.lan;

import com.dragonflow.siteview.infra.ipmi.IPMIException;

import java.io.IOException;
import java.net.*;

public class RMCP
{
    public static class RMCPHeader
    {

        public byte getVersion()
        {
            return version;
        }

        public byte getRmcpSeqNum()
        {
            return rmcpSeqNum;
        }

        public byte getMessageClass()
        {
            return messageClass;
        }

        public int getMessageData(byte headerData[], int offset)
        {
            headerData[offset + 0] = version;
            headerData[offset + 1] = reserved;
            headerData[offset + 2] = rmcpSeqNum;
            headerData[offset + 3] = messageClass;
            return 4;
        }

        public int processData(byte responseData[], int offset)
        {
            offset += 4;
            return 4;
        }

        private byte version;
        protected byte reserved;
        private byte rmcpSeqNum;
        private byte messageClass;

        public RMCPHeader(byte messageClass, byte RMCPSeqNum)
        {
            version = 6;
            reserved = 0;
            rmcpSeqNum = RMCPSeqNum;
            this.messageClass = messageClass;
        }
    }


    public RMCP()
        throws SocketException, SecurityException
    {
        saddress = null;
        recvBuf = new byte[256];
        socket = new DatagramSocket();
    }

    public boolean sendPacket(String SIP, int port, byte payLoad[], int datalength)
        throws IPMIException
    {
        boolean gotback = false;
        int retries = 5;
        int SIPint = 0;
        try
        {
            saddress = InetAddress.getByName(SIP);
            SIPint = saddress.hashCode();
            if(SIPint == 0)
                return false;
        }
        catch(UnknownHostException ue)
        {
            return false;
        }
        int slength = datalength;
        DatagramPacket s_packet = new DatagramPacket(payLoad, slength, saddress, port);
        DatagramPacket r_packet = new DatagramPacket(recvBuf, recvBuf.length);
        do
        {
            if(socket == null || socket.isClosed())
                return false;
            try
            {
                socket.send(s_packet);
            }
            catch(SecurityException e)
            {
                return false;
            }
            catch(IOException ie)
            {
                return false;
            }
            try
            {
                socket.setSoTimeout(5000);
            }
            catch(SecurityException e)
            {
                return false;
            }
            catch(IOException ie)
            {
                return false;
            }
            gotback = false;
            try
            {
                socket.receive(r_packet);
                gotback = true;
                break;
            }
            catch(SecurityException e)
            {
                throw new IPMIException("Failed to recieve a packate:", e);
            }
            catch(SocketTimeoutException sto)
            {
                if(retries <= 1)
                    throw new IPMIException("No more retries!!!, Failed to send RMCP packet", sto);
            }
            catch(IOException ie)
            {
                if(retries <= 1)
                    throw new IPMIException("No more retries!!!, Failed to send RMCP packet", ie);
            }
        } while(--retries > 0);
        return gotback;
    }

    public byte[] getRecvBuf()
    {
        return recvBuf;
    }

    private DatagramSocket socket;
    private InetAddress saddress;
    private byte recvBuf[];

}

