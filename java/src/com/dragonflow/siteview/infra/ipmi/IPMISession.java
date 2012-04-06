package com.dragonflow.siteview.infra.ipmi;

import com.dragonflow.siteview.infra.ipmi.util.IPMIUtils;

public class IPMISession {
    public IPMISession(String targetIP, byte userName[], byte userPassword[])
    {
        port = 623;
        authType = 0;
        maxPrivLevel = 0;
        this.targetIP = targetIP;
        setUserName(userName);
        setUserPassword(userPassword);
        init();
    }

    private void init()
    {
        authType = 0;
        sessionID = (new byte[] {
            0, 0, 0, 0
        });
        inboundSessionSeq = (new byte[] {
            0, 0, 0, 0
        });
        challengeStringData = (new byte[] {
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0
        });
        maxPrivLevel = 0;
        outboundSessionSeq = (new byte[] {
            0, 0, 1, 0
        });
        sessionActivated = false;
        inUse = false;
        lastSessionTime = System.currentTimeMillis();
    }

    public int getPort()
    {
        return port;
    }

    public void setPort(int port)
    {
        this.port = port;
    }

    public byte[] getOutboundSessionSeq()
    {
        return outboundSessionSeq;
    }

    public byte[] getUserName()
    {
        return userName;
    }

    private void setUserName(byte userName[])
    {
        if(userName != null)
            System.arraycopy(userName, 0, this.userName, 0, userName.length);
    }

    public byte[] getInboundSessionSeq()
    {
        return inboundSessionSeq;
    }

    public void setInboundSessionSeq(byte inboundSessionSeq[])
        throws IPMIException
    {
        if(inboundSessionSeq == null)
            throw new IPMIException("Failed to set the session seq. can't set null session seq");
        if(inboundSessionSeq.length != 4)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to set the session seq. incorrect parameter length. expected 4, got:").append(inboundSessionSeq.length).toString());
        } else
        {
            this.inboundSessionSeq = inboundSessionSeq;
            return;
        }
    }

    public byte getAuthType()
    {
        return authType;
    }

    public void setAuthType(byte authType)
    {
        this.authType = authType;
    }

    public byte[] getSessionID()
    {
        return sessionID;
    }

    public void setSessionID(byte sessionID[])
        throws IPMIException
    {
        if(sessionID == null)
            throw new IPMIException("Failed to set the session id. can't set null session id");
        if(sessionID.length != 4)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to set the session id. incorrect parameter length. expected 4, got:").append(sessionID.length).toString());
        } else
        {
            this.sessionID = sessionID;
            return;
        }
    }

    public byte[] getChallengeStringData()
    {
        return challengeStringData;
    }

    public void setChallengeStringData(byte challengeStringData[])
        throws IPMIException
    {
        if(challengeStringData == null)
            throw new IPMIException("Failed to set the session challengeStringData. can't set null challange string");
        if(challengeStringData.length != 16)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to set the challengeStringData. incorrect parameter length. expected 16, got:").append(challengeStringData.length).toString());
        } else
        {
            this.challengeStringData = challengeStringData;
            return;
        }
    }

    public byte getMaxPrivLevel()
    {
        return maxPrivLevel;
    }

    public void setMaxPrivLevel(byte maxPrivLevel)
    {
        this.maxPrivLevel = maxPrivLevel;
    }

    public byte[] getUserPassword()
    {
        return userPassword;
    }

    private void setUserPassword(byte userPassword[])
    {
        if(userName != null)
            System.arraycopy(userPassword, 0, this.userPassword, 0, userPassword.length);
    }

    public synchronized boolean lockout()
    {
        inUse = true;
        lastSessionTime = System.currentTimeMillis();
        return true;
    }

    public synchronized void releaselock()
    {
        inUse = false;
    }

    public synchronized void increaseInboundSessionSeq()
    {
        if(sessionActivated)
        {
            int SeqNum = IPMIUtils.bytesToInt(inboundSessionSeq, 0, 4) + 1;
            IPMIUtils.intToBytes(inboundSessionSeq, 0, SeqNum);
        }
    }

    public String getTargetIP()
    {
        return targetIP;
    }

    public boolean isInUse()
    {
        return inUse;
    }

    public void setInUse(boolean inUse)
    {
        this.inUse = inUse;
    }

    public boolean isSessionActivated()
    {
        return sessionActivated;
    }

    public void activateSession()
    {
        sessionActivated = true;
    }

    public void terminateSession()
    {
        init();
    }

    public int getSize()
    {
        return !isSessionActivated() || authType == 0 ? 9 : 25;
    }

    public int getMessageData(byte data[], int offset)
        throws IPMIException
    {
        int orgOffset = offset;
        data[offset] = isSessionActivated() ? getAuthType() : 0;
        offset++;
        System.arraycopy(inboundSessionSeq, 0, data, offset, 4);
        offset += 4;
        System.arraycopy(sessionID, 0, data, offset, 4);
        offset += 4;
        if(isSessionActivated() && authType != 0)
        {
            System.arraycopy(getUserPassword(), 0, data, offset, 16);
            offset += 16;
        }
        return offset - orgOffset;
    }

    public int processData(byte responseData[], int offset)
    {
        byte authType = responseData[offset++];
        offset += 4;
        offset += 4;
        int headerSize = 9;
        if(authType != 0)
            headerSize += 16;
        return headerSize;
    }

    private String targetIP;
    private int port;
    private byte authType;
    private byte sessionID[];
    private byte inboundSessionSeq[];
    private byte challengeStringData[];
    private byte maxPrivLevel;
    private byte userName[] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0
    };
    private byte userPassword[] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0
    };
    private boolean sessionActivated;
    private byte outboundSessionSeq[];
    private boolean inUse;
    private long lastSessionTime;
}
