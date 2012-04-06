package com.dragonflow.siteview.infra.ipmi.interfaces.lan;

import java.net.SocketException;
import com.dragonflow.siteview.infra.ipmi.util.IPMIUtils;
import com.dragonflow.siteview.infra.ipmi.IPMISession;
import com.dragonflow.siteview.infra.ipmi.IPMIException;
import com.dragonflow.siteview.infra.ipmi.Command;
import com.dragonflow.siteview.infra.ipmi.CommandBase;
import com.dragonflow.siteview.infra.ipmi.IPMIMessage;
import com.dragonflow.siteview.infra.ipmi.Constants;

import com.dragonflow.siteview.infra.ipmi.interfaces.IPMIInterface;
import com.dragonflow.siteview.infra.ipmi.interfaces.lan.ASFMessage;
import com.dragonflow.siteview.infra.ipmi.interfaces.lan.LanIPMIMessage;
import com.dragonflow.siteview.infra.ipmi.interfaces.lan.RMCP;
import com.dragonflow.siteview.infra.ipmi.util.TranslatorFactory;

public class LANIfs
implements IPMIInterface
{

private LANIfs()
{
}

public LANIfs(String targetIP, int port, byte user[], byte password[])
    throws SocketException, SecurityException
{
    rmcp = new RMCP();
    session = new IPMISession(targetIP, user, password);
    if(port != 0)
        session.setPort(port);
}

public void init()
    throws IPMIException
{
    startSession();
}

public void end()
    throws IPMIException
{
    if(!session.isSessionActivated())
        return;
    byte messageData[] = new byte[4];
    messageData = session.getSessionID();
    Command command = new CommandBase((byte)6, (byte)60, messageData);
    IPMIMessage responseMessage = executeCommand(command, false);
    if(responseMessage == null)
        throw new IPMIException("Failed to getUserAuhentications:");
    if(responseMessage.getCcode() != 0)
    {
        if(responseMessage.getCcode() == 135)
            throw new IPMIException("Failed to end lan session: invalid session ID in request");
        else
            throw new IPMIException("Failed to end lan session: unknown error");
    } else
    {
        session.terminateSession();
        return;
    }
}

private boolean startSession()
    throws IPMIException
{
    session.setMaxPrivLevel(Constants.IPMI_SESSION_PRIV_ADMIN);
    boolean ret = getUserAuhentications();
    if(!ret)
        throw new IPMIException("failed to getUserAuhentications");
    ret = getSessionChallenge();
    if(!ret)
        throw new IPMIException("failed to getSessionChallenge");
    ret = activateSession();
    if(!ret)
        throw new IPMIException("failed to activateSession");
    else
        return true;
}

public IPMIMessage executeCommand(Command command, boolean isSessionRequired)
    throws IPMIException
{
    if(session == null)
        throw new IPMIException("error in executeCommand: IPMISession  is null");
    if(isSessionRequired && !session.isSessionActivated())
    {
        activateSession();
        if(session.isSessionActivated())
            throw new IPMIException("error in executeComand: IPMISession is not active");
    }
    LanIPMIMessage message = new LanIPMIMessage(session);
    message.setCommand(command);
    byte data[] = new byte[256];
    int length = -1;
    length = message.buildDataToSend(data, 0);
    boolean ret = rmcp.sendPacket(session.getTargetIP(), session.getPort(), data, length);
    if(!ret)
        throw new IPMIException("Failed to send packat");
    byte retData[] = rmcp.getRecvBuf();
    LanIPMIMessage responseMessage = new LanIPMIMessage(session);
    try
    {
        responseMessage.parseDataToMessage(retData, 0);
    }
    catch(IPMIException e)
    {
        throw new IPMIException((new StringBuilder()).append("Failed to parse command response message").append(e).toString());
    }
    session.increaseInboundSessionSeq();
    return responseMessage;
}

private boolean pingDevice(String targetIP)
{
    ASFMessage asfMessage = new ASFMessage((byte)-128, (byte)0);
    byte data[] = new byte[256];
    int length = asfMessage.getMessageData(data, 0);
    try
    {
        boolean ret = rmcp.sendPacket(targetIP, session.getPort(), data, length);
        if(ret)
            rmcp.getRecvBuf();
    }
    catch(Exception e)
    {
    	return true;
    }
    return true;
}

private boolean getUserAuhentications()
    throws IPMIException
{
    if(session == null)
        throw new IPMIException("sessionData obj is null.");
    byte messageData[] = new byte[2];
    messageData[0] = Constants.IPMI_LAN_CHANNEL_E;
    messageData[1] = session.getMaxPrivLevel();
    Command command = new CommandBase((byte)6, (byte)56, messageData);
    IPMIMessage responseMessage = null;
    try
    {
        responseMessage = executeCommand(command, false);
    }
    catch(IPMIException e)
    {
        throw new IPMIException((new StringBuilder()).append("Failed to getUserAuhentications. Commad: ").append(command.toString()).toString(), e);
    }
    if(responseMessage.getCcode() != 0)
        throw new IPMIException((new StringBuilder()).append("Failed to get getUserAuhentications:").append(IPMIUtils.byteToHex(responseMessage.getCcode())).toString());
    byte commandData[] = responseMessage.getCmd().getCommandData();
    printAuthenticationStatus(commandData);
    if(session.getUserPassword() != null)
    {
        if((commandData[1] & 1 << Constants.IPMI_SESSION_AUTHTYPE_MD5) > 0)
            session.setAuthType(Constants.IPMI_SESSION_AUTHTYPE_MD5);
        else
        if((commandData[1] & 1 << Constants.IPMI_SESSION_AUTHTYPE_MD2) > 0)
            session.setAuthType(Constants.IPMI_SESSION_AUTHTYPE_MD2);
        else
        if((commandData[1] & 1 << Constants.IPMI_SESSION_AUTHTYPE_PASSWORD) > 0)
            session.setAuthType(Constants.IPMI_SESSION_AUTHTYPE_PASSWORD);
    } else
    if((commandData[1] & 1 << Constants.IPMI_SESSION_AUTHTYPE_NONE) > 0)
        session.setAuthType(Constants.IPMI_SESSION_AUTHTYPE_NONE);
    else
        throw new IPMIException("unknown supported authentication type");
    return true;
}

private void printAuthenticationStatus(byte commandData[])
{
}

private boolean getSessionChallenge()
    throws IPMIException
{
    if(session == null)
        throw new IPMIException("sessionData obj is null.");
    byte messageData[] = new byte[17];
    messageData[0] = session.getAuthType();
    System.arraycopy(session.getUserName(), 0, messageData, 1, 16);
    Command command = new CommandBase((byte)6, (byte)57, messageData);
    IPMIMessage responseMessage = null;
    try
    {
        responseMessage = executeCommand(command, false);
    }
    catch(IPMIException e)
    {
        throw new IPMIException((new StringBuilder()).append("Failed to get get User Challange. Command: ").append(command.toString()).toString(), e);
    }
    if(responseMessage.getCcode() != 0)
    {
        String err = (new StringBuilder()).append("Failed to get User Challange:").append(IPMIUtils.byteToHex(responseMessage.getCcode())).append("\n").toString();
        switch(responseMessage.getCcode())
        {
        case -127: 
            err = (new StringBuilder()).append(err).append("Invalid user name").toString();
            break;

        case -126: 
            err = (new StringBuilder()).append(err).append("NULL user name not enabled").toString();
            break;

        default:
            err = (new StringBuilder()).append(err).append("unknown error").toString();
            break;
        }
        throw new IPMIException(err);
    } else
    {
    	try
    	{
    		byte commnadData[] = responseMessage.getCmd().getCommandData();
    		byte sessionId[] = new byte[4];
    		System.arraycopy(commnadData, 0, sessionId, 0, 4);
    		session.setSessionID(sessionId);
    		byte sessionChallange[] = new byte[16];
    		System.arraycopy(commnadData, 4, sessionChallange, 0, 16);
    		session.setChallengeStringData(sessionChallange);
    		return true;
    	}
    	catch(Exception e)
    	{
    		throw new IPMIException(e.getMessage());
    	}
    }
}

private boolean activateSession()
    throws IPMIException
{
    if(session == null)
        throw new IPMIException("sessionData obj is null.");
    session.activateSession();
    byte messageData[] = new byte[22];
    messageData[0] = session.getAuthType();
    messageData[1] = session.getMaxPrivLevel();
    System.arraycopy(session.getChallengeStringData(), 0, messageData, 2, 16);
    System.arraycopy(session.getOutboundSessionSeq(), 0, messageData, 18, 4);
    Command command = new CommandBase((byte)6, (byte)58, messageData);
    IPMIMessage responseMessage = null;
    try
    {
        responseMessage = executeCommand(command, false);
    }
    catch(IPMIException e)
    {
        throw new IPMIException((new StringBuilder()).append("Failed to activate session. session id:").append(IPMIUtils.bytesToHex(session.getSessionID())).toString(), e);
    }
    if(responseMessage.getCcode() != 0)
    {
        String err = (new StringBuilder()).append("Failed to activate sessionData :").append(IPMIUtils.byteToHex(responseMessage.getCcode())).append("\n").toString();
        switch(responseMessage.getCcode())
        {
        case -127: 
            err = (new StringBuilder()).append(err).append(" No session slot available (BMC cannot accept any more sessions)").toString();
            break;

        case -126: 
            err = (new StringBuilder()).append(err).append("No slot available for given user. (Limit of user sessions allowed under that name has been reached)").toString();
            break;

        case -125: 
            err = (new StringBuilder()).append(err).append("No slot available to support user due to maximum privilege capability.").toString();
            break;

        case -124: 
            err = (new StringBuilder()).append(err).append("session sequence number out-of-range").toString();
            break;

        case -123: 
            err = (new StringBuilder()).append(err).append("invalid session ID in request").toString();
            break;

        case -122: 
            err = (new StringBuilder()).append(err).append("requested maximum privilege level exceeds user and/or channel privilege limit").toString();
            break;

        default:
            err = (new StringBuilder()).append(err).append("unknown error").toString();
            break;
        }
        throw new IPMIException(err);
    } else
    {
        byte commnadData[] = responseMessage.getCmd().getCommandData();
        session.setAuthType(commnadData[0]);
        byte sessionId[] = new byte[4];
        System.arraycopy(commnadData, 1, sessionId, 0, 4);
        session.setSessionID(sessionId);
        byte sessionSeq[] = new byte[4];
        System.arraycopy(commnadData, 5, sessionSeq, 0, 4);
        session.setInboundSessionSeq(sessionSeq);
        session.setMaxPrivLevel(commnadData[9]);
        return true;
    }
}

public static final byte ASF_MESSAGE = 6;
public static final byte IPMI_MESSAGE = 7;
private IPMISession session;
private RMCP rmcp;

}


