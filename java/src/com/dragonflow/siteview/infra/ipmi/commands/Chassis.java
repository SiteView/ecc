package com.dragonflow.siteview.infra.ipmi.commands;

import com.dragonflow.siteview.infra.ipmi.*;
import com.dragonflow.siteview.infra.ipmi.interfaces.IPMIInterface;
import com.dragonflow.siteview.infra.ipmi.util.IPMIUtils;

public class Chassis
{
    public static class ChassisStatus
    {

        public boolean isPowerOn()
        {
            return (returnedData[0] & 1) != 0;
        }

        public boolean isPowerOverload()
        {
            return (returnedData[0] & 2) != 0;
        }

        public boolean isPowerInterlock()
        {
            return (returnedData[0] & 4) != 0;
        }

        public boolean isPowerFault()
        {
            return (returnedData[0] & 8) != 0;
        }

        public boolean isPowerControlFault()
        {
            return (returnedData[0] & 16) != 0;
        }

        public boolean isChassisIntrusion()
        {
            return (returnedData[2] & 1) != 0;
        }

        public boolean isFrontPanelLockout()
        {
            return (returnedData[2] & 2) != 0;
        }

        public boolean isDriveFault()
        {
            return (returnedData[2] & 4) != 0;
        }

        public boolean isCoolingFanFault()
        {
            return (returnedData[2] & 8) != 0;
        }

        private byte returnedData[];

        public ChassisStatus(byte returnedData[])
        {
            this.returnedData = returnedData;
        }
    }


    public Chassis()
    {
    }

    public static ChassisStatus getChassisStatus(IPMIInterface ipmiInterface)
        throws IPMIException
    {
        Command cmd = new CommandBase((byte)0, (byte)1, null);
        IPMIMessage response = null;
        try
        {
            response = ipmiInterface.executeCommand(cmd, true);
        }
        catch(Exception e)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to execute command: ").append(cmd.toString()).toString(), e);
        }
        if(response == null || response.getCcode() != 0)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to get Chassis Status:").append(IPMIUtils.byteToHex(response.getCcode())).toString());
        } else
        {
            ChassisStatus chassisStatus = new ChassisStatus(response.getCmd().getCommandData());
            return chassisStatus;
        }
    }

    public static IPMIMessage getLastRestartCause(IPMIInterface ipmiInterface)
        throws IPMIException
    {
        Command cmd = new CommandBase((byte)0, (byte)7, null);
        IPMIMessage responseMessage = null;
        try
        {
            responseMessage = ipmiInterface.executeCommand(cmd, true);
        }
        catch(IPMIException e)
        {
            throw new IPMIException((new StringBuilder()).append("Failed to execute command: ").append(cmd.toString()).toString(), e);
        }
        if(responseMessage.getCcode() != 0)
            throw new IPMIException((new StringBuilder()).append("Failed to get tLastRestartCause, code=").append(IPMIUtils.byteToHex(responseMessage.getCcode())).toString());
        else
            return responseMessage;
    }
}