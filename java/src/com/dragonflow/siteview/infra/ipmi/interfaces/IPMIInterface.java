package com.dragonflow.siteview.infra.ipmi.interfaces;

import com.dragonflow.siteview.infra.ipmi.Command;
import com.dragonflow.siteview.infra.ipmi.IPMIException;
import com.dragonflow.siteview.infra.ipmi.IPMIMessage;

public interface IPMIInterface
{

    public abstract void init()
        throws IPMIException;

    public abstract IPMIMessage executeCommand(Command command, boolean flag)
        throws IPMIException;

    public abstract void end()
        throws IPMIException;
}

