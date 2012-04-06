package com.dragonflow.siteview.infra.ipmi;

public class IPMIException 
	extends Exception
{

    public IPMIException()
    {
    }

    public IPMIException(String message, Throwable cause)
    {
        super(message, cause);
    }

    public IPMIException(Throwable cause)
    {
        super(cause);
    }

    public IPMIException(String message)
    {
        super(message);
    }
}

