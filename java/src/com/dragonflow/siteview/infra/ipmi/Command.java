package com.dragonflow.siteview.infra.ipmi;

public interface Command
{

    public abstract byte getNetFn();

    public abstract byte[] getCommandData();

    public abstract byte getCommand();
}
