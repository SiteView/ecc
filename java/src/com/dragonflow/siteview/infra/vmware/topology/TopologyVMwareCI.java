package com.dragonflow.siteview.infra.vmware.topology;

import java.util.HashMap;
import java.util.Map;

public abstract class TopologyVMwareCI
{

    public TopologyVMwareCI()
    {
    }

    public Map getAttributes()
    {
        return attributes;
    }

    protected final Map attributes = new HashMap();
}
