package com.dragonflow.siteview.infra.vmware;

import com.vmware.vim.*;

class Counter
{

    Counter(PerfCounterInfo counter)
    {
        info = counter;
    }

    int getId()
    {
        return info.getKey();
    }

    String getGroup()
    {
        return info.getGroupInfo().getKey();
    }

    String getName()
    {
        return info.getNameInfo().getKey();
    }

    String getRollup()
    {
        return info.getRollupType().getValue();
    }

    String getStatsType()
    {
        return info.getStatsType().getValue();
    }

    String getStringId()
    {
        String group = info.getGroupInfo().getKey();
        String name = info.getNameInfo().getKey();
        String roll = info.getRollupType().getValue();
        String stats = info.getStatsType().getValue();
        return (new StringBuilder()).append(group).append(".").append(name).append(".").append(roll).append("(").append(stats).append(")").toString();
    }

    private final PerfCounterInfo info;
}

