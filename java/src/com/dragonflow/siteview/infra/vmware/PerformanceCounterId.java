package com.dragonflow.siteview.infra.vmware;

class PerformanceCounterId
{

    static PerformanceCounterId parseId(String id)
    {
        try
        {
            String splits[] = id.split("#", 5);
            return new PerformanceCounterId(splits[0], splits[1], Integer.parseInt(splits[2]), splits[3]);
        }
        catch(RuntimeException exc)
        {
            throw new IllegalArgumentException((new StringBuilder()).append("Can't parse [").append(id).append("] with pattern {licensed}#{countername}#{instanse}#{timePeriod}#{instanceId}").toString(), exc);
        }
    }

    PerformanceCounterId(String counterName, String instance, int timePeriod, String instId)
    {
        this.counterName = "";
        this.instance = "";
        this.timePeriod = null;
        instanceId = "";
        this.counterName = counterName;
        this.instance = instance;
        this.timePeriod = Integer.valueOf(timePeriod);
        instanceId = instId;
    }

    String getCounterName()
    {
        return counterName;
    }

    String getInstance()
    {
        return instance;
    }

    int getTimePeriod()
    {
        return timePeriod.intValue();
    }

    String getInstanceId()
    {
        return instanceId;
    }

    public String toString()
    {
        return (new StringBuilder()).append(counterName).append("#").append(instance).append("#").append(timePeriod).append("#").append(instanceId).toString();
    }

    private String counterName;
    private String instance;
    private Integer timePeriod;
    private String instanceId;
}
