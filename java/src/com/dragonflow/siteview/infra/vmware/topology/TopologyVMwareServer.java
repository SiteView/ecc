package com.dragonflow.siteview.infra.vmware.topology;

import com.vmware.vim.DynamicProperty;
import java.util.HashMap;
import java.util.Map;

public class TopologyVMwareServer extends TopologyVMwareCI
{

    public TopologyVMwareServer(DynamicProperty propSet[])
    {
        attributes.put("host_type", "vmware_esx_server");
        DynamicProperty arr$[] = propSet;
        int len$ = arr$.length;
        for(int i$ = 0; i$ < len$; i$++)
        {
            DynamicProperty property = arr$[i$];
            if("name".equals(property.getName()))
            {
                attributes.put("name", property.getVal().toString());
                continue;
            }
            if("summary.hardware.uuid".equals(property.getName()))
            {
                attributes.put("host_key", property.getVal().toString());
                continue;
            }
            if("config.product.version".equals(property.getName()))
            {
                attributes.put("full_version", property.getVal().toString());
                continue;
            }
            if("config.product.fullName".equals(property.getName()))
            {
                attributes.put("full_name", property.getVal().toString());
                continue;
            }
            if("summary.config.name".equals(property.getName()))
            {
                attributes.put("target_name", property.getVal().toString());
                continue;
            }
            if("config.network.dnsConfig.domainName".equals(property.getName()))
            {
                attributes.put("domain_name", property.getVal().toString());
                continue;
            }
            if("config.network.dnsConfig.hostName".equals(property.getName()))
                attributes.put("target_name_short", property.getVal().toString());
        }

    }

    public Map getAttributes()
    {
        return attributes;
    }

    public static final String vmwareAPIpropertyPaths[] = {
        "name", "summary.hardware.uuid", "config.product.version", "config.product.fullName", "config.product.name", "summary.config.name", "config.network.dnsConfig.domainName", "config.network.dnsConfig.hostName"
    };
    private final Map attributes = new HashMap();

}
