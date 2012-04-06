package com.dragonflow.siteview.infra.vmware.topology;

import java.net.*;
import com.vmware.vim.DynamicProperty;
import com.vmware.vim.GuestNicInfo;
import java.util.Map;


public class TopologyVMwareVirtualHost extends TopologyVMwareCI
{

    public TopologyVMwareVirtualHost(DynamicProperty propSet[])
    {
        attributes.put("domain_name", getDefaultDomain());
        String name = null;
        DynamicProperty arr$[] = propSet;
        int len$ = arr$.length;
        for(int i$ = 0; i$ < len$; i$++)
        {
            DynamicProperty property = arr$[i$];
            if("guest.hostName".equals(property.getName()))
            {
                String targetName = property.getVal().toString();
                attributes.put("target_name", targetName);
                attributes.put("target_name_short", getShortHostName(targetName));
                continue;
            }
            if("guest.ipAddress".equals(property.getName()))
            {
                String ipAddress = property.getVal().toString();
                attributes.put("target_ip", ipAddress);
                attributes.put("domain_name", getDomain(ipAddress));
                continue;
            }
            if("guest.guestFamily".equals(property.getName()))
            {
                attributes.put("host_type", findHostType(property.getVal().toString()));
                continue;
            }
            if("summary.config.annotation".equals(property.getName()))
            {
                attributes.put("host_description", property.getVal().toString());
                continue;
            }
            if("guest.guestFullName".equals(property.getName()))
            {
                attributes.put("host_os", property.getVal().toString());
                continue;
            }
            if("name".equals(property.getName()))
                name = property.getVal().toString();
        }

        if(!attributes.containsKey("target_name"))
            attributes.put("target_name", name);
    }

    private String findHostType(String family)
    {
        String hostClass = "host";
        if("windowsGuest".equals(family))
            hostClass = "nt";
        else
        if("linuxGuest".equals(family))
            hostClass = "unix";
        return hostClass;
    }

    private String findHostKey(GuestNicInfo guestNics[])
    {
        if(guestNics == null || guestNics.length == 0)
            return null;
        String minHostKey = guestNics[0].getMacAddress();
        long minNumberValue = macToNumber(minHostKey);
        for(int i = 1; i < guestNics.length; i++)
        {
            String nextHostKey = guestNics[i].getMacAddress();
            long nextNumberValue = macToNumber(nextHostKey);
            if(nextNumberValue > 0L && nextNumberValue < minNumberValue)
            {
                minNumberValue = nextNumberValue;
                minHostKey = nextHostKey;
            }
        }

        return minHostKey;
    }

    private long macToNumber(String macAddress)
    {
        try
        {
            return Long.parseLong(macAddress.replace(":", ""), 16);
        }
        catch(NumberFormatException nfe)
        {
            return -1L;
        }
    }

    public static String getShortHostName(String hostName)
    {
        if(hostName == null || !hostName.contains(".") || isValidIP(hostName))
            return hostName;
        else
            return hostName.substring(0, hostName.indexOf('.'));
    }

    public static boolean isValidIP(String hostIP)
    {
        if(hostIP == null || hostIP.length() == 0 || "unknown host".equals(hostIP))
            return false;
        else
            return hostIP.contains(".") && hostIP.matches("[0-9].*") || hostIP.contains(":");
    }
    
    public static String getDomain(String hostIP)
    {
        String domain = null;
        try
        {
            domain = InetAddress.getByName(hostIP).getHostName();
        }
        catch(Exception e)
        {
        }
        if(domain == null)
            domain = getDefaultDomain();
        return domain;
    }

    public static String getDefaultDomain()
    {
        return "DefaultDomain";
    }
    
    public static final String vmwareAPIpropertyPaths[] = {
        "name", "guest.ipAddress", "guest.hostName", "guest.guestFamily", "guest.guestFullName", "summary.config.annotation"
    };

}
