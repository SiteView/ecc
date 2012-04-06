package com.dragonflow.siteview.infra.vmware;

import com.dragonflow.siteview.infra.vmware.topology.TopologyVMwareCI;
import com.dragonflow.siteview.infra.vmware.topology.TopologyVMwareServer;
import com.dragonflow.siteview.infra.vmware.topology.TopologyVMwareVirtualHost;
import com.dragonflow.siteview.infra.util.RawXmlWriter;
import com.vmware.vim.*;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.*;

import org.apache.axis.client.Stub;


public class Connector
{

    private Connector(String url, String user, String password, int timeout)
    {
        this.url = url;
        this.user = user;
        this.password = password;
        this.timeout = timeout;
    }

    private void actualize()
        throws Exception
    {
        if(webService != null && isLoggedIn())
            return;
        dispose();
        VimServiceLocator locator = new VimServiceLocator();
        locator.setMaintainSession(true);
        try
        {
            webService = locator.getVimPort(new URL(url));
            ((Stub)webService).setTimeout(timeout);
            ManagedObjectReference sicMoRef = new ManagedObjectReference();
            sicMoRef.setType("ServiceInstance");
            sicMoRef.set_value("ServiceInstance");
            content = webService.retrieveServiceContent(sicMoRef);
            login(user, password);
            retreiveCounters();
        }
        catch(Exception ex)
        {
            dispose();
            throw ex;
        }
    }

    public String buildXML(Map<String,Object> counters)
        throws Exception
    {
        actualize();
        List providers = retrieveAllPerfProviders(false);

        String currentType = null;
        ManagedObjectReference performanceManager = content.getPerfManager();
        PerfInterval histIntervals[] = (PerfInterval[])(PerfInterval[])getVMProperty(performanceManager, "historicalInterval");
        IdFactory idFactory = new IdFactory(this);
        Iterator i$ = providers.iterator();
        do
        {
            if(!i$.hasNext())
                break;
            NamedManagedObject namedObject = (NamedManagedObject)i$.next();
            String name = namedObject.name;
            String sortName = namedObject.sortName;
            sortName= sortName.replaceFirst("\\.", "/");
            ManagedObjectReference obj = namedObject.obj;
            Id idObj = idFactory.getId(obj);
            if(idObj == null)
            {
            } 
            else
            {
                String type = obj.getType();
                if(!type.equals(currentType))
                {
                    currentType = type;
                }
                String idPrefix = "VirtualMachine".equals(type) ? "1" : "0";
                PerfProviderSummary perfProviderSummary = webService.queryPerfProviderSummary(performanceManager, obj);
                StringBuilder metricsLog = null;
                if(perfProviderSummary.isCurrentSupported())
                {
                    Integer realtimeRefreshRate = perfProviderSummary.getRefreshRate();
                    PerfMetricId perfMetrics[] = fillCounters(counters, performanceManager, obj, (new StringBuilder()).append(idPrefix).append(idObj.stringId()).toString(), sortName, "Realtime", realtimeRefreshRate, null);
                } 
                if(perfProviderSummary.isSummarySupported())
                {
                    PerfMetricId histIds[] = null;
                    PerfInterval arr$[] = histIntervals;
                    int len$ = arr$.length;
                    for(int j = 0; j < len$; j++)
                    {
                        PerfInterval perfInterval = arr$[j];
                        Integer histPeriod = Integer.valueOf(perfInterval.getSamplingPeriod());
                        histIds = fillCounters(counters, performanceManager, obj, (new StringBuilder()).append(idPrefix).append(idObj.stringId()).toString(), sortName, (new StringBuilder()).append("Historical[").append(histPeriod).append(" secs]").toString(), histPeriod, histIds);
                    }

                } 
            }
        } while(true);
        return "";
    }

    private void dispose()
    {
        content = null;
        webService = null;
        counters.clear();
    }

    private PerfMetricId[] fillCounters(Map<String,Object> counters1, ManagedObjectReference performanceManager, ManagedObjectReference obj, String invPath, String sortName, String counterGroup, Integer refreshRate, PerfMetricId perfMetricIds[])
        throws RemoteException
    {
        if(perfMetricIds == null || perfMetricIds.length == 0)
            perfMetricIds = webService.queryAvailablePerfMetric(performanceManager, obj, null, null, refreshRate);
        if(perfMetricIds == null || perfMetricIds.length == 0)
            return null;
      
        int nPos = sortName.indexOf('/');
        String topSortName = sortName.substring(0, nPos);
        counters1.put(topSortName, topSortName);
        counters1.put(sortName, sortName);
        
        String prefix = sortName + "/" + counterGroup;
        counters1.put(prefix, prefix);
        prefix += "/";
        
        TreeMap namesToIDs = new TreeMap();
        PerfMetricId arr$[] = perfMetricIds;
        int len$ = arr$.length;
        for(int i$ = 0; i$ < len$; i$++)
        {
            PerfMetricId perfMetricId = arr$[i$];
            int id = perfMetricId.getCounterId();
            String instance = perfMetricId.getInstance();
            Counter counter = (Counter)counters.get(Integer.valueOf(id));
            TreeMap group = (TreeMap)namesToIDs.get(counter.getGroup());
            if(group == null)
            {
                group = new TreeMap();
                namesToIDs.put(counter.getGroup(), group);
            }
            String idToSort = (new StringBuilder()).append(counter.getName()).append(".").append(counter.getRollup()).append("[").append(instance).append("]").toString();
            PerformanceCounterId cId = new PerformanceCounterId(counter.getStringId(), instance, refreshRate.intValue(), invPath);
            group.put(idToSort, cId);
        }

        for(Iterator i$ = namesToIDs.entrySet().iterator(); i$.hasNext(); )
        {
            java.util.Map.Entry groupEntry = (java.util.Map.Entry)i$.next();
            String groupName = (String)groupEntry.getKey();
            String groupPrefix = prefix + groupName;
            counters1.put(groupPrefix, groupPrefix);
            groupPrefix += "/";
            
            TreeMap groupContent = (TreeMap)groupEntry.getValue();
            String name;
            String id;
            for(Iterator j = groupContent.entrySet().iterator(); j.hasNext(); )
            {
                java.util.Map.Entry groupCounterEntry = (java.util.Map.Entry)j.next();
                name = (String)groupCounterEntry.getKey();
                id = ((PerformanceCounterId)groupCounterEntry.getValue()).toString();
                counters1.put(id, groupPrefix + name);
            }
        }

        return perfMetricIds;
    }

    private Counter findCounterByName(String id)
    {
        for(Iterator i$ = counters.values().iterator(); i$.hasNext();)
        {
            Counter counter = (Counter)i$.next();
            if(counter.getStringId().equals(id))
                return counter;
        }

        return null;
    }

    DynamicProperty[] getVMProperty(ManagedObjectReference obj, String propNameArr[])
        throws RemoteException
    {
        PropertySpec pSpec = new PropertySpec();
        pSpec.setAll(Boolean.valueOf(false));
        pSpec.setType(obj.getType());
        pSpec.setPathSet(propNameArr);
        ObjectSpec oSpec = new ObjectSpec();
        oSpec.setObj(obj);
        oSpec.setSkip(Boolean.FALSE);
        PropertyFilterSpec pfSpec = new PropertyFilterSpec();
        pfSpec.setObjectSet(new ObjectSpec[] {
            oSpec
        });
        pfSpec.setPropSet(new PropertySpec[] {
            pSpec
        });
        ObjectContent objs[] = webService.retrieveProperties(content.getPropertyCollector(), new PropertyFilterSpec[] {
            pfSpec
        });
        if(objs != null)
        {
            DynamicProperty dProps[] = objs[0].getPropSet();
            return dProps;
        } else
        {
            return null;
        }
    }

    Object getVMProperty(ManagedObjectReference obj, String propName)
        throws RemoteException
    {
        DynamicProperty dProps[] = getVMProperty(obj, new String[] {
            propName
        });
        if(dProps != null && dProps.length > 0 && dProps[0] != null)
            return dProps[0].getVal();
        else
            return null;
    }

    private boolean isLoggedIn()
    {
        try
        {
            ManagedObjectReference sessionManager = content.getSessionManager();
            UserSession session = (UserSession)getVMProperty(sessionManager, "currentSession");
            if(session != null)
                return true;
        }
        catch(Exception e)
        {
        }
        return false;
    }

    private void login(String uName, String pWord)
        throws RemoteException
    {
        webService.login(content.getSessionManager(), uName, pWord, null);
    }

    private void logout()
    {
        if(webService != null)
            try
            {
                webService.logout(content.getSessionManager());
            }
            catch(RemoteException e)
            {
            }
    }

    public List readMetrics(List ids)
        throws Exception
    {
        actualize();
        IdFactory idFactory = new IdFactory(this);
        ManagedObjectReference performanceManager = content.getPerfManager();
        List results = new ArrayList();
        Map idsToMOR = new HashMap();
        int numGoodCounters = 0;
        Iterator i$ = ids.iterator();
        do
        {
            if(!i$.hasNext())
                break;
            String id = (String)i$.next();
            try
            {
                PerformanceCounterId perfCounterId = PerformanceCounterId.parseId(id);
                String bulkId = perfCounterId.getInstanceId();
                ManagedObjectReference objToMonitor = (ManagedObjectReference)idsToMOR.get(bulkId);
                if(objToMonitor == null)
                {
                    String instId = bulkId.substring(1);
                    Id idObj = idFactory.getId(instId);
                    objToMonitor = idObj.getObject(this);
                    idsToMOR.put(bulkId, objToMonitor);
                }
                Counter counterByName = findCounterByName(perfCounterId.getCounterName());
                PerfMetricId metricId = new PerfMetricId();
                metricId.setCounterId(counterByName.getId());
                metricId.setInstance(perfCounterId.getInstance());
                PerfQuerySpec querySpec = new PerfQuerySpec();
                querySpec.setEntity(objToMonitor);
                querySpec.setMetricId(new PerfMetricId[] {
                    metricId
                });
                querySpec.setIntervalId(Integer.valueOf(perfCounterId.getTimePeriod()));
                querySpec.setMaxSample(Integer.valueOf(1));
                querySpec.setFormat("csv");
                com.vmware.vim.PerfEntityMetricBase perfResult[] = webService.queryPerf(performanceManager, new PerfQuerySpec[] {
                    querySpec
                });
                String lastValue = "unavailable";
                if(perfResult != null)
                {
                    com.vmware.vim.PerfEntityMetricBase arr$[] = perfResult;
                    int len$ = arr$.length;
                    for(int j = 0; j < len$; j++)
                    {
                        com.vmware.vim.PerfEntityMetricBase metric = arr$[j];
                        PerfEntityMetricCSV csv = (PerfEntityMetricCSV)metric;
                        if(csv.getValue() == null)
                        {
                            lastValue = "unavailable";
                        } else
                        {
                            PerfMetricSeriesCSV series = csv.getValue(0);
                            String values = series.getValue();
                            String valArray[] = values.split(",");
                            lastValue = valArray[valArray.length - 1];
                            numGoodCounters++;
                        }
                    }

                }
                results.add(lastValue);
            }
            catch(Exception e)
            {
                results.add("unavailable");
            }
        } while(true);
        return results;
    }

    ManagedObjectReference findByUUID(ManagedObjectReference datacenter, String uuid, boolean vmSearch)
        throws RemoteException
    {
        ManagedObjectReference searchIndex = content.getSearchIndex();
        ManagedObjectReference objectReference = webService.findByUuid(searchIndex, datacenter, uuid, vmSearch);
        return objectReference;
    }

    ManagedObjectReference findByInventoryPath(String invPath)
        throws RemoteException
    {
        ManagedObjectReference searchIndex = content.getSearchIndex();
        ManagedObjectReference objectReference = webService.findByInventoryPath(searchIndex, invPath);
        return objectReference;
    }

    private void retreiveCounters()
        throws RemoteException
    {
        counters.clear();
        ManagedObjectReference performanceManager = content.getPerfManager();
        com.vmware.vim.PerfCounterInfo counterArray[] = (com.vmware.vim.PerfCounterInfo[])(com.vmware.vim.PerfCounterInfo[])getVMProperty(performanceManager, "perfCounter");
        if(counterArray != null)
        {
            com.vmware.vim.PerfCounterInfo arr$[] = counterArray;
            int len$ = arr$.length;
            for(int i$ = 0; i$ < len$; i$++)
            {
                com.vmware.vim.PerfCounterInfo aCounterArray = arr$[i$];
                Counter counter = new Counter(aCounterArray);
                counters.put(Integer.valueOf(counter.getId()), counter);
            }

        }
    }

    private List retrieveAllPerfProviders(boolean all)
        throws Exception
    {
        String names[] = {
            "name"
        };
        PropertySpec vmSpec = new PropertySpec();
        vmSpec.setType("VirtualMachine");
        vmSpec.setAll(Boolean.valueOf(all));
        vmSpec.setPathSet(new String[] {"name"});
        PropertySpec resPoolSpec = new PropertySpec();
        resPoolSpec.setType("ResourcePool");
        resPoolSpec.setAll(Boolean.valueOf(all));
        resPoolSpec.setPathSet(new String[] {"name"});
        PropertySpec hostSysSpec = new PropertySpec();
        hostSysSpec.setType("HostSystem");
        hostSysSpec.setAll(Boolean.valueOf(all));
        hostSysSpec.setPathSet(new String[] {"name"});
        ObjectSpec oSpec = new ObjectSpec();
        oSpec.setObj(content.getRootFolder());
        oSpec.setSkip(Boolean.FALSE);
        oSpec.setSelectSet(buildFullTraversal());
        PropertyFilterSpec pfSpec = new PropertyFilterSpec();
        pfSpec.setObjectSet(new ObjectSpec[] {
            oSpec
        });
        pfSpec.setPropSet(new PropertySpec[] {
        	vmSpec,hostSysSpec,resPoolSpec
        });

        ManagedObjectReference r = content.getPropertyCollector();
        PropertyFilterSpec rt[] = new PropertyFilterSpec[] {
            pfSpec
        };
        ObjectContent objs[] = webService.retrieveProperties(r,rt);
        
        if(objs == null)
            return null;
        List refs = new ArrayList(objs.length);
        ObjectContent arr$[] = objs;
        int len$ = arr$.length;
        for(int i$ = 0; i$ < len$; i$++)
        {
            ObjectContent objProperty = arr$[i$];
            ManagedObjectReference obj = objProperty.getObj();
            String name = (String)findProperty("name", objProperty.getPropSet());
            String nameToSort = (new StringBuilder()).append(obj.getType()).append(".").append(name).toString();
            refs.add(new NamedManagedObject(obj, name, nameToSort));
        }

        Collections.sort(refs);
        return refs;
    }

    private Object findProperty(String propName, DynamicProperty propSet[])
    {
        DynamicProperty arr$[] = propSet;
        int len$ = arr$.length;
        for(int i$ = 0; i$ < len$; i$++)
        {
            DynamicProperty prop = arr$[i$];
            if(propName.equals(prop.getName()))
                return prop.getVal();
        }

        return null;
    }

    private SelectionSpec[] buildFullTraversal()
    {
        SelectionSpec visitFolders = new SelectionSpec(null, null, "visitFolders");
        SelectionSpec toResourcePool = new SelectionSpec(null, null, "rpToRp");
        TraversalSpec dcToHf = new TraversalSpec();
        dcToHf.setName("dcToHf");
        dcToHf.setType("Datacenter");
        dcToHf.setPath("hostFolder");
        dcToHf.setSkip(Boolean.FALSE);
        dcToHf.setSelectSet(new SelectionSpec[] {
            visitFolders
        });
        TraversalSpec dcToVmf = new TraversalSpec();
        dcToVmf.setName("dcToVmf");
        dcToVmf.setType("Datacenter");
        dcToVmf.setPath("vmFolder");
        dcToVmf.setSkip(Boolean.FALSE);
        dcToVmf.setSelectSet(new SelectionSpec[] {
            visitFolders
        });
        TraversalSpec crToH = new TraversalSpec();
        crToH.setName("crToH");
        crToH.setType("ComputeResource");
        crToH.setPath("host");
        crToH.setSkip(Boolean.FALSE);
        crToH.setSelectSet(new SelectionSpec[0]);
        TraversalSpec crToRp = new TraversalSpec();
        crToRp.setName("crToRp");
        crToRp.setType("ComputeResource");
        crToRp.setPath("resourcePool");
        crToRp.setSkip(Boolean.FALSE);
        crToRp.setSelectSet(new SelectionSpec[] {
            toResourcePool
        });
        TraversalSpec rpToRp = new TraversalSpec();
        rpToRp.setName("rpToRp");
        rpToRp.setType("ResourcePool");
        rpToRp.setPath("resourcePool");
        rpToRp.setSkip(Boolean.FALSE);
        rpToRp.setSelectSet(new SelectionSpec[] {
            toResourcePool
        });
        TraversalSpec folderTSpec = new TraversalSpec();
        folderTSpec.setName("visitFolders");
        folderTSpec.setType("Folder");
        folderTSpec.setPath("childEntity");
        folderTSpec.setSkip(Boolean.FALSE);
        folderTSpec.setSelectSet(new SelectionSpec[] {
            visitFolders, dcToHf, dcToVmf, crToH, crToRp, rpToRp
        });
        return (new SelectionSpec[] {
            folderTSpec
        });
    }

    public static Connector getConnector(String url, String user, String password, int timeout)
    {
        return new Connector(url, user, password, timeout);
    }

    public static void disposeConnector(Connector connector)
    {
        if(connector != null)
        {
            connector.logout();
            connector.dispose();
        }
    }

    public int countLicensedInstances(List ids)
    {
        HashSet idsHash = new HashSet();
        Iterator i$ = ids.iterator();
        do
        {
            if(!i$.hasNext())
                break;
            String id = (String)i$.next();
            PerformanceCounterId perfCounterId = PerformanceCounterId.parseId(id);
            String instId = perfCounterId.getInstanceId();
            if('1' == instId.charAt(0))
                idsHash.add(instId.substring(1));
        } while(true);
        return idsHash.size();
    }

    private ObjectContent[] retrieveTopologyProperties()
        throws InvalidProperty, RuntimeFault, RemoteException
    {
        PropertySpec vmSpec = new PropertySpec(null, null, "VirtualMachine", Boolean.FALSE, TopologyVMwareVirtualHost.vmwareAPIpropertyPaths);
        PropertySpec hostSpec = new PropertySpec(null, null, "HostSystem", Boolean.FALSE, TopologyVMwareServer.vmwareAPIpropertyPaths);
        SelectionSpec visitFolders = new SelectionSpec(null, null, "visitFolders");
        TraversalSpec dcToHf = new TraversalSpec(null, null, "dcToHf", "Datacenter", "hostFolder", Boolean.FALSE, new SelectionSpec[] {
            visitFolders
        });
        TraversalSpec dcToVmf = new TraversalSpec(null, null, "dcToVmf", "Datacenter", "vmFolder", Boolean.FALSE, new SelectionSpec[] {
            visitFolders
        });
        TraversalSpec crToH = new TraversalSpec(null, null, "crToH", "ComputeResource", "host", Boolean.FALSE, new SelectionSpec[0]);
        TraversalSpec trSpec = new TraversalSpec(null, null, visitFolders.getName(), "Folder", "childEntity", Boolean.FALSE, new SelectionSpec[] {
            visitFolders, dcToHf, crToH, dcToVmf
        });
        ObjectSpec oSpec = new ObjectSpec(null, null, content.getRootFolder(), Boolean.FALSE, new SelectionSpec[] {
            trSpec
        });
        PropertyFilterSpec pfSpec = new PropertyFilterSpec(null, null, new PropertySpec[] {
            hostSpec, vmSpec
        }, new ObjectSpec[] {
            oSpec
        });
        return webService.retrieveProperties(content.getPropertyCollector(), new PropertyFilterSpec[] {
            pfSpec
        });
    }

    public Map buildTopologyIdentifiers()
    {
        ObjectContent objs[] = null;
        try
        {
            actualize();
            objs = retrieveTopologyProperties();
        }
        catch(Exception e)
        {
        }
        if(objs == null)
        {
            return null;
        }
        Map res = new HashMap();
        IdFactory idFactory = new IdFactory(this);
        ObjectContent arr$[] = objs;
        int len$ = arr$.length;
        for(int i$ = 0; i$ < len$; i$++)
        {
            ObjectContent obj = arr$[i$];
            try
            {
                Id id = idFactory.getId(obj.getObj());
                TopologyVMwareCI configItem = constructConfigItem(obj.getObj(), obj.getPropSet());
                if(configItem != null)
                    res.put(id, configItem);
            }
            catch(RemoteException e)
            {
            }
        }

        return res;
    }

    private TopologyVMwareCI constructConfigItem(ManagedObjectReference objRef, DynamicProperty propSet[])
    {
        if("VirtualMachine".equals(objRef.getType()))
            return new TopologyVMwareVirtualHost(propSet);
        if("HostSystem".equals(objRef.getType()))
            return new TopologyVMwareServer(propSet);
        else
            return null;
    }

    public void linkMeasuremensToHosts(Map properties, Map countIdToMeasId)
    {
        Map idsToTopologyItems = buildTopologyIdentifiers();
        IdFactory idFactory = new IdFactory(this);
        Map hostsToMeasurements = new HashMap();
        Iterator i$ = countIdToMeasId.entrySet().iterator();
        do
        {
            if(!i$.hasNext())
                break;
            java.util.Map.Entry countToMeasure = (java.util.Map.Entry)i$.next();
            PerformanceCounterId perfCounterId = PerformanceCounterId.parseId((String)countToMeasure.getKey());
            String bulkId = perfCounterId.getInstanceId();
            Id id = idFactory.getId(bulkId.substring(1));
            TopologyVMwareCI configItem = (TopologyVMwareCI)idsToTopologyItems.get(id);
            if(configItem != null)
            {
                Map hostAttributes = configItem.getAttributes();
                List list = (List)hostsToMeasurements.get(hostAttributes);
                if(list == null)
                {
                    list = new ArrayList();
                    hostsToMeasurements.put(hostAttributes, list);
                }
                list.add(countToMeasure.getValue());
            }
        } while(true);
        properties.put("vmware_hosts_to_measurements", hostsToMeasurements);
    }

    public static final String UNAVAILABLE = "unavailable";
    public static final String VIRTUAL_MACHINE_REFERENCE_TYPE = "VirtualMachine";
    public static final String HOST_SYSTEM_REFERENCE_TYPE = "HostSystem";
    public static final String RESOURCE_POOL_REFERENCE_TYPE = "ResourcePool";
    private ServiceContent content;
    private final HashMap counters = new HashMap();
    private VimPortType webService;
    private final String url;
    private final String user;
    private final String password;
    private final int timeout;

}

