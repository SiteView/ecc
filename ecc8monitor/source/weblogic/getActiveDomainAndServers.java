import java.io.PrintStream;
import java.util.Iterator;
import java.util.Set;
import javax.naming.Context;
import weblogic.jndi.Environment;
import weblogic.management.MBeanHome;
import weblogic.management.WebLogicObjectName;
import weblogic.management.runtime.*;

public class getActiveDomainAndServers
{

    static String strUrl = "t3://localhost:7001";
    static String strUsername = "weblogic";
    static String strPwd = "weblogic";
    static String strTaskType = "";
    static String strTaskParam = "";

    public getActiveDomainAndServers()
    {
    }

    public static void main(String args[])
    {
        MBeanHome home = null;
        try
        {
            ProcessInput(args);
            Environment env = new Environment();
            env.setProviderUrl(strUrl);
            env.setSecurityPrincipal(strUsername);
            env.setSecurityCredentials(strPwd);
            Context ctx = env.getInitialContext();
            home = (MBeanHome)ctx.lookup("weblogic.management.adminhome");
            if(strTaskType.compareTo("WlsStatus") == 0)
            {
                if(isRuning(home))
                {
                    System.out.println("FileSplitFlagWLSRunStatus=Running$EndSplitFlag");
                } else
                {
                    System.out.println("FileSplitFlagWLSRunStatus=ShutDown$EndSplitFlag");
                }
            } else
            if(strTaskType.compareTo("JmsStatus") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                GetJmsStatus(home);
            } else
            if(strTaskType.compareTo("ReqExecuteQueueInfo") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetBaseExecuteQueueInfo(home);
                } else
                {
                    GetExecuteQueueInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("ConnectionPoolInfo") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetConnectionPoolInfo(home);
                } else
                {
                    GetConnectionPoolInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("WebApp") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetWebAppComponentInfo(home);
                } else
                {
                    GetWebAppComponentInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("Cluster") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetClusterInfo(home);
                } else
                {
                    GetClusterInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("JVM") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetJVMInfo(home);
                } else
                {
                    GetJVMInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("Server") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetServerInfo(home);
                } else
                {
                    GetServerInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("EJBCacheInfo") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetEJBCacheInfo(home);
                } else
                {
                    GetEJBCacheInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("EJBTransactionInfo") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetEJBTransactionInfo(home);
                } else
                {
                    GetEJBTransactionInfo(home, strTaskParam);
                }
            } else
            if(strTaskType.compareTo("EJBPoolInfo") == 0)
            {
                if(!isRuning(home))
                {
                    System.out.println("FileSplitFlagError=WLSRunStatus is ShutDown$EndSplitFlag");
                    return;
                }
                if(strTaskParam.compareTo("null") == 0)
                {
                    GetEJBPoolInfo(home);
                } else
                {
                    GetEJBPoolInfo(home, strTaskParam);
                }
            } else
            {
                die();
            }
        }
        catch(Exception e)
        {
            System.err.println("FileSplitFlagError=Exception: " + e + "$EndSplitFlag");
            return;
        }
    }

    public static void ProcessInput(String args[])
    {
        if(args.length < 1)
        {
            die();
        }
        for(int k = 0; k < args.length; k++)
        {
            if(args[k].equals("-U"))
            {
                if(++k < args.length)
                {
                    strUsername = args[k];
                } else
                {
                    die();
                }
            } else
            if(args[k].equals("-P"))
            {
                if(++k < args.length)
                {
                    strPwd = args[k];
                } else
                {
                    die();
                }
            } else
            if(args[k].equals("-I"))
            {
                if(++k < args.length)
                {
                    strUrl = args[k];
                } else
                {
                    die();
                }
            } else
            if(args[k].equals("-T"))
            {
                if(++k < args.length)
                {
                    strTaskType = args[k];
                } else
                {
                    die();
                }
            } else
            if(args[k].equals("-C"))
            {
                if(++k < args.length)
                {
                    strTaskParam = args[k];
                } else
                {
                    die();
                }
            }
        }

    }

    public static void die()
    {
        System.err.println("Error=Input Error!$");
        System.exit(0);
    }

    public static boolean isRuning(MBeanHome home)
    {
        Set mbeanSet = home.getMBeansByType("ServerRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        if(mbeanIterator.hasNext())
        {
            ServerRuntimeMBean serverRuntime = (ServerRuntimeMBean)mbeanIterator.next();
            return serverRuntime.getState().equalsIgnoreCase("RUNNING");
        } else
        {
            return false;
        }
    }

    public static void GetJmsStatus(MBeanHome home)
    {
        String strReturn = "FileSplitFlag";
        Set mbeanSet = home.getMBeansByType("JMSRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext(); System.out.println(strReturn))
        {
            JMSRuntimeMBean jmsRuntime = (JMSRuntimeMBean)mbeanIterator.next();
            strReturn = strReturn + "ConnectionsTotalCoun=" + jmsRuntime.getConnectionsTotalCount() + "$";
            strReturn = strReturn + "ConnectionsCurrentCount=" + jmsRuntime.getConnectionsCurrentCount() + "$";
            strReturn = strReturn + "ConnectionsHighCount=" + jmsRuntime.getConnectionsHighCount() + "$";
            strReturn = strReturn + "JMSServersTotalCount=" + jmsRuntime.getJMSServersTotalCount() + "$";
            strReturn = strReturn + "JMSServersCurrentCount=" + jmsRuntime.getJMSServersCurrentCount() + "$";
            strReturn = strReturn + "JMSServersHighCount=" + jmsRuntime.getJMSServersHighCount() + "$";
            JMSConnectionRuntimeMBean Connections[] = (JMSConnectionRuntimeMBean[])null;
            JMSConnectionRuntimeMBean Connection = null;
            Connections = jmsRuntime.getConnections();
            long SessionsTotalCount = 0L;
            long SessionsCurrentCount = 0L;
            long SessionsHighCount = 0L;
            long MessagesReceivedCount = 0L;
            long MessagesPendingCount = 0L;
            long MessagesSentCount = 0L;
            for(int i = 0; i < Connections.length; i++)
            {
                Connection = Connections[i];
                SessionsTotalCount += Connection.getSessionsTotalCount();
                SessionsCurrentCount += Connection.getSessionsCurrentCount();
                SessionsHighCount += Connection.getSessionsHighCount();
                JMSSessionRuntimeMBean Sessions[] = (JMSSessionRuntimeMBean[])null;
                JMSSessionRuntimeMBean Session = null;
                Sessions = Connection.getSessions();
                for(int j = 0; j < Sessions.length; j++)
                {
                    Session = Sessions[j];
                    MessagesReceivedCount += Session.getMessagesReceivedCount();
                    MessagesPendingCount += Session.getMessagesPendingCount();
                    MessagesSentCount += Session.getMessagesSentCount();
                }

            }

            strReturn = strReturn + "SessionsTotalCount=" + SessionsTotalCount + "$";
            strReturn = strReturn + "SessionsCurrentCount=" + SessionsCurrentCount + "$";
            strReturn = strReturn + "SessionsHighCount=" + SessionsHighCount + "$";
            strReturn = strReturn + "MessagesReceivedCount=" + MessagesReceivedCount + "$";
            strReturn = strReturn + "MessagesPendingCount=" + MessagesPendingCount + "$";
            strReturn = strReturn + "MessagesSentCount=" + MessagesSentCount + "$";
            strReturn = strReturn + "EndSplitFlag";
        }

    }

    public static void GetBaseExecuteQueueInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("ExecuteQueueRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            ExecuteQueueRuntimeMBean executeQueueRuntime = (ExecuteQueueRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("executeQueueName").append(i).append("=").append(executeQueueRuntime.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetExecuteQueueInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("ExecuteQueueRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext();)
        {
            ExecuteQueueRuntimeMBean executeQueueRuntime = (ExecuteQueueRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(executeQueueRuntime.getName()) == 0)
            {
                stringbuffer.append("executeQueueName=").append(executeQueueRuntime.getName()).append("$");
                stringbuffer.append("ExecuteThreadTotalCount=").append(executeQueueRuntime.getExecuteThreadTotalCount()).append("$");
                stringbuffer.append("ExecuteThreadCurrentIdleCount=").append(executeQueueRuntime.getExecuteThreadCurrentIdleCount()).append("$");
                stringbuffer.append("PendingRequestOldestTime=").append(executeQueueRuntime.getPendingRequestOldestTime()).append("$");
                stringbuffer.append("PendingRequestCurrentCount=").append(executeQueueRuntime.getPendingRequestCurrentCount()).append("$");
                stringbuffer.append("ServicedRequestTotalCount=").append(executeQueueRuntime.getServicedRequestTotalCount()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }

        System.out.println("FileSplitFlagError=Query Param Error!$EndSplitFlag");
    }

    public static void GetWebAppComponentInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("WebAppComponentRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            WebAppComponentRuntimeMBean WebApp = (WebAppComponentRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("WebAppName").append(i).append("=").append(WebApp.getComponentName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetWebAppComponentInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("WebAppComponentRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext();)
        {
            WebAppComponentRuntimeMBean WebApp = (WebAppComponentRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(WebApp.getComponentName()) == 0)
            {
                stringbuffer.append("WebAppName=").append(WebApp.getComponentName()).append("$");
                stringbuffer.append("mbeanName=").append(WebApp.getName()).append("$");
                stringbuffer.append("Status=").append(WebApp.getStatus()).append("$");
                stringbuffer.append("OpenSessionsCurrentCount=").append(WebApp.getOpenSessionsCurrentCount()).append("$");
                stringbuffer.append("OpenSessionsHighCount=").append(WebApp.getOpenSessionsHighCount()).append("$");
                stringbuffer.append("SessionsOpenedTotalCount=").append(WebApp.getSessionsOpenedTotalCount()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }

        System.out.println("FileSplitFlagError=ConnectionPool Param Error!$EndSplitFlag");
    }

    public static void GetClusterInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("ClusterRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            ClusterRuntimeMBean Cluster = (ClusterRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("ClusterName").append(i).append("=").append(Cluster.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetClusterInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("ClusterRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        int i = 0;
        while(mbeanIterator.hasNext()) 
        {
            ClusterRuntimeMBean Cluster = (ClusterRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(Cluster.getName()) == 0)
            {
                stringbuffer.append("AliveServerCount=").append(Cluster.getAliveServerCount()).append("$");
                stringbuffer.append("ClusterName=").append(Cluster.getName()).append("$");
                stringbuffer.append("MulticastMessagesLostCount=").append(Cluster.getMulticastMessagesLostCount()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }
    }

    public static void GetServerInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("ServerRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            ServerRuntimeMBean Server = (ServerRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("ServerName").append(i).append("=").append(Server.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetServerInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("ServerRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        int i = 0;
        while(mbeanIterator.hasNext()) 
        {
            ServerRuntimeMBean Server = (ServerRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(Server.getName()) == 0)
            {
                stringbuffer.append("RestartsTotalCount=").append(Server.getRestartsTotalCount()).append("$");
                stringbuffer.append("OpenSocketsCurrentCount=").append(Server.getOpenSocketsCurrentCount()).append("$");
                stringbuffer.append("SocketsOpenedTotalCount=").append(Server.getSocketsOpenedTotalCount()).append("$");
                stringbuffer.append("ConnectionsCount=").append(Server.getConnections().length).append("$");
                stringbuffer.append("WeblogicVersion=").append(Server.getWeblogicVersion()).append("$");
                stringbuffer.append("Name=").append(Server.getName()).append("$");
                stringbuffer.append("ListenAddress=").append(Server.getListenAddress()).append("$");
                stringbuffer.append("ListenPort=").append(Server.getListenPort()).append("$");
                stringbuffer.append("SSLListenPort=").append(Server.getSSLListenPort()).append("$");
                stringbuffer.append("AdministrationPort=").append(Server.getAdministrationPort()).append("$");
                stringbuffer.append("AdminServerListenPort=").append(Server.getAdminServerListenPort()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }
        System.out.println("FileSplitFlagError=ConnectionPool Param Error!$EndSplitFlag");
    }

    public static void GetJVMInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("JVMRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            JVMRuntimeMBean JVM = (JVMRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("HeapName").append(i).append("=").append(JVM.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetJVMInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("JVMRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext();)
        {
            JVMRuntimeMBean JVM = (JVMRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(JVM.getName()) == 0)
            {
                stringbuffer.append("HeapSizeCurrent=").append(JVM.getHeapSizeCurrent()).append("$");
                stringbuffer.append("HeapFreeCurrent=").append(JVM.getHeapFreeCurrent()).append("$");
                stringbuffer.append("Name=").append(JVM.getName()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }

        System.out.println("FileSplitFlagError=ConnectionPool Param Error!$EndSplitFlag");
    }

    public static void GetConnectionPoolInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("JDBCConnectionPoolRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            JDBCConnectionPoolRuntimeMBean jdbcPool = (JDBCConnectionPoolRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("ConnectionPoolName").append(i).append("=").append(jdbcPool.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetConnectionPoolInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("JDBCConnectionPoolRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext();)
        {
            JDBCConnectionPoolRuntimeMBean jdbcPool = (JDBCConnectionPoolRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(jdbcPool.getName()) == 0)
            {
                stringbuffer.append("ConnectionPoolName=").append(jdbcPool.getName()).append("$");
                stringbuffer.append("Server=").append(jdbcPool.getObjectName().getLocation()).append("$");
                stringbuffer.append("Status=").append(jdbcPool.getObjectName().isRuntime()).append("$");
                stringbuffer.append("ConnectionsCurrentCount=").append(jdbcPool.getActiveConnectionsCurrentCount()).append("$");
                stringbuffer.append("WaitingForConnectionCurrentCount=").append(jdbcPool.getWaitingForConnectionCurrentCount()).append("$");
                stringbuffer.append("NLeakedConnectionCount=").append(jdbcPool.getLeakedConnectionCount()).append("$");
                stringbuffer.append("MaxCapacity=").append(jdbcPool.getMaxCapacity()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }

        System.out.println("FileSplitFlagError=ConnectionPool Param Error!$EndSplitFlag");
    }

    public static void GetEJBCacheInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("EJBCacheRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            EJBCacheRuntimeMBean EJBCache = (EJBCacheRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("EJBCacheName").append(i).append("=").append(EJBCache.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetEJBCacheInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("EJBCacheRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext();)
        {
            EJBCacheRuntimeMBean EJBCache = (EJBCacheRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(EJBCache.getName()) == 0)
            {
                stringbuffer.append("EJBCacheName=").append(EJBCache.getName()).append("$");
                stringbuffer.append("CurrentCount=").append(EJBCache.getCachedBeansCurrentCount()).append("$");
                stringbuffer.append("AccessCount=").append(EJBCache.getCacheAccessCount()).append("$");
                stringbuffer.append("HitCount=").append(EJBCache.getCacheHitCount()).append("$");
                stringbuffer.append("ActivationCount=").append(EJBCache.getActivationCount()).append("$");
                stringbuffer.append("PassivationCount=").append(EJBCache.getPassivationCount()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }

        System.out.println("FileSplitFlagError=EJBCache Param Error!$EndSplitFlag");
    }

    public static void GetEJBTransactionInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("EJBTransactionRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            EJBTransactionRuntimeMBean EJBTransaction = (EJBTransactionRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("EJBTransactionName").append(i).append("=").append(EJBTransaction.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetEJBTransactionInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("EJBTransactionRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext();)
        {
            EJBTransactionRuntimeMBean EJBTransaction = (EJBTransactionRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(EJBTransaction.getName()) == 0)
            {
                stringbuffer.append("EJBTransactionName=").append(EJBTransaction.getName()).append("$");
                stringbuffer.append("CommittedTotalCount=").append(EJBTransaction.getTransactionsCommittedTotalCount()).append("$");
                stringbuffer.append("RolledBackTotalCount=").append(EJBTransaction.getTransactionsRolledBackTotalCount()).append("$");
                stringbuffer.append("TimedOutTotalCount=").append(EJBTransaction.getTransactionsTimedOutTotalCount()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }

        System.out.println("FileSplitFlagError=EJBTransaction Param Error!$EndSplitFlag");
    }

    public static void GetEJBPoolInfo(MBeanHome home)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("EJBPoolRuntime");
        Iterator mbeanIterator = mbeanSet.iterator();
        for(int i = 0; mbeanIterator.hasNext(); i++)
        {
            EJBPoolRuntimeMBean EJBPool = (EJBPoolRuntimeMBean)mbeanIterator.next();
            stringbuffer.append("EJBPoolName").append(i).append("=").append(EJBPool.getName());
            stringbuffer.append("*@");
        }

        stringbuffer.append("EndSplitFlag");
        System.out.println(stringbuffer.toString());
    }

    public static void GetEJBPoolInfo(MBeanHome home, String strName)
    {
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append("FileSplitFlag");
        Set mbeanSet = home.getMBeansByType("EJBPoolRuntime");
        for(Iterator mbeanIterator = mbeanSet.iterator(); mbeanIterator.hasNext();)
        {
            EJBPoolRuntimeMBean EJBPool = (EJBPoolRuntimeMBean)mbeanIterator.next();
            if(strName.compareTo(EJBPool.getName()) == 0)
            {
                stringbuffer.append("EJBPoolName=").append(EJBPool.getName()).append("$");
                stringbuffer.append("IdleBeansCount=").append(EJBPool.getIdleBeansCount()).append("$");
                stringbuffer.append("InUseCount=").append(EJBPool.getBeansInUseCount()).append("$");
                stringbuffer.append("WaiterTotalCount=").append(EJBPool.getWaiterTotalCount()).append("$");
                stringbuffer.append("TimeoutTotalCount=").append(EJBPool.getTimeoutTotalCount()).append("$");
                stringbuffer.append("EndSplitFlag");
                System.out.println(stringbuffer.toString());
                return;
            }
        }

        System.out.println("FileSplitFlagError=EJBPool Param Error!$EndSplitFlag");
    }

}
