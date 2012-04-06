package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.CharArrayWriter;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.dragonflow.erlangecc.monitor.JMXInterface;
import com.dragonflow.erlangecc.monitor.Weblogic6xObject;

import com.sun.net.ssl.HostnameVerifier;
import com.sun.net.ssl.HttpsURLConnection;

public class WebLogicServerImpl extends UnicastRemoteObject
    implements WebLogicServer
{
    static class NullHostnameVerifier
        implements HostnameVerifier
    {

        public boolean verify(String urlHostname, String certHostname)
        {
            return true;
        }

        NullHostnameVerifier()
        {
        }
    }

    JMXInterface management;
    private Object mbs;
    private Hashtable addedMBeans;
    private Object initialContextObject;
    boolean debug;
    private WeblogicConnectionProperties connProps;
    private ClassLoader weblogicJarLoader;
    

    public WebLogicServerImpl(WeblogicConnectionProperties connProps, ClassLoader jarLoader)
    throws RemoteException
    {
    	super(0, new TimeoutClientSocketFactory(connProps.getTimeout()), new RegularServerSocketFactory());
    	debug = Boolean.getBoolean("weblogic.monitor.debug");
    	weblogicJarLoader = jarLoader;
    	setClassLoader();
    	this.connProps = connProps;
    	management = new JMXInterface();
    }
    private void closeInitialContext()
    {
        if(initialContextObject != null)
        {
            Method closeMethod = null;
            try
            {
                Class initialContextClass = initialContextObject.getClass();
                closeMethod = initialContextClass.getMethod("close", null);
                closeMethod.invoke(initialContextObject, null);
            }
            catch(NoSuchMethodException e)
            {
                error("WebLogic6xMonitorImpl.closeInitialContext() couldn't find IntialContext.close() method: ", e);
            }
            catch(IllegalAccessException e)
            {
                error("WebLogic6xMonitorImpl.closeInitialContext() couldn't access IntialContext.close() method: ", e);
            }
            catch(InvocationTargetException e)
            {
                error("WebLogic6xMonitorImpl.closeInitialContext() error invoking IntialContext.close() method: ", e);
            }
            catch(Exception e)
            {
                error("WebLogic6xMonitorImpl.closeInitialContext() caught a general exception while invoking IntialContext.close() method: ", e);
            }
        }
    }
    
    protected void error(String msg, Throwable e)
    {
        System.err.println(msg);
        if(debug && e != null)
            e.printStackTrace();
    }
    
    private void setClassLoader()
    {
        if(weblogicJarLoader != null)
            Thread.currentThread().setContextClassLoader(weblogicJarLoader);
    }

    public Object[] update(String as[], String as1[])throws  RemoteException {
        try {
        	setClassLoader();
            connect();
        } catch (Exception exception) {
        	closeInitialContext();
            return null;
        }
        Set set = null;
        try {
            set = (Set) management.queryNamesMethod.invoke(mbs, new Object[] { null,
                    management.finalSubStringMethod.invoke(null, new Object[] { management.attrMethod.invoke(null, new Object[] { "Type" }), management.valueMethod.invoke(null, new Object[] { "Runtime" }) }) });
        } catch (Exception exception1) {
        	closeInitialContext();
            return null;
        }
        //System.out.println("Start search counter....");
        Object[] as2 = new Object[as1.length];
        for (int i = 0; i < as1.length; i ++) {
            int j = as1[i].lastIndexOf('/');
            if (j < 0) {
                continue;
            }
            String s = as1[i].substring(j + 1);
            String s2 = as1[i].substring(0, j);
            j = as[i].lastIndexOf('/');
            if (j < 0) {
                continue;
            }
            int k = as[i].lastIndexOf('/', j - 1);
            int l = as[i].indexOf(':', k);
            String s1 = as[i].substring(l + 1, j);
            //System.out.println("---"+s1+":"+s2);
            Object obj = findObjectName(s1, s2, set);
            
            try {
                //as2[i] = String.valueOf(management.getAttributeMethod.invoke(mbs, new Object[] { obj, s }));
            	as2[i] = management.getAttributeMethod.invoke(mbs, new Object[] { obj, s });
                continue;
            } catch (Exception exception2) {
                System.out.println(exception2);
            }
            System.out.println("Failed to get value for counter " + as1[i] + ". If this happens only a handfull of counters, it is probably due to a WebLogic bug");
            as2[i] = "Error";
        }
        closeInitialContext();
        return as2;
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @param s
     * @param s1
     * @param set
     * @return
     */
    Object findObjectName(String s, String s1, Set set) {
        Object obj = null;
        try {
            Object obj1;
            obj = management.objectNameCtr.newInstance(new Object[] { s1 });
            obj1 = set.iterator();

            Object obj2;
            while (((Iterator) (obj1)).hasNext()) {

                obj2 = ((Iterator) (obj1)).next();
                if (((Boolean) management.equalsMethod.invoke(obj2, new Object[] { obj })).booleanValue()) {
                    return obj2;
                }
            }
        } catch (Exception exception) {
            return null;
        }

        System.out.println("Fannie Mae Problem start");

        try {
            if (management.getKeyPropertyMethod.invoke(obj, new Object[] { "Type" }).equals("ServletRuntime")) {
                String result = (String) management.getKeyPropertyMethod.invoke(obj, new Object[] { "Name" });
                int i;
                for (i = result.length() - 1; i >= 0 && Character.isDigit(result.charAt(i)); i --) {
                }

                Object obj3 = null;
                Object obj4;
                String s3;
                String s2;
                Iterator iterator = set.iterator();
                while (iterator.hasNext()) {
                    obj4 = iterator.next();
                    s2 = (String) management.getKeyPropertyMethod.invoke(obj4, new Object[] { "Type" });
                    if (s2 != null && s2.equals("ServletRuntime")) {
                        s3 = (String) management.getKeyPropertyMethod.invoke(obj4, new Object[] { "Name" });
                        if (s3.regionMatches(0, result, 0, i)) {

                            try {
                                String s4 = (String) management.getAttributeMethod.invoke(mbs, new Object[] { obj4, "ServletName" });
                                if (s4 != null) {
                                    if (s4.equals(s)) {
                                        System.out.println("Found object by ServletName");
                                        return obj4;

                                    } else {
                                        break;
                                    }
                                }
                            } catch (Exception e1) {
                                if (obj3 != null) {

                                	System.out.println( "Ambiguous near macth found:\n   " + obj3 + "\n" + "   " + obj4);
                                    return null;
                                }
                                obj3 = obj4;
                            }
                        }
                    }
                }
                return obj3;
            }
        } catch (Exception e) {
            return null;
        }

        return null;
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @param stringbuffer
     * @return
     */
    public Map<String,Object> getBrowseData() throws  RemoteException{
        try {
        	setClassLoader();
            connect();
            Weblogic6xObject weblogic6xobject = new Weblogic6xObject();
            addedMBeans = new Hashtable();
            Set set = (Set) management.queryNamesMethod.invoke(mbs, new Object[] { null,
                    management.finalSubStringMethod.invoke(null, new Object[] { management.attrMethod.invoke(null, new Object[] { "Type" }), management.valueMethod.invoke(null, new Object[] { "Runtime" }) }) });

            int i = set.size();
            System.out.println("Weblogic: Found " + i + " beans.");
            Object obj = set.iterator();
            while (((Iterator) (obj)).hasNext()) {
                Object obj1 = ((Iterator) (obj)).next();
                if (!addedMBeans.containsKey(obj1)) {
                    Weblogic6xObject weblogic6xobject1 = new Weblogic6xObject(obj1, mbs, management);
                    if ((!weblogic6xobject1.getType().equals("WebAppComponentRuntime") || !weblogic6xobject1.getName().equals("console") && !weblogic6xobject1.getName().equals("wl_management_internal1")
                            && !weblogic6xobject1.getName().equals("wl_management_internal2"))
                            && (!weblogic6xobject1.getType().equals("ServletRuntime") || management.getKeyPropertyMethod.invoke(weblogic6xobject1.getParentName(), new Object[] { "Type" }).equals("WebAppComponentRuntime"))) {
                        addObject(weblogic6xobject1, weblogic6xobject);
                        //System.out.println(weblogic6xobject1.getParentName());
                    }
                }
            }
            Map<String,Object> map = new HashMap<String,Object>();
            weblogic6xobject.purge();
            //obj = new CharArrayWriter();
            //weblogic6xobject.printXML(new PrintWriter(((java.io.Writer) (obj))), 0);
            //return ((CharArrayWriter) (obj)).toString();
            weblogic6xobject.toMap(map, "");
            System.out.println("map size:" + map.size());
            closeInitialContext();
            return map;
        } catch (Throwable throwable) {
            Throwable throwable1 = throwable.getCause();
            if (throwable1 != null) {
            	System.out.println( "WebLogic exception cause: " + throwable1.toString());
            }
            throwable.printStackTrace();
            String s = throwable.getMessage();
            closeInitialContext();
            if (s==null){
            	return null;
            }else
            	return null;
        }
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @throws Exception
     */
    private void connect() throws Exception {
        try {
            String s;
            if (connProps.isSecureConnection()) {
                s = "t3s";
                System.setProperty("weblogic.security.SSL.ignoreHostnameVerification", "true");
                HttpsURLConnection.setDefaultHostnameVerifier(new NullHostnameVerifier());
            } else {
                s = "t3";
            }
            Hashtable env = new Hashtable();
            env.put("java.naming.factory.initial", "weblogic.jndi.WLInitialContextFactory");
            env.put("java.naming.provider.url", s + "://" + connProps.getServerName());
            if (connProps.getUsername().length() > 0 && connProps.getPassword().length() > 0) {
            	env.put("java.naming.security.principal", connProps.getUsername());
            	env.put("java.naming.security.credentials", connProps.getPassword());
            }
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            Class initialContextClass = classLoader.loadClass("javax.naming.InitialContext");
            
            Constructor initialContextCtr = initialContextClass.getConstructor(new Class[] {
                    java.util.Hashtable.class
                });
            
            Method lookupMethod = initialContextClass.getMethod("lookup", new Class[] {
                    java.lang.String.class
                });
            
            initialContextObject = initialContextCtr.newInstance(new Object[] {
                    env
                });
            Object home = lookupMethod.invoke(initialContextObject, new Object[] {
                    "weblogic.management.home.localhome"
                });
            mbs = home.getClass().getMethod("getMBeanServer", null).invoke(home, null);
            System.out.println( "WebLogic: Connected");
        } catch (Throwable throwable) {
            Throwable throwable1 = throwable.getCause();
            if (throwable1 != null) {
            	System.out.println("WebLogic exception cause: " + throwable1.toString());
                String s1 = throwable1.getClass().getName();
                if ("javax.naming.NoInitialContextException".equals(s1)) {
                	System.out.println(throwable.toString());
                    throw new Exception("No WebLogic server found on " + connProps.getServerName());
                }
                if ("javax.naming.AuthenticationException".equals(s1)) {
                	System.out.println( throwable.toString());
                    throw new Exception("Access denied for user " + connProps.getUsername());
                }
                if ("javax.naming.CommunicationException".equals(s1)) {
                	System.out.println(throwable.toString());
                    throw new Exception("No WebLogic server found on " + connProps.getServerName());
                }
            } else {
            	System.out.println( "Exception '" + throwable.toString() + "' with cause == null");
            }
            throwable.printStackTrace();
            System.out.println( throwable.toString());
            String s2 = throwable.getClass().getName();
            if (s2.equals("weblogic.management.NoAccessRuntimeException")) {
                throw new Exception("Access denied for user " + connProps.getUsername());
            }
            if (s2.equals("weblogic.common.internal.VersioningError")) {
                throw new Exception(
                        "Server may be running external jar files. This is a known WebLogic issue and has a few workarounds:\n1. Place the jar file after the weblogic.jar entry in the classpath\n2. Instead of a jar file, keep the external classes in a directory structure and include the root directory instead of the jar file in classpatch\n3. If all else fails, provide the path to the weblogic jar file in the provided edit field of the \"Choose Counters\" screen");
            } else {
                throw new Exception("Unhandled exception thrown: " + s2 + " See log for details");
            }
        }
    }

    private void addObject(Weblogic6xObject weblogic6xobject, Weblogic6xObject weblogic6xobject1) {
        Weblogic6xObject weblogic6xobject2;
        try {
            Object obj = weblogic6xobject.getParentName();
            weblogic6xobject2 = (Weblogic6xObject) addedMBeans.get(obj);
            if (weblogic6xobject2 == null) {
                weblogic6xobject2 = new Weblogic6xObject(obj, mbs, management);
                addObject(weblogic6xobject2, weblogic6xobject1);
            }
        } catch (Exception exception) {
            weblogic6xobject2 = weblogic6xobject1;
        }
        weblogic6xobject.setParent(weblogic6xobject2);
        weblogic6xobject2.addSubObject(weblogic6xobject);
        addedMBeans.put(weblogic6xobject.getObjectName(), weblogic6xobject);
        if (weblogic6xobject.getType().equals("WebAppComponentRuntime")) {
            try {
                Object aobj[] = (Object[]) management.getAttributeMethod.invoke(mbs, new Object[] { weblogic6xobject.getObjectName(), "Servlets" });
                for (int i = 0; i < aobj.length; i ++) {
                    if (!addedMBeans.containsKey(aobj[i])) {
                        Weblogic6xObject weblogic6xobject3 = new Weblogic6xObject(aobj[i], weblogic6xobject, mbs, management);
                        weblogic6xobject.addSubObject(weblogic6xobject3);
                    }
                }

            } catch (Exception exception1) {
                exception1.printStackTrace();
            }
        }
    }
    
    public String getServerName()
    throws RemoteException
	{
    	String strHost = connProps.getServerName();
    	strHost.substring(strHost.indexOf(";"));
	    return strHost;
	}
}