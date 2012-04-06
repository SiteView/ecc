package com.dragonflow.siteview.websphere.util;
import java.io.File;
import java.io.IOException;
import java.net.BindException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.*;




public class RMIProcessLauncher
{


    public static final int PROCESS_STARTED = 0;

    public static final int PROCESS_ALREADY_RUNNING = 1;

    public static final int PROCESS_FAILED = 2;

    private static Map RMIProcesses = Collections.synchronizedMap(new HashMap());
  

    private static java.rmi.registry.Registry registry = null;

    private static int registryPort = -1;

    private static int defaultRegistryPort;

    public RMIProcessLauncher() {
    }

    public static synchronized int launch(WebSphereProcessProperties props) {
        if (registry == null) {
            try {
                System.out.println("No registry found, attempting to create one on port " + defaultRegistryPort);
                registryPort = createRegistry(defaultRegistryPort);
            } catch (java.rmi.RemoteException remoteexception) {
            	 System.err.println( "WebSphereProcessLauncher failed to create RMI registry.  Exception was: " + remoteexception.getMessage());
                return 2;
            }
        }
        RMIProcessProperties procProps = (RMIProcessProperties)RMIProcesses.get(props.getHashKeyAsInteger());
          if ((procProps != null) && 
             (procProps.isRunning())) {
              return 1;
             }
          
          String strCur = System.getProperty("user.dir");
  		if (strCur.endsWith(";"))
  			strCur.replaceAll(";", "");
  		//getRoot() + File.separator + "WEB-INF" + File.separator + "classes";
          File workingDirectory = new File(strCur);

       // java.io.File file = new File(COM.dragonflow.SiteView.Platform.getRoot() + java.io.File.separator + "classes");
        try {
        	System.out.println("Starting new RMIProcess[" + props.getHashKey() + "] with command line: " + props.getCommandLine());
        	 Process proc = Runtime.getRuntime().exec(props.getCommandLineArray(), null, workingDirectory);
        	 props.registerNewProcess(proc);
        } catch (java.io.IOException ioexception) {
        	System.err.println("Could not start IBM JVM for WebSphere RMI service.  Execption was: " + ioexception.getMessage());
            return 2;
        }
        RMIProcesses.put(props.getHashKeyAsInteger(), props);
        return 0;
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @param i
     * @return
     * @throws java.rmi.RemoteException
     */
    private static int createRegistry(int port) throws java.rmi.RemoteException {
        while (true) {
            try {
                registry = java.rmi.registry.LocateRegistry.createRegistry(port);
                System.out.println("WebSphereProcessLauncher successfully created RMI registry on port " + port + ".");
                return port;
            } catch (java.rmi.RemoteException remoteexception) {
                if (remoteexception.getCause() instanceof java.net.BindException) {
                	System.out.println( "WebSphereProcessLauncher failed to create RMI registry on port " + port + " because the port was already in use. ");
                	port ++;
                } else {
                    throw remoteexception;
                }
            }
        }

    }

    public static int getRegistryPort() {
        return registryPort;
    }

    static {
        defaultRegistryPort = 1099;
       
    }

}