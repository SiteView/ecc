package com.dragonflow.erlangecc.monitor.weblogic;
import java.io.File;
import java.io.IOException;
import java.net.BindException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.*;


public class RMIProcessLauncher
{

    public RMIProcessLauncher()
    {
    }

    public static synchronized int launch(RMIProcessProperties props)
    {
        if(registry == null)
            try
            {
                System.out.println("No registry found, attempting to create one on port " + defaultRegistryPort);
                registryPort = createRegistry(defaultRegistryPort);
            }
            catch(RemoteException e)
            {
                System.err.println("RMIProcessLauncher failed to create RMI registry.  Exception was: " + e.getMessage());
                return 2;
            }
        RMIProcessProperties procProps = (RMIProcessProperties)RMIProcesses.get(props.getHashKeyAsInteger());
        if(procProps != null && procProps.isRunning())
            return 1;
        
        String strCur = System.getProperty("user.dir");
		if (strCur.endsWith(";"))
			strCur.replaceAll(";", "");
		
        File workingDirectory = new File(strCur);
        try
        {
            System.out.println("Starting new RMIProcess[" + props.getHashKey() + "] with command line: " + props.getCommandLine());
            Process proc = Runtime.getRuntime().exec(props.getCommandLine(), null, workingDirectory);
            props.registerNewProcess(proc);
        }
        catch(IOException e)
        {
            System.err.println("Could not start process : " + e.getMessage());
            return 2;
        }
        RMIProcesses.put(props.getHashKeyAsInteger(), props);
        return 0;
    }

    private static int createRegistry(int port)
        throws RemoteException
    {
        do
            try
            {
                registry = LocateRegistry.createRegistry(port);
                System.out.println("RMIProcessLauncher successfully created RMI registry on port " + port);
                return port;
            }
            catch(RemoteException e)
            {
                if(e.getCause() instanceof BindException)
                {
                	
                    System.err.println("RMIProcessLauncher failed to create RMI registry on port  because the port was already in use. ");
                    System.err.println(e.toString());
                    port++;
                } else
                {
                    throw e;
                }
            }
        while(true);
    }

    public static int getRegistryPort()
    {
        return registryPort;
    }

    public static final int PROCESS_STARTED = 0;
    public static final int PROCESS_ALREADY_RUNNING = 1;
    public static final int PROCESS_FAILED = 2;
    private static Map RMIProcesses = Collections.synchronizedMap(new HashMap());
    private static Registry registry = null;
    private static int registryPort = -1;
    private static int defaultRegistryPort;

    static 
    {
        defaultRegistryPort = 1099;
    }
}