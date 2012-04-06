package com.dragonflow.erlangecc.monitor.weblogic;
import java.io.*;
import java.util.Properties;


public abstract class RMIProcessProperties
{

    public RMIProcessProperties()
    {
        isWindows = System.getProperties().getProperty("os.name").startsWith("Windows");
        isRunning = false;
    }

    public Process getProcess()
    {
        return proc;
    }

    public void setRunning(boolean running)
    {
        isRunning = running;
    }

    public String getCommandLine()
    {
        StringBuffer cmd = new StringBuffer();
        String cmdLineArgs[] = getCommandLineArray();
        for(int i = 0; i < cmdLineArgs.length; i++)
            cmd.append(cmdLineArgs[i]).append(" ");

        return cmd.toString();
    }

    public Integer getHashKeyAsInteger()
    {
        return new Integer(getHashKey());
    }

    public boolean isRunning()
    {
        return isRunning;
    }

    public void registerNewProcess(Process p)
    {
        isRunning = true;
        proc = p;
        ProcessWaiter waiter = new ProcessWaiter(this);
        waiter.start();
        String logPrefix = getLogPrefix();
        BufferedReader out = new BufferedReader(new InputStreamReader(p.getInputStream()));
        Thread outputReader = new ReaderThread(out, logPrefix);
        outputReader.start();
        BufferedReader err = new BufferedReader(new InputStreamReader(p.getErrorStream()));
        Thread errorReader = new ReaderThread(err, logPrefix);
        errorReader.start();
    }

    public abstract String getURL();

    public abstract int getHashKey();

    public abstract String[] getCommandLineArray();

    public abstract String getLogPrefix();

    protected static long siteViewToken = System.currentTimeMillis();
    public static final String CODEBASE;
    public static final String HOSTNAME = "127.0.0.1";
    protected boolean isWindows;
    protected boolean isRunning;
    protected Process proc;

    static 
    {
    	String strCur = System.getProperty("user.dir");
		if (strCur.endsWith(";"))
			strCur.replaceAll(";", "");
		
        CODEBASE = "file:///"+strCur.replace(" ", "%20").replace("\\", "/")+ File.separator + "lib";
        File f = new File(CODEBASE);
        
        //CODEBASE = "file:////"+strCur + File.separator + "bin";
    }
}
