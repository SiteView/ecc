package com.dragonflow.siteview.websphere.util;
import java.io.*;
import java.util.Properties;




public abstract class RMIProcessProperties
{
  protected static long siteViewToken = System.currentTimeMillis();

 // public static final String CODEBASE = "file:/" + PlatformNew.getClassesDir() + File.separator;
  
  //"file:///"+strCur + File.separator + "lib";
  
  public static final String CODEBASE = "file:///" + System.getProperty("user.dir").replaceAll(";", "")+File.separator+"lib"+File.separator ;
  public static final String HOSTNAME = "localhost";
  protected boolean isWindows;
  protected boolean isRunning;
  protected Process proc;

  public RMIProcessProperties()
  {
    this.isWindows = System.getProperties().getProperty("os.name").startsWith("Windows");
    this.isRunning = false;
  }

  public Process getProcess()
  {
    return this.proc;
  }

  public void setRunning(boolean running)
  {
    this.isRunning = running;
  }

  public String getCommandLine()
  {
    StringBuffer cmd = new StringBuffer();
    String[] cmdLineArgs = getCommandLineArray();

    for (int i = 0; i < cmdLineArgs.length; ++i) {
      cmd.append(cmdLineArgs[i]).append(" ");
    }
    return cmd.toString();
  }

  public Integer getHashKeyAsInteger()
  {
    return new Integer(getHashKey());
  }

  public boolean isRunning()
  {
    return this.isRunning;
  }

  public void registerNewProcess(Process p)
  {
    this.isRunning = true;
    this.proc = p;

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
}
