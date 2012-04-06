package com.dragonflow.siteview.websphere.util;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Properties;

import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.dragonflow.siteview.node.JavaNode;

public class WebSphereProcessProperties extends RMIProcessProperties
{
  public static final String WEBSPHERE_MS_KEY = "_websphere_ms";
  public static final String WEBSPHERE_MX_KEY = "_websphere_mx";
//  private static Log logger = LogFactory.getEasyLog(WebSphereProcessProperties.class);

  private static long siteViewToken = System.currentTimeMillis();

  private static Properties props = new Properties();
  private static String propsFilename = ServicePlatform.getRoot()
	+ "/templates.applications/websphere.props";
  private static long heartBeatFrequency = 3000L;
  public static final int VERSION_3_5 = 0;
  public static final int VERSION_4_X = 1;
  public static final int VERSION_5_X = 2;
  public static final int VERSION_6_0 = 3;
  public static final int VERSION_6_1 = 4;
  public static final int VERSION_7_0 = 5;
  public static final String VERSION_3_5_STR = "3.5x";
  public static final String VERSION_4_X_STR = "4.x";
  public static final String VERSION_5_X_STR = "5.x";
  public static final String VERSION_6_0_STR = "6.x";
  public static final String VERSION_6_1_STR = "6.1x";
  public static final String VERSION_7_0_STR = "7.0x";
  private String hostname;
  private String classpath;
  private String webSphereDir;
  private String clientProps = "sas.client.props";
  private String jvmName;
  private String api;
  private int hashKey;
  private int version;
  private static final String WEBSPHERE_SERVICE_CLASSNAME;
  private static final String FILE_SEP;
  private static final String PATH_SEP;

  public WebSphereProcessProperties(String classpath, String cProps, String webSphereDir, String version, boolean windows, String hostname)
  {
    this.hostname = hostname.toLowerCase();
    this.classpath = classpath;
    this.webSphereDir = webSphereDir;
    this.isWindows = windows;

    setJVMName();
    setClientProps(cProps, windows);
    detectVersion(version);
    detectAPI(this.version);
    augmentClasspath();
    if (this.version == 1) {
      this.hashKey = this.webSphereDir.hashCode() + this.classpath.hashCode() + this.version + this.clientProps.hashCode() + this.hostname.hashCode();
    }
    else
      this.hashKey = this.webSphereDir.hashCode() + this.classpath.hashCode() + this.version + this.clientProps.hashCode();
  }

  private void setClientProps(String clientProps, boolean isWindows)
  {
    if ((clientProps == null) || (clientProps.length() == 0)) {
      clientProps = this.clientProps;
    }
   clientProps = clientProps.replace('\\', '/');
    if ((clientProps.charAt(0) != '/') && (clientProps.charAt(0) != ':')) {
     clientProps = this.webSphereDir.replace('\\', '/') + "/properties/" + clientProps;
    }

    if (isWindows) {
      clientProps = "/" + clientProps;
    }
    this.clientProps = clientProps;
  }

  public String getURL()
  {
    StringBuffer u = new StringBuffer("rmi://").append("localhost").append(":").append(RMIProcessLauncher.getRegistryPort());
    u.append("/WebSphereService").append(this.hashKey);
    return u.toString();
  }

  private void setJVMName()
  {
	  
    if (this.isWindows)
      this.jvmName = "java.exe";
    else
      this.jvmName = "java";
  }

  private void detectAPI(int version)
  {
    switch (version)
    {
    case 1:
      this.api = "PMI";
      break;
    case 2:
    case 3:
    case 4:
    case 5:
      this.api = "JMX";
    }
  }

  private void detectVersion(String version)
  {
    if (version.equals("4.x"))
      this.version = 1;
    else if (version.equals("5.x"))
      this.version = 2;
    else if (version.equals("6.x"))
      this.version = 3;
    else if (version.equals("6.1x"))
      this.version = 4;
    else
      this.version = 5;
  }

  private void augmentClasspath()
  {
    StringBuffer cp = new StringBuffer(this.classpath);
    if (cp.length() > 0) {
      cp.append(PATH_SEP);
    }
    String strCur = JavaNode.getCurrentDir();
    cp.append(".").append(PATH_SEP).append(strCur).append(File.pathSeparator);

    if ((this.version == 3) || (this.version == 4) || (this.version == 5))
    {
      String wasVersionArgId = "";

      if (this.version == 3)
        wasVersionArgId = "was60x";
      else if (this.version == 4)
        wasVersionArgId = "was61x";
      else {
        wasVersionArgId = "was70x";
      }

      String paths = props.getProperty(wasVersionArgId + "_classpath");
      StringBuffer wasClasses = new StringBuffer();
      for (String path : paths.split(";")) {
        File pathFile = new File(this.webSphereDir + FILE_SEP + path);
        if (pathFile.exists()) {
          wasClasses.append(PATH_SEP + pathFile.getAbsolutePath());
        }
//        else if (logger.isDebugEnabled()) {
//          logger.debug("WebSphereProcessProperties.augmentClasspath() is skipping the classpath file " + pathFile.getAbsolutePath() + " because it could not be found.");
//        }

      }

      cp.append(wasClasses.toString());
    } else {
      for (int i = 1; ; ++i) {
        String path = props.getProperty("classpath" + this.api + i);
        if (path == null) {
          break;
        }
        cp.append(PATH_SEP + this.webSphereDir + FILE_SEP + "lib" + FILE_SEP + path);
      }
    }

    this.classpath = cp.toString();
  }

  public String[] getCommandLineArray()
  {
	  

    int dirLength = this.webSphereDir.length();
    if ((dirLength > 0) && (this.webSphereDir.charAt(dirLength - 1) == '\\')) {
      this.webSphereDir = this.webSphereDir.substring(0, dirLength - 1);
    }

//    IMasterConfig masterConfig = ConfigManagerSession.getReadOnlyMasterConfig();
//    String memMin = (String)masterConfig.get("_websphere_ms");
    String memMin ="-Xms256m";
//    if ((memMin == null) || (memMin.length() == 0))
//      memMin = "-Xms256m";
//    else {
//      memMin = "-Xms" + memMin.trim() + "m";
//    }
//    String memMax = (String)masterConfig.get("_websphere_mx");
    String memMax="-Xmx512m";

      memMax = "-Xmx512m";


    String cmd = this.webSphereDir + FILE_SEP + "java" + FILE_SEP + "jre" + FILE_SEP + "bin" + FILE_SEP + this.jvmName;

    String packagePrefix = super.getClass().getPackage().getName();

    ArrayList argumentList = new ArrayList();
    argumentList.add(cmd);
    argumentList.add(memMin);
    argumentList.add(memMax);
    argumentList.add("-classpath");
    argumentList.add(this.classpath);
    argumentList.add("-Djava.rmi.server.codebase=" + CODEBASE);
    argumentList.add("-Djava.rmi.server.hostname=localhost");
    argumentList.add("-D" + packagePrefix + "." + "registryURL" + "=" + getURL());
    argumentList.add("-D" + packagePrefix + "." + "heartBeatFrequency" + "=" + heartBeatFrequency);
    argumentList.add("-D" + packagePrefix + "." + "token" + "=" + siteViewToken);
    argumentList.add("-Dcom.ibm.CORBA.ConfigURL=file:" + this.clientProps);
    argumentList.add("-Dcom.ibm.SOAP.ConfigURL=file:" + this.clientProps);
    argumentList.add("-Dwas.install.root=" + this.webSphereDir);
    argumentList.add("-Dibm.websphere.preload.classes=true");
    argumentList.add("-Djava.naming.factory.initial=com.ibm.websphere.naming.WsnInitialContextFactory");

    String launchWithRemoteDebug = System.getProperty("websphere.monitor.remote.debugger");
    String remoteDebugPort = System.getProperty("websphere.monitor.remote.debugger.port");
    if ((launchWithRemoteDebug != null) && (launchWithRemoteDebug.equalsIgnoreCase("true"))) {
      argumentList.add("-Xdebug");
      argumentList.add("-Xnoagent");
      argumentList.add("-Djava.compiler=NONE");
      if (remoteDebugPort == null)
        argumentList.add("-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005");
      else {
        argumentList.add("-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=" + remoteDebugPort);
      }
    }

//    if (logger.isDebugEnabled()) {
 //     argumentList.add("-Dwebsphere.monitor.debug=true");
//    }

    argumentList.add("com.dragonflow.siteview.websphere.util.WebSphereServiceImpl");

    String[] args = new String[argumentList.size()];
    args = (String[])argumentList.toArray(args);

    return args;
  }

  public String getClasspath()
  {
    return this.classpath;
  }

  public String getWebSphereDir()
  {
    return this.webSphereDir;
  }

  public int getVersion()
  {
    return this.version;
  }

  public boolean isWindows()
  {
    return this.isWindows;
  }

  public String getJvmName()
  {
    return this.jvmName;
  }

  public String getApi()
  {
    return this.api;
  }

  public String getLogPrefix() {
    return "WebSphereService[" + this.hashKey + "]: ";
  }

  public int getHashKey()
  {
    return this.hashKey;
  }

  static
  {
    FileInputStream fileInputStream = null;
    try {
      fileInputStream = new FileInputStream(propsFilename);
      props.load(fileInputStream);
    } catch (Exception e) {
//      logger.error("Cannot open Websphere properties file: " + propsFilename);
    } finally {
      try {
        if (fileInputStream != null)
          fileInputStream.close();
      } catch (Exception e) {
//        logger.debug("static initializer - Cannot close the FileInputStream", (logger.isDebugEnabled()) ? e : null);
      }
    }

//    IMasterConfig config = ConfigManagerSession.getReadOnlyMasterConfig();

//    String freq = (String)TextUtils.getValue(config, "_webSphereServiceHeartBeatFrequency");
    String freq="";
    if (freq.length() > 0) {
      try {
        heartBeatFrequency = Long.parseLong(freq);
      } catch (NumberFormatException e) {
        heartBeatFrequency = 3000L;
//        logger.error("Could not parse a number from _webSphereServiceHeartBeatFrequency=" + freq + ".  Using the default of " + heartBeatFrequency);
      }

    }

   WEBSPHERE_SERVICE_CLASSNAME = WebSphereServiceImpl.class.getName();
    FILE_SEP = System.getProperties().getProperty("file.separator");
    PATH_SEP = System.getProperties().getProperty("path.separator");
  }
}