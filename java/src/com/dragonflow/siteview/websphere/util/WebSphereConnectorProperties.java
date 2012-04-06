package com.dragonflow.siteview.websphere.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

import com.dragonflow.siteview.infra.util.ServicePlatform;

public class WebSphereConnectorProperties
{
//  private static Log logger = LogFactory.getEasyLog(WebSphereConnectorProperties.class);

  public static final String FILE_SEP = System.getProperties().getProperty("file.separator");
  public static final String PATH_SEP = System.getProperties().getProperty("path.separator");

  public static final String PROPS_FILENAME = ServicePlatform.getRoot()
	+ "/templates.applications/websphere.props";
  public static final String WEBSPHERE_CLASSPATH_PREFIX_WAS_6_0X = "was60x";
  public static final String WEBSPHERE_CLASSPATH_PREFIX_WAS_6_1X = "was61x";
  public static final String WEBSPHERE_CLASSPATH_PREFIX_WAS_7_0X = "was70x";
  private static Properties PROPERTIES = new Properties();
  private String hostname;
  private String classpath;
  private String webSphereDir;
  private String clientProps = "sas.client.props";
  private String api;
  private boolean isWindows;
  private int hashKey;
  private int version;

  public WebSphereConnectorProperties(String additionalClassPath, String clientProps, String webSphereDir, String version, boolean isWindows, String hostname)
    throws Exception
  {
    this.hostname = hostname.toLowerCase();
    this.classpath = additionalClassPath;
    this.webSphereDir = webSphereDir;
    this.isWindows = isWindows;

    detectVersion(version);
    initClientPropsFile(clientProps, isWindows);

    initialize();
  }

  private void initialize() throws Exception {
    initAPI(getVersion());
    initClasspath(getClassPath());
    initHashKey(getVersion());
  }

  private void initHashKey(int version) {
    if (version == 1) {
      this.hashKey = getWebSphereDir().hashCode() + getClassPath().hashCode() + getVersion() + getClientProps().hashCode() + getHostname().hashCode();
    }
    else
      this.hashKey = getWebSphereDir().hashCode() + getClassPath().hashCode() + getVersion() + getClientProps().hashCode();
  }

  private void initClientPropsFile(String clientProps, boolean isWindows)
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

  private void initAPI(int version)
    throws Exception
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
      break;
    default:
      throw new Exception("Unable to initialize API for WebSphere version: " + version);
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

  private void initClasspath(String additionalClassPath)
    throws Exception
  {
    StringBuilder classPath = new StringBuilder(additionalClassPath);

    if (classPath.length() > 0) {
      classPath.append(PATH_SEP);
    }

    if ((this.version == 3) || (this.version == 4) || (this.version == 5))
    {
      String websphereVersionPrefix = "";

      switch (this.version)
      {
      case 3:
        websphereVersionPrefix = "was60x";
        break;
      case 4:
        websphereVersionPrefix = "was61x";
        break;
      case 5:
        websphereVersionPrefix = "was70x";
        break;
      default:
        throw new Exception("Unable to initialize WebSphere classpath version prefix to use in " + PROPS_FILENAME + " for Websphere version: " + this.version);
      }

      String classPathKey = PROPERTIES.getProperty(websphereVersionPrefix + "_classpath");

      if (classPathKey == null) {
        throw new Exception("The key " + classPathKey + " is not found in the property list");
      }
      StringBuilder websphereJars = new StringBuilder();
      for (String path : classPathKey.split(";")) {
        File pathFile = new File(this.webSphereDir + FILE_SEP + path);
        if (pathFile.exists()) {
          websphereJars.append(PATH_SEP + pathFile.getAbsolutePath());
        }
//        else if (logger.isWarnEnabled()) {
//          logger.warn("WebSphereProcessProperties.augmentClasspath() is skipping the classpath file " + pathFile.getAbsolutePath() + " because it could not be found.");
//        }

      }

      classPath.append(websphereJars.toString());
    }
    else {
      for (int i = 1; ; ++i) {
        String websphereJar = PROPERTIES.getProperty("classpath" + this.api + i);
        if (websphereJar == null) {
          break;
        }
        classPath.append(PATH_SEP + this.webSphereDir + FILE_SEP + "lib" + FILE_SEP + websphereJar);
      }
    }

    this.classpath = classPath.toString();
  }

  public String getClassPath()
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

  public String getApi()
  {
    return this.api;
  }

  public int getHashKey()
  {
    return this.hashKey;
  }

  public String getClientProps() {
    return this.clientProps;
  }

  public String getHostname() {
    return this.hostname;
  }

  public String toString()
  {
    return getHostname() + "," + getWebSphereDir() + "," + getVersion() + "," + getClientProps() + "," + getApi() + "," + getHashKey() + "," + getClassPath();
  }

  static
  {
    FileInputStream fileInputStream = null;
    try {
      fileInputStream = new FileInputStream(PROPS_FILENAME);
      PROPERTIES.load(fileInputStream);
    } catch (FileNotFoundException e) {
//      if (logger.isErrorEnabled())
//        logger.error("Static initializer - Cannot open Websphere properties file: " + PROPS_FILENAME + ". The file does not exist or for some other reason cannot be opened for reading", e);
    }
    catch (IOException e)
    {
//      if (logger.isErrorEnabled())
//        logger.error("Static initializer - an error occurred when reading from the file input stream", e);
    }
    catch (IllegalArgumentException e) {
//      if (logger.isErrorEnabled())
//        logger.error("Static initializer - file input stream contains a malformed Unicode escape sequence", e);
    }
    finally {
      try {
        if (fileInputStream != null)
          fileInputStream.close();
      } catch (IOException e) {
//        if (logger.isErrorEnabled())
//O error occurs.", e);
      }
    }
  }
}
