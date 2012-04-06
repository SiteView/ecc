package com.dragonflow.siteview.websphere.util;

import java.io.Serializable;

public class WebSphereConnectionProperties extends ConnectionProperties
  implements Serializable
{
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
  private int version;
  private String trustStore;
  private String trustStorePassword;
  private String keyStore;
  private String keyStorePassword;

  public WebSphereConnectionProperties(String serverName, String username, String password, int port, String version, String trustStore, String trustStorePassword, String keyStore, String keyStorePassword, String uniqueID)
  {
    super(serverName, username, password, port, uniqueID);
    detectVersion(version);
    this.trustStore = trustStore;
    this.trustStorePassword = trustStorePassword;
    this.keyStore = keyStore;
    this.keyStorePassword = keyStorePassword;
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

  public String getAPI()
  {
    if (this.version == 1) {
      return "PMI";
    }
    return "JMX";
  }

  public int getVersion() {
    return this.version;
  }

  public String getTrustStore() {
    return this.trustStore;
  }

  public String getTrustStorePassword() {
    return this.trustStorePassword;
  }

  public String getKeyStore() {
    return this.keyStore;
  }

  public String getKeyStorePassword() {
    return this.keyStorePassword;
  }
}
