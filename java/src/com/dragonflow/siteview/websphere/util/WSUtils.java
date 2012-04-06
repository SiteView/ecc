package com.dragonflow.siteview.websphere.util;

public class WSUtils
{
  static final int kWIN = 1;
  static final int kSUN = 2;
  static final int kSGI = 3;
  static final int kMac = 4;
  static final int kHP = 5;
  static final int kLinux = 6;
  static final int kMacOSX = 7;
  static final int kOtherUnix = 8;
  public static final String ADDITIONAL_METRICS = "Additional Metrics";
  public static final String ADDITIONAL_TYPE = "additional";


  public static int getOs()
  {
    int os = -1;
    String osName = System.getProperty("os.name").toUpperCase();

    if (osName.startsWith("WINDOWS"))
      os = 1;
    else if (osName.equals("IRIX"))
      os = 3;
    else if ((osName.equals("SOLARIS")) || (osName.equals("SUNOS")))
      os = 2;
    else if (osName.equals("HP-UX"))
      os = 5;
    else if (osName.equals("LINUX"))
      os = 6;
    else if ((osName.equals("MAC OS")) || (osName.equals("MACOS")))
      os = 4;
    else if (osName.equals("MacOSX")) {
      os = 7;
    }

    return os;
  }

  static String indent(int indent) {
    char[] pad = new char[indent];
    for (int i = 0; i < indent; ++i) {
      pad[i] = ' ';
    }

    return new String(pad);
  }
}
