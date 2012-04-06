package com.dragonflow.siteview.websphere.util;

public final class ObjectChecks
{
  private static final String[] monitorsDisabledInTemplateMod = { "VuGenMonitor", "EBusinessTransactionMonitor", "NTDialupMonitor" };

  public static <T> T nonNull(T object, String name)
  {
    if (object != null) {
      return object;
    }
    throw new NullPointerException(name + " was expected to be non-null, but was null");
  }

  public static boolean nonEmpty(String str)
  {
    return ((str != null) && (str.trim().length() > 0));
  }

  public static String nonEmpty(String str, String name) {
    if (nonEmpty(str)) {
      return str;
    }
    throw new IllegalArgumentException(name + " was expected to be non-null and non-empty");
  }

  public static boolean isMonitorDisabledInTemplate(String monitorType)
  {
    for (String disabledType : monitorsDisabledInTemplateMod) {
      if (disabledType.equals(monitorType)) {
        return true;
      }
    }
    return false;
  }
}