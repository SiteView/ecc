package com.dragonflow.siteview.websphere.util;

import java.io.FileDescriptor;
import java.net.InetAddress;
import java.security.Permission;

public class NullSecurityManager extends SecurityManager
{
  public void checkPermission(Permission perm, Object context)
  {
  }

  public void checkRead(String file, Object context)
  {
  }

  public void checkConnect(String host, int port, Object context)
  {
  }

  public void checkPermission(Permission perm)
  {
  }

  public void checkMulticast(InetAddress maddr)
  {
  }

  public void checkAccess(ThreadGroup g)
  {
  }

  public void checkConnect(String host, int port)
  {
  }

  public void checkAccess(Thread t)
  {
  }

  public void checkAwtEventQueueAccess()
  {
  }

  public void checkCreateClassLoader()
  {
  }

  public void checkPrintJobAccess()
  {
  }

  public void checkPropertiesAccess()
  {
  }

  public void checkSetFactory()
  {
  }

  public void checkSystemClipboardAccess()
  {
  }

  public void checkExit(int status)
  {
  }

  public void checkListen(int port)
  {
  }

  public void checkRead(FileDescriptor fd)
  {
  }

  public void checkWrite(FileDescriptor fd)
  {
  }

  public void checkMemberAccess(Class clazz, int which)
  {
  }

  public boolean checkTopLevelWindow(Object window)
  {
    return true;
  }

  public void checkDelete(String file)
  {
  }

  public void checkExec(String cmd)
  {
  }

  public void checkLink(String lib)
  {
  }

  public void checkPackageAccess(String pkg)
  {
  }

  public void checkPackageDefinition(String pkg)
  {
  }

  public void checkPropertyAccess(String key)
  {
  }

  public void checkRead(String file)
  {
  }

  public void checkSecurityAccess(String target)
  {
  }

  public void checkWrite(String file)
  {
  }

  public void checkAccept(String host, int port)
  {
  }
}