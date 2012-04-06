package com.dragonflow.siteview.websphere.util;

import java.io.BufferedReader;
import java.io.IOException;

public class ReaderThread extends Thread
{
//  private static Log logger = LogFactory.getEasyLog(ReaderThread.class);
  BufferedReader input;
  String logPrefix = "";
  StringBuffer buffer = null;

  public ReaderThread(BufferedReader input, String logPrefix)
  {
    this.input = input;
    if (logPrefix != null)
      this.logPrefix = logPrefix;
  }

  public ReaderThread(BufferedReader input, StringBuffer buf)
  {
    this.input = input;
    this.buffer = buf;
  }

  public void run()
  {
    try
    {
      String line;
      if ((line = this.input.readLine()) != null) {
        if (this.buffer != null) {
          this.buffer.append(line).append("\r\n");
        }
//        logger.error(this.logPrefix + line);
      }
    }
    catch (IOException e) {
//      logger.error("ReaderThread caught IOException: " + e);
    }
  }
}