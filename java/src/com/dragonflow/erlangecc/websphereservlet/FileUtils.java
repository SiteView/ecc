package com.dragonflow.erlangecc.websphereservlet;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.util.Date;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import jgl.Array;
import jgl.HashMap;
import jgl.Pair;

// Referenced classes of package COM.dragonflow.Utils:
// Braf, I18N, TextUtils

public class FileUtils
{

    public static final int DEFAULT_BUFFER_SIZE = 32768;
    public static final java.lang.String INI_LINE_SEPARATOR = "\r\n";
    public static final int INI_LINE_SEPARATOR_LEN = "\r\n".length();
    public static final java.lang.String INI_EQUAL = "=";
    public static final java.lang.String INI_SECTION_PREFIX = "[";
    static jgl.HashMap fileLockMap = new HashMap();
    public static boolean singleMatchOnly = false;
    public static final int SEARCHING = 0;
    public static final int RECORDING = 1;
    public static final int DONE = 2;
    public static final int START_TAG = 0;
    public static final int END_TAG = 1;

    public FileUtils()
    {
    }


    
    public static StringBuffer readFile(String path)
    throws IOException
  {
    FileInputStream fis = null;
    StringBuffer b = new StringBuffer();
    ByteArrayOutputStream baos = new ByteArrayOutputStream();

    fis = new FileInputStream(path);
     int byteCount = 0;
    byte[] buffer = new byte[32768];
    try {
      while ((byteCount = fis.read(buffer)) != -1) {
         baos.write(buffer, 0, byteCount);
       }
       b.append(baos.toString());
     } finally {
      try {
        fis.close();
       }
       catch (IOException ioe) {
//         logger.error("Exception in FileUtils.readFile()" + ioe);
//         logger.debug("", ioe);
    	   ioe.printStackTrace();
       }
       }
     return b;
     }
 
}
