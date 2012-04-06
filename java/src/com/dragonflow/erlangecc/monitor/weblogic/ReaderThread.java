package com.dragonflow.erlangecc.monitor.weblogic;

import java.io.BufferedReader;
import java.io.IOException;

public class ReaderThread extends Thread
{

    public ReaderThread(BufferedReader input, String logPrefix)
    {
        this.logPrefix = "";
        buffer = null;
        this.input = input;
        if(logPrefix != null)
            this.logPrefix = logPrefix;
    }

    public ReaderThread(BufferedReader input, StringBuffer buf)
    {
        logPrefix = "";
        buffer = null;
        this.input = input;
        buffer = buf;
    }

    public void run()
    {
        String line;
        try
        {
            while((line = input.readLine()) != null) 
                if(buffer != null)
                    buffer.append(line).append("\r\n");
                else
                	System.err.println((new StringBuilder()).append(logPrefix).append(line).toString());
        }
        catch(IOException e)
        {
            System.err.println((new StringBuilder()).append("ReaderThread caught IOException: ").append(e).toString());
        }
    }

    BufferedReader input;
    String logPrefix;
    StringBuffer buffer;

}