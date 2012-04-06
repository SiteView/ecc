package com.dragonflow.siteview.infra.vmware;

import com.vmware.vim.MethodFault;
import java.net.UnknownHostException;
import java.util.HashMap;
import javax.net.ssl.SSLHandshakeException;

public class ExceptionTranslator
{

    public ExceptionTranslator()
    {
    }

    public static String translate(Exception ex)
    {
        try
        {
            Throwable cause = ex.getCause();
            if(cause == null)
                cause = ex;
            String translation;
            if(cause instanceof MethodFault)
            {
                translation = ((MethodFault)cause).getFaultString();
            } 
            else
            {
                String exName = cause.getClass().getName();
                translation = (String)translations.get(exName);
                if(translation == null)
                    translation = "unknown";
                translation = (new StringBuilder()).append(translation).append(" - ").append(getDippestCause(cause).getMessage()).toString();
            }
            return translation;
        }
        catch(Exception e)
        {
            return "unknown";
        }
    }

    private static Throwable getDippestCause(Throwable thr)
    {
        Throwable cause = thr.getCause();
        if(cause == null)
            return thr;
        else
            return getDippestCause(cause);
    }

    private static final HashMap translations;

    static 
    {
        translations = new HashMap();
        translations.put("java.net.UnknownHostException", "unknown host");
        translations.put("javax.net.ssl.SSLHandshakeException", "SSL");
    }
}
