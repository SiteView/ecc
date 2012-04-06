package com.dragonflow.siteview.infra.ipmi.util;

import com.dragonflow.siteview.infra.ipmi.IPMIMessage;
import java.util.Locale;


public class TranslatorFactory
{

    public TranslatorFactory()
    {
    }

  
    public static IPMITranslator getTranslator(Locale locale)
    {
        if(locale == null)
        {
            return new en_Translator();
        }
        String lang = locale.getLanguage();
        IPMITranslator ipmiTranslator;
        try
        {
            String className = "com.dragonflow.siteview.infra.ipmi.util.IPMITranslator_1";//(new StringBuilder()).append(com.dragonflow.siteview.infra.ipmi.util.TranslatorFactory.getName()).append(".").append(lang).append("_Translator").toString();
            Class ipmiTranslatorClass = Class.forName(className);
            ipmiTranslator = (IPMITranslator)ipmiTranslatorClass.newInstance();
        }
        catch(Exception e)
        {
            return new en_Translator();
        }
        return ipmiTranslator;
    }

    public static IPMITranslator getDefaultTranslator()
    {
        return new en_Translator();
    }

}
