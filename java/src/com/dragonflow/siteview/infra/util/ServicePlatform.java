package com.dragonflow.siteview.infra.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class ServicePlatform {

	public static String getRoot()
	{
        String path = System.getProperty("PATH_TRANSLATED", System.getProperty("user.dir"));
        int s = path.toLowerCase().lastIndexOf("\\java");
        if(s != -1)
            path =  path.substring(0, s);
//        if(s != -1)
//            path = path.substring(0, s + 6);
        
        path = path.replaceAll("\\\\", "/");
        return path;
	}

	public static String replaceUnicodeChar(String inStr)
	{
		byte[] outStr =  new byte[inStr.getBytes().length];
		byte[] inteByte = inStr.getBytes();
		
		int j = 0;
        for(int i=0; i<inStr.getBytes().length; i++)   
        {   
        	if(inteByte[i]>=0 && inteByte[i]<=255)
        	{
        		outStr[j] += inteByte[i]; 
        		j++;
        	}
        }
        return new String(outStr).replaceAll("\0", "");
	}
	
    public static boolean containsValidXMLCharsOnly(String in)
    {
        int length;
        if(in == null || (length = in.length()) == 0)
            return true;
        for(int i = 0; i < length; i++)
        {
            char currentChar = in.charAt(i);
            if(currentChar != '\t' && currentChar != '\n' && currentChar != '\r' && (currentChar < ' ' || currentChar > '\uD7FF') && (currentChar < '\uE000' || currentChar > '\uFFFD') && (currentChar < 65536 || currentChar > 1114111))
            {
                return false;
            }
        }

        return true;
    }	
  
    public static  boolean isInteger(String in)
    {
		try
		{
			Integer.parseInt(in);
		}
		catch(NumberFormatException e)
		{
			return false;
		}
		return true;
    }
    
    public static boolean isFloat(String in)
    {
		try
		{
			Float.parseFloat(in);
		}
		catch(NumberFormatException e)
		{
			return false;
		}
		return true;    	
    }
    
    public static Map<String, Object> adjustValueResult(Map<String, Object> map)
    {
		Iterator<Map.Entry<String, Object>> iter = map.entrySet().iterator();
		while(iter.hasNext())
		{
		    Map.Entry<String, Object> entry = (Map.Entry<String, Object>)iter.next();     
			String key = entry.getKey();
			String value = (String)entry.getValue();
			if(isInteger(value))
				map.put(key, Integer.parseInt(value));
			else if(isFloat(value))
				map.put(key, Float.parseFloat(value));				
		}    	
		return map;
    }
	public static void main(String[] args) 
	{
		String str= "122e";
		if(isInteger(str))
			System.out.println("ok");
	}
}
