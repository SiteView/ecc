package com.dragonflow.siteview.infra.db.util;

import java.io.*;
import java.util.*;

public class ArgsPackagerUtil
{

    public ArgsPackagerUtil()
    {
    }

    public static String packageArgs(List arrArgs)
    {
        return packageArgs(arrArgs, 0, arrArgs.size() - 1);
    }

    public static String packageArgs(List arrArgs, int start, int end)
    {
        StringBuffer buffArgs = new StringBuffer();
        for(int arg = start; arg <= end; arg++)
        {
            Object objArg = arrArgs.get(arg);
            String nextArg;
            if(objArg instanceof String)
            {
                nextArg = (String)objArg;
            } else
            {
                List arrRecursiveArgs = (List)arrArgs.get(arg);
                nextArg = packageArgs(arrRecursiveArgs, 0, arrRecursiveArgs.size() - 1);
            }
            buffArgs.append(packageArg(nextArg));
        }

        return buffArgs.toString();
    }

    public static String packageArg(String arg)
    {
        String argLength = Integer.toString(arg.length());
        return (new StringBuilder()).append(argLength).append(" ").append(arg).toString();
    }

    public static String packageArgs(String arrArgs[], int start, int end)
    {
        StringBuffer buffArgs = new StringBuffer();
        for(int arg = start; arg <= end; arg++)
        {
            String nextArg = arrArgs[arg];
            buffArgs.append(packageArg(nextArg));
        }

        return buffArgs.toString();
    }

    public static int getNextArg(String strArgs, int start, StringBuffer arg)
    {
        int end = strArgs.indexOf(' ', start);
        if(end < 0)
        {
            return -1;
        } else
        {
            int argSize = Integer.parseInt(strArgs.substring(start, end));
            arg.append(strArgs.substring(end + 1, end + 1 + argSize));
            return end + 1 + argSize;
        }
    }

    public static List unpackageArgs(String strArgs)
    {
        List arrArgs = Collections.synchronizedList(new ArrayList());
        int index = 0;
        do
        {
            StringBuffer arg = new StringBuffer();
            index = getNextArg(strArgs, index, arg);
            if(index < 0)
                return arrArgs;
            arrArgs.add(arg.toString());
        } while(true);
    }

    public static String[] unpackageArgsToStrArray(String strArgs)
    {
        int argsNum = getArgsNum(strArgs);
        if(argsNum <= 0)
            return null;
        String arrArgs[] = new String[argsNum];
        int argStart = 0;
        for(int i = 0; i < argsNum; i++)
        {
            StringBuffer arg = new StringBuffer();
            argStart = getNextArg(strArgs, argStart, arg);
            arrArgs[i] = arg.toString();
        }

        return arrArgs;
    }

    public static String unpackageStreamArgsToStrings(BufferedReader bis, StringBuffer numberString)
    {
        char forReader[];
        forReader = new char[1];
        if(bis == null || numberString == null)
            return null;
        numberString.delete(0, numberString.length());
        try
        {
        	if(bis.ready())
        	{
        		int index = 0;
        		do
        		{
        			if(index >= 1)
        				break;
        			bis.read(forReader);
        			numberString.append(forReader[0]);
        			if(forReader[0] == ' ')
        				index++;
        		} while(true);
        		int size = Integer.parseInt((new String(numberString)).trim());
        		char dataChars[] = new char[size];
        		bis.read(dataChars);
        		return new String(dataChars);
        	}
        }
        catch(IOException e)
        {
        }
        return null;
    }

    public static int getArgsNum(String strArgs)
    {
        int argsNum = 0;
        int start = 0;
        do
        {
            int end = strArgs.indexOf(' ', start);
            if(end < 0)
                return argsNum;
            argsNum++;
            int argSize = Integer.parseInt(strArgs.substring(start, end));
            start = end + 1 + argSize;
        } while(true);
    }

    public static String encodeArgs(String strArgs)
    {
        StringBuffer buff = new StringBuffer();
        for(int chr = 0; chr < strArgs.length(); chr++)
        {
            if(chr != 0)
                buff.append('.');
            buff.append(strArgs.charAt(chr));
        }

        return buff.toString();
    }

    public static String decodeArgs(String encodedArgs)
    {
        int start = 0;
        StringBuffer decodedBuff = new StringBuffer();
        int end;
        for(int size = encodedArgs.length(); start < size; start = end + 1)
        {
            end = encodedArgs.indexOf('.', start);
            if(end < 0)
                end = size;
            String chr = encodedArgs.substring(start, end);
            decodedBuff.append((char)Integer.parseInt(chr));
        }

        return decodedBuff.toString();
    }

    public static void main(String argv[])
    {
        String str1_1 = "str1_1";
        String str1_2 = "str1_2";
        String strEmpty = "";
        List arr1 = new ArrayList();
        arr1.add(str1_1);
        arr1.add(str1_2);
        arr1.add(strEmpty);
        String strPackaged = packageArgs(arr1, 0, arr1.size() - 1);
        String strExpected = "6 str1_16 str1_20 ";
        if(strPackaged.compareTo(strExpected) != 0)
            System.out.println("Error");
        else
            System.out.println("Success");
        System.out.println((new StringBuilder()).append("input   : ").append(strExpected).toString());
        System.out.println((new StringBuilder()).append("output  : ").append(strPackaged).toString());
        List arr2 = new ArrayList();
        String str2 = "str2";
        arr2.add(arr1);
        arr2.add(str2);
        strPackaged = packageArgs(arr2, 0, arr2.size() - 1);
        strExpected = "18 6 str1_16 str1_20 4 str2";
        if(strPackaged.compareTo(strExpected) != 0)
            System.out.println("Error");
        else
            System.out.println("Success");
        System.out.println((new StringBuilder()).append("input   : ").append(strExpected).toString());
        System.out.println((new StringBuilder()).append("output  : ").append(strPackaged).toString());
        String strEncoded = encodeArgs(strPackaged);
        String strDecoded = decodeArgs(strEncoded);
        if(strDecoded.compareTo(strPackaged) != 0)
            System.out.println("Encode Error");
        else
            System.out.println("Encode Success");
        System.out.println((new StringBuilder()).append("input   : ").append(strPackaged).toString());
        System.out.println((new StringBuilder()).append("output  : ").append(strDecoded).toString());
    }

    public static Properties unpackageToProperties(String args)
    {
        String propsArray[] = unpackageArgsToStrArray(args);
        Properties props = new Properties();
        String arr$[] = propsArray;
        int len$ = arr$.length;
        for(int i$ = 0; i$ < len$; i$++)
        {
            String aPropsArray = arr$[i$];
            String propArray[] = unpackageArgsToStrArray(aPropsArray);
            if(propArray.length != 2)
                return null;
            props.setProperty(propArray[0], propArray[1]);
        }

        return props;
    }

    public static String packageProps(Properties props)
    {
        String packagedPropsArray[] = new String[props.size()];
        Set propsSet = props.entrySet();
        Iterator it = propsSet.iterator();
        for(int i = 0; i < props.size(); i++)
        {
            java.util.Map.Entry prop = (java.util.Map.Entry)it.next();
            String propArray[] = new String[2];
            propArray[0] = (String)prop.getKey();
            propArray[1] = (String)prop.getValue();
            packagedPropsArray[i] = packageArgs(propArray, 0, 1);
        }

        return packageArgs(packagedPropsArray, 0, packagedPropsArray.length - 1);
    }
}