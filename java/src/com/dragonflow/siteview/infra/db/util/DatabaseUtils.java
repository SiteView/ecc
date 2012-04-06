package com.dragonflow.siteview.infra.db.util;

import java.io.StringWriter;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;


public class DatabaseUtils
{

    public DatabaseUtils()
    {
    }

    public static String getTreeKey(String counterID)
    {
        int seperatorIndex = counterID.indexOf("|||");
        if(seperatorIndex >= 0)
            return counterID.substring(0, seperatorIndex);
        else
            return "MALFORMED_COUNTER_ID";
    }

    public static String getCounterKey(String counterID)
    {
        int index = counterID.indexOf("|||");
        if(index >= 0)
            return counterID.substring(index + "|||".length());
        else
            return "MALFORMED_COUNTER_ID";
    }

    public static HashMap<String, Object> buildXml(Map result, StringBuffer errorStr)
    {
    	HashMap<String, Object> newMap = new HashMap<String, Object>();
        try
        {
            Set treeKeys = getFirstLevelObjects(result);
            String treeKey;
            Map counters;
            for(Iterator iterator = treeKeys.iterator(); iterator.hasNext(); addCounters(counters, newMap, treeKey, treeKey))
            {
                treeKey = (String)iterator.next();
                counters = (Map)result.get(treeKey);
            	newMap.put(treeKey, treeKey);
            }

            return newMap;
        }
        catch(Exception e)
        {
            errorStr.append(e.getMessage());
            return null;
        }
    }

    private static void addCounters(Map counters, HashMap<String, Object> newMap, String nameId, String treeKey)
    {
        Set keys = counters.keySet();
        Iterator it = keys.iterator();
        do
        {
            if(!it.hasNext())
                break;
            String key = (String)it.next();
            Object description = counters.get(key);
            if(description instanceof String)
            {
                String id = (new StringBuilder()).append(treeKey).append("|||").append(key).toString();
                addCounter(newMap, key, id, nameId+"/"+(String)key);
            } else
            if(description instanceof Map)
            {
            	newMap.put((new StringBuilder()).append(nameId).append("/").append(key).toString(), (new StringBuilder()).append(nameId).append("/").append(key).toString());
                addCounters((Map)description, newMap, (new StringBuilder()).append(nameId).append("/").append(key).toString(), (new StringBuilder()).append(treeKey).append("|||").append(key).toString());
            }
        } while(true);
    }

    private static Element newObject(Document doc, Element parent, String objectName)
    {
        Element object = doc.createElement("object");
        parent.appendChild(object);
        object.setAttribute("name", objectName);
        return object;
    }

    public static Set getFirstLevelObjects(Map result)
    {
        Set treeKeys = result.keySet();
        treeKeys.remove("SITEVIEW_RUN_TIME_MILLIS_KEY");
        treeKeys.remove("SITEVIEW_DIVISOR_QUERY_RESULT");
        return treeKeys;
    }

    public static void addCounter( HashMap<String, Object> newMap, String name, String id, String description)
    {
        if(name.equals("SITEVIEW_RUN_TIME_MILLIS_KEY"))
        {
            return;
        } else
        {
            newMap.put(id, description);
            return;
        }
    }
/*
    public static String printDocumentAsString(Document doc)
    {
        OutputFormat of = new OutputFormat();
        of.setIndent(3);
        of.setOmitXMLDeclaration(true);
        of.setOmitDocumentType(true);
        XMLSerializer serializer = new XMLSerializer(of);
        StringWriter writer = new StringWriter();
        try
        {
            serializer.setOutputCharStream(writer);
            serializer.serialize(doc);
        }
        catch(Exception excp)
        {
        }
        return writer.toString();
    }
*/
    public static void populateMap(ResultSet results, Map tree, StringBuffer errorBuffer, String namedColumn)
        throws SQLException
    {
        String counterNames[];
        Map mapForNoResults;
        int numberedColumn;
        boolean hasResults;
        if(results == null || tree == null)
            return;
        
        try
        {
        	ResultSetMetaData metaData = results.getMetaData();
        	int columnCount = metaData.getColumnCount();
        	counterNames = new String[columnCount];
        	mapForNoResults = new TreeMap();
        	if(namedColumn != null && namedColumn.equals("SS_FIRST_COLUMN"))
        		numberedColumn = 1;
        	else
        		numberedColumn = -1;
        	for(int i = 0; i < counterNames.length; i++)
        	{
        		counterNames[i] = metaData.getColumnLabel(i + 1);
        		if(counterNames[i].equals(namedColumn))
        			numberedColumn = i + 1;
        		mapForNoResults.put(counterNames[i], "n/a");
        	}

        	hasResults = false;
        	while(true)
        	{
        		if(!results.next())
        			break; /* Loop/switch isn't completed */
        		hasResults = true;
        		Map map;
        		if(numberedColumn > 0)
        		{
        			String name = results.getString(numberedColumn);
        			map = new TreeMap();
        			tree.put(name, map);
        		} else
        		{
        			map = tree;
        		}
        		int columnIndex = 1;
        		while(columnIndex <= counterNames.length) 
        		{
        			if(columnIndex != numberedColumn)
        			{
        				String counterName = counterNames[columnIndex - 1];
        				map.put(counterName, results.getString(columnIndex));
        			}
        			columnIndex++;
        		}
        	}
        	if(!hasResults)
        		tree.putAll(mapForNoResults);                    	
        }
        catch(Exception e)
        {
        	errorBuffer.append(e.getMessage());
        }
        
        return;
    }

    public static void main(String args[])
    {
        Map top = new TreeMap();
        Map m = new TreeMap();
        Map m2 = new TreeMap();
        for(int i = 0; i < 35; i++)
        {
            m.put((new StringBuilder()).append("L1 ").append(i).toString(), (new StringBuilder()).append("").append(i).toString());
            m2.put((new StringBuilder()).append("L2 ").append(i).toString(), (new StringBuilder()).append("").append(i).toString());
        }

        top.put("Hello", m);
        m.put("Lower", m2);
//        System.out.println(buildXml(top, new StringBuffer()));
    }

    public static String getCounterValue(Map resultsMap, String counterID)
    {
        return getCounterValue(resultsMap, getCounterPath(counterID));
    }

    private static String[] getCounterPath(String counterID)
    {
        return counterID.split("\\|\\|\\|");
    }

    public static String getCounterValue(Map resultsMap, String counterPath[])
    {
        for(int i = 0; i < counterPath.length && resultsMap != null; i++)
        {
            String counterPart = counterPath[i];
            Object counterValue = resultsMap.get(counterPart);
            if(counterValue == null)
                break;
            if(counterValue instanceof String)
                return (String)counterValue;
            if(counterValue instanceof Map)
                resultsMap = (Map)counterValue;
        }

        return "n/a";
    }

    public static String caculateCounterValue(String thisRun, String lastRun, boolean cumulative, boolean shouldDivideCounters, double counterDivisor)
    {
        String value;
        try
        {
            double thisRunDouble = Double.parseDouble(thisRun);
            double lastRunDouble = Double.parseDouble(lastRun);
            double val = thisRunDouble;
            if(cumulative)
                val -= lastRunDouble;
            if(shouldDivideCounters)
                if(counterDivisor != 0.0D)
                    val /= counterDivisor;
                else
                    val = 0.0D;
            value = ((float)val) + "";
        }
        catch(NumberFormatException e)
        {
            value = "n/a";
        }
        return value;
    }

    public static String getCounterIDFromRawID(String rawCounterID)
    {
        String counterID;
//        String counterPath[] = ArgsPackagerUtil.unpackageArgsToStrArray(rawCounterID);
//        if(counterPath!=null)
//        	counterID = counterPath[0];
//        else
        	counterID = rawCounterID;
        return counterID;
    }

    public static final String COUNTER_SEPERATOR = "|||";
    public static final String COUNTER_SEPERATOR_REGEX = "\\|\\|\\|";
    public static final String COUNTER_ID_ERROR = "MALFORMED_COUNTER_ID";
    public static final String RUN_TIME_MILLIS = "SITEVIEW_RUN_TIME_MILLIS_KEY";
    public static final String DIVISOR_RESULT = "SITEVIEW_DIVISOR_QUERY_RESULT";
    public static final String SS_FIRST_COLUMN = "SS_FIRST_COLUMN";

}
