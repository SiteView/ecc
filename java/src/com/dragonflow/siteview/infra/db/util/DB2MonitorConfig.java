package com.dragonflow.siteview.infra.db.util;

import java.io.*;
import java.util.*;

import javax.xml.parsers.*;
import javax.xml.transform.TransformerException;
import org.apache.xpath.XPathAPI;
import org.w3c.dom.*;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

//            DB2Query

public class DB2MonitorConfig
{

    public DB2MonitorConfig()
    {
    }

    public static Set getQueries(String dataBase, String partiion, String fileName)
        throws Exception
    {
        Set queries = new TreeSet();
        Document doc = parseConfigFile(fileName);
        Map templateName2SQL = new HashMap();
        String templateXPATH = "/queries/sql_template";
        NodeList templates = getNodes(templateXPATH, doc);
        for(int i = 0; i < templates.getLength(); i++)
        {
            Node template = templates.item(i);
            String templateName = getAttribute(template, "name");
            NodeList sqlNodes = getNodes((new StringBuilder()).append(templateXPATH).append("[@name=\"").append(templateName).append("\"]").append("/").append("sql").toString(), doc);
            String sql = sqlNodes.item(0).getFirstChild().getNodeValue();
            templateName2SQL.put(templateName, sql);
        }

        String sqlXPATH = "/queries/query";
        NodeList queriesXML = getNodes(sqlXPATH, doc);
        for(int i = 0; i < queriesXML.getLength(); i++)
        {
            Node query = queriesXML.item(i);
            String queryName = getAttribute(query, "name");
            NodeList sqlNodes = getNodes((new StringBuilder()).append(sqlXPATH).append("[@name=\"").append(queryName).append("\"]").toString(), doc);
            for(int j = 0; j < sqlNodes.getLength(); j++)
            {
                Node sqlNode = sqlNodes.item(j);
                String sql = findElementValue(sqlNode, "sql_template");
                if(sql != null)
                    sql = (String)templateName2SQL.get(sql);
                else
                    sql = findElementValue(sqlNode, "sql");
                String nodeName = findElementValue(sqlNode, "node_name");
                String snapshotName = findElementValue(sqlNode, "snapshot_name");
                if(snapshotName != null)
                    sql = sql.replaceAll("SS_SNAPSHOT_NAME_PLACEHOLDER", snapshotName);
                sql = sql.replaceAll("DATABSE_NAME_PLACEHOLDER", dataBase);
                sql = sql.replaceAll("PARTITION_PLACEHOLDER", partiion);
                String tableIndex = findElementValue(sqlNode, "table_index");
                String description = findElementValue(sqlNode, "description");
                queries.add(new DB2Query(nodeName, sql, tableIndex, description));
            }

        }

        return queries;
    }

    private static String findElementValue(Node sqlNode, String nodeName)
    {
        String name = sqlNode.getNodeName();
        if(name.equals(nodeName) && sqlNode.getNodeType() == 3)
            return sqlNode.getNodeValue();
        if(sqlNode.getNodeType() == 3 && sqlNode.getParentNode().getNodeName().equals(nodeName))
            return sqlNode.getNodeValue();
        if(sqlNode.hasChildNodes())
        {
            NodeList children = sqlNode.getChildNodes();
            for(int j = 0; j < children.getLength(); j++)
            {
                Node child = children.item(j);
                String value = findElementValue(child, nodeName);
                if(value != null)
                    return value;
            }

        }
        return null;
    }

    private static Document parseConfigFile(String fname)
        throws FileNotFoundException, SAXException, IOException, ParserConfigurationException
    {
        DocumentBuilder db;
        FileReader fileReader;
        File f = new File(fname);
        if(!f.exists())
            throw new FileNotFoundException(fname);
        db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        fileReader = new FileReader(f);
        Document document = db.parse(new InputSource(fileReader));
        try
        {
            if(fileReader != null)
                fileReader.close();
        }
        catch(Exception e)
        {
        }
        return document;
    }

    private static NodeList getNodes(String xpath, Document doc)
        throws TransformerException
    {
        NodeList nodelist = XPathAPI.selectNodeList(doc, xpath);
        return nodelist;
    }

    private static Node getSingleNode(String xpath, Document doc)
        throws TransformerException
    {
        NodeList nodes = getNodes(xpath, doc);
        if(nodes.getLength() > 0)
            return nodes.item(0);
        else
            return null;
    }

    private static List getAttributes(NodeList nodes, String attribute)
    {
        List l = new ArrayList(nodes.getLength());
        for(int i = 0; i < nodes.getLength(); i++)
        {
            Node n = nodes.item(i);
            l.add(getAttribute(n, attribute));
        }

        return l;
    }

    private static String getAttribute(Node n, String att)
    {
        NamedNodeMap attributes = n.getAttributes();
        Node namedItem = attributes.getNamedItem(att);
        return namedItem.getNodeValue();
    }

    public static void main(String args[])
        throws Exception
    {
        DB2MonitorConfig config = new DB2MonitorConfig();
        Set m = getQueries("cy", "-1", "E:\\Server\\templates.applications\\DB2_Queries.xml");
        for(Iterator iterator = m.iterator(); iterator.hasNext();)
        {
            DB2Query db2Query = (DB2Query)iterator.next();
            System.out.println(db2Query.sql);
        }    
    }

    private static final String SS_SNAPSHOT_NAME_PLACEHOLDER = "SS_SNAPSHOT_NAME_PLACEHOLDER";
    private static final String DATABSE_NAME_PLACEHOLDER = "DATABSE_NAME_PLACEHOLDER";
    private static final String PARTITION_PLACEHOLDER = "PARTITION_PLACEHOLDER";
    private static final String SLASH = "/";
    private static final String QUERIES_ROOT = "queries";
    private static final String SQL_TEMPLATE = "sql_template";
    private static final String SQL = "sql";
    private static final String QUERIEY = "query";
    private static final String NODE_NAME = "node_name";
    private static final String SNAPSHOT_NAME = "snapshot_name";
    private static final String TABLE_INDEX = "table_index";
    private static final String DESCRIPTION = "description";

}
