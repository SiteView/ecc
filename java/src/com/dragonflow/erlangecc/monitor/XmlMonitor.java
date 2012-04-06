package com.dragonflow.erlangecc.monitor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import jgl.Array;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.dragonflow.siteview.ErlangUtils;
import com.ericsson.otp.erlang.OtpErlangList;



import COM.datachannel.xml.om.Document;


public class XmlMonitor extends BaseMonitor{

	private Map<String,Object> map;
    String doc = "";  //keep transform() return's value
	StringBuffer lastError = new StringBuffer("");
	
	public static final String ERROR = "error";
	@Override
	public int handleMessage()
	{
		String error = null;
		Map<String,Object> value =new LinkedHashMap<String,Object>();
		Map<String, Object> resp = null;
		String action = this.getMessage().getAction();
	    map = this.getMessage().getParams();
	    Object xsl = map.get("xsl");
	    Object url = map.get("xml");	    
	    String xsl_str = xsl.toString();
	    String xml_str = url.toString();
	    doc = transform(xsl_str,xml_str);
	    System.out.print(doc);
	    resp = new HashMap<String, Object>();
	    resp.put("ok", doc);
	    this.sendResponse("ok", resp);

	     	        
		return 0;
	}

	Map<String, Object> update_util(String s,Map<String, Object> counters)
	{
		Map<String, Object> res_map = new HashMap<String, Object>();
		Document document = new Document();
		document.loadXML(s);
		Element root = (Element) document.getFirstChild();
		NodeList nodelist = root.getElementsByTagName("counter");
        int nodeListLength = nodelist.getLength();
        int k = 0;
        for (int i = 0; i < nodeListLength; i++) {
            Element element = (Element) nodelist.item(i);
            Array displayNames = getNodeDisplayNames(element);            
            String browseName = this.setBrowseName(displayNames);
            Object Obj =  counters.get("\"" + browseName + "\"");
            if (Obj == null) {
            	continue;
            }             
            String value = element.getAttribute("value");
            if (value != null && value.equals("")) {
                value = "0.0";
            }
           // this.setProperty(browseNameProperty, value);
            if (counters.size() == 0) {
                break;
            }
            res_map.put(browseName.toString(), value);
        }	
		return res_map;		
	}
	
	public String transform(String xsl,String url)
	{
		String out_xml = null;
		if (xsl.length() != 0 )
		{
		try {		
		    TransformerFactory tFactory = TransformerFactory.newInstance();
		    InputStream  input  =  new  ByteArrayInputStream(xsl.getBytes());
		    Transformer transformer = tFactory.newTransformer(new StreamSource(input));			    
			ByteArrayOutputStream stream = new ByteArrayOutputStream();
			StreamResult streamres = new StreamResult(stream);	
			transformer.transform(new StreamSource(url), streamres);
			out_xml = streamres.getOutputStream().toString();				
		}catch (TransformerException e){					
			return e.getMessage();		
		}
		}else{
			TransformerFactory tFactory = TransformerFactory.newInstance();
			Transformer transformer = null;
			try {
				transformer = tFactory.newTransformer();
			} catch (TransformerConfigurationException e) {
				// TODO Auto-generated catch block
				return e.getMessage();
			}
			ByteArrayOutputStream stream = new ByteArrayOutputStream();
			StreamResult streamres = new StreamResult(stream);	
			try {
				transformer.transform(new StreamSource(url), streamres);
			} catch (TransformerException e) {
				// TODO Auto-generated catch block
				return e.getMessage();
			}			
			out_xml = streamres.getOutputStream().toString();
		}
		return out_xml;
	}
	
	Map<String, Object> getBrowseData(String s) 
	{
		Map<String, Object> res_map = new HashMap<String, Object>();
		Document document = new Document();
		document.loadXML(s);
		Element root = (Element) document.getFirstChild();
		NodeList nodelist = root.getElementsByTagName("counter");
		int nodeListLength = nodelist.getLength();
	    for (int i = 0; i < nodeListLength; i++) {
	    	Element element = (Element) nodelist.item(i);
	    	Array displayNames = getNodeDisplayNames(element); 
	    	String browseName = this.setBrowseName(displayNames);
	    	res_map.put(browseName, (Object)browseName);
	    }		
		return res_map;		
	}

	   static Element getChildElement(Element element, String s, String s1,
	            boolean flag) {
	        Object obj = null;
	        NodeList nodelist = element.getChildNodes();
	        for (int i = 0; i < nodelist.getLength(); i++) {
	            String s2 = nodelist.item(i).getNodeName();
	            if (!s2.equals(s)) {
	                continue;
	            }
	            Element element1 = (Element) nodelist.item(i);
	            String s3 = element1.getAttribute("name");
	            if (s3.equals(s1)) {
	                return element1;
	            }
	        }

	        if (flag) {
	            Element element2 = element.getOwnerDocument().createElement(s);
	            element2.setAttribute("name", s1);
	            element.appendChild(element2);
	            return element2;
	        } else {
	            return null;
	        }
	    }	
	
	    public static jgl.Array getNodeDisplayNames(org.w3c.dom.Node node)
	    {
	        jgl.Array array = new Array();
	        java.lang.String s = ((org.w3c.dom.Element)node).getAttribute("name");
	        if(s == null)
	        {
	            return array;
	        }
	        array.add(s);
	        org.w3c.dom.Node node1 = node.getParentNode();
	        do
	        {
	            if(node1 == null)
	            {
	                break;
	            }
	            java.lang.String s1 = ((org.w3c.dom.Element)node1).getAttribute("name");
	            if(s1 == null || s1.length() <= 0)
	            {
	                break;
	            }
	            array.add(s1);
	            node1 = node1.getParentNode();
	        } while(true);
	        return array;
	    }

	    public String setBrowseName(Array array) {
	        String s = "";
	        for (int i = array.size(); i > 0; i--) {
	            if (i < array.size()) {
	                s = s + '/';
	            }
	            s = s + escapeString((String) array.at(i - 1));
	        }

	        return s;
	    }

	    private String escapeString(String s) {
	        StringBuffer stringbuffer = new StringBuffer();
	        for (int i = 0; i < s.length(); i++) {
	            char c = s.charAt(i);
	            if (c == '/' || c == '\\') {
	                stringbuffer.append('\\');
	            }
	            stringbuffer.append(c);
	        }

	        return stringbuffer.toString();
	    }
	
	    public static Document stringToDocument(String str)
	    {
	        Document doc = null;
	        try
	        {
	            doc = (Document) DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(str)));
	        }
	        catch(Exception e)
	        {
	            e.printStackTrace();
	        }
	        return doc;
	    }
	    
	public static void main(String[] args) {
		System.out.println("runing xml monitor!");		
	}
	

}
