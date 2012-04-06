package com.dragonflow.erlangecc.websphereservlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.StringTokenizer;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import jgl.Array;
import COM.datachannel.xml.om.Document;
import COM.datachannel.xml.om.IXMLDOMNode;

// Referenced classes of package COM.dragonflow.Utils:
// SoapRpc, TextUtils, FileUtils

public class XSLUtils {
	public abstract interface ObjectCache<T> {
		public abstract T getObject();

		public abstract void returnObject(T paramT);

		public abstract int cacheSize();
	}

	public static final boolean SHOW_FOOTER = true;
	public static final boolean NO_FOOTER = false;
	private static ObjectCache<ArrayList<String>> arrayListCacheForSplit;

	public XSLUtils() {
	}

	private static String[] splitImplementation(StringTokenizer tokenizer) {
		ArrayList strs = (ArrayList) arrayListCacheForSplit.getObject();

		while (tokenizer.hasMoreTokens()) {
			strs.add(tokenizer.nextToken());
		}

		if (strs.size() == 0) {
			String[] arrayOfString1 = new String[0];
		}

		String[] rtn = (String[]) strs.toArray(new String[0]);
		arrayListCacheForSplit.returnObject(strs);
		return rtn;
	}

	public static String[] split(String s, String delimiters) {
		return splitImplementation(new StringTokenizer(s, delimiters));
	}

	public static void appendStringRightJustify(StringBuffer b, String s,
			int slen) {
		appendString(b, s, slen, false, true);
	}

	public static void appendString(StringBuffer b, String s, int slen,
			boolean leftJustify, boolean rightJustify) {
		if (slen < 0) {
			b.append(s);
		} else if (slen < s.length()) {
			b.append(s.substring(0, slen));
		} else {
			int i = slen - s.length();
			while ((rightJustify) && (i-- > 0)) {
				b.append(' ');
			}

			b.append(s);
			while ((leftJustify) && (i-- > 0))
				b.append(' ');
		}
	}

	private static String addLineNumbers(String s) {
		String[] lines = split(s, "\n");
		StringBuffer b = new StringBuffer();
		for (int i = 0; i < lines.length; ++i) {
			appendStringRightJustify(b, "" + (i + 1), 5);
			b.append("  ");
			b.append(lines[i]);
			b.append("\n");
		}
		return b.toString();
	}

	private static String convertDataChannel(String xml, String xsl,
			StringBuffer errorTitleBuffer, StringBuffer errorBuffer,
			StringBuffer errorDetailBuffer) {
		Document xmlDoc = new Document();
		try {
			xmlDoc = new Document();
			xmlDoc.loadXML(xml);
		} catch (Exception e) {
			errorTitleBuffer.append("Error Reading XML");
			errorBuffer.append(e.getMessage());
			errorDetailBuffer.append(addLineNumbers(xml));
		}

		Document xslDoc = new Document();
		try {
			xslDoc = new Document();
			xslDoc.loadXML(xsl);
		} catch (Exception e) {
			errorTitleBuffer.append("Error Reading XSL Template");
			errorBuffer.append(e.getMessage());
		    errorDetailBuffer.append(addLineNumbers(xml));
		}

		String result = "";

		if (errorBuffer.length() == 0) {
			try {
				result = xmlDoc
						.transformNode((IXMLDOMNode) (IXMLDOMNode) xslDoc
								.getDocumentElement());
			} catch (Exception e) {
				errorTitleBuffer.append("Error Reading transforming XML");
				errorBuffer.append(e.getMessage());
				if (errorBuffer.length() == 0) {
					errorBuffer.append("" + e);
				}
			}
		}
		return result;
	}

	private static String convertSun(String xml, String xsl,
			StringBuffer errorTitleBuffer, StringBuffer errorBuffer,
			StringBuffer errorDetailBuffer) {
		 Transformer transformer = null;
		      try {
		     TransformerFactory tFactory = TransformerFactory.newInstance();
		       transformer = tFactory.newTransformer(new StreamSource(new StringReader(xsl)));
		      }
		    catch (Exception e) {
		       errorTitleBuffer.append("Error Reading XSL Template");
		        errorBuffer.append(e.getMessage());
		       errorDetailBuffer.append(addLineNumbers(xml));
		      }
		 
		     String result = "";
		 
		     
		    if ((errorBuffer.length() == 0) && (transformer != null)) {
		       try {
		         StringWriter sw = new StringWriter();
		        

		         transformer.transform(new StreamSource(new StringReader(xml)), new StreamResult(sw));
		         result = sw.toString();
		        }
		       catch (Exception e) {
		         errorTitleBuffer.append("Error Reading transforming XML");
		          errorBuffer.append(e.getMessage());
		         if (errorBuffer.length() == 0) {
		            errorBuffer.append("" + e);
		          }
		       }
		      }
		     return result;
	}

	 public static void convert(String xml, String xsl, PrintWriter outputStream)
	  {
	     StringBuffer errorTitleBuffer = new StringBuffer();
	     StringBuffer errorBuffer = new StringBuffer();
	     StringBuffer errorDetailBuffer = new StringBuffer();
	     String result;
	     if (xsl.indexOf("xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\"") != -1)
	       result = convertSun(xml, xsl, errorTitleBuffer, errorBuffer, errorDetailBuffer);
	     else {
	       result = convertDataChannel(xml, xsl, errorTitleBuffer, errorBuffer, errorDetailBuffer);
	     }
	 
	     String errorTitle = errorTitleBuffer.toString();
	     String error = errorBuffer.toString();
	     String errorDetail = errorDetailBuffer.toString();
	 
	     if (error.length() > 0) {
	     
	     }
	 
	     String suffix = "";
	 
	   
	     outputStream.print(result);
	     outputStream.print(suffix);
	     outputStream.flush();
	   }

}
