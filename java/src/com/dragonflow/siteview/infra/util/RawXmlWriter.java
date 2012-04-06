package com.dragonflow.siteview.infra.util;

import java.util.List;

public class RawXmlWriter
{

 public RawXmlWriter(StringBuffer buf)
 {
     this.buf = buf;
 }

 public void writeSOAPHeader(List header)
 {
     if(header.size() <= 0)
     {
         buf.append("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
         buf.append("<SOAP-ENV:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ");
         buf.append("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" ");
         buf.append("xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\">\n");
     } else
     {
         for(int i = 0; i < header.size(); i++)
             buf.append((String)header.get(i));

     }
 }

 public void startElement(String elem)
 {
     buf.append("<");
     buf.append(elem);
     buf.append(">");
 }

 public void endElement(String elem)
 {
     buf.append("</");
     buf.append(elem);
     buf.append(">");
 }

 public void emptyElement(String elem)
 {
     buf.append("<");
     buf.append(elem);
     buf.append("/>");
 }

 public void startElementEnc(String elem)
 {
     buf.append(xmlEncode("<"));
     buf.append(xmlEncode(elem));
     buf.append(xmlEncode(">"));
 }

 public void endElementEnc(String elem)
 {
     buf.append(xmlEncode("</"));
     buf.append(xmlEncode(elem));
     buf.append(xmlEncode(">"));
 }

 public void emptyElementEnc(String elem)
 {
     buf.append(xmlEncode("<"));
     buf.append(xmlEncode(elem));
     buf.append(xmlEncode("/>"));
 }

 public void chardata(String text)
 {
     int l = text.length();
     for(int i = 0; i < l; i++)
     {
         char c = text.charAt(i);
         switch(c)
         {
         case 60: // '<'
             buf.append("&lt;");
             break;

         case 62: // '>'
             buf.append("&gt;");
             break;

         case 34: // '"'
             buf.append("&quot;");
             break;

         case 38: // '&'
             buf.append("&amp;");
             break;

         default:
             buf.append(c);
             break;
         }
     }

 }

 public String xmlEncode(String s)
 {
     String target = "&";
     String replace = "&amp;";
     String source = s;
     for(int index = -1; (index = source.indexOf(target, index + 1)) != -1;)
         source = (new StringBuilder()).append(source.substring(0, index)).append(replace).append(source.substring(index + target.length())).toString();

     source = replaceString(source, "\"", "&quot;");
     source = replaceString(source, ">", "&gt;");
     source = replaceString(source, "<", "&lt;");
     return source;
 }

 public void write(char text[])
 {
     buf.append(text);
 }

 public void write(String text)
 {
     buf.append(text);
 }

 public String toString()
 {
     return buf.toString();
 }

 public static String enXMLElement(String element)
 {
     return element;
 }

 public static String enCodeElement(String s)
 {
     String target = "&";
     String replace = "&amp;";
     String source = s;
     for(int index = -1; (index = source.indexOf(target, index + 1)) != -1;)
         source = (new StringBuilder()).append(source.substring(0, index)).append(replace).append(source.substring(index + target.length())).toString();

     source = replaceString(source, "\"", "&quot;");
     source = replaceString(source, ">", "&gt;");
     source = replaceString(source, "<", "&lt;");
     return source;
 }

 public static String replaceString(String source, String target, String replace)
 {
     if(!target.equals(replace))
     {
         int index = 0;
         do
         {
             if((index = source.indexOf(target, index)) == -1)
                 break;
             source = (new StringBuilder()).append(source.substring(0, index)).append(replace).append(source.substring(index + target.length())).toString();
             if(replace.length() > 1)
                 index += replace.length();
         } while(true);
     }
     return source;
 }

 StringBuffer buf;

}
