package com.dragonflow.siteview.websphere.util;

import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.Vector;


import com.dragonflow.erlangecc.monitor.JMXInterface;

public class JMXObject
  implements Comparable
{
  protected Hashtable cachedCounters;
  protected Object objectName;
  protected JMXObject parent;
  protected Object mbs;
  public TreeSet children;
  protected Vector counters;
  protected String type;
  protected String name;
  protected Object parentName;
  protected JMXInterface management;

  public JMXObject()
  {
    this.cachedCounters = new Hashtable();

    this.objectName = null;

    this.parent = null;

    this.mbs = null;

    this.children = new TreeSet();

    this.counters = new Vector();

    this.type = null;
    this.name = null;
    this.parentName = null;
  }

  public JMXObject(Object objectName, Object mbs, JMXInterface management)
  {
    this.cachedCounters = new Hashtable();

    this.objectName = null;

    this.parent = null;

    this.mbs = null;

    this.children = new TreeSet();

    this.counters = new Vector();

    this.type = null;
    this.name = null;
    this.parentName = null;

    this.objectName = objectName;
    this.mbs = mbs;
    this.management = management;
  }

  public JMXObject(Object objectName, JMXObject parent, Object mbs, JMXInterface management)
  {
    this(objectName, mbs, management);
    this.parent = parent;
  }

  public void setParent(JMXObject parent) {
    this.parent = parent;
  }

  public void addSubObject(JMXObject obj) {
    this.children.add(obj);
  }

  public Object getObjectName() {
    return this.objectName;
  }

  public String getName() {
    return this.name;
  }

  public Object getParentName() {
    return this.parentName;
  }

  public String getType() {
    return this.type;
  }

  public Object getAttribute(String attribute) {
    try {
      return this.management.getAttributeMethod.invoke(this.mbs, new Object[] { this.objectName, attribute }); } catch (Exception e) {
    }
    return null;
  }

  public static String safeAttribute(String attr)
  {
    StringBuffer retval = new StringBuffer();
    int len = attr.length();
    for (int i = 0; i < len; ++i) {
      char c = attr.charAt(i);
      if (c == '<')
        retval.append("&lt;");
      else if (c == '>')
        retval.append("&gt;");
      else if (c == '&')
        retval.append("&amp;");
      else if (c == '"')
        retval.append("&quot;");
      else if (c == '\'')
        retval.append("&apos;");
      else if (Character.isISOControl(c))
        retval.append(" ");
      else {
        retval.append(c);
      }
    }

    return retval.toString();
  }

  public void printXML(PrintWriter out, int nest) {
    for (int i = 0; i < nest; ++i) {
      out.print(" ");
    }
    if (this.parent == null)
      out.println("<browse_data>");
    else {
      out.println("<object name=\"" + safeAttribute(new StringBuffer().append(this.type).append(":").append(this.name).toString()) + "\" id=\"" + safeAttribute((this.objectName != null) ? this.objectName.toString() : "") + "\" desc=\"" + safeAttribute(this.type) + "\" type=\"" + safeAttribute(this.type) + "\" plainName=\"" + safeAttribute(this.name) + "\" >");
    }

    for (Iterator i = this.children.iterator(); i.hasNext(); ) {
      JMXObject o = (JMXObject)i.next();
      o.printXML(out, nest + 1);
    }

 
	for (Iterator i = this.counters.iterator(); i.hasNext(); ) {
      Counter counter = (Counter)i.next();
      for (int j = 0; j <= nest; ++j) {
        out.print(" ");
      }
       out.println("<counter name=\"" + safeAttribute(counter.name) + "\" id=\"" + safeAttribute(counter.name) + "\" desc=\"" + safeAttribute(counter.description) + "\"/>");
    }

    for (int k = 0; k < nest; ++k) {
      out.print(" ");
    }
    if (this.parent == null)
     out.println("</browse_data>");
    else
     out.println("</object>");
  }

  public int compareTo(Object o)
  {
    int rc = 0;
    try
    {
      JMXObject obj = (JMXObject)o;

      String s1 = this.type + this.name + this.objectName;
      String s2 = obj.type + obj.name + obj.objectName;

      rc = s1.compareTo(s2);
    } catch (ClassCastException e) {
      e.printStackTrace(System.err);
    }
    return rc;
  }

  void removeSubObject(Object object) {
    this.children.remove(object);
  }

  public void purge()
  {
    Object[] c = this.children.toArray();

    for (int i = 0; i < c.length; ++i) {
      ((JMXObject)c[i]).purge();
    }

    if ((this.children.isEmpty()) && (this.counters.isEmpty()) && (this.parent != null))
      this.parent.removeSubObject(this);
  }

  public class Counter
  {
    private String name;
    private String description;
    private String id;

    public Counter()
    {
    }

    public Counter(JMXObject jmxo,String paramString1, String paramString2)
    {
      this.name = paramString1;
      this.description = description;
    }

    public Counter(JMXObject jmxo,String paramString1, String paramString2, String paramString3) {
      this.name = paramString1;
      this.description = paramString2;
      this.id = paramString3;
    }

    public String getName()
    {
      return this.name;
    }

    public String getId() {
      return this.id;
    }

    public String getDescription() {
      return this.description;
    }
  }
}