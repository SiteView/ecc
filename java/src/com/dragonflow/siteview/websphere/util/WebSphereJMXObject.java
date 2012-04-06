package com.dragonflow.siteview.websphere.util;


import java.io.PrintStream;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.Vector;

public class WebSphereJMXObject extends JMXObject
{
  String cell;
  String node;
  String process;
  String id;
  String normelizedId;
  HashSet filters;
  HashMap configs;
  Object perfOName;
  private char COUNTER_NAME_DELIMITER;
 private static final String COUNTER_COMPONENT_DELIMITER = "/";

  public WebSphereJMXObject()
  {
    this.cell = null;
    this.node = null;
    this.process = null;
    this.id = null;
    this.normelizedId = null;

    this.filters = null;
    this.configs = null;
    this.perfOName = null;
    this.COUNTER_NAME_DELIMITER = '.';
  }

  public WebSphereJMXObject(Object objectName, Object mbs, WebSphereJMXInterface management, HashSet filters, HashMap configs, Object perfOName)
  {
    super(objectName, mbs, management);

    this.cell = null;
    this.node = null;
    this.process = null;
    this.id = null;
    this.normelizedId = null;

    this.filters = null;
    this.configs = null;
    this.perfOName = null;
    this.COUNTER_NAME_DELIMITER = '.';

    if (objectName == null)
      this.id = "";
    else {
      this.id = objectName.toString();
    }

    if ((management.getVersion() == 3) || (management.getVersion() == 4) || (management.getVersion() == 5)) {
      this.COUNTER_NAME_DELIMITER = '/';
    }

    this.filters = filters;
    this.configs = configs;
    this.perfOName = perfOName;
    try
    {
      this.type = ((String)management.getKeyPropertyMethod.invoke(objectName, new Object[] { "type" }));
      this.name = ((String)management.getKeyPropertyMethod.invoke(objectName, new Object[] { "name" }));
      this.cell = ((String)management.getKeyPropertyMethod.invoke(objectName, new Object[] { "cell" }));
      this.node = ((String)management.getKeyPropertyMethod.invoke(objectName, new Object[] { "node" }));
      this.process = ((String)management.getKeyPropertyMethod.invoke(objectName, new Object[] { "process" }));

      if ((this.type != null) && (this.cell != null) && (this.node != null) && (this.process != null))
        addCounters();
    }
    catch (Exception e) {
      System.err.println("WebSphereJMXObject exception: " + e);
    }
  }

  public WebSphereJMXObject(Object objectName, WebSphereJMXObject parent, Object mbs, WebSphereJMXInterface management, HashSet filters, HashMap configs, Object perfOName)
  {
    this(objectName, mbs, management, filters, configs, perfOName);
    this.parent = parent;
    parent.addSubObject(this);
  }

  public WebSphereJMXObject(Object perfOName, Object mbs, WebSphereJMXInterface management, HashSet filters, Object stats, HashMap descriptionMap, String parentId, String parentNormelizedId)
  {
    super(perfOName, mbs, management);

    this.cell = null;
    this.node = null;
    this.process = null;
    this.id = null;
    this.normelizedId = null;

    this.filters = null;
    this.configs = null;
    this.perfOName = null;
    this.COUNTER_NAME_DELIMITER = '.';

    if (perfOName != null) {
      try {
        this.type = ((String)management.getKeyPropertyMethod.invoke(perfOName, new Object[] { "type" }));
        this.name = ((String)management.getKeyPropertyMethod.invoke(perfOName, new Object[] { "name" }));
        this.cell = ((String)management.getKeyPropertyMethod.invoke(perfOName, new Object[] { "cell" }));
        this.node = ((String)management.getKeyPropertyMethod.invoke(perfOName, new Object[] { "node" }));
        this.process = ((String)management.getKeyPropertyMethod.invoke(perfOName, new Object[] { "process" }));

        if ((this.type != null) && (this.cell != null) && (this.node != null) && (this.process != null))
          addCounters();
      }
      catch (Exception e) {
        System.err.println("WebSphereJMXObject exception: " + e);
      }
    }
    this.filters = filters;

    if (stats == null) {
      return;
    }
    try
    {
      String nameId = (String)management.getStatsNameMethod.invoke(stats, null);
      this.name = ((String)management.getNLSValueMethod.invoke(null, new Object[] { nameId }));
      this.id = parentId + "/" + nameId;
      this.normelizedId = parentNormelizedId + "/" + WebSphereCounter.normalize(nameId);
      this.type = this.name;

      Object[] statistics = (Object[])(Object[])management.getStatisticsMethod.invoke(stats, null);
      if (statistics != null) {
        for (int sCount = 0; sCount < statistics.length; ++sCount) {
          String statId = (String)management.getStatisticNameMethod.invoke(statistics[sCount], null);

          if (!(filterCounter(this.type, statId))) {
            String desc = (String)descriptionMap.get(statId);
            if (desc == null) {
              desc = statId;
            }

           this.counters.add(new JMXObject.Counter(this, this.id + "/" + statId, desc, this.normelizedId + "/" + WebSphereCounter.normalize(statId)));
          }

        }

      }

      Object[] subStats = (Object[])(Object[])management.getSubStatsMethod.invoke(stats, null);
      if (subStats != null)
        for (int i = 0; i < subStats.length; ++i) {
          WebSphereJMXObject child = new WebSphereJMXObject(null, mbs, management, filters, subStats[i], descriptionMap, this.id, this.normelizedId);
          addSubObject(child);
          child.setParent(this);
        }
    }
    catch (Exception e)
    {
      System.err.println("WebSphereJMXObject constructor: " + e.getStackTrace());
    }
  }

  public String getCell()
  {
    return this.cell;
  }

  public String getNode() {
    return this.node;
  }

  public String getProcess() {
    return this.process;
  }

  int addCounters()
  {
    if (this.cachedCounters.containsKey(this.type)) {
      this.counters = ((Vector)this.cachedCounters.get(this.type));
    } else {
      this.counters = new Vector();
      try
      {
        Object info = this.management.getMBeanInfoMethod.invoke(this.mbs, new Object[] { this.objectName });
        Object[] attrs = (Object[])(Object[])this.management.getAttributesMethod.invoke(info, null);
        for (int i = 0; i < attrs.length; ++i) {
          if (((Boolean)this.management.isReadableMethod.invoke(attrs[i], null)).booleanValue()) {
            String attrType = (String)this.management.getTypeMethod.invoke(attrs[i], null);
            String counterName = (String)this.management.getNameMethod.invoke(attrs[i], null);

            String[] validTypes = { "byte", "short", "int", "long", "float", "double", "java.lang.String", "java.lang.Integer" };
            for (int j = 0; j < validTypes.length; ++j)
              if (validTypes[j].equals(attrType)) {
                String counterDescription = (String)this.management.getDescriptionMethod.invoke(attrs[i], null);
                if ((counterDescription == null) || (counterDescription.length() == 0)) {
                  counterDescription = counterName;
                }

                if (filterCounter(this.type, counterName)) break;
                this.counters.add(new JMXObject.Counter(this, counterName, counterDescription, counterName));
                break;
              }
          }
        }
      }
      catch (Exception e)
      {
        System.err.println("WebSphereJMXObject addCounters exception for objectName '" + this.objectName + "': " + e);
      }
      this.cachedCounters.put(this.type, this.counters);
    }

    return this.counters.size();
  }

  protected boolean filterCounter(String type, String counterName)
  {
    return ((this.filters.contains(type + ":" + counterName)) || (this.filters.contains("*:" + counterName)) || (this.filters.contains(type + ":*")));
  }

  void setParent(WebSphereJMXObject parent)
  {
    this.parent = parent;
  }

  public void printXML(PrintWriter out, int nest) {
    for (int i = 0; i < nest; ++i) {
      out.print(" ");
    }
    if (this.parent == null)
      out.println("<browse_data>");
    else {
      out.println("<object name=\"" + safeAttribute(this.name) + "\" id=\"" + safeAttribute((this.objectName != null) ? this.objectName.toString() : "") + "\" desc=\"" + safeAttribute(this.type) + "\" type=\"" + safeAttribute(this.type) + "\" solutionsID=\"" + safeAttribute((this.normelizedId != null) ? WebSphereCounter.normalize(this.normelizedId) : "") + "\">");
    }

    for (Iterator i = this.children.iterator(); i.hasNext(); ) {
      JMXObject o = (JMXObject)i.next();
      o.printXML(out, nest + 1);
    }

	for (Iterator i = this.counters.iterator(); i.hasNext(); ) {
      JMXObject.Counter counter = (JMXObject.Counter)i.next();
      for (int j = 0; j <= nest; ++j) {
        out.print(" ");
      }

      out.println("<counter name=\"" + safeAttribute(WebSphereCounter.getLeafName(counter.getName(), this.COUNTER_NAME_DELIMITER)) + "\" id=\"" + safeAttribute(counter.getId()) + "\" desc=\"" + safeAttribute(counter.getDescription()) + "\"/>");
    }

    for (int i = 0; i < nest; ++i) {
      out.print(" ");
    }
    if (this.parent == null)
     out.println("</browse_data>");
    else
     out.println("</object>");
  }
}