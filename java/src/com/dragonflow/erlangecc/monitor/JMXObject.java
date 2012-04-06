/*
 * 
 * Created on 2005-2-16 15:14:00
 *
 * JMXObject.java
 *
 * History:
 *
 */
package com.dragonflow.erlangecc.monitor;

/**
 * Comment for <code>JMXObject</code>
 * 
 * @author
 * @version 0.0
 * 
 * 
 */
import java.io.PrintWriter;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;


public class JMXObject implements Comparable {
    class Counter {

        String name;

        String description;

        private JMXObject jmxObject;

        Counter(JMXObject jmxo) {
            jmxObject = jmxo;
        }

        Counter(JMXObject jmxo, String s, String s1) {
            jmxObject = jmxo;
            name = s;
            description = s1;
        }
    }

    Hashtable cachedCounters;

    Object objectName;

    JMXObject parent;

    Object mbs;

    TreeSet children;

    Vector counters;

    String type;

    String name;

    Object parentName;

    JMXInterface management;

    public JMXObject() {
        cachedCounters = new Hashtable();
        objectName = null;
        parent = null;
        mbs = null;
        children = new TreeSet();
        counters = new Vector();
        type = null;
        name = null;
        parentName = null;
    }

    public JMXObject(Object obj, Object obj1, JMXInterface jmxinterface) {
        cachedCounters = new Hashtable();
        objectName = null;
        parent = null;
        mbs = null;
        children = new TreeSet();
        counters = new Vector();
        type = null;
        name = null;
        parentName = null;
        objectName = obj;
        mbs = obj1;
        management = jmxinterface;
    }

    public JMXObject(Object obj, JMXObject jmxobject, Object obj1,
            JMXInterface jmxinterface) {
        this(obj, obj1, jmxinterface);
        parent = jmxobject;
    }

    public void setParent(JMXObject jmxobject) {
        parent = jmxobject;
    }

    public void addSubObject(JMXObject jmxobject) {
        children.add(jmxobject);
    }

    public Object getObjectName() {
        return objectName;
    }

    public String getName() {
        return name;
    }

    public Object getParentName() {
        return parentName;
    }

    public String getType() {
        return type;
    }

    Object getAttribute(String s) {
        Object object;
        try {
            object = management.getAttributeMethod.invoke(mbs, new Object[] {
                    objectName, s });
        } catch (Exception exception) {
            return null;
        }
        return object;
    }

    public static String safeAttribute(String s) {
        StringBuffer stringbuffer = new StringBuffer();
        int i = s.length();
        for (int j = 0; j < i; j++) {
            char c = s.charAt(j);
            if (c == '<') {
                stringbuffer.append("&lt;");
                continue;
            }
            if (c == '>') {
                stringbuffer.append("&gt;");
                continue;
            }
            if (c == '&') {
                stringbuffer.append("&amp;");
                continue;
            }
            if (c == '"') {
                stringbuffer.append("&quot;");
                continue;
            }
            if (c == '\'') {
                stringbuffer.append("&apos;");
                continue;
            }
            if (Character.isISOControl(c)) {
                stringbuffer.append(" ");
            } else {
                stringbuffer.append(c);
            }
        }

        return stringbuffer.toString();
    }
   
    public void toMap(Map<String,Object> map,String fullParentName){
    	String id = objectName == null ? "" : objectName.toString();
    	String myname = name == null? fullParentName: (fullParentName.length()>0? fullParentName + "/" 
    				+ type +":"+ name : type +":"+ name ) ;
    	
    	if (parent !=null){
    		map.put( id, myname);
    	}
    	JMXObject jmxobject;
        for (Iterator iterator = children.iterator(); iterator.hasNext(); jmxobject
                .toMap(map,myname)) {
            jmxobject = (JMXObject) iterator.next();
        }
        
        Counter counter;
        for (Iterator iterator1 = counters.iterator(); iterator1.hasNext(); 
        		map.put(id + "/" +counter.name, myname + "/" + counter.name)) {
            counter = (Counter) iterator1.next();

        }
    	
    }

    void printXML(PrintWriter printwriter, int i) {
        for (int j = 0; j < i; j++) {
            printwriter.print(" ");
        }

        if (parent == null) {
            printwriter.println("<browse_data>");
        } else {
            printwriter.println("<object name=\""
                    + safeAttribute(type + ":" + name)
                    + "\" id=\""
                    + safeAttribute(objectName == null ? "" : objectName
                            .toString()) + "\" desc=\"" + safeAttribute(type)
                    + "\" type=\"" + safeAttribute(type) + "\" plainName=\""
                    + safeAttribute(name) + "\" >");
        }
        JMXObject jmxobject;
        for (Iterator iterator = children.iterator(); iterator.hasNext(); jmxobject
                .printXML(printwriter, i + 1)) {
            jmxobject = (JMXObject) iterator.next();
        }

        Counter counter;
        for (Iterator iterator1 = counters.iterator(); iterator1.hasNext(); printwriter
                .println("<counter name=\"" + safeAttribute(counter.name)
                        + "\" id=\"" + safeAttribute(counter.name)
                        + "\" desc=\"" + safeAttribute(counter.description)
                        + "\"/>")) {
            counter = (Counter) iterator1.next();
            for (int l = 0; l <= i; l++) {
                printwriter.print(" ");
            }

        }

        for (int k = 0; k < i; k++) {
            printwriter.print(" ");
        }

        if (parent == null) {
            printwriter.println("</browse_data>");
        } else {
            printwriter.println("</object>");
        }
    }

    public int compareTo(Object obj) {
        int i = 0;
        try {
            JMXObject jmxobject = (JMXObject) obj;
            String s = type + name + objectName;
            String s1 = jmxobject.type + jmxobject.name + jmxobject.objectName;
            i = s.compareTo(s1);
        } catch (ClassCastException classcastexception) {
            classcastexception.printStackTrace(System.err);
        }
        return i;
    }

    void removeSubObject(Object obj) {
        children.remove(obj);
    }

    public void purge() {
        Object aobj[] = children.toArray();
        for (int i = 0; i < aobj.length; i++) {
            ((JMXObject) aobj[i]).purge();
        }

        if (children.isEmpty() && counters.isEmpty() && parent != null) {
            parent.removeSubObject(this);
        }
    }
}
