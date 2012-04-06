
package com.dragonflow.erlangecc.monitor;

import java.util.Vector;



public class Weblogic6xObject extends JMXObject {

    public Weblogic6xObject() {
    }

    public Weblogic6xObject(Object obj, Object obj1, JMXInterface jmxinterface) {
        super(obj, obj1, jmxinterface);
        try {
            type = (String) jmxinterface.getKeyPropertyMethod.invoke(obj, new Object[] { "Type" });
            name = null;
            if ("TransactionNameRuntime".equals(type)) {
                name = (String) getAttribute("TransactionName");
            } else if ("WebAppComponentRuntime".equals(type)) {
                name = (String) jmxinterface.getKeyPropertyMethod.invoke(obj, new Object[] { "Name" });
                for (String s = (String) jmxinterface.getKeyPropertyMethod.invoke(obj, new Object[] { "Location" }) + "_"; name.toString().startsWith(s); name = name.toString().substring(s.length())) {
                }
            } else if ("ServletRuntime".equals(type)) {
                name = (String) getAttribute("ServletName");
                if (name == null) {
                    name = (String) jmxinterface.getKeyPropertyMethod.invoke(obj, new Object[] { "Name" });
                }
            }
            if (name == null) {
                name = (String) jmxinterface.getKeyPropertyMethod.invoke(obj, new Object[] { "Name" });
            }
            try {
                parentName = jmxinterface.getAttributeMethod.invoke(obj1, new Object[] { obj, "Parent" });
            } catch (Exception exception) {
                parentName = null;
            }
            addCounters();
        } catch (Exception exception1) {
            System.out.println("Failed to parse MBean object: " + exception1);
        }
    }

    public Weblogic6xObject(Object obj, Weblogic6xObject weblogic6xobject, Object obj1, JMXInterface jmxinterface) {
        this(obj, obj1, jmxinterface);
        parent = weblogic6xobject;
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @return
     */
    int addCounters() {
        if (cachedCounters.containsKey(type)) {
            counters = (Vector) cachedCounters.get(type);
        } else {
            counters = new Vector();
            try {
                Object obj = management.getMBeanInfoMethod.invoke(mbs, new Object[] { objectName });
                Object aobj[] = (Object[]) management.getAttributesMethod.invoke(obj, null);
                for (int i = 0; i < aobj.length; i ++) {
                    if (((Boolean) management.isReadableMethod.invoke(aobj[i], null)).booleanValue()) {
                        String s = (String) management.getTypeMethod.invoke(aobj[i], null);
                        String as[] = { "byte", "short", "int", "long", "float", "double","java.lang.Long","java.lang.Integer" };

                        for (int j = 0; j < as.length; j ++) {
                            if (s.equals(as[j])) {
                                String s1 = (String) management.getNameMethod.invoke(aobj[i], null);
                                String s2 = (String) management.getDescriptionMethod.invoke(aobj[i], null);
                                counters.add(new JMXObject.Counter(this, s1, s2));
                                break;
                            }
                        }
                    }
                }

            } catch (Exception exception) {
            	System.out.println( "Failed to add counters for MBean " + objectName + ", exception: " + exception);
            }
            cachedCounters.put(type, counters);
        }
        return counters.size();
    }
}
