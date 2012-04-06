
package com.dragonflow.erlangecc.monitor;


public class JMXInterface {

    public java.lang.Class MBeanServerClass;

    public java.lang.Class QueryExpClass;

    public java.lang.Class QueryClass;

    public java.lang.Class AttributeValueExpClass;

    public java.lang.Class StringValueExpClass;

    public java.lang.Class ObjectNameClass;

    public java.lang.Class MBeanInfoClass;

    public java.lang.Class MBeanAttributeInfoClass;

    public java.lang.reflect.Method finalSubStringMethod;

    public java.lang.reflect.Method attrMethod;

    public java.lang.reflect.Method valueMethod;

    public java.lang.reflect.Method queryNamesMethod;

    public java.lang.reflect.Method getAttributeMethod;

    public java.lang.reflect.Method getMBeanInfoMethod;

    public java.lang.reflect.Method invokeMethod;

    public java.lang.reflect.Constructor objectNameCtr;

    public java.lang.reflect.Method equalsMethod;

    public java.lang.reflect.Method getKeyPropertyMethod;

    public java.lang.reflect.Method getAttributesMethod;

    public java.lang.reflect.Method getInfoDescriptionMethod;

    public java.lang.reflect.Method isReadableMethod;

    public java.lang.reflect.Method getTypeMethod;

    public java.lang.reflect.Method getNameMethod;

    public java.lang.reflect.Method getDescriptionMethod;

    public JMXInterface() {
        java.lang.ClassLoader classloader = java.lang.Thread.currentThread().getContextClassLoader();
        try {
            QueryClass = classloader.loadClass("javax.management.Query");
            QueryExpClass = classloader.loadClass("javax.management.QueryExp");
            AttributeValueExpClass = classloader.loadClass("javax.management.AttributeValueExp");
            StringValueExpClass = classloader.loadClass("javax.management.StringValueExp");
            MBeanServerClass = classloader.loadClass("javax.management.MBeanServer");
            ObjectNameClass = classloader.loadClass("javax.management.ObjectName");
            MBeanInfoClass = classloader.loadClass("javax.management.MBeanInfo");
            MBeanAttributeInfoClass = classloader.loadClass("javax.management.MBeanAttributeInfo");
            finalSubStringMethod = QueryClass.getMethod("finalSubString", new java.lang.Class[] { AttributeValueExpClass, StringValueExpClass });
            attrMethod = QueryClass.getMethod("attr", new java.lang.Class[] { java.lang.String.class });
            valueMethod = QueryClass.getMethod("value", new java.lang.Class[] { java.lang.String.class });
            queryNamesMethod = MBeanServerClass.getMethod("queryNames", new java.lang.Class[] { ObjectNameClass, QueryExpClass });
            getAttributeMethod = MBeanServerClass.getMethod("getAttribute", new java.lang.Class[] { ObjectNameClass, java.lang.String.class });
            getMBeanInfoMethod = MBeanServerClass.getMethod("getMBeanInfo", new java.lang.Class[] { ObjectNameClass });
            invokeMethod = MBeanServerClass.getMethod("invoke", new java.lang.Class[] { ObjectNameClass, java.lang.String.class, java.lang.Object[].class, java.lang.String[].class });
            objectNameCtr = ObjectNameClass.getConstructor(new java.lang.Class[] { java.lang.String.class });
            equalsMethod = ObjectNameClass.getMethod("equals", new java.lang.Class[] { java.lang.Object.class });
            getKeyPropertyMethod = ObjectNameClass.getMethod("getKeyProperty", new java.lang.Class[] { java.lang.String.class });
            getAttributesMethod = MBeanInfoClass.getMethod("getAttributes", null);
            getInfoDescriptionMethod = MBeanInfoClass.getMethod("getDescription", null);
            isReadableMethod = MBeanAttributeInfoClass.getMethod("isReadable", null);
            getTypeMethod = MBeanAttributeInfoClass.getMethod("getType", null);
            getNameMethod = MBeanAttributeInfoClass.getMethod("getName", null);
            getDescriptionMethod = MBeanAttributeInfoClass.getMethod("getDescription", null);
        } catch (java.lang.Exception exception) {
            java.lang.System.err.println("Failed to load javax.management class or method: " + exception.toString());
        }
    }
}
