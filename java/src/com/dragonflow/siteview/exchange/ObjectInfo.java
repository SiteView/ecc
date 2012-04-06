package com.dragonflow.siteview.exchange;

import java.util.ArrayList;
 import java.util.LinkedList;
 import java.util.List;
 
 public class ObjectInfo
 {
   protected String name;
  protected String id;
   protected String desc;
   private List<ObjectInfo> countersList;
   private List<String> roles;
 
   public ObjectInfo(String name, String desc, List<String> roles)
   {
     this(name, name, desc, roles);
   }
 
   public ObjectInfo(String name, String id, String desc, List<String> roles)
   {
     this(name, id, desc);
     this.roles = roles;
     this.countersList = new ArrayList();
   }
 
   public ObjectInfo(String name, String id, String desc)
   {
     this.name = name;
     this.desc = desc;
     this.id = id;
   }
 
   public String toString() {
     StringBuilder retVal = new StringBuilder("   <object name=\"" + this.name + "\" desc=\"" + this.desc + "\">");
     for (ObjectInfo counter : this.countersList) {
       retVal.append(counter.toString());
     }
     retVal.append("\t</object>");
     return retVal.toString();
   }
 
   public String getName() {
     return this.name;
   }
 
   public void addCounter(String counterName, String description)
   {
     this.countersList.add(new CounterInfo(counterName, this.id + "/" + counterName, description));
   }
 
   public ObjectInfo addObject(String objName, String description)
   {
     ObjectInfo child = new ObjectInfo(objName, this.id + "/" + objName, description, new LinkedList());
     this.countersList.add(child);
     return child;
   }
 
   public List<String> getRoles() {
     return this.roles;
   }
 
   public class CounterInfo extends ObjectInfo
   {
     public CounterInfo(String paramString1, String paramString2, String paramString3)
     {
       super(paramString1, paramString2, paramString3);
     }
 
     public String toString()
     {
       return "        <counter name=\"" + this.name + "\" id=\"" + this.id + "\" desc=\"" + this.desc + "\"/>";
     }
   }
 }
