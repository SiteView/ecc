package com.dragonflow.siteview.websphere.util;

import java.io.Serializable;

public class WebSphereCounter
  implements Serializable
{
  private String name = "";
  private String id;
  private String description;
  private String denormalizedName = "";
  private String value = "";
  private String errorMessage = "";
  private boolean error = false;
  public static final String msInnerNameSlashRepresentation = "ORIGINAL_NAME_SLASH";
  public static final String msHierarchySlashRepresentation = "HIERARCHY_NAME_SLASH";
 public static final String msHierarchySeparator = "/";

  public WebSphereCounter(String name)
  {
    this.name = name;
    this.denormalizedName = denormalize(this.name);
  }

  public WebSphereCounter(String name, String id) {
    this.name = name;
    this.denormalizedName = denormalize(this.name);
    this.id = denormalize(id);
  }

  public String getId() {
    return this.id;
  }

  public boolean isError() {
    return this.error;
  }

  public void setError(boolean error) {
    this.error = error;
  }

  public String getErrorMessage() {
    return this.errorMessage;
  }

  public void setErrorMessage(String error) {
    this.errorMessage = error;
  }

  public String getName() {
    return this.name;
  }

  public String getDenormalizedCounterName() {
    return this.denormalizedName;
  }

  public String getValue() {
    return this.value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public WebSphereCounter(String name, String id, String desc)
  {
    this.name = name;
    this.id = makeId(id);
    this.description = desc;
  }

  public void toXML(StringBuffer xml, int indent) {
    xml.append(WSUtils.indent(indent)).append("<counter name=\"").append(JMXObject.safeAttribute(this.name)).append("\"");

    if (this.id.length() > 0) {
      xml.append(" id=\"").append(JMXObject.safeAttribute(this.id)).append("\"");
    }

    if (this.description.length() > 0) {
      xml.append(" desc=\"").append(JMXObject.safeAttribute(this.description)).append("\"");
    }

   xml.append("/>\n");
  }

  public static String makeId(String id) {
    StringBuffer newId = new StringBuffer();

    for (int i = 0; i < id.length(); ++i) {
     if ((id.charAt(i) == '/') || (id.charAt(i) == '\\')) {
        newId.append('\\');
      }
      newId.append(id.charAt(i));
    }

    return newId.toString();
  }

  public static String denormalize(String id)
  {
    String retval = denormalizeHierarchySlash(id);
    return replaceAllOcurences("ORIGINAL_NAME_SLASH", "/", retval);
  }

  private static String replaceAllOcurences(String toReplaceWith, String replaceWith, String originalString)
  {
    StringBuffer replacedOriginalString = new StringBuffer(originalString);
    int pos = 0;
    int toReplaceLen = toReplaceWith.length();
    int replaceWithLen = replaceWith.length();
    while (pos != -1) {
      pos = new String(replacedOriginalString).indexOf(toReplaceWith, pos);
      if (pos != -1){
      replacedOriginalString.replace(pos, pos + toReplaceLen, replaceWith);
      pos += replaceWithLen;
      }
    }

    return replacedOriginalString.toString();
  }

  public static String denormalizeHierarchySlash(String id)
  {
    return replaceAllOcurences("HIERARCHY_NAME_SLASH", "/", id);
  }

  public static String normalize(String id)
  {
    if ((id.indexOf("ORIGINAL_NAME_SLASH") != -1) || ((id.indexOf("HIERARCHY_NAME_SLASH") != -1) && (id.indexOf("/") == -1)))
    {
      return id;
    }
    String retval = id;
    retval = replace(retval, "HIERARCHY_NAME_SLASH", "ORIGINAL_NAME_SLASH");
    retval = replace(retval, "/", "HIERARCHY_NAME_SLASH");
    return retval;
  }

  public String getObjectNameFromID()
  {
    if (this.id == null) {
      return "";
    }
    int objectEnd = this.id.lastIndexOf(47);
    if (objectEnd == -1) {
      return denormalize(this.id);
    }
    String objectString = this.id.substring(0, objectEnd);
    int objectBeginning = objectString.lastIndexOf(47);
    if (objectBeginning == -1) {
      return denormalize(objectString);
    }
    return denormalize(objectString.substring(objectBeginning + 1));
  }

  public String getAttributeNameFromID()
  {
    if (this.id == null) {
      return "";
    }
    int objectEnd = this.id.lastIndexOf(47);
    if (objectEnd == -1) {
      return denormalize(this.id);
    }
    return denormalize(this.id.substring(objectEnd + 1));
  }

  public static String replace(String toCheck, String toReplace, String replaceWith)
  {
    String retval = toCheck;
    int pos = 0;
    while (pos != -1) {
      pos = retval.indexOf(toReplace, pos);
      if (pos != -1){
      retval = retval.substring(0, pos) + replaceWith + retval.substring(pos + toReplace.length());
      pos += replaceWith.length();
   }
      
    }

    return retval;
  }

  public static String getLeafName(String name, int delimiter) {
    int last = name.lastIndexOf(delimiter);
    if (last > 0) {
      return name.substring(last + 1);
    }

    return name;
  }

  public String getObjectName()
  {
    String idToUse = this.id;
    if (this.id == null) {
      idToUse = this.denormalizedName;
    }

    int objectEnd = idToUse.lastIndexOf(47);
    if (objectEnd == -1) {
      return denormalize(idToUse);
    }
    String objectString = idToUse.substring(0, objectEnd);
    int objectBeginning = objectString.lastIndexOf(47);
    if (objectBeginning == -1) {
      return denormalize(objectString);
    }
    return denormalize(objectString.substring(objectBeginning + 1));
  }

  public String getObjectNameWithMBeanIdentifier()
  {
    String idToUse = this.id;
    if (this.id == null) {
      idToUse = this.denormalizedName;
    }

    int domainEnd = idToUse.lastIndexOf(58);
    if (domainEnd == -1) {
      return "";
    }
    int domainBegin = idToUse.lastIndexOf(47, domainEnd);
    if (domainBegin == -1) {
      domainBegin = 0;
    }

    String objectName = idToUse.substring(domainBegin + 1);
    int objectEnd = objectName.lastIndexOf(47);
    if (objectEnd > -1) {
      objectName = objectName.substring(0, objectEnd);
    }
    return denormalize(objectName);
  }

  public String getAttributeName()
  {
    String idToUse = this.id;
    if (this.id == null) {
      idToUse = this.denormalizedName;
    }
    int objectEnd = idToUse.lastIndexOf(47);
    if (objectEnd == -1) {
      return denormalize(idToUse);
    }
    return denormalize(idToUse.substring(objectEnd + 1));
  }

  public void changeNameToNewRepresentation()
  {
    boolean changed = false;
    if (this.name.indexOf("0x2F0x2F") != -1)
    {
      this.name = replaceAllOcurences("0x2F0x2F", "ORIGINAL_NAME_SLASH", this.name);
      changed = true;
    }
    if (this.name.indexOf("0x2F") != -1)
    {
      this.name = replaceAllOcurences("0x2F", "HIERARCHY_NAME_SLASH", this.name);
      changed = true;
    }
    if (this.name.indexOf("HIERARCHY_SLASH") != -1)
    {
      this.name = replaceAllOcurences("HIERARCHY_SLASH", "HIERARCHY_NAME_SLASH", this.name);
      changed = true;
    }
    if (changed)
      this.denormalizedName = denormalize(this.name);
  }
}
