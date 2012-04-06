package com.dragonflow.siteview.san.beans;

public class SLP
{
  private Integer id;
  private String templateUrlSyntax;
  private String serviceId;
  private String serviceHiName;
  private String serviceHiDescription;
  private String templateType;
  private String templateVersion;
  private String templateDescription;
  private String interopSchemaNamespace;
  private String functionalProfilesSupported;
  private String vendorNamespace;
  private String registeredProfilesSupported;
  private String multipleOperationsSupported;
  private String authenticationMechanismsSupported;
  private String authenticationMechanismDescriptions;
  private String communicationMechanism;
  private String protocolVersion;
  private String ipAddress;
  private String host;
  private String protocol;
  private Integer port;
  private Integer discoveryInterval;
  private String timeOfCreation;

  public SLP()
  {
  }

  public SLP(Integer id)
  {
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Integer getDiscoveryInterval()
  {
    return this.discoveryInterval;
  }

  public void setDiscoveryInterval(Integer discoveryInterval)
  {
    this.discoveryInterval = discoveryInterval;
  }

  public String getIpAddress()
  {
    return this.ipAddress;
  }

  public void setIpAddress(String ipAddress)
  {
    this.ipAddress = ipAddress;
  }

  public Integer getPort()
  {
    return this.port;
  }

  public void setPort(Integer port)
  {
    this.port = port;
  }

  public String getProtocol()
  {
    return this.protocol;
  }

  public void setProtocol(String protocol)
  {
    this.protocol = protocol;
  }

  public String getRegisteredProfilesSupported()
  {
    return this.registeredProfilesSupported;
  }

  public void setRegisteredProfilesSupported(String registeredProfilesSupported)
  {
    this.registeredProfilesSupported = registeredProfilesSupported;
  }

  public String getServiceHiDescription()
  {
    return this.serviceHiDescription;
  }

  public void setServiceHiDescription(String serviceHiDescription)
  {
    this.serviceHiDescription = serviceHiDescription;
  }

  public String getServiceHiName()
  {
    return this.serviceHiName;
  }

  public void setServiceHiName(String serviceHiName)
  {
    this.serviceHiName = serviceHiName;
  }

  public String getTemplateUrlSyntax()
  {
    return this.templateUrlSyntax;
  }

  public void setTemplateUrlSyntax(String templateUrlSyntax)
  {
    this.templateUrlSyntax = templateUrlSyntax;
  }

  public String getTimeOfCreation()
  {
    return this.timeOfCreation;
  }

  public void setTimeOfCreation(String timeOfCreation)
  {
    this.timeOfCreation = timeOfCreation;
  }

  public String getServiceId()
  {
    return this.serviceId;
  }

  public void setServiceId(String serviceId)
  {
    this.serviceId = serviceId;
  }

  public String getTemplateType()
  {
    return this.templateType;
  }

  public void setTemplateType(String templateType)
  {
    this.templateType = templateType;
  }

  public String getTemplateVersion()
  {
    return this.templateVersion;
  }

  public void setTemplateVersion(String templateVersion)
  {
    this.templateVersion = templateVersion;
  }

  public String getTemplateDescription()
  {
    return this.templateDescription;
  }

  public void setTemplateDescription(String templateDescription)
  {
    this.templateDescription = templateDescription;
  }

  public String getInteropSchemaNamespace()
  {
    return this.interopSchemaNamespace;
  }

  public void setInteropSchemaNamespace(String interopSchemaNamespace)
  {
    this.interopSchemaNamespace = interopSchemaNamespace;
  }

  public String getFunctionalProfilesSupported()
  {
    return this.functionalProfilesSupported;
  }

  public void setFunctionalProfilesSupported(String functionalProfilesSupported)
  {
    this.functionalProfilesSupported = functionalProfilesSupported;
  }

  public String getMultipleOperationsSupported()
  {
    return this.multipleOperationsSupported;
  }

  public void setMultipleOperationsSupported(String multipleOperationsSupported)
  {
    this.multipleOperationsSupported = multipleOperationsSupported;
  }

  public String getAuthenticationMechanismsSupported()
  {
    return this.authenticationMechanismsSupported;
  }

  public void setAuthenticationMechanismsSupported(String authenticationMechanismsSupported)
  {
    this.authenticationMechanismsSupported = authenticationMechanismsSupported;
  }

  public String getAuthenticationMechanismDescriptions()
  {
    return this.authenticationMechanismDescriptions;
  }

  public void setAuthenticationMechanismDescriptions(String authenticationMechanismDescriptions)
  {
    this.authenticationMechanismDescriptions = authenticationMechanismDescriptions;
  }

  public String getCommunicationMechanism()
  {
    return this.communicationMechanism;
  }

  public void setCommunicationMechanism(String communicationMechanism)
  {
    this.communicationMechanism = communicationMechanism;
  }

  public String getProtocolVersion()
  {
    return this.protocolVersion;
  }

  public void setProtocolVersion(String protocolVersion)
  {
    this.protocolVersion = protocolVersion;
  }

  public String getHost()
  {
    return this.host;
  }

  public void setHost(String host)
  {
    this.host = host;
  }

  public String getVendorNamespace()
  {
    return this.vendorNamespace;
  }

  public void setVendorNamespace(String vendorNamespace)
  {
    this.vendorNamespace = vendorNamespace;
  }
}