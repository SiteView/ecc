package com.dragonflow.siteview.san.beans;


public class CIM_FileSystem
{
  private Integer id;
  private int percentageSpaceUsed;
  private String csCreationClassName;
  private String csName;
  private String creationClassName;
  private String name;
  private String root;
  private Long blockSize;
  private Long fileSystemSize;
  private Long availableSpace;
  private boolean readOnly;
  private int maxFileNameLength;
  private String fileSystemType;
  private int enabledState;
  private int requestedState;
  private String status;
  private String caption;

  public Long getAvailableSpace()
  {
    return this.availableSpace;
  }

  public void setAvailableSpace(Long availableSpace)
  {
    this.availableSpace = availableSpace;
  }

  public Long getBlockSize()
  {
    return this.blockSize;
  }

  public void setBlockSize(Long blockSize)
  {
    this.blockSize = blockSize;
  }

  public String getCreationClassName()
  {
    return this.creationClassName;
  }

  public void setCreationClassName(String creationClassName)
  {
    this.creationClassName = creationClassName;
  }

  public String getCsCreationClassName()
  {
    return this.csCreationClassName;
  }

  public void setCsCreationClassName(String csCreationClassName)
  {
    this.csCreationClassName = csCreationClassName;
  }

  public String getCsName()
  {
    return this.csName;
  }

  public void setCsName(String csName)
  {
    this.csName = csName;
  }

  public int getEnabledState()
  {
    return this.enabledState;
  }

  public void setEnabledState(int enabledState)
  {
    this.enabledState = enabledState;
  }

  public Long getFileSystemSize()
  {
    return this.fileSystemSize;
  }

  public void setFileSystemSize(Long fileSystemSize)
  {
    this.fileSystemSize = fileSystemSize;
  }

  public String getFileSystemType()
  {
    return this.fileSystemType;
  }

  public void setFileSystemType(String fileSystemType)
  {
    this.fileSystemType = fileSystemType;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public int getMaxFileNameLength()
  {
    return this.maxFileNameLength;
  }

  public void setMaxFileNameLength(int maxFileNameLength)
  {
    this.maxFileNameLength = maxFileNameLength;
  }

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public int getPercentageSpaceUsed()
  {
    return this.percentageSpaceUsed;
  }

  public void setPercentageSpaceUsed(int percentageSpaceUsed)
  {
    this.percentageSpaceUsed = percentageSpaceUsed;
  }

  public boolean isReadOnly()
  {
    return this.readOnly;
  }

  public void setReadOnly(boolean readOnly)
  {
    this.readOnly = readOnly;
  }

  public int getRequestedState()
  {
    return this.requestedState;
  }

  public void setRequestedState(int requestedState)
  {
    this.requestedState = requestedState;
  }

  public String getRoot()
  {
    return this.root;
  }

  public void setRoot(String root)
  {
    this.root = root;
  }

  public String getStatus()
  {
    return this.status;
  }

  public void setStatus(String status)
  {
    this.status = status;
  }

  public String getCaption()
  {
    return this.caption;
  }

  public void setCaption(String caption)
  {
    this.caption = caption;
  }
}