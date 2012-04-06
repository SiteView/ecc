package com.dragonflow.siteview.san.beans;

import java.util.Calendar;

public class BSP_Port
{
  private Integer id;
  private int csid;
  private int portid;
  private int ccscid;
  Calendar timeOfCreationCS;
  private Long statisticTimeDelta;
  private String permanentAddress;
  private Long totalIos;
  private Long kBytesTransferred;

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Calendar getTimeOfCreationCS()
  {
    return this.timeOfCreationCS;
  }

  public void setTimeOfCreationCS(Calendar timeOfCreationCS)
  {
    this.timeOfCreationCS = timeOfCreationCS;
  }

  public String getPermanentAddress()
  {
    return this.permanentAddress;
  }

  public void setPermanentAddress(String permanentAddress)
  {
    this.permanentAddress = permanentAddress;
  }

  public Long getTotalIos()
  {
    return this.totalIos;
  }

  public void setTotalIos(Long totalIos)
  {
    this.totalIos = totalIos;
  }

  public Long getKBytesTransferred()
  {
    return this.kBytesTransferred;
  }

  public void setKBytesTransferred(Long bytesTransferred)
  {
    this.kBytesTransferred = bytesTransferred;
  }

  public Long getStatisticTimeDelta()
  {
    return this.statisticTimeDelta;
  }

  public void setStatisticTimeDelta(Long statisticTimeDelta)
  {
    this.statisticTimeDelta = statisticTimeDelta;
  }

  public int getCsid()
  {
    return this.csid;
  }

  public void setCsid(int csid)
  {
    this.csid = csid;
  }

  public int getPortid()
  {
    return this.portid;
  }

  public void setPortid(int portid)
  {
    this.portid = portid;
  }

  public int getCcscid()
  {
    return this.ccscid;
  }

  public void setCcscid(int ccscid)
  {
    this.ccscid = ccscid;
  }
}