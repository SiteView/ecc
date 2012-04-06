package com.dragonflow.siteview.san.beans;

import java.util.Calendar;

public class BSP_Volume
{
  private Integer id;
  private int csid;
  private int volumeid;
  private int poolid;
  Calendar timeOfCreationCS;
  private Long statisticTimeDelta;
  private Long totalIos;
  private Long kBytesTransferred;
  private Long readIos;
  private Long readHitIos;
  private Long writeIos;
  private Long writeHitIos;

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

  public int getVolumeid()
  {
    return this.volumeid;
  }

  public void setVolumeid(int volumeid)
  {
    this.volumeid = volumeid;
  }

  public int getPoolid()
  {
    return this.poolid;
  }

  public void setPoolid(int poolid)
  {
    this.poolid = poolid;
  }

  public Long getReadIos()
  {
    return this.readIos;
  }

  public void setReadIos(Long readIos)
  {
    this.readIos = readIos;
  }

  public Long getReadHitIos()
  {
    return this.readHitIos;
  }

  public void setReadHitIos(Long readHitIos)
  {
    this.readHitIos = readHitIos;
  }

  public Long getWriteIos()
  {
    return this.writeIos;
  }

  public void setWriteIos(Long writeIos)
  {
    this.writeIos = writeIos;
  }

  public Long getWriteHitIos()
  {
    return this.writeHitIos;
  }

  public void setWriteHitIos(Long writeHitIos)
  {
    this.writeHitIos = writeHitIos;
  }
}