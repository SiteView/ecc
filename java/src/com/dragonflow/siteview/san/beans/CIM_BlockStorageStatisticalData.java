package com.dragonflow.siteview.san.beans;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_BlockStorageStatisticalData
{
  private Set cim_FCPort = new HashSet();

  private Set cim_StorageExtent = new HashSet();
  private Integer id;
  private Long kBytesRead;
  private Long totalIos;
  private int elementType;
  private Long kBytesWritten;
  private Long readIos;
  private Long writeIos;
  private Long writeHitIos;
  private String instanceId;
  private Long kBytesTransferred;
  private Long readHitIos;
  private Calendar statisticTime;
  private Long bytesTransmitted;
  private Long bytesReceived;
  private Long packetsTransmitted;
  private Long packetsReceived;
  private Long crcErrors;
  private Long linkFailures;
  private Long primitiveSeqProtocolErrCount;
  private Long lossOfSignalCounter;
  private Long invalidTransmissionWords;
  private Long lipCount;
  private Long nosCount;
  private Long errorFrames;
  private Long dumpedFrames;

  public Set getCim_FCPort()
  {
    return this.cim_FCPort;
  }

  public void setCim_FCPort(Set cim_FCPort)
  {
    this.cim_FCPort = cim_FCPort;
  }

  public int getElementType()
  {
    return this.elementType;
  }

  public void setElementType(int elementType)
  {
    this.elementType = elementType;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public String getInstanceId()
  {
    return this.instanceId;
  }

  public void setInstanceId(String instanceId)
  {
    this.instanceId = instanceId;
  }

  public Long getKBytesRead()
  {
    return this.kBytesRead;
  }

  public void setKBytesRead(Long bytesRead)
  {
    this.kBytesRead = bytesRead;
  }

  public Long getKBytesTransferred()
  {
    return this.kBytesTransferred;
  }

  public void setKBytesTransferred(Long bytesTransferred)
  {
    this.kBytesTransferred = bytesTransferred;
  }

  public Long getKBytesWritten()
  {
    return this.kBytesWritten;
  }

  public void setKBytesWritten(Long bytesWritten)
  {
    this.kBytesWritten = bytesWritten;
  }

  public Long getReadHitIos()
  {
    return this.readHitIos;
  }

  public void setReadHitIos(Long readHitIos)
  {
    this.readHitIos = readHitIos;
  }

  public Long getReadIos()
  {
    return this.readIos;
  }

  public void setReadIos(Long readIos)
  {
    this.readIos = readIos;
  }

  public Calendar getStatisticTime()
  {
    return this.statisticTime;
  }

  public void setStatisticTime(Calendar statisticTime)
  {
    this.statisticTime = statisticTime;
  }

  public Long getTotalIos()
  {
    return this.totalIos;
  }

  public void setTotalIos(Long totalIos)
  {
    this.totalIos = totalIos;
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

  public Set getCim_StorageExtent()
  {
    return this.cim_StorageExtent;
  }

  public void setCim_StorageExtent(Set cim_StorageExtent)
  {
    this.cim_StorageExtent = cim_StorageExtent;
  }

  public Long getBytesReceived()
  {
    return this.bytesReceived;
  }

  public void setBytesReceived(Long bytesReceived)
  {
    this.bytesReceived = bytesReceived;
  }

  public Long getBytesTransmitted()
  {
    return this.bytesTransmitted;
  }

  public void setBytesTransmitted(Long bytesTransmitted)
  {
    this.bytesTransmitted = bytesTransmitted;
  }

  public Long getCrcErrors()
  {
    return this.crcErrors;
  }

  public void setCrcErrors(Long crcErrors)
  {
    this.crcErrors = crcErrors;
  }

  public Long getDumpedFrames()
  {
    return this.dumpedFrames;
  }

  public void setDumpedFrames(Long dumpedFrames)
  {
    this.dumpedFrames = dumpedFrames;
  }

  public Long getErrorFrames()
  {
    return this.errorFrames;
  }

  public void setErrorFrames(Long errorFrames)
  {
    this.errorFrames = errorFrames;
  }

  public Long getInvalidTransmissionWords()
  {
    return this.invalidTransmissionWords;
  }

  public void setInvalidTransmissionWords(Long invalidTransmissionWords)
  {
    this.invalidTransmissionWords = invalidTransmissionWords;
  }

  public Long getLinkFailures()
  {
    return this.linkFailures;
  }

  public void setLinkFailures(Long linkFailures)
  {
    this.linkFailures = linkFailures;
  }

  public Long getLipCount()
  {
    return this.lipCount;
  }

  public void setLipCount(Long lipCount)
  {
    this.lipCount = lipCount;
  }

  public Long getLossOfSignalCounter()
  {
    return this.lossOfSignalCounter;
  }

  public void setLossOfSignalCounter(Long lossOfSignalCounter)
  {
    this.lossOfSignalCounter = lossOfSignalCounter;
  }

  public Long getNosCount()
  {
    return this.nosCount;
  }

  public void setNosCount(Long nosCount)
  {
    this.nosCount = nosCount;
  }

  public Long getPacketsReceived()
  {
    return this.packetsReceived;
  }

  public void setPacketsReceived(Long packetsReceived)
  {
    this.packetsReceived = packetsReceived;
  }

  public Long getPacketsTransmitted()
  {
    return this.packetsTransmitted;
  }

  public void setPacketsTransmitted(Long packetsTransmitted)
  {
    this.packetsTransmitted = packetsTransmitted;
  }

  public Long getPrimitiveSeqProtocolErrCount()
  {
    return this.primitiveSeqProtocolErrCount;
  }

  public void setPrimitiveSeqProtocolErrCount(Long primitiveSeqProtocolErrCount)
  {
    this.primitiveSeqProtocolErrCount = primitiveSeqProtocolErrCount;
  }
}