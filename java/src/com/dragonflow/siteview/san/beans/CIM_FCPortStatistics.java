package com.dragonflow.siteview.san.beans;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

public class CIM_FCPortStatistics
{
  private Set<CIM_FCPort> cim_FCPort = new HashSet();
  private String elementName;
  private String instanceId;
  private Integer id;
  private Calendar statisticTime;
  private Long statisticTimeDiff;
  private Long bytesTransmitted;
  private Long bytesTransmittedDiff;
  private Long bytesReceived;
  private Long bytesReceivedDiff;
  private Long packetsTransmitted;
  private Long packetsTransmittedDiff;
  private Long packetsReceived;
  private Long packetsReceivedDiff;
  private Long crcErrors;
  private Long linkFailures;
  private Long primitiveSeqProtocolErrCount;
  private Long lossOfSignalCounter;
  private Long invalidTransmissionWords;
  private Long lipCount;
  private Long nosCount;
  private Long errorFrames;
  private Long dumpedFrames;

  public Set<CIM_FCPort> getCim_FCPort()
  {
    return this.cim_FCPort;
  }

  public void setCim_FCPort(Set<CIM_FCPort> cim_FCPort)
  {
    this.cim_FCPort = cim_FCPort;
  }

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Calendar getStatisticTime()
  {
    return this.statisticTime;
  }

  public void setStatisticTime(Calendar statisticTime)
  {
    this.statisticTime = statisticTime;
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

  public void setBytesReceivedDiff(Long bytesReceivedDiff)
  {
    this.bytesReceivedDiff = bytesReceivedDiff;
  }

  public Long getBytesTransmittedDiff()
  {
    return this.bytesTransmittedDiff;
  }

  public void setBytesTransmittedDiff(Long bytesTransmittedDiff)
  {
    this.bytesTransmittedDiff = bytesTransmittedDiff;
  }

  public Long getPacketsReceivedDiff()
  {
    return this.packetsReceivedDiff;
  }

  public void setPacketsReceivedDiff(Long packetsReceivedDiff)
  {
    this.packetsReceivedDiff = packetsReceivedDiff;
  }

  public Long getPacketsTransmittedDiff()
  {
    return this.packetsTransmittedDiff;
  }

  public void setPacketsTransmittedDiff(Long packetsTransmittedDiff)
  {
    this.packetsTransmittedDiff = packetsTransmittedDiff;
  }

  public Long getStatisticTimeDiff()
  {
    return this.statisticTimeDiff;
  }

  public void setStatisticTimeDiff(Long statisticTimeDiff)
  {
    this.statisticTimeDiff = statisticTimeDiff;
  }

  public String getInstanceId() {
    return this.instanceId;
  }

  public void setInstanceId(String instanceId) {
    this.instanceId = instanceId;
  }

  public String getElementName() {
    return this.elementName;
  }

  public void setElementName(String elementName) {
    this.elementName = elementName;
  }

  public Long getBytesReceivedDiff() {
    return this.bytesReceivedDiff;
  }
}