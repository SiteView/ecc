package com.dragonflow.siteview.san.beans;

import java.util.Calendar;

public class HistoricalRollupHourlyConcretePools
{
  private Integer id;
  private Calendar timeOfCreation;
  private String timeOfCreationString;
  private Long allocated;
  private Long unallocated;
  private Long totalManagedSpace;

  public Integer getId()
  {
    return this.id;
  }

  public void setId(Integer id)
  {
    this.id = id;
  }

  public Calendar getTimeOfCreation()
  {
    return this.timeOfCreation;
  }

  public void setTimeOfCreation(Calendar timeOfCreation)
  {
    this.timeOfCreation = timeOfCreation;
  }

  public String getTimeOfCreationString()
  {
    return this.timeOfCreationString;
  }

  public void setTimeOfCreationString(String timeOfCreationString)
  {
    this.timeOfCreationString = timeOfCreationString;
  }

  public Long getAllocated()
  {
    return this.allocated;
  }

  public void setAllocated(Long allocated)
  {
    this.allocated = allocated;
  }

  public Long getUnallocated()
  {
    return this.unallocated;
  }

  public void setUnallocated(Long unallocated)
  {
    this.unallocated = unallocated;
  }

  public Long getTotalManagedSpace()
  {
    return this.totalManagedSpace;
  }

  public void setTotalManagedSpace(Long totalManagedSpace)
  {
    this.totalManagedSpace = totalManagedSpace;
  }
}