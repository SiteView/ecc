package com.dragonflow.siteview.san.beans;


public class Geography
{
  private Integer id;
  private String name;
  private String address;
  private float lat;
  private float lng;
  private String type;

  public Geography()
  {
  }

  public Geography(Integer id)
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

  public String getName()
  {
    return this.name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getAddress()
  {
    return this.address;
  }

  public void setAddress(String address)
  {
    this.address = address;
  }

  public float getLat()
  {
    return this.lat;
  }

  public void setLat(float lat)
  {
    this.lat = lat;
  }

  public float getLng()
  {
    return this.lng;
  }

  public void setLng(float lng)
  {
    this.lng = lng;
  }

  public String getType()
  {
    return this.type;
  }

  public void setType(String type)
  {
    this.type = type;
  }
}