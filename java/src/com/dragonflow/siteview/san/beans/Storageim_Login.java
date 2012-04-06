package com.dragonflow.siteview.san.beans;

public class Storageim_Login
{
  private Integer id;
  private String firstName;
  private String lastName;
  private String email;
  private String username;
  private String password;
  private Integer role;
  private String admin;

  public Storageim_Login()
  {
  }

  public Storageim_Login(Integer id)
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

  public String getUsername()
  {
    return this.username;
  }

  public void setUsername(String username)
  {
    this.username = username;
  }

  public String getPassword()
  {
    return this.password;
  }

  public void setPassword(String password)
  {
    this.password = password;
  }

  public Integer getRole()
  {
    return this.role;
  }

  public void setRole(Integer role)
  {
    this.role = role;
  }

  public String getFirstName()
  {
    return this.firstName;
  }

  public void setFirstName(String firstName)
  {
    this.firstName = firstName;
  }

  public String getLastName()
  {
    return this.lastName;
  }

  public void setLastName(String lastName)
  {
    this.lastName = lastName;
  }

  public String getEmail()
  {
    return this.email;
  }

  public void setEmail(String email)
  {
    this.email = email;
  }

  public String getAdmin()
  {
    return this.admin;
  }

  public void setAdmin(String admin)
  {
    this.admin = admin;
  }
}