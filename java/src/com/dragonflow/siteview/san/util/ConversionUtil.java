package com.dragonflow.siteview.san.util;

import java.io.PrintStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class ConversionUtil
{
  public long dateToLong(Date date)
  {
    long millisecondDate = date.getTime();
    return millisecondDate;
  }

  public Date stringToDate(String s)
    throws ParseException
  {
    SimpleDateFormat df = new SimpleDateFormat("yyyy-mm-dd hh:mm:ss");
    Date date = df.parse(s);
    return date;
  }

  public Calendar stringToCalendar(String cal)
  {
    Calendar calendar = Calendar.getInstance();
    return calendar;
  }

  public Date calanderToDate(Calendar cal)
  {
    Date date = new Date();
    return date;
  }

  public long calanderToMilli(Calendar cal)
  {
    long millis = 0L;
    return millis;
  }

  public static void main(String[] args)
    throws Exception
  {
    ConversionUtil cu = new ConversionUtil();

    Date stringToDateTest = cu.stringToDate("2009-12-09 09:00:00");
    System.out.println("stringToDate1 = " + stringToDateTest);

    long dateToMillisecondTest = cu.dateToLong(stringToDateTest);
    System.out.println("dateToMillisecond1 = " + dateToMillisecondTest);

    Date stringToDateTest1 = cu.stringToDate("2009-02-09 09:00:00");
    System.out.println("stringToDate2 = " + stringToDateTest1);

    long dateToMillisecondTest1 = cu.dateToLong(stringToDateTest1);
    System.out.println("dateToMillisecond2 = " + dateToMillisecondTest1);

    long diff = dateToMillisecondTest1 - dateToMillisecondTest;
    System.out.println("Delta = " + diff);
  }
}